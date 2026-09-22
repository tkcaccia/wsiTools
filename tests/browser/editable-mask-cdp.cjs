// Run with Chrome started using --remote-debugging-port and a disposable live
// viewer containing an editable TIFF annotation mask.
// WSITOOLS_CDP_PORT=9224 node tests/browser/editable-mask-cdp.cjs
const assert = require('node:assert/strict');
const fs = require('node:fs');

const port = Number(process.env.WSITOOLS_CDP_PORT || 9224);
let socket;
let sequence = 0;
const pending = new Map();

async function connect() {
  const pages = await fetch(`http://127.0.0.1:${port}/json/list`).then(response => response.json());
  const page = pages.find(item => item.type === 'page' && /wsitools-mask-live-installed\.html/.test(item.url));
  assert.ok(page, 'Editable mask test page was not found');
  socket = new WebSocket(page.webSocketDebuggerUrl);
  await new Promise((resolve, reject) => {
    socket.addEventListener('open', resolve, { once: true });
    socket.addEventListener('error', reject, { once: true });
  });
  socket.addEventListener('message', event => {
    const message = JSON.parse(event.data);
    if (!message.id || !pending.has(message.id)) return;
    const callback = pending.get(message.id);
    pending.delete(message.id);
    if (message.error) callback.reject(new Error(message.error.message));
    else callback.resolve(message.result);
  });
}

function command(method, params = {}) {
  const id = ++sequence;
  return new Promise((resolve, reject) => {
    pending.set(id, { resolve, reject });
    socket.send(JSON.stringify({ id, method, params }));
  });
}

async function evaluate(expression, awaitPromise = false) {
  const result = await command('Runtime.evaluate', {
    expression,
    awaitPromise,
    returnByValue: true
  });
  if (result.exceptionDetails) {
    throw new Error(result.exceptionDetails.exception?.description || result.exceptionDetails.text);
  }
  return result.result.value;
}

async function mouse(type, x, y, button = 'none', buttons = 0) {
  await command('Input.dispatchMouseEvent', { type, x, y, button, buttons });
}

(async () => {
  await connect();
  await command('Runtime.enable');
  await command('Page.enable');
  await evaluate(`new Promise((resolve, reject) => {
    const deadline = Date.now() + 30000;
    const poll = () => {
      if (typeof osdReady !== 'undefined' && osdReady && activeEditableAnnotationMaskSource()) resolve(true);
      else if (Date.now() > deadline) reject(new Error('Viewer or editable mask did not become ready'));
      else setTimeout(poll, 100);
    };
    poll();
  })`, true);

  const initial = await evaluate(`({
    source: activeEditableAnnotationMaskSource().id,
    maskCount: annotationMaskPayload().length,
    maskVisible: visibleChannelSources().some(source => source.metadata?.editable_annotation_mask === true),
    errors: viewerLogPayload().filter(item => item.level === 'error').map(item => item.message)
  })`);
  assert.equal(initial.maskCount, 0);
  assert.equal(initial.maskVisible, true);
  assert.deepEqual(initial.errors, []);

  await evaluate(`document.getElementById('toolBrush').click()`);
  assert.equal(await evaluate(`mode`), 'brush');
  const brush = await evaluate(`slideToCanvas({ x: 680, y: 330 })`);
  await mouse('mouseMoved', brush.x - 30, brush.y - 10);
  await mouse('mousePressed', brush.x - 30, brush.y - 10, 'left', 1);
  for (let step = 1; step <= 12; step += 1) {
    await mouse(
      'mouseMoved',
      brush.x - 30 + step * 5,
      brush.y - 10 + step * 2,
      'left',
      1
    );
  }
  await mouse('mouseReleased', brush.x + 30, brush.y + 14, 'left', 0);
  await evaluate(`new Promise(resolve => {
    const poll = () => (!brushing && !wsiEditPromise) ? resolve(true) : setTimeout(poll, 50);
    poll();
  })`, true);

  const afterBrush = await evaluate(`annotationMaskPayload()`);
  assert.ok(afterBrush.some(layer => layer.storage === 'browser_sparse_mask_tiles'));
  assert.ok(afterBrush.some(layer => layer.storage === 'browser_sparse_mask_erasures'));
  const brushRevision = Math.max(...afterBrush.map(layer => Number(layer.updated || 0)));

  await evaluate(`document.getElementById('toolWand').click()`);
  assert.equal(await evaluate(`mode`), 'wand');
  const wand = await evaluate(`slideToCanvas({ x: 800, y: 300 })`);
  await mouse('mousePressed', wand.x, wand.y, 'left', 1);
  await mouse('mouseReleased', wand.x, wand.y, 'left', 0);
  await evaluate(`new Promise((resolve, reject) => {
    const deadline = Date.now() + 30000;
    const poll = () => {
      if (!wandBusy) resolve(true);
      else if (Date.now() > deadline) reject(new Error('Wand edit timed out'));
      else setTimeout(poll, 50);
    };
    poll();
  })`, true);

  const finalState = await evaluate(`({
    mode,
    masks: annotationMaskPayload(),
    dirty: annotationsDirty,
    errors: viewerLogPayload().filter(item => item.level === 'error').map(item => item.message)
  })`);
  assert.equal(finalState.mode, 'wand');
  assert.equal(finalState.dirty, true);
  assert.ok(finalState.masks.every(layer => layer.tile_count > 0));
  assert.ok(
    Math.max(...finalState.masks.map(layer => Number(layer.updated || 0))) > brushRevision,
    'Wand must create a newer sparse mask revision'
  );
  assert.deepEqual(finalState.errors, []);

  const screenshot = await command('Page.captureScreenshot', { format: 'png' });
  fs.writeFileSync('/tmp/wsitools-editable-mask-after.png', Buffer.from(screenshot.data, 'base64'));
  console.log(JSON.stringify({ initial, afterBrush, finalState }, null, 2));
})().catch(error => {
  console.error(error);
  process.exitCode = 1;
}).finally(() => {
  if (socket) socket.close();
});
