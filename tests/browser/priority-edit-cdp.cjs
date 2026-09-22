// Run with Chrome started using --remote-debugging-port and the synthetic
// wsiTools priority-edit viewer open.
// WSITOOLS_CDP_PORT=9334 node tests/browser/priority-edit-cdp.cjs
const assert = require('node:assert/strict');
const fs = require('node:fs');

const port = Number(process.env.WSITOOLS_CDP_PORT || 9334);
let socket;
let sequence = 0;
const pending = new Map();

async function command(method, params = {}) {
  const id = ++sequence;
  return new Promise((resolve, reject) => {
    pending.set(id, { resolve, reject });
    socket.send(JSON.stringify({ id, method, params }));
  });
}

async function evaluate(expression, awaitPromise = false) {
  const result = await command('Runtime.evaluate', { expression, awaitPromise, returnByValue: true });
  if (result.exceptionDetails) throw new Error(result.exceptionDetails.exception?.description || result.exceptionDetails.text);
  return result.result.value;
}

(async () => {
  const pages = await fetch(`http://127.0.0.1:${port}/json/list`).then(response => response.json());
  const page = pages.find(item => item.type === 'page' && /wsitools-priority-edit\.html/.test(item.url));
  assert.ok(page, 'Priority-edit test page was not found');
  socket = new WebSocket(page.webSocketDebuggerUrl);
  await new Promise((resolve, reject) => {
    socket.addEventListener('open', resolve, { once: true });
    socket.addEventListener('error', reject, { once: true });
  });
  socket.addEventListener('message', event => {
    const message = JSON.parse(event.data);
    if (!message.id || !pending.has(message.id)) return;
    const callback = pending.get(message.id); pending.delete(message.id);
    if (message.error) callback.reject(new Error(message.error.message)); else callback.resolve(message.result);
  });
  await command('Runtime.enable');
  await evaluate(`new Promise(resolve => {
    const poll = () => typeof rois !== 'undefined' && rois.length === 2 ? resolve(true) : setTimeout(poll, 50);
    poll();
  })`, true);
  const before = await evaluate(`(() => {
    const neighbour = rois[1];
    rois.splice(1, 1);
    layers.push({ id:'dense_tissue_test', name:'Tissue annotation', source_type:'tissue_annotation',
      visible:true, items:[neighbour], count:1 });
    selectAnnotation(0, false);
    if (typeof setRoiPanelOpen === 'function') setRoiPanelOpen(false);
    if (typeof setProjectPanelOpen === 'function') setProjectPanelOpen(false);
    ['workspacePanel','annotationHistory'].forEach(id=>{const node=document.getElementById(id);if(node)node.style.display='none';});
    brushPriorityModifier = () => true;
    document.getElementById('toolBrush').click();
    const area = roi => positiveRingGroups(roi).reduce((sum, group) => sum + polygonArea(group), 0);
    return { targetId:rois[0].id, neighbourId:neighbour.id, targetArea:area(rois[0]),
      neighbourArea:area(neighbour), screen:slideToCanvas({x:300,y:210}) };
  })()`);
  const started = Date.now();
  await evaluate(`(async () => {
    const event={altKey:false,metaKey:false,shiftKey:false,ctrlKey:false,getModifierState:key=>key==='CapsLock'};
    startBrush({x:275,y:205},event);
    if(!brushing)throw new Error('Brush stroke did not start');
    for(let step=1;step<=16;step++)addBrushPoint({x:275+step*5,y:205+step},event);
    await finishBrush();
    if(wsiEditPromise)await wsiEditPromise;
    return true;
  })()`, true);
  const final = await evaluate(`(() => {
    const area = roi => roi ? positiveRingGroups(roi).reduce((sum, group) => sum + polygonArea(group), 0) : 0;
    const target=rois.find(roi=>String(roi.id)===${JSON.stringify(before.targetId)}), neighbour=rois.find(roi=>String(roi.id)===${JSON.stringify(before.neighbourId)});
    const point={x:335,y:217};
    return { ids:rois.map(roi=>roi.id), targetArea:area(target), neighbourArea:area(neighbour), targetOwns:!!target&&roiContainsPoint(target,point),
      neighbourOwns:!!neighbour&&roiContainsPoint(neighbour,point), layerItems:layers.find(layer=>layer.id==='dense_tissue_test').items.length,
      errors:viewerLogPayload().filter(item=>item.level==='error').map(item=>item.message) };
  })()`);
  console.log(JSON.stringify({ elapsed_ms:Date.now()-started, before, final }, null, 2));
  assert.ok(final.targetArea > before.targetArea, 'Selected annotation expands');
  assert.ok(final.neighbourArea < before.neighbourArea, 'Neighboring annotation is reduced');
  assert.equal(final.targetOwns, true, 'Selected annotation owns the claimed pixels');
  assert.equal(final.neighbourOwns, false, 'Claimed pixels are removed from the neighbor');
  assert.equal(final.layerItems, 0, 'Edited layer annotation is materialized exactly once');
  assert.deepEqual(final.errors, []);
  const screenshot = await command('Page.captureScreenshot', { format:'png', captureBeyondViewport:false });
  fs.writeFileSync('/tmp/wsitools-priority-edit-after.png', Buffer.from(screenshot.data, 'base64'));
})().catch(error => { console.error(error); process.exitCode = 1; }).finally(() => { if (socket) socket.close(); });
