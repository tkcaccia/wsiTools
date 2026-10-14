// Open a disposable wsiTools viewer in Chrome with --remote-debugging-port=9338.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const port = Number(process.env.WSITOOLS_CDP_PORT || 9338);
let socket, sequence = 0;
const pending = new Map();

function command(method, params = {}) {
  return new Promise((resolve, reject) => {
    const id = ++sequence;
    pending.set(id, { resolve, reject });
    socket.send(JSON.stringify({ id, method, params }));
  });
}

async function evaluate(expression, awaitPromise = false) {
  const response = await command('Runtime.evaluate', { expression, awaitPromise, returnByValue: true });
  if (response.exceptionDetails) throw new Error(response.exceptionDetails.exception?.description || response.exceptionDetails.text);
  return response.result.value;
}

(async () => {
  const pages = await fetch(`http://127.0.0.1:${port}/json/list`).then(response => response.json());
  const page = pages.find(item => item.type === 'page' && /wsitools-annotation-layer-smoke\.html/.test(item.url));
  assert.ok(page, 'Open the disposable annotation-layer viewer in Chrome first');
  socket = new WebSocket(page.webSocketDebuggerUrl);
  await new Promise((resolve, reject) => {
    socket.addEventListener('open', resolve, { once: true });
    socket.addEventListener('error', reject, { once: true });
  });
  socket.addEventListener('message', event => {
    const message = JSON.parse(event.data), callback = pending.get(message.id);
    if (!callback) return;
    pending.delete(message.id);
    if (message.error) callback.reject(new Error(message.error.message));
    else callback.resolve(message.result);
  });
  await command('Runtime.enable');
  await command('Page.enable');
  const loaded = new Promise(resolve => {
    const onMessage = event => {
      if (JSON.parse(event.data).method !== 'Page.loadEventFired') return;
      socket.removeEventListener('message', onMessage);
      resolve();
    };
    socket.addEventListener('message', onMessage);
  });
  await command('Page.reload', { ignoreCache: true });
  await loaded;
  await evaluate(`new Promise(resolve => {
    const poll = () => typeof rois !== 'undefined' && typeof wsiApplyWandEdit === 'function'
      ? resolve(true) : setTimeout(poll, 50);
    poll();
  })`, true);
  const setup = await evaluate(`(() => {
    const ring = [[10,10],[110,10],[110,110],[10,110],[10,10]].map(([x,y])=>({x,y}));
    rois.push({id:'drawn-test',name:'Drawn tumour',class:'Tumour',colour:'#e46528',
      fill:'rgba(228,101,40,.18)',source:'brush',rings:[ring],add_groups:[],
      add_rings:[],subtract_rings:[],drawable:true});
    refreshRoiGeometry(rois[0]);
    const feature={type:'Feature',id:'imported-test',properties:{name:'Imported stroma',
      classification:{name:'Stroma',color:[50,170,90]}},geometry:{type:'Polygon',
      coordinates:[[[110,20],[190,20],[190,100],[110,100],[110,20]]]}};
    const item=denseStaticFeatureItem(feature,0,{name:'Tissue annotation',source_type:'annotation'});
    upsertViewerLayer({id:'imported-test-layer',name:'Seurat tissue annotation',
      type:'vector',source_type:'annotation',items:[item],visible:true});
    selectAnnotation(0);buildRoiList();updateButtons();wsiRequestDraw();
    return {note:document.getElementById('selectedAnnotationText').textContent,
      visible:getComputedStyle(document.getElementById('selectedAnnotationNote')).display!=='none',
      enabled:!document.getElementById('roiToggle').disabled,
      importedListed:document.querySelectorAll('#roiList .roiItem[data-layer-index]').length,
      annotationRows:document.querySelectorAll('#roiList .roiItem').length,
      kind:overlayFocusLayerKind(layers.find(layer=>layer.id==='imported-test-layer'))};
  })()`);
  assert.equal(setup.note, 'Selected: Drawn tumour');
  assert.equal(setup.visible, true);
  assert.equal(setup.enabled, true);
  assert.equal(setup.importedListed, 1, 'Imported tissue appears in the Annotations panel');
  assert.equal(setup.annotationRows, 2, 'Imported and drawn regions share one Annotations list');
  assert.equal(setup.kind, 'annotation');
  const visibility = await evaluate(`(() => {
    const layer=layers.find(layer=>layer.id==='imported-test-layer');
    document.getElementById('roiToggle').click();
    const hidden=!showRois&&!overlayFocusLayerAllowed(layer);
    document.getElementById('roiToggle').click();
    selectLayerObject(layers.indexOf(layer),0);wsiRequestDraw();
    return {hidden,shown:showRois&&overlayFocusLayerAllowed(layer),
      note:document.getElementById('selectedAnnotationText').textContent,
      listedActive:document.querySelector('#roiList .roiItem[data-layer-index]')?.classList.contains('active')};
  })()`);
  assert.equal(visibility.hidden, true);
  assert.equal(visibility.shown, true);
  assert.equal(visibility.note, 'Selected: Imported stroma');
  assert.equal(visibility.listedActive, true, 'Imported row follows viewer selection');
  const result = await evaluate(`(async () => {
    selectAnnotation(0);
    const before=JSON.stringify(wsiRoiCoordinates(rois[0]));
    await wsiApplyWandEdit([[[{x:105,y:40},{x:145,y:40},{x:145,y:70},
      {x:105,y:70},{x:105,y:40}]]],0,'extend');
    const target=rois[0], neighbour=layers.find(layer=>layer.id==='imported-test-layer').items[0];
    const edited={insideOriginal:roiContainsPoint(target,{x:20,y:20}),
      insideNeighbour:roiContainsPoint(target,{x:130,y:50}),
      neighbourIntact:roiContainsPoint(neighbour,{x:130,y:50}),
      changed:JSON.stringify(wsiRoiCoordinates(target))!==before};
    wsiRestoreEditHistory();
    return {...edited,undoExact:JSON.stringify(wsiRoiCoordinates(rois[0]))===before,
      note:document.getElementById('selectedAnnotationText').textContent,
      errors:viewerLogPayload().filter(entry=>entry.level==='error').map(entry=>entry.message)};
  })()`, true);
  assert.equal(result.insideOriginal, true, 'Original annotation survives Wand extension');
  assert.equal(result.insideNeighbour, false, 'Wand does not overlap imported tissue annotation');
  assert.equal(result.neighbourIntact, true, 'Imported neighbor is unchanged');
  assert.equal(result.changed, true, 'Wand extends into available space');
  assert.equal(result.undoExact, true, 'Undo restores the exact annotation');
  assert.deepEqual(result.errors, []);
  const promotion = await evaluate(`(() => {
    const layer=layers.find(layer=>layer.id==='imported-test-layer');
    rois.push({id:'imported-test',name:'Imported stroma',class:'Stroma',drawable:false});
    selectLayerObject(layers.indexOf(layer),0);
    const index=promoteSelectedLayerAnnotationForEdit();
    return {index,count:rois.filter(roi=>roi.id==='imported-test').length,
      layerCount:layer.items.length,drawable:isDrawable(rois[index]),selected:selectedRoi===index,
      importedRows:document.querySelectorAll('#roiList .roiItem[data-layer-index]').length};
  })()`);
  assert.equal(promotion.count, 1, 'Imported annotation and editable ROI are one logical object');
  assert.equal(promotion.layerCount, 0, 'Promoted item is removed from the imported vector source');
  assert.equal(promotion.drawable, true);
  assert.equal(promotion.selected, true);
  assert.equal(promotion.importedRows, 0, 'Promotion leaves one editable panel row');
  const screenshot = await command('Page.captureScreenshot', { format:'png', captureBeyondViewport:false });
  fs.writeFileSync('/tmp/wsitools-annotation-layer-smoke.png', Buffer.from(screenshot.data, 'base64'));
  console.log('Shared annotation layer, selection note, non-overlap and undo passed');
})().catch(error => { console.error(error); process.exitCode = 1; }).finally(() => socket?.close());
