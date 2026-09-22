// Uses the real imported polygons in a disposable live colorectal viewer.
const { chromium } = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs'), path = require('node:path'), os = require('node:os');
const url = process.env.WSITOOLS_TEST_URL;
assert.match(url || '', /^http:\/\/(127\.0\.0\.1|localhost)(:|\/)/);
const output = fs.mkdtempSync(path.join(os.tmpdir(), 'wsitools-priority-'));
let browser;
(async () => {
  browser = await chromium.launch({ channel: 'chrome', headless: true });
  const page = await browser.newPage({ viewport: { width: 1600, height: 1100 } });
  const errors = []; page.on('pageerror', e => errors.push(e.message));
  await page.goto(url, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => osdReady && layers.some(l => l.items?.some(i => i.point_count > 100000)), {}, { timeout: 60000 });
  const input = await page.evaluate(() => {
    const li = layers.findIndex(l => l.items?.some(i => i.point_count > 100000)), layer = layers[li];
    const ii = layer.items.reduce((best, item, i, list) => !lockedRoi(item) &&
      (best < 0 || item.point_count > list[best].point_count) ? i : best, -1);
    const original = layer.items[ii], bounds = roiBounds(original);
    const neighbor = layer.items.find(item => item !== original && !lockedRoi(item) &&
      roiClassKey(item) !== roiClassKey(original) && boundsOverlap(roiBounds(item), bounds));
    if (!neighbor) throw new Error('No real neighbouring tissue polygon available');
    const p = denseLayerEditableGroups(neighbor)[0][0][0];
    selectLayerObject(li, ii); promoteSelectedLayerAnnotationForEdit();
    const width = 6500, height = width * innerHeight / innerWidth;
    osdViewer.viewport.fitBounds(osdViewer.world.getItemAt(0).imageToViewportRectangle(p.x-width/2,p.y-height/2,width,height), true);
    window.testClaim = [[Array.from({ length: 65 }, (_, i) => ({ x: p.x + 180 * Math.cos(i * Math.PI / 32), y: p.y + 180 * Math.sin(i * Math.PI / 32) }))]];
    window.testAnnotationState = () => JSON.stringify({ rois: rois.map(r => ({ id:r.id, coordinates:wsiRoiCoordinates(r) })),
      layers: layers.filter(l => /annotation|tissue/.test(l.source_type || '')).map(l => ({id:l.id, items:l.items.map(r => ({id:r.id,coordinates:wsiRoiCoordinates(r)}))})) });
    window.testBeforePriority = testAnnotationState();
    return { selected: original.id, vertices: original.point_count, neighbor: neighbor.id };
  });
  await page.waitForTimeout(800); await page.screenshot({ path: path.join(output, 'before.png') });
  // Exercise the same atomic worker/commit path used by Caps Lock + Wand.
  const result = await page.evaluate(async () => {
    const start = performance.now();
    await wsiApplyPriorityClaim(testClaim, selectedRoi, 'wand');
    const elapsed = performance.now() - start, transaction = annotationUndo.at(-1);
    const after = testAnnotationState();
    if (!transaction.wsi_delta || transaction.before.length < 2) throw new Error('Neighbour was not part of the priority transaction');
    restoreAnnotationUndo();
    const undoExact = testAnnotationState() === testBeforePriority;
    restoreAnnotationRedo();
    const redoExact = testAnnotationState() === after;
    return { elapsed_ms:elapsed, undo_exact:undoExact, redo_exact:redoExact, changed:transaction.before.length,
      kernel:wsiRuntimeMetrics.geometry_kernel, cache_bytes:wsiRuntimeMetrics.geometry_cache_bytes,
      before:transaction.before.map(e=>({id:e.id,geometry:e.snapshot&&wsiRoiCoordinates(e.snapshot)})),
      after:transaction.after.map(e=>({id:e.id,geometry:e.snapshot&&wsiRoiCoordinates(e.snapshot)})) };
  });
  await page.waitForFunction(() => !wsiSyncSending && !wsiSyncQueue.length, {}, { timeout: 60000 });
  const sync = await page.evaluate(async () => ({ errors: viewerSyncHistory.filter(e => e.direction === 'error'),
    acknowledged: wsiSyncAcknowledged?.revision, count: rois.length, response: await (await fetch(cfg.viewer_state_url)).json() }));
  await page.screenshot({ path: path.join(output, 'edited.png') });
  fs.writeFileSync(path.join(output, 'report.json'), JSON.stringify({ input, ...result, sync, errors }, null, 2));
  assert.equal(result.undo_exact, true); assert.equal(result.redo_exact, true);
  assert.equal(result.kernel, 'clipper2-wasm'); assert.deepEqual(errors, []);
  assert.deepEqual(sync.errors, []); assert.equal(sync.response.roi_count, sync.count);
  assert.ok(sync.acknowledged);
  console.log('Largest-polygon priority edit, imported-neighbour trimming and exact undo/redo passed:', output);
  console.log(JSON.stringify({ ...input, ...result, before:undefined, after:undefined, errors }));
})().catch(e => { console.error(e); process.exitCode = 1; }).finally(async () => { if (browser) await browser.close(); });
