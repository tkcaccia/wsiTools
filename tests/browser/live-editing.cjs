// Run against a disposable, live viewer with tissue annotations or four spatial images.
// WSITOOLS_TEST_URL=http://127.0.0.1:PORT/... node tests/browser/live-editing.cjs
const { chromium } = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const os = require('node:os');
const url = process.env.WSITOOLS_TEST_URL;
assert.ok(url && /^https?:\/\/(127\.0\.0\.1|localhost)(:|\/)/.test(url), 'Set a local disposable viewer URL');
const output = fs.mkdtempSync(path.join(os.tmpdir(), 'wsitools-editing-'));
let browser;

(async () => {
  browser = await chromium.launch({ headless: true, channel: process.env.PLAYWRIGHT_CHANNEL || 'chrome' });
  const page = await browser.newPage({ viewport: { width: 1600, height: 1100 } });
  const errors = [], failures = [], tileLevels = new Set();
  page.on('pageerror', error => errors.push(error.message));
  page.on('requestfailed', request => failures.push({ url: request.url(), error: request.failure() }));
  page.on('response', response => {
    const match = response.url().match(/\/(\d+)\/\d+[_/]\d+\.(jpg|png)/);
    if (response.ok() && match) tileLevels.add(Number(match[1]));
  });
  await page.goto(url, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => osdReady, {}, { timeout: 60000 });
  const multi = await page.evaluate(() => projectItems.length >= 4);
  let point, screen;
  if (multi) {
    await page.evaluate(() => setMultiViewLayout(4));
    await page.waitForFunction(() => multiViewPanes.length === 4 && multiViewPanes.every(p => p.viewer?.world.getItemCount()), {}, { timeout: 60000 });
    await page.waitForTimeout(4000);
    const rect = await page.locator('.multiViewPane').nth(1).boundingBox();
    screen = { x: rect.x + rect.width * .6, y: rect.y + rect.height * .5 };
    const before = await page.evaluate(() => multiViewPanes.map(p => p.viewer.viewport.getZoom()));
    await page.mouse.move(screen.x, screen.y); await page.mouse.wheel(0, -500);
    await page.waitForTimeout(1000);
    const after = await page.evaluate(() => multiViewPanes.map(p => p.viewer.viewport.getZoom()));
    assert.notEqual(after[1], before[1]);
    for (const i of [0, 2, 3]) assert.equal(after[i], before[i], 'Other panes retain independent zoom');
  } else {
    await page.waitForFunction(() => layers.some(l => l.items?.some(i => i.point_count > 2000)), {}, { timeout: 60000 });
    point = await page.evaluate(options => {
      const largest = options.largest;
      const li = layers.findIndex(l => l.items?.some(i => i.point_count > 2000));
      const ii = largest ? layers[li].items.reduce((best, item, i, list) =>
        !lockedRoi(item) && (best < 0 || item.point_count > list[best].point_count) ? i : best, -1) :
        layers[li].items.findIndex(i => i.point_count > 2000 && !lockedRoi(i));
      const item = layers[li].items[ii], groups = denseLayerEditableGroups(item);
      const group = options.largestPart ? groups.reduce((a,b) => a[0].length > b[0].length ? a : b) : groups[0];
      const p = group[0][0];
      window.testOriginalPointCount = item.point_count;
      const width = 9000, height = width * innerHeight / innerWidth;
      osdViewer.viewport.fitBounds(osdViewer.world.getItemAt(0).imageToViewportRectangle(p.x-width/2, p.y-height/2, width, height), true);
      selectLayerObject(li, ii); requestDraw(); return p;
    }, { largest:process.env.WSITOOLS_TEST_LARGEST === 'true', largestPart:process.env.WSITOOLS_TEST_LARGEST_PART === 'true' });
    await page.waitForTimeout(1500);
    screen = await page.evaluate(p => slideToCanvas(p), point);
  }
  await page.screenshot({ path: path.join(output, 'before.png') });
  await page.evaluate(() => document.getElementById('toolBrush').addEventListener('click', () => {
    const start = performance.now(); requestAnimationFrame(() => { window.testToolFeedbackMs = performance.now() - start; });
  }, { capture: true, once: true }));
  await page.locator('#toolBrush').click();
  await page.mouse.move(screen.x, screen.y); await page.waitForTimeout(100);
  const paths = await page.evaluate(() => wsiRuntimeMetrics.path_builds);
  for (let i = 0; i < 20; i++) await page.mouse.move(screen.x + i, screen.y + 10);
  assert.equal(await page.evaluate(() => wsiRuntimeMetrics.path_builds), paths, 'Idle brush does not rebuild annotation paths');
  const timings = [];
  const stroke = async (subtract = false) => {
    const modifier = process.platform === 'darwin' ? 'Meta' : 'Alt';
    if (subtract) await page.keyboard.down(modifier);
    await page.mouse.move(screen.x - 15, screen.y);
    const strokeStart = Date.now(); await page.mouse.down();
    await page.mouse.move(screen.x + (subtract ? 20 : 40), screen.y + 20, { steps: 15 });
    const start = Date.now();
    await page.mouse.up(); if (subtract) await page.keyboard.up(modifier);
    await page.waitForFunction(() => !wsiEditPromise && !brushing, {}, { timeout: 60000 });
    timings.push({ subtract, release_to_completion_ms: Date.now() - start, stroke_wall_ms:Date.now()-strokeStart });
  };
  await stroke();
  const beforeSubtract = await page.evaluate(() => JSON.stringify(roiGeojsonObject()));
  await stroke(true);
  assert.notEqual(await page.evaluate(() => JSON.stringify(roiGeojsonObject())), beforeSubtract);
  await page.keyboard.press('Control+z');
  assert.equal(await page.evaluate(() => JSON.stringify(roiGeojsonObject())), beforeSubtract, 'Undo restores exact geometry');
  if (multi) {
    const counts = await page.evaluate(() => multiViewPanes.map(p => multiViewPaneState(p).rois.length));
    assert.deepEqual(counts, [0, 1, 0, 0], 'Brush annotation belongs only to pane 2');
    assert.equal(await page.evaluate(() => applySeuratGeneColour('Mbp')), true, 'Live gene lookup succeeds');
  }
  await page.evaluate(() => syncViewerState('roi_updated', { test: true }));
  await page.waitForFunction(() => !wsiSyncSending && !wsiSyncQueue.length, {}, { timeout: 60000 });
  assert.ok(await page.evaluate(() => wsiSyncAcknowledged?.revision), 'R acknowledged the edit');
  fs.writeFileSync(path.join(output, 'browser-annotations.geojson'), await page.evaluate(() => JSON.stringify(roiGeojsonObject())));
  await page.screenshot({ path: path.join(output, 'edited.png') });
  const max = await page.evaluate(multi => {
    const viewer = multi ? multiViewPanes[1].viewer : osdViewer;
    // Request more than one source pixel per CSS pixel. A 2x request can stop
    // one level below level 0 on high-DPI/WebGL configurations.
    viewer.viewport.zoomTo(viewer.viewport.imageToViewportZoom(4), null, true);
    if (!multi) {
      const item = viewer.world.getItemAt(0), size = item.getContentSize();
      viewer.viewport.panTo(item.imageToViewportCoordinates(size.x / 2, size.y / 2), true);
    }
    return viewer.world.getItemAt(0).source.maxLevel;
  }, multi);
  const deadline = Date.now() + 30000;
  while (!tileLevels.has(max) && Date.now() < deadline) await page.waitForTimeout(250);
  assert.ok(tileLevels.has(max), 'Close zoom loads original-resolution tiles');
  await page.screenshot({ path: path.join(output, 'full-resolution.png') });
  const report = await page.evaluate(() => ({ performance: viewerPerformancePayload(), original_points: window.testOriginalPointCount, tool_feedback_ms: window.testToolFeedbackMs, logs: viewerLogPayload(), sync_errors: viewerSyncHistory.filter(e => e.direction === 'error') }));
  fs.writeFileSync(path.join(output, 'report.json'), JSON.stringify({ ...report, timings, errors, failures }, null, 2));
  assert.deepEqual(errors, []);
  assert.deepEqual(report.sync_errors, []);
  assert.deepEqual(report.logs.filter(l => l.level === 'error' && !l.message.includes('Image load aborted.')), [], 'No runtime errors other than cancelled obsolete tiles');
  assert.deepEqual(failures.filter(f => f.error?.errorText !== 'net::ERR_ABORTED'), [], 'No failed network requests');
  console.log('Live editing, subtraction, undo, R acknowledgement and full-resolution checks passed:', output);
})().catch(error => { console.error(error); process.exitCode = 1; }).finally(async () => { if (browser) await browser.close(); });
