// Verify a disposable live viewer keeps its first image and annotation source
// ready across navigation without re-downloading dense GeoJSON.
// WSITOOLS_TEST_URL=http://127.0.0.1:PORT/ node tests/browser/startup-performance.cjs
const { chromium } = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

const url = process.env.WSITOOLS_TEST_URL;
assert.ok(url && /^https?:\/\/(127\.0\.0\.1|localhost)(:|\/)/.test(url),
  'Set WSITOOLS_TEST_URL to a disposable local viewer URL');

const output = fs.mkdtempSync(path.join(os.tmpdir(), 'wsitools-startup-'));
let browser;

(async () => {
  browser = await chromium.launch({
    headless: true,
    channel: process.env.PLAYWRIGHT_CHANNEL || 'chrome'
  });
  const page = await browser.newPage({ viewport: { width: 1600, height: 1100 } });
  const started = Date.now();
  const errors = [];
  const failures = [];
  const annotationRequests = [];

  page.on('pageerror', error => errors.push(error.message));
  page.on('requestfailed', request => failures.push({
    url: request.url(),
    error: request.failure()
  }));
  page.on('request', request => {
    const requestUrl = request.url();
    if (/dense_geojson_sources|\/dense-geojson(?:\?|$)|\.geojson(?:\?|$)/i.test(requestUrl)) {
      annotationRequests.push(requestUrl);
    }
  });

  await page.goto(url, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => typeof osdReady !== 'undefined' && osdReady, {}, {
    timeout: 60000
  });
  const imageReadyMs = Date.now() - started;
  await page.waitForFunction(() => Array.isArray(layers) && layers.some(layer =>
    layer && layer.metadata && layer.metadata.static_source && Array.isArray(layer.items) && layer.items.length
  ), {}, { timeout: 60000 });
  const annotationReadyMs = Date.now() - started;

  const before = await page.evaluate(() => ({
    sourceCount: denseGeojsonStaticLoaded.size,
    annotationCount: layers.reduce((sum, layer) => sum +
      (layer && layer.metadata && layer.metadata.static_source && Array.isArray(layer.items)
        ? layer.items.length : 0), 0),
    tileCount: viewerPerformance.tile_loaded,
    sync: document.getElementById('syncStatus')?.textContent || '',
    heap: performance.memory ? performance.memory.usedJSHeapSize : null
  }));
  assert.ok(before.sourceCount > 0, 'At least one annotation source is retained in memory');
  assert.ok(before.annotationCount > 0, 'Annotations are visible before navigation');
  const requestsAfterLoad = annotationRequests.length;
  assert.ok(requestsAfterLoad > 0, 'The initial annotation source was requested');

  await page.evaluate(() => {
    const viewport = osdViewer.viewport;
    viewport.zoomBy(2, null, true);
    viewport.panBy(new OpenSeadragon.Point(0.04, 0.025), true);
    viewport.applyConstraints(true);
  });
  await page.waitForTimeout(1800);
  await page.evaluate(() => {
    const viewport = osdViewer.viewport;
    viewport.zoomBy(0.65, null, true);
    viewport.panBy(new OpenSeadragon.Point(-0.025, -0.015), true);
    viewport.applyConstraints(true);
  });
  await page.waitForTimeout(1800);

  const after = await page.evaluate(() => ({
    sourceCount: denseGeojsonStaticLoaded.size,
    annotationCount: layers.reduce((sum, layer) => sum +
      (layer && layer.metadata && layer.metadata.static_source && Array.isArray(layer.items)
        ? layer.items.length : 0), 0),
    tileCount: viewerPerformance.tile_loaded,
    denseRequests: viewerPerformance.dense_requests,
    heap: performance.memory ? performance.memory.usedJSHeapSize : null,
    performance: viewerPerformancePayload()
  }));
  assert.equal(annotationRequests.length, requestsAfterLoad,
    'Pan and zoom must not download the static GeoJSON again');
  assert.equal(after.sourceCount, before.sourceCount,
    'The retained annotation source survives navigation');
  assert.equal(after.annotationCount, before.annotationCount,
    'All retained tissue annotations survive navigation');
  assert.ok(after.tileCount >= before.tileCount, 'Tile delivery remains active during navigation');

  await page.evaluate(() => syncViewerState('viewport_changed', { acceptance_test: true }));
  await page.waitForFunction(() => !wsiSyncSending && !wsiSyncQueue.length, {}, {
    timeout: 30000
  });
  assert.ok(await page.evaluate(() => wsiSyncAcknowledged?.revision),
    'R acknowledges browser events after tile and annotation activity');

  await page.screenshot({ path: path.join(output, 'retained-after-navigation.png') });
  const report = {
    image_ready_ms: imageReadyMs,
    annotation_ready_ms: annotationReadyMs,
    annotation_network_requests: annotationRequests,
    before,
    after,
    errors,
    failures
  };
  fs.writeFileSync(path.join(output, 'report.json'), JSON.stringify(report, null, 2));
  assert.deepEqual(errors, []);
  assert.deepEqual(
    failures.filter(failure => failure.error?.errorText !== 'net::ERR_ABORTED'),
    [],
    'No required network request failed'
  );
  console.log('Startup/cache/navigation checks passed:', output);
  console.log(JSON.stringify(report));
})().catch(error => {
  console.error(error);
  process.exitCode = 1;
}).finally(async () => {
  if (browser) await browser.close();
});
