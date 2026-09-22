// Exercise rapid image switching and independent multi-view navigation.
// WSITOOLS_TEST_URL=http://127.0.0.1:PORT/ node tests/browser/multi-project-performance.cjs
const { chromium } = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

const url = process.env.WSITOOLS_TEST_URL;
assert.ok(url && /^https?:\/\/(127\.0\.0\.1|localhost)(:|\/)/.test(url),
  'Set WSITOOLS_TEST_URL to a disposable local viewer URL');

const output = fs.mkdtempSync(path.join(os.tmpdir(), 'wsitools-multi-project-'));
let browser;

(async () => {
  browser = await chromium.launch({
    headless: true,
    channel: process.env.PLAYWRIGHT_CHANNEL || 'chrome'
  });
  const page = await browser.newPage({ viewport: { width: 1600, height: 1100 } });
  const errors = [];
  const failures = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('requestfailed', request => failures.push({
    url: request.url(),
    error: request.failure()
  }));

  await page.goto(url, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => osdReady && projectItems.length >= 4, {}, {
    timeout: 60000
  });

  const switchResults = [];
  for (const index of [1, 2, 3, 0, 2, 1, 0]) {
    const result = await page.evaluate(async target => {
      const started = performance.now();
      switchProjectItem(target);
      const deadline = performance.now() + 30000;
      while (performance.now() < deadline) {
        const item = osdViewer.world && osdViewer.world.getItemAt(0);
        if (activeProjectIndex === target && item && item.source) {
          const max = Number(item.source.maxLevel);
          return {
            index: target,
            elapsed_ms: performance.now() - started,
            label: projectItems[target].label,
            tile_url: String(item.source.getTileUrl(max, 0, 0))
          };
        }
        await new Promise(resolve => setTimeout(resolve, 50));
      }
      throw new Error('Timed out switching project image to index ' + target);
    }, index);
    switchResults.push(result);
  }
  assert.equal(new Set(switchResults.map(result => result.label)).size, 4,
    'Rapid switching reaches all four project images');
  assert.equal(new Set(switchResults.slice(0, 4).map(result => result.tile_url)).size, 4,
    'Each project image retains a distinct fingerprinted tile source');

  await page.evaluate(() => setMultiViewLayout(4));
  await page.waitForFunction(() => multiViewPanes.length === 4 &&
    multiViewPanes.every(pane => pane.viewer && pane.viewer.world.getItemCount()), {}, {
    timeout: 60000
  });
  await page.waitForTimeout(1200);
  const beforeZoom = await page.evaluate(() => multiViewPanes.map(pane =>
    pane.viewer.viewport.getZoom()));
  const paneRect = await page.locator('.multiViewPane').nth(2).boundingBox();
  // Stay clear of the resizable left-side panels while exercising a tissue pane.
  const pointer = { x: paneRect.x + paneRect.width * 0.84, y: paneRect.y + paneRect.height / 2 };
  const hitBeforeWheel = await page.evaluate(point => {
    window.testMultiWheelEvents = 0;
    document.querySelectorAll('.multiViewPaneOverlay').forEach(node =>
      node.addEventListener('wheel', () => { window.testMultiWheelEvents += 1; }, {
        capture: true,
        once: true
      }));
    const hit = document.elementFromPoint(point.x, point.y);
    return {
      tag: hit && hit.tagName,
      className: hit && hit.className,
      active: multiViewActiveIndex
    };
  }, pointer);
  await page.mouse.move(pointer.x, pointer.y);
  await page.mouse.wheel(0, -520);
  await page.waitForTimeout(900);
  const zoomState = await page.evaluate(() => ({
    zooms: multiViewPanes.map(pane => pane.viewer.viewport.getZoom()),
    wheelEvents: window.testMultiWheelEvents || 0,
    active: multiViewActiveIndex
  }));
  const afterZoom = zoomState.zooms;
  assert.equal(hitBeforeWheel.className, 'multiViewPaneOverlay',
    'The physical wheel event targets the pane overlay');
  assert.notEqual(afterZoom[2], beforeZoom[2], 'The active pane zooms independently');
  for (const index of [0, 1, 3]) {
    assert.equal(afterZoom[index], beforeZoom[index],
      'An inactive pane does not move when another pane zooms');
  }

  await page.evaluate(() => syncViewerState('multi_view_layout_updated', {
    layout: 4,
    acceptance_test: true
  }));
  await page.waitForFunction(() => !wsiSyncSending && !wsiSyncQueue.length, {}, {
    timeout: 30000
  });
  assert.ok(await page.evaluate(() => wsiSyncAcknowledged?.revision),
    'R remains responsive after rapid switching and multi-view navigation');

  await page.screenshot({ path: path.join(output, 'four-pane-project.png') });
  const report = await page.evaluate(results => ({
    switches: results,
    pane_labels: multiViewPanes.map(pane => pane.entry && pane.entry.label),
    pane_zooms: multiViewPanes.map(pane => pane.viewer.viewport.getZoom()),
    heap: performance.memory ? performance.memory.usedJSHeapSize : null,
    performance: viewerPerformancePayload()
  }), switchResults);
  fs.writeFileSync(path.join(output, 'report.json'), JSON.stringify({
    ...report,
    errors,
    failures
  }, null, 2));
  assert.deepEqual(errors, []);
  assert.deepEqual(
    failures.filter(failure => failure.error?.errorText !== 'net::ERR_ABORTED'),
    [],
    'No required project tile request failed'
  );
  console.log('Multi-image switching and independent-view checks passed:', output);
  console.log(JSON.stringify(report));
})().catch(error => {
  console.error(error);
  process.exitCode = 1;
}).finally(async () => {
  if (browser) await browser.close();
});
