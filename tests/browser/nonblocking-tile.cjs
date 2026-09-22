// Verify an uncached tile decode does not block browser-to-R synchronization.
// WSITOOLS_TEST_URL=http://127.0.0.1:PORT/ node tests/browser/nonblocking-tile.cjs
const { chromium } = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const assert = require('node:assert/strict');

const url = process.env.WSITOOLS_TEST_URL;
assert.ok(url && /^https?:\/\/(127\.0\.0\.1|localhost)(:|\/)/.test(url),
  'Set WSITOOLS_TEST_URL to a disposable local viewer URL');

let browser;
(async () => {
  browser = await chromium.launch({
    headless: true,
    channel: process.env.PLAYWRIGHT_CHANNEL || 'chrome'
  });
  const page = await browser.newPage({ viewport: { width: 1500, height: 1000 } });
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(url, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(() => osdReady && osdViewer.world.getItemAt(0), {}, {
    timeout: 60000
  });

  const result = await page.evaluate(async () => {
    const source = osdViewer.world.getItemAt(0).source;
    const level = Math.max(Number(source.minLevel || 0), Number(source.maxLevel || 1) - 8);
    const tileUrl = String(source.getTileUrl(level, 0, 0));
    const tileStarted = performance.now();
    const tilePromise = fetch(tileUrl, { cache: 'reload' }).then(async response => {
      const bytes = (await response.arrayBuffer()).byteLength;
      return { ok: response.ok, status: response.status, bytes, elapsed_ms: performance.now() - tileStarted };
    });

    const previousRevision = Number(wsiSyncAcknowledged && wsiSyncAcknowledged.revision || 0);
    const syncStarted = performance.now();
    syncViewerState('viewer_log_updated', {
      level: 'info',
      message: 'nonblocking tile acceptance probe'
    });
    while (performance.now() - syncStarted < 10000) {
      if (Number(wsiSyncAcknowledged && wsiSyncAcknowledged.revision || 0) > previousRevision) break;
      await new Promise(resolve => setTimeout(resolve, 20));
    }
    const syncElapsed = performance.now() - syncStarted;
    const acknowledged = Number(wsiSyncAcknowledged && wsiSyncAcknowledged.revision || 0) > previousRevision;
    const tile = await Promise.race([
      tilePromise,
      new Promise((_, reject) => setTimeout(() => reject(new Error('Tile decode timed out')), 60000))
    ]);
    return { level, tile_url: tileUrl, tile, acknowledged, sync_elapsed_ms: syncElapsed };
  });

  assert.equal(result.tile.ok, true, 'The uncached overview tile is eventually delivered');
  assert.equal(result.acknowledged, true, 'R acknowledges an event while the tile is decoding');
  assert.ok(result.sync_elapsed_ms < 2000,
    'Live synchronization remains responsive during tile decoding');
  assert.deepEqual(errors, []);
  console.log('Non-blocking tile/R-sync check passed:', JSON.stringify(result));
})().catch(error => {
  console.error(error);
  process.exitCode = 1;
}).finally(async () => {
  if (browser) await browser.close();
});
