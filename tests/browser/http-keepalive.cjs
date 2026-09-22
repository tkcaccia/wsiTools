// WSITOOLS_TEST_BRIDGE=http://127.0.0.1:PORT/viewer-state node tests/browser/http-keepalive.cjs
const http = require('node:http'), assert = require('node:assert/strict');
const url = new URL(process.env.WSITOOLS_TEST_BRIDGE);
assert.equal(url.hostname, '127.0.0.1');
const agent = new http.Agent({ keepAlive: true, maxSockets: 1 });
const request = (method, target = url, extra = {}) => new Promise((resolve, reject) => {
  const req = http.request(target, { agent, method, headers: {
    'Accept-Encoding': 'gzip', Origin: 'http://127.0.0.1:8972',
    'Access-Control-Request-Method': 'POST', 'Access-Control-Request-Headers': 'content-type', ...extra
  } }, response => {
    let bytes = 0;
    response.on('data', chunk => { bytes += chunk.length; });
    response.on('error', reject);
    response.on('end', () => resolve({ status: response.statusCode, headers: response.headers, bytes }));
  });
  req.on('error', reject); req.end();
});
(async () => {
  for (let i = 0; i < 20; i++) {
    const options = await request('OPTIONS');
    assert.equal(options.status, 204); assert.equal(options.bytes, 0);
    assert.equal(options.headers['content-encoding'], undefined);
    assert.equal(options.headers['transfer-encoding'], undefined);
    assert.equal((await request('GET')).status, 200);
    if (process.env.WSITOOLS_TEST_TILE_URL) {
      const tile = new URL(process.env.WSITOOLS_TEST_TILE_URL);
      const first = await request('GET', tile);
      const cached = await request('GET', tile, { 'If-None-Match': first.headers.etag });
      assert.equal(cached.status, 304); assert.equal(cached.bytes, 0);
      assert.equal(cached.headers['content-encoding'], undefined);
      assert.equal(cached.headers['transfer-encoding'], undefined);
    }
  }
  console.log('20 compressed preflight/GET cycles on a reused connection passed.');
})().catch(error => { console.error(error); process.exitCode = 1; }).finally(() => agent.destroy());
