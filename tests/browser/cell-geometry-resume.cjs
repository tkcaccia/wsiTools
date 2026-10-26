// Exercise real async outline/cache logic. A viewport can move while geometry
// is arriving, then return to cached positions before another point query runs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

const source = fs.readFileSync(
  process.env.WSITOOLS_CELL_SOURCE || require.resolve('../../inst/viewer/cell-workbench.js'), 'utf8'
);
const bounds = { xmin: 0, ymin: 0, xmax: 1000, ymax: 1000 };
const model = { model_id: 'pannuke', backbone: 'SAM', taxonomy: 'pannuke' };
const cell = {
  id: 't1-cell-1', cell_id: 'cell-1', source_id: 'cell-1',
  tissue_id: 't1', frame_id: 'frame', source_frame_id: 'frame', x: 100, y: 100,
  contour: [[95, 95], [105, 95], [105, 105], [95, 105]],
  predictions: { pannuke: { status: 'predicted', label: 'Inflammatory', probability: 0.9 } }
};
const entry = {
  indexed: true, tissue_id: 't1', models: [model], cells: [], cell_count: 1,
  source_sha256: 'source', index_reference: { sha256: 'index' },
  frame_id: 'frame', source_frame_id: 'frame', feature_set_id: 'features'
};
const nodes = {
  cellModel: { value: 'pannuke' }, cellShow: { checked: true },
  cellIndexStatus: { textContent: '' }
};
let outlineRequests = 0, pointRequests = 0, releaseFirst;
const response = {
  ok: true,
  json: async () => ({ cell_index: {
    source_sha256: 'source', index_sha256: 'index', feature_set_id: 'features',
    frame_id: 'frame', total: 1, cells: [cell], next_cursor: null
  } })
};
const context = vm.createContext({
  console, module: { exports: {} }, AbortController,
  cfg: { experiment: { cell_classification: [entry] }, slide_width: 10000, slide_height: 10000 },
  projectItems: [{ id: 't1', tissue_id: 't1', width: 10000, height: 10000 }],
  testEntry: entry, testCell: cell, testBounds: bounds,
  el: id => nodes[id] || null,
  wsiExperimentTissueId: () => 't1', predictionUrl: () => '/prediction',
  visibleSlideBounds: () => context.testBounds,
  hexToRgba: () => 'rgba(56,189,248,0.16)',
  fetch: async (url, options) => {
    const request = JSON.parse(options.body);
    if (request.mode === 'display_points') {
      pointRequests++;
      throw new Error('The valid cached point population must not be requested again');
    }
    assert.equal(request.mode, 'viewport');
    outlineRequests++;
    // Keep the first response pending so navigation can invalidate its scope.
    if (outlineRequests === 1) return new Promise(resolve => { releaseFirst = () => resolve(response); });
    return response;
  }
});
vm.runInContext(source, context);
vm.runInContext(`
  wsiCellDraw = () => {};
  const cachedDisplay = {
    key: wsiCellDisplayKey(testEntry, testEntry.models[0]),
    complete: true, loading: false, error: null, total: 1,
    regions: [testBounds], points: [testCell]
  };
  cachedDisplay.promise = Promise.resolve(cachedDisplay);
  wsiCellDisplayCache.set(testEntry, cachedDisplay);
  wsiCellActiveDisplay = cachedDisplay;
`, context);

(async () => {
  const first = context.wsiCellLoadGeometry();
  assert.equal(outlineRequests, 1);
  context.wsiCellDisplayStatus();
  assert.match(nodes.cellIndexStatus.textContent, /Loading cell outlines/);

  // The response belongs to the previous viewport and must not be accepted.
  context.testBounds = { xmin: 1000, ymin: 0, xmax: 2000, ymax: 1000 };
  releaseFirst();
  await first;
  const interrupted = context.wsiCellGeometryState(entry, model);
  assert.equal(interrupted, null, 'Geometry from a previous viewport is not current');
  context.testBounds = bounds;
  const cachedGeometry = context.wsiCellGeometryState(entry, model);
  assert.equal(cachedGeometry.loading, false);
  assert.equal(cachedGeometry.complete, false);
  assert.equal(cachedGeometry.error, null);
  assert.equal(cachedGeometry.parts.size, 0, 'The stale outline response was discarded');

  // This is the scheduler's ordinary cached-display path, with no forced reload.
  await context.wsiCellLoadDisplay();
  await Promise.resolve();
  assert.equal(outlineRequests, 2,
    'Returning to cached complete cell positions must resume interrupted outlines automatically');
  const resumed = context.wsiCellGeometryState(entry, model);
  await resumed.promise;
  assert.equal(pointRequests, 0, 'Resuming outlines must reuse the valid point population');
  assert.equal(resumed.complete, true);
  assert.equal(resumed.loading, false);
  assert.equal(resumed.outlined, 1);
  assert.equal(resumed.centroidOnly, 0);
  assert.doesNotMatch(nodes.cellIndexStatus.textContent, /Loading cell outlines/);
  assert.match(nodes.cellIndexStatus.textContent, /1 cell boundaries/);
  console.log('Cell geometry resume passed: stale viewport response discarded, cached positions reused and interrupted outlines completed automatically.');
})().catch(error => { console.error(error); process.exitCode = 1; });
