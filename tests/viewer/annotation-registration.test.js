const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');

const source = fs.readFileSync(path.join(__dirname, '../../inst/viewer/annotation-registration.js'), 'utf8');
const transactions = fs.readFileSync(path.join(__dirname, '../../inst/viewer/edit-transactions.js'), 'utf8');
const controls = new Map([
  ['annotationRegistrationScope', { value: 'selected' }],
  ['annotationRegistrationMoveStep', { value: '50' }],
  ['annotationRegistrationScaleStep', { value: '1.05' }],
  ['annotationRegistrationRotationStep', { value: '5' }],
  ['annotationRegistrationSummary', { textContent: '' }],
  ['annotationRegistrationSubtitle', { textContent: '' }]
]);
const roi = (id, x) => ({ id, name: id, rings: [[
  { x, y: 20 }, { x: x + 10, y: 20 }, { x: x + 10, y: 30 },
  { x, y: 30 }, { x, y: 20 }
]], add_groups: [], subtract_rings: [], bbox: { xmin: x, xmax: x + 10, ymin: 20, ymax: 30 } });
const rois = [roi('first', 10), roi('second', 40)];
const events = [];
const context = {
  rois, layers: [], selectedRoi: 0, newRoiCount: 0, mode: 'pan',
  annotationUndo: [], annotationRedo: [],
  wsiWorkerSourceVersions: new Map(), wsiAnnotationEpoch: 0, wsiEditGeneration: 0,
  setTimeout() {},
  el: id => controls.get(id) || null,
  wsiProjectKey: () => 'slide-a',
  wsiRoiId: item => item.id,
  wsiSnapshotRoi: item => structuredClone(item),
  pushHistory: (stack, state) => stack.push(state),
  markAnnotationsDirty() {}, wsiResetBrush() {}, buildRoiList() {}, buildLayerList() {}, updateRoiList() {},
  wsiRequestDraw() {},
  wsiRefreshEditedAnnotations() {}, updateButtons() {}, draw() {},
  saveActiveProjectAnnotations() {}, recordAnnotationHistory: (...args) => events.push(args),
  scheduleViewerStateSync: (...args) => events.push(args),
  notify() {},
  isDrawable: item => !!item.rings?.length || !!item.dense_static_geometry,
  editableRoi: () => true, tissueAnnotationRoi: () => true, roiIsCellLike: () => false,
  clonePlain: value => structuredClone(value),
  denseLayerEditableGroups: item => [[item.raw_coordinates[0].map(([x, y]) => ({ x, y }))]],
  denseGeojsonPromotedIds: new Set(),
  denseGeojsonPromotionKey: (source, id) => source + '::' + id,
  roiBounds: item => item.bbox,
  positiveRingGroups: item => [item.rings, ...(item.add_groups || [])],
  subtractRings: item => item.subtract_rings || [],
  refreshRoiGeometry: item => {
    const points = item.rings.flat();
    item.bbox = {
      xmin: Math.min(...points.map(p => p.x)), xmax: Math.max(...points.map(p => p.x)),
      ymin: Math.min(...points.map(p => p.y)), ymax: Math.max(...points.map(p => p.y))
    };
  },
  unionBounds: (a, b) => ({
    xmin: Math.min(a.xmin, b.xmin), xmax: Math.max(a.xmax, b.xmax),
    ymin: Math.min(a.ymin, b.ymin), ymax: Math.max(a.ymax, b.ymax)
  })
};
vm.createContext(context);
vm.runInContext(transactions, context);
vm.runInContext(source, context);

assert.equal(context.annotationRegistrationMove(5, -3), true);
assert.equal(rois[0].bbox.xmin, 15);
assert.equal(rois[0].bbox.ymin, 17);
assert.equal(rois[1].bbox.xmin, 40);
assert.equal(context.annotationUndo.length, 1);
assert.ok(events.some(row => row[0] === 'roi_updated'));
assert.equal(context.wsiRestoreEditHistory(false), true);
assert.equal(rois[0].bbox.xmin, 10);
assert.equal(context.wsiRestoreEditHistory(true), true);
assert.equal(rois[0].bbox.xmin, 15);

context.annotationRegistrationScale('both', true);
assert.ok(rois[0].bbox.xmin < 15);
assert.ok(rois[0].bbox.xmax > 25);
context.annotationRegistrationReset();
assert.equal(rois[0].bbox.xmin, 10);
assert.equal(rois[0].bbox.ymin, 20);

controls.get('annotationRegistrationScope').value = 'all';
context.annotationRegistrationMove(-4, 0);
assert.equal(rois[0].bbox.xmin, 6);
assert.equal(rois[1].bbox.xmin, 36);

rois.push({ id: 'dense', drawable: false, rings: [], bbox: { xmin: 5, xmax: 15, ymin: 5, ymax: 15 } });
const layer = { id: 'dense_geojson_viewport_tissue', metadata: { static_source: true, source_id: 'tissue' }, items: [{
  id: 'dense', dense_static_geometry: true, geometry_type: 'Polygon',
  raw_coordinates: [[[5, 5], [15, 5], [15, 15], [5, 15], [5, 5]]],
  bbox: { xmin: 5, xmax: 15, ymin: 5, ymax: 15 }
}] };
context.layers.push(layer);
assert.equal(context.annotationRegistrationMove(2, 0), true);
assert.equal(rois.length, 3);
assert.equal(layer.items.length, 0);
assert.equal(rois[2].bbox.xmin, 7);
assert.ok(context.denseGeojsonPromotedIds.has('tissue::dense'));

console.log('annotation registration transforms, scope and reset passed');
