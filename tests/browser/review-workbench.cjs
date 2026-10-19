// CPU-only contracts: node --test tests/browser/review-workbench.cjs
const assert = require('node:assert/strict');
const test = require('node:test');
const workbench = require('../../inst/viewer/review-workbench.js');
const clone = value => JSON.parse(JSON.stringify(value));

function fixture() {
  const images = [], observations = [], tilePoints = [], spotPoints = [];
  for (const [index, tissue] of ['a', 'b', 'c'].entries()) {
    images.push({tissue_id: tissue, frame_id: tissue + '-native', sha256: String(index + 1).repeat(64),
      width: 40, height: 40, mpp_x: .5, mpp_y: .5, calibration_status: 'verified',
      coordinate_convention: workbench.coordinateConvention});
    for (let row = 0; row < 3; row++) for (let col = 0; col < 3; col++) {
      const original = 'tile-' + (row * 3 + col), id = tissue + '::tiles::' + original;
      observations.push({id, original_id: original, tissue_id: tissue, frame_id: tissue + '-native',
        observation_unit: 'tile', x: col * 10 + 5, y: row * 10 + 5,
        footprint: {x0: col * 10, y0: row * 10, x1: col * 10 + 10, y1: row * 10 + 10},
        footprint_width_px: 10, footprint_height_px: 10,
        cluster_values: {kodama: col < 2 ? '0' : '1', transcriptomic_hint: 'unreviewed'},
        model_sources: [{id: 'immutable-model', label: 'predicted stroma', scores: [.25, .75]}]});
      tilePoints.push({id, x: col, y: row});
    }
    const id = tissue + '::spots::barcode-1';
    observations.push({id, original_id: 'barcode-1', tissue_id: tissue, frame_id: tissue + '-native',
      observation_unit: 'spot', x: 7, y: 8, cluster_values: {seurat: '2'},
      dataset_id: 'rna', model_sources: []});
    spotPoints.push({id, x: index, y: 0});
  }
  return {schema: 'wsitools-review-context', schema_version: 1, context_id: 'three-section-review',
    source_signature: 'a'.repeat(64), experiment_id: 'test-study', images, observations,
    reductions: [{id: 'kodama', label: 'KODAMA', observation_unit: 'tile', points: tilePoints.reverse()},
      {id: 'rna', label: 'RNA reduction', observation_unit: 'spot', points: spotPoints.reverse()}]};
}

function create(extra) {
  const context = fixture(), calls = {selection: [], sync: [], exports: [], overlays: []};
  const host = Object.assign({context, getActiveTissueId: () => 'a',
    onSelection: (ids, rows) => calls.selection.push({ids, rows}),
    selectOverlay: rows => calls.overlays.push(rows), sync: snapshot => calls.sync.push(snapshot),
    export: rows => { calls.exports.push(rows); return rows; }}, extra);
  return {context, calls, ui: workbench.create(host)};
}
const square = (x0, y0, x1, y1) => [[x0, y0], [x1, y0], [x1, y1], [x0, y1], [x0, y0]];

test('three tissues may repeat local IDs while canonical IDs and model sources remain distinct', () => {
  const {context, ui} = create(), original = clone(context);
  assert.equal(workbench.validateContext(context).observations.length, 30);
  ui.selectIds(['c::tiles::tile-0', 'a::tiles::tile-0', 'b::tiles::tile-0']);
  assert.deepEqual(ui.selection(), ['a::tiles::tile-0', 'b::tiles::tile-0', 'c::tiles::tile-0']);
  ui.review({action: 'correct', label: 'human tumor', reviewer: 'reviewer-A'});
  assert.deepEqual(context, original);
  const copy = ui.selectedRows(); copy[0].model_sources[0].label = 'tampered copy';
  assert.equal(ui.selectedRows()[0].model_sources[0].label, 'predicted stroma');
  assert.equal(ui.snapshot().review_records.length, 3);
});

test('duplicate canonical IDs, unknown frames, invalid footprints and unsigned contexts fail', () => {
  let c = fixture(); c.observations.push(clone(c.observations[0]));
  assert.throws(() => workbench.create({context: c}), /Duplicate canonical/);
  c = fixture(); c.observations[0].frame_id = 'foreign';
  assert.throws(() => workbench.create({context: c}), /unknown image\/frame/);
  c = fixture(); c.observations[0].footprint.x1 = -1;
  assert.throws(() => workbench.create({context: c}), /footprint/);
  c = fixture(); delete c.source_signature;
  assert.throws(() => workbench.create({context: c}), /signature/);
  c = fixture(); c.images[0].coordinate_convention = 'screen';
  assert.throws(() => workbench.create({context: c}), /coordinate convention/);
});

test('shuffled reduction points join canonical IDs instead of positional row order', () => {
  const {ui, calls} = create();
  ui.selectReductionPolygon(square(-.1, -.1, .4, .4));
  assert.deepEqual(ui.selection(), ['a::tiles::tile-0']);
  assert.equal(calls.overlays.at(-1)[0].x, 5);
  ui.setActiveTissue('c');
  ui.selectReductionPolygon(square(-.1, -.1, .4, .4));
  assert.deepEqual(ui.selection(), ['c::tiles::tile-0']);
});

test('unknown or mixed-unit reduction IDs fail before a panel can attach', () => {
  const c = fixture(); c.reductions[0].points[0].id = 'unmatched';
  assert.throws(() => workbench.create({context: c}), /Unknown or duplicate reduction/);
  const mixed = fixture(); mixed.reductions[0].points[0].id = 'a::spots::barcode-1';
  assert.throws(() => workbench.create({context: mixed}), /units differ/);
});

test('cluster checkboxes are scoped to current tissue and observation unit', () => {
  const {ui} = create(); ui.selectCluster('kodama', '0');
  assert.equal(ui.selection().length, 6);
  assert.ok(ui.selectedRows().every(row => row.tissue_id === 'a' && row.observation_unit === 'tile'));
  ui.setActiveTissue('b'); ui.selectCluster('kodama', '0');
  assert.equal(ui.selection().length, 12);
  assert.equal(ui.selectedTileRows().length, 6);
  ui.selectCluster('kodama', '0', {mode: 'subtract'});
  assert.ok(ui.selectedRows().every(row => row.tissue_id === 'a'));
  ui.setReduction('rna'); ui.selectCluster('seurat', '2', {mode: 'replace'});
  assert.deepEqual(ui.selection(), ['b::spots::barcode-1']);
  assert.deepEqual(ui.selectedTileRows(), []);
});

test('empty selection never previews or exports all, including after switching tissues', () => {
  const {ui, calls} = create();
  assert.deepEqual(ui.selectedRows(), []); assert.deepEqual(ui.selectedTileRows(), []);
  assert.throws(() => ui.exportSelected(), /Empty selection exports nothing/);
  ui.selectIds(['a::tiles::tile-0', 'c::tiles::tile-8']);
  const exported = ui.exportSelected();
  assert.equal(exported.length, 1); assert.equal(exported[0].tissue_id, 'a');
  ui.setActiveTissue('b'); assert.deepEqual(ui.selectedTileRows(), []);
  assert.throws(() => ui.exportSelected(), /No observations are selected/);
  ui.selectIds([]); assert.equal(calls.exports.length, 1);
});

test('image lasso preserves holes and disconnected islands without a convex hull', () => {
  const {ui} = create();
  const donut = {type: 'Polygon', coordinates: [square(0, 0, 30, 30), square(10, 10, 20, 20)]};
  ui.selectImageGeometry(donut, {tissue_id: 'a', frame_id: 'a-native'});
  assert.equal(ui.selection().length, 8);
  assert.ok(!ui.selection().includes('a::tiles::tile-4'));
  const islands = {type: 'MultiPolygon', coordinates: [[square(0, 0, 10, 10)], [square(20, 20, 30, 30)]]};
  ui.selectImageGeometry(islands, {tissue_id: 'a', frame_id: 'a-native'});
  assert.deepEqual(ui.selection(), ['a::tiles::tile-0', 'a::tiles::tile-8']);
  assert.deepEqual(ui.selectedTileRows().map(row => row.footprint), [
    {x0: 0, y0: 0, x1: 10, y1: 10}, {x0: 20, y0: 20, x1: 30, y1: 30}]);
  assert.throws(() => ui.selectImageGeometry(donut, {tissue_id: 'b', frame_id: 'a-native'}), /exact tissue/);
});

test('replace/add/subtract/toggle are deterministic and rejected IDs are atomic', () => {
  const {ui} = create(); ui.selectIds(['a::tiles::tile-0']);
  ui.selectIds(['a::tiles::tile-1'], {mode: 'add'});
  ui.selectIds(['a::tiles::tile-0'], {mode: 'toggle'});
  assert.deepEqual(ui.selection(), ['a::tiles::tile-1']);
  const saved = ui.snapshot();
  assert.throws(() => ui.selectIds(['a::tiles::tile-0', 'unknown']), /Unknown observation/);
  assert.throws(() => ui.selectIds(['a::tiles::tile-0', 'a::tiles::tile-0']), /Duplicate selection/);
  assert.throws(() => ui.selectIds([], {mode: 'everything'}), /Selection mode/);
  assert.deepEqual(ui.snapshot(), saved);
  ui.selectIds(null); assert.deepEqual(ui.selection(), []);
});

test('confirm/correct/exclude/uncertain are explicit human records, never model replacements', () => {
  const {ui, context} = create(), original = clone(context);
  assert.throws(() => ui.review({action: 'confirm', label: 'tumor', reviewer: 'A'}), /Select observations/);
  ui.selectIds(['a::tiles::tile-0']);
  assert.throws(() => ui.review({action: 'confirm', reviewer: 'A'}), /Enter the class/);
  assert.throws(() => ui.review({action: 'confirm', label: 'tumor'}), /reviewer/);
  for (const [action, status] of [['confirm', 'reviewed'], ['correct', 'reviewed'], ['exclude', 'excluded'], ['uncertain', 'uncertain']]) {
    ui.review({action, label: 'human class', reviewer: 'A'});
    const record = ui.snapshot().review_records[0];
    assert.equal(record.action, action); assert.equal(record.status, status);
    assert.equal(record.label, status === 'reviewed' ? 'human class' : null);
    assert.match(record.updated_at, /Z$/);
  }
  assert.deepEqual(context, original);
});

test('durable checkpoint restores selection, decisions and both directions of undo history', () => {
  const {ui} = create(); ui.selectIds(['a::tiles::tile-0']);
  ui.review({action: 'confirm', label: 'tumor', reviewer: 'A'});
  ui.review({action: 'correct', label: 'stroma', reviewer: 'A'});
  ui.undo(); const checkpoint = JSON.parse(JSON.stringify(ui.snapshot()));
  const replay = create().ui; replay.restore(checkpoint);
  assert.deepEqual(replay.snapshot(), checkpoint);
  assert.equal(replay.snapshot().review_records[0].label, 'tumor');
  assert.equal(replay.redo(), true); assert.equal(replay.snapshot().review_records[0].label, 'stroma');
  replay.undo(); replay.undo(); assert.deepEqual(replay.snapshot().review_records, []);
  replay.undo(); assert.deepEqual(replay.selection(), []);
});

test('wrong context/signature, unknown history IDs and malformed reviews cannot partially restore', () => {
  const {ui} = create(); ui.selectIds(['a::tiles::tile-0']); const initial = ui.snapshot();
  for (const change of [s => { s.context_id = 'other'; }, s => { s.source_signature = 'f'.repeat(64); },
    s => { s.selected_ids = ['unknown']; }, s => { s.history.past.push({selected_ids: ['unknown'], review_records: []}); },
    s => { s.review_records = [{id: 'a::tiles::tile-0', status: 'reviewed', reviewer: 'A', updated_at: new Date().toISOString()}]; }]) {
    const wrong = clone(initial); change(wrong); assert.throws(() => ui.restore(wrong)); assert.deepEqual(ui.snapshot(), initial);
  }
});

test('unavailable calibration remains explicit and review works without fabricated physical units', () => {
  const c = fixture(); c.images[0].calibration_status = 'unavailable'; c.images[0].mpp_x = null; c.images[0].mpp_y = null;
  const ui = workbench.create({context: c}); ui.selectIds(['a::tiles::tile-0']);
  assert.equal(ui.selectedRows()[0].x, 5);
  const invalid = clone(c); invalid.images[0].calibration_status = 'verified';
  assert.throws(() => workbench.create({context: invalid}), /per-axis MPP/);
});

test('partial observation footprints are preserved and only explicit work planning handles image bounds', () => {
  const context = fixture(); context.observations[0].footprint.x0 = -2;
  context.observations[0].footprint_width_px = 12;
  const ui = workbench.create({context}); ui.selectIds(['a::tiles::tile-0']);
  assert.equal(ui.selectedRows()[0].footprint.x0, -2);
  const wrong = clone(context); wrong.observations[0].footprint_width_px = 10;
  assert.throws(() => workbench.create({context: wrong}), /Footprint width differs/);
});

test('explicit review subset cannot accidentally label hidden or unselected observations', () => {
  const {ui} = create(); ui.selectIds(['a::tiles::tile-0', 'b::tiles::tile-0']);
  ui.review({action: 'correct', label: 'tumor', reviewer: 'A', ids: ['a::tiles::tile-0']});
  assert.deepEqual(ui.snapshot().review_records.map(row => row.id), ['a::tiles::tile-0']);
  const before = ui.snapshot();
  assert.throws(() => ui.review({action: 'correct', label: 'tumor', reviewer: 'A', ids: ['c::tiles::tile-0']}), /only apply to selected/);
  assert.deepEqual(ui.snapshot(), before);
});

test('all-tissue cluster scope is opt-in and selection history is bounded', () => {
  const {ui} = create(); ui.setScope('all'); ui.selectCluster('kodama', '1');
  assert.equal(ui.selection().length, 9);
  for (let i = 0; i < 110; i++) ui.selectIds(i % 2 ? ['a::tiles::tile-0'] : []);
  assert.equal(ui.snapshot().history.past.length, 100);
});

test('RNA observations can plan and export work tiles without becoming image-tile observations', async () => {
  const context = fixture(), first = clone(context.observations.find(row => row.id === 'a::spots::barcode-1'));
  first.id = 'a::spots::barcode-2'; first.original_id = 'barcode-2'; first.x = 8;
  context.observations.push(first);
  const exports = [], inputs = [];
  const ui = workbench.create({context, getActiveTissueId: () => 'a', export: rows => { exports.push(rows); },
    plan: async rows => {
      inputs.push(clone(rows));
      return {context_id: context.context_id, source_signature: context.source_signature,
        tiles: [{id: 'a::work::0::0', tissue_id: 'a', frame_id: 'a-native',
          footprint: {x0: 0, y0: 0, x1: 10, y1: 10}, source_ids: rows.map(row => row.id)}]};
    }});
  ui.setReduction('rna'); ui.selectCluster('seurat', '2');
  assert.equal(ui.selection().length, 2); assert.deepEqual(ui.selectedTileRows(), []);
  const planned = await ui.planSelected();
  assert.equal(planned.tiles.length, 1); assert.equal(planned.tiles[0].source_ids.length, 2);
  assert.ok(inputs[0].every(row => row.observation_unit === 'spot'));
  ui.exportSelected(); assert.equal(exports[0].length, 2);
  assert.ok(exports[0].every(row => row.observation_unit === 'spot' && row.tissue_id === 'a'));
  assert.ok(context.observations.filter(row => row.id.includes('::spots::')).every(row => row.observation_unit === 'spot'));
  ui.selectIds([]); assert.equal(ui.workPlan(), null);
  assert.throws(() => ui.exportSelected(), /Empty selection/);
});

test('late or foreign work-tile plans cannot install after selection changes', async () => {
  let release;
  const {ui} = create({plan: () => new Promise(resolve => { release = resolve; })});
  ui.selectIds(['a::spots::barcode-1']); const pending = ui.planSelected();
  ui.selectIds([]); release({tiles: []});
  await assert.rejects(pending, /old work-tile plan was discarded/);
  assert.equal(ui.workPlan(), null);
  const foreign = create({plan: async () => ({tiles: [{id: 'foreign', tissue_id: 'b', frame_id: 'b-native',
    footprint: {x0: 0, y0: 0, x1: 10, y1: 10}, source_ids: ['b::spots::barcode-1']}]})}).ui;
  foreign.selectIds(['a::spots::barcode-1']);
  await assert.rejects(foreign.planSelected(), /foreign image tile/);
  assert.equal(foreign.workPlan(), null);
});

test('checkpoint and host callbacks cannot replace immutable model evidence', () => {
  const outputs = [];
  const {ui} = create({onSelection: (ids, rows) => { if (rows.length) rows[0].model_sources[0].label = 'callback mutation'; },
    selectOverlay: rows => outputs.push(rows)});
  ui.selectIds(['a::tiles::tile-0']);
  assert.equal(outputs.at(-1)[0].model_sources[0].label, 'predicted stroma');
  const before = ui.snapshot(), changed = clone(before);
  changed.review_records = [{id: 'a::tiles::tile-0', status: 'reviewed', label: 'human tumor',
    reviewer: 'A', updated_at: new Date().toISOString(), model_sources: [{label: 'forged model'}]}];
  assert.throws(() => ui.restore(changed), /must not overwrite/);
  assert.deepEqual(ui.snapshot(), before);
});

test('view state independently restores reduction, unit, cluster field and scope without changing review evidence', () => {
  const {ui} = create(); ui.selectIds(['a::spots::barcode-1']);
  const evidence = ui.snapshot();
  const view = {reduction_id: 'rna', scope: 'all', observation_unit: 'spot', cluster_field: 'seurat'};
  ui.restoreViewState(view); assert.deepEqual(ui.viewState(), view); assert.deepEqual(ui.snapshot(), evidence);
  const replay = create().ui; replay.restore(ui.snapshot()); replay.restoreViewState(clone(ui.viewState()));
  assert.deepEqual(replay.viewState(), view); assert.deepEqual(replay.snapshot(), evidence);
  for (const change of [v => { v.reduction_id = 'unknown'; }, v => { v.scope = 'unknown'; },
    v => { v.observation_unit = 'tile'; }, v => { v.cluster_field = 'missing'; }, v => { v.unknown = true; }]) {
    const wrong = clone(view); change(wrong); assert.throws(() => ui.restoreViewState(wrong));
    assert.deepEqual(ui.viewState(), view); assert.deepEqual(ui.snapshot(), evidence);
  }
});
