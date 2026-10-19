/* Human review of declared observations. This module never runs a model or job.
 *
 * WsiReviewWorkbench.create({context, getActiveTissueId, onSelection, sync,
 *   selectOverlay, draw, export, plan, previewTile, focusObservation}).mount(node)
 *
 * All joins use canonical observation IDs, never row positions/local barcodes.
 * snapshot()/restore() use wsitools-review-checkpoint v1 and exact context/source
 * identity. Human decisions and bounded undo/redo history are separate from the
 * immutable source observations. Empty selection always means no observations.
 * selectIds(), selectCluster(), image/reduction lassos update the same selection.
 * review({action,label,reviewer,ids?}) records explicit human decisions; the UI
 * scopes them to displayed selected observations. exportSelected() passes only
 * selected CURRENT-tissue observations to the host, including unmodified RNA
 * spots. planSelected() requests separate derived work tiles; it never retags a
 * spot/cell as an image tile. Image lassos preserve holes/disjoint polygons;
 * exports retain separate native footprints and never construct a convex hull.
 */
(function (root, factory) {
  'use strict';
  const api = factory();
  if (typeof module === 'object' && module.exports) module.exports = api;
  else root.WsiReviewWorkbench = api;
})(typeof globalThis !== 'undefined' ? globalThis : this, function () {
  'use strict';
  const CONVENTION = 'pixel-edge-origin_centres-in-native-pixels';
  const STATUSES = new Set(['reviewed', 'excluded', 'uncertain']);
  const ACTIONS = new Set(['confirm', 'correct', 'exclude', 'uncertain']);
  const MODES = new Set(['replace', 'add', 'subtract', 'toggle']);
  const clone = value => JSON.parse(JSON.stringify(value));
  const text = value => typeof value === 'string' && value.trim().length > 0;
  const finite = value => typeof value === 'number' && Number.isFinite(value);
  const assert = (condition, message) => { if (!condition) throw new Error(message); };
  const key = (tissue, frame) => JSON.stringify([tissue, frame]);

  function validateContext(input) {
    assert(input && input.schema === 'wsitools-review-context' && input.schema_version === 1,
      'Open a validated wsitools-review-context version 1 first.');
    assert(text(input.context_id) && text(input.experiment_id) && /^[a-f0-9]{64}$/i.test(input.source_signature || ''),
      'Review context needs its context ID, experiment ID and source signature.');
    assert(Array.isArray(input.images) && input.images.length && Array.isArray(input.observations),
      'Review context needs image identities and an observation table.');
    const context = clone(input), images = new Map(), tissues = new Set(), ids = new Map();
    for (const image of context.images) {
      assert(text(image.tissue_id) && text(image.frame_id) && /^[a-f0-9]{64}$/i.test(image.sha256 || ''),
        'Each review image needs a tissue, coordinate frame and SHA-256.');
      assert(Number.isInteger(image.width) && image.width > 0 && Number.isInteger(image.height) && image.height > 0,
        'Review image dimensions must be positive integers.');
      assert(image.coordinate_convention === CONVENTION, 'Review image coordinate convention is incompatible.');
      assert(image.same_section == null || image.same_section === true, 'Review mappings must refer to the same tissue section.');
      const unavailable = image.calibration_status === 'unavailable';
      assert(unavailable ? image.mpp_x == null && image.mpp_y == null :
        finite(image.mpp_x) && image.mpp_x > 0 && finite(image.mpp_y) && image.mpp_y > 0,
      'Review image needs verified per-axis MPP, or explicit unavailable calibration.');
      const imageKey = key(image.tissue_id, image.frame_id);
      assert(!images.has(imageKey) && !tissues.has(image.tissue_id), 'Duplicate review image tissue/frame identity.');
      images.set(imageKey, image);
      tissues.add(image.tissue_id);
    }
    for (const row of context.observations) {
      assert(text(row.id) && text(row.original_id) && text(row.observation_unit),
        'Each observation needs canonical/original IDs and its observation unit.');
      assert(['spot', 'cell', 'tile', 'token'].includes(row.observation_unit), 'Unknown review observation unit.');
      assert(!ids.has(row.id), 'Duplicate canonical observation ID: ' + row.id);
      const image = images.get(key(row.tissue_id, row.frame_id));
      assert(image, 'Observation refers to an unknown image/frame: ' + row.id);
      assert(finite(row.x) && finite(row.y) && row.x >= 0 && row.x <= image.width && row.y >= 0 && row.y <= image.height,
        'Observation coordinates must be finite native image pixels: ' + row.id);
      assert(row.cluster_values && typeof row.cluster_values === 'object' && !Array.isArray(row.cluster_values),
        'Observation cluster_values must be a metadata object.');
      for (const value of Object.values(row.cluster_values))
        assert(value == null || ['string', 'boolean'].includes(typeof value) || finite(value), 'Cluster values must be scalar metadata.');
      if (row.footprint != null) {
        const p = row.footprint;
        assert(p && ['x0', 'y0', 'x1', 'y1'].every(name => finite(p[name])) && p.x1 > p.x0 && p.y1 > p.y0 &&
          row.x >= p.x0 && row.x <= p.x1 && row.y >= p.y0 && row.y <= p.y1,
        'Observation footprint must be a native rectangle containing its representative point: ' + row.id);
        if (row.footprint_width_px != null) assert(Math.abs(row.footprint_width_px - (p.x1 - p.x0)) <= 1e-7, 'Footprint width differs from its native rectangle.');
        if (row.footprint_height_px != null) assert(Math.abs(row.footprint_height_px - (p.y1 - p.y0)) <= 1e-7, 'Footprint height differs from its native rectangle.');
      }
      for (const name of ['footprint_width_px', 'footprint_height_px'])
        if (row[name] != null) assert(finite(row[name]) && row[name] > 0, 'Observation footprint size must be positive.');
      ids.set(row.id, row);
    }
    assert(context.reductions == null || Array.isArray(context.reductions), 'Reductions must be an array.');
    context.reductions = context.reductions || [];
    const reductions = new Set();
    for (const reduction of context.reductions) {
      assert(text(reduction.id) && text(reduction.label) && text(reduction.observation_unit) && !reductions.has(reduction.id),
        'Each reduction needs a unique ID, label and observation unit.');
      reductions.add(reduction.id);
      assert(Array.isArray(reduction.points), 'Reduction points must be an ID-indexed array.');
      const seen = new Set();
      for (const point of reduction.points) {
        const row = ids.get(point.id);
        assert(row && !seen.has(point.id), 'Unknown or duplicate reduction observation ID: ' + point.id);
        assert(row.observation_unit === reduction.observation_unit, 'Reduction and observation units differ: ' + point.id);
        assert(finite(point.x) && finite(point.y), 'Reduction coordinates must be finite.');
        seen.add(point.id);
      }
    }
    return context;
  }

  function ringContains(point, ring) {
    let inside = false;
    for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
      const a = ring[j], b = ring[i], cross = (point[0] - a[0]) * (b[1] - a[1]) - (point[1] - a[1]) * (b[0] - a[0]);
      if (Math.abs(cross) <= 1e-9 && point[0] >= Math.min(a[0], b[0]) && point[0] <= Math.max(a[0], b[0]) &&
          point[1] >= Math.min(a[1], b[1]) && point[1] <= Math.max(a[1], b[1])) return true;
      if ((a[1] > point[1]) !== (b[1] > point[1]) &&
          point[0] < (b[0] - a[0]) * (point[1] - a[1]) / (b[1] - a[1]) + a[0]) inside = !inside;
    }
    return inside;
  }

  function geometryPolygons(geometry) {
    const g = geometry && geometry.type === 'Feature' ? geometry.geometry : geometry;
    assert(g && ['Polygon', 'MultiPolygon'].includes(g.type), 'Image selection needs native Polygon or MultiPolygon geometry.');
    const polygons = g.type === 'Polygon' ? [g.coordinates] : g.coordinates;
    assert(Array.isArray(polygons) && polygons.length, 'Image selection geometry is empty.');
    for (const polygon of polygons) {
      assert(Array.isArray(polygon) && polygon.length, 'Selection polygon needs an exterior ring.');
      for (const ring of polygon) {
        assert(Array.isArray(ring) && ring.length >= 4 && ring.every(p => Array.isArray(p) && p.length === 2 && p.every(finite)) &&
          ring[0][0] === ring[ring.length - 1][0] && ring[0][1] === ring[ring.length - 1][1],
        'Selection rings must be finite closed native-coordinate rings.');
      }
    }
    return polygons;
  }

  function pointInGeometry(x, y, geometry) {
    return geometryPolygons(geometry).some(polygon => ringContains([x, y], polygon[0]) &&
      !polygon.slice(1).some(hole => ringContains([x, y], hole)));
  }

  function create(host) {
    assert(host && host.context, 'A validated review context is required.');
    const context = validateContext(host.context), rows = context.observations;
    const byId = new Map(rows.map(row => [row.id, row]));
    const units = [...new Set(rows.map(row => row.observation_unit))];
    const clusterFields = [...new Set(rows.flatMap(row => Object.keys(row.cluster_values)))].sort();
    let activeTissue = typeof host.getActiveTissueId === 'function' ? String(host.getActiveTissueId()) : context.images[0].tissue_id;
    let scope = 'current', unit = units[0] || '', reductionId = context.reductions.find(r => r.observation_unit === unit)?.id || '';
    let clusterField = clusterFields[0] || '';
    let state = {selected_ids: [], review_records: []}, revision = 0, past = [], future = [];
    let panel = null, ui = {}, destroyed = false, generation = 0, plotPoints = [], drag = null;
    let planned = null, planGeneration = 0;
    let message = 'Select clusters or draw around points. Human review leaves original model results unchanged.';
    let errorStatus = false;

    function ordered(ids) {
      assert(ids == null || Array.isArray(ids), 'Selection must be an array of canonical observation IDs.');
      const selected = new Set();
      for (const id of ids || []) {
        assert(typeof id === 'string' && byId.has(id), 'Unknown observation ID: ' + id);
        assert(!selected.has(id), 'Duplicate selection observation ID: ' + id);
        selected.add(id);
      }
      return rows.filter(row => selected.has(row.id)).map(row => row.id);
    }

    function validState(input) {
      assert(input && Array.isArray(input.selected_ids) && Array.isArray(input.review_records), 'Malformed review checkpoint state.');
      const result = {selected_ids: ordered(input.selected_ids), review_records: []}, records = new Map();
      for (const record of input.review_records) {
        assert(record && byId.has(record.id) && !records.has(record.id), 'Unknown or duplicate human review observation ID.');
        assert(STATUSES.has(record.status), 'Unknown human review status.');
        assert(record.status !== 'reviewed' || text(record.label), 'Confirmed/corrected reviews need an explicit class label.');
        assert(record.label == null || text(record.label), 'Human review label must be nonempty or null.');
        assert(text(record.reviewer) && text(record.updated_at) && /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z$/.test(record.updated_at) && Number.isFinite(Date.parse(record.updated_at)),
          'Human reviews need an explicit reviewer and UTC timestamp.');
        assert(record.action == null || ACTIONS.has(record.action), 'Unknown human review action.');
        assert(!['cluster_values', 'predictions', 'model_sources'].some(name => Object.prototype.hasOwnProperty.call(record, name)),
          'Human reviews must not overwrite raw cluster values, predictions or model sources.');
        records.set(record.id, clone(record));
      }
      result.review_records = rows.filter(row => records.has(row.id)).map(row => records.get(row.id));
      return result;
    }

    function selection() { return state.selected_ids.slice(); }
    function selectedRows(options) {
      const opts = options || {}, selected = new Set(state.selected_ids);
      return clone(rows.filter(row => selected.has(row.id) && (!opts.tissue_id || row.tissue_id === opts.tissue_id) &&
        (!opts.observation_unit || row.observation_unit === opts.observation_unit)));
    }
    function selectedTileRows() { return selectedRows({tissue_id: activeTissue, observation_unit: 'tile'}); }
    function currentSelectedRows() { return selectedRows({tissue_id: activeTissue}); }
    function snapshot() {
      return clone({schema: 'wsitools-review-checkpoint', schema_version: 1, context_id: context.context_id,
        source_signature: context.source_signature, selected_ids: state.selected_ids, review_records: state.review_records,
        revision, history: {past, future}});
    }
    function status(value, error) {
      message = String(value); errorStatus = !!error;
      if (ui.status) { ui.status.textContent = message; ui.status.style.color = errorStatus ? '#a32222' : ''; }
      if (typeof host.status === 'function') host.status(message, errorStatus);
    }
    function call(name, ...args) {
      if (typeof host[name] !== 'function') return;
      try {
        const result = host[name](...args);
        if (result && typeof result.catch === 'function') result.catch(error => status(error.message, true));
        return result;
      } catch (error) { status(error.message, true); }
    }
    function changed() {
      planned = null; planGeneration++;
      render();
      const selected = selectedRows(), saved = snapshot();
      call('onSelection', selection(), clone(selected)); call('selectOverlay', clone(selected));
      call('onReview', clone(state.review_records), clone(saved)); call('draw'); call('sync', clone(saved));
    }
    function commit(next, description) {
      assert(!destroyed, 'The review workbench has been closed.');
      const checked = validState(next);
      if (JSON.stringify(checked) === JSON.stringify(state)) return selection();
      past.push(clone(state)); if (past.length > 100) past.shift(); future = [];
      state = checked; revision++; status(description, false); changed(); return selection();
    }
    function selectIds(ids, options) {
      const incoming = ordered(ids), mode = (options || {}).mode || 'replace';
      assert(MODES.has(mode), 'Selection mode must be replace, add, subtract or toggle.');
      const selected = new Set(mode === 'replace' ? [] : state.selected_ids);
      for (const id of incoming) {
        if (mode === 'subtract' || mode === 'toggle' && selected.has(id)) selected.delete(id);
        else selected.add(id);
      }
      return commit({selected_ids: ordered([...selected]), review_records: state.review_records},
        selected.size + ' observations selected. Selection does not assign a biological label.');
    }
    function visible(row) { return row.observation_unit === unit && (scope === 'all' || row.tissue_id === activeTissue); }
    function selectCluster(field, value, options) {
      const opts = options || {};
      assert(text(field) && rows.some(row => Object.prototype.hasOwnProperty.call(row.cluster_values, field)), 'Unknown cluster metadata field.');
      if (opts.tissue_ids != null) assert(Array.isArray(opts.tissue_ids) && opts.tissue_ids.every(id => context.images.some(image => image.tissue_id === id)), 'Unknown cluster-selection tissue identity.');
      if (opts.observation_unit != null) assert(units.includes(opts.observation_unit), 'Unknown cluster-selection observation unit.');
      const matches = rows.filter(row => (opts.tissue_ids ? opts.tissue_ids.includes(row.tissue_id) : scope === 'all' || row.tissue_id === activeTissue) &&
        row.observation_unit === (opts.observation_unit || unit) && row.cluster_values[field] != null && String(row.cluster_values[field]) === String(value));
      return selectIds(matches.map(row => row.id), {mode: opts.mode || 'add'});
    }
    function review(options) {
      const opts = options || {}, action = opts.action;
      const reviewIds = opts.ids == null ? state.selected_ids : ordered(opts.ids);
      assert(reviewIds.length, 'Select observations before recording a review.');
      assert(reviewIds.every(id => state.selected_ids.includes(id)), 'Human review can only apply to selected observation IDs.');
      assert(ACTIONS.has(action), 'Choose confirm, correct, exclude or uncertain.');
      assert(text(opts.reviewer), 'Enter a reviewer name before recording a decision.');
      if (action === 'confirm' || action === 'correct') assert(text(opts.label), 'Enter the class you are confirming or correcting.');
      const records = new Map(state.review_records.map(record => [record.id, record]));
      const timestamp = new Date().toISOString();
      for (const id of reviewIds) records.set(id, Object.assign({}, records.get(id), {id,
        label: action === 'confirm' || action === 'correct' ? opts.label.trim() : null,
        status: action === 'exclude' ? 'excluded' : action === 'uncertain' ? 'uncertain' : 'reviewed',
        action, reviewer: opts.reviewer.trim(), updated_at: timestamp}));
      commit({selected_ids: state.selected_ids, review_records: [...records.values()]},
        'Recorded ' + action + ' for ' + reviewIds.length + ' selected observations. Original model results are preserved.');
      return clone(state.review_records);
    }
    function undo() {
      if (!past.length) return false;
      future.push(clone(state)); state = past.pop(); revision++; status('Undid the last review/selection change.'); changed(); return true;
    }
    function redo() {
      if (!future.length) return false;
      past.push(clone(state)); state = future.pop(); revision++; status('Redid the review/selection change.'); changed(); return true;
    }
    function restore(saved) {
      assert(saved && saved.schema === 'wsitools-review-checkpoint' && saved.schema_version === 1, 'Unsupported review checkpoint.');
      assert(saved.context_id === context.context_id && saved.source_signature === context.source_signature,
        'This checkpoint belongs to a different or changed image/observation context.');
      assert(Number.isInteger(saved.revision) && saved.revision >= 0, 'Invalid checkpoint revision.');
      const next = validState(saved), history = saved.history || {past: [], future: []};
      assert(Object.keys(history).every(name => ['past', 'future'].includes(name)) && Array.isArray(history.past) && Array.isArray(history.future) && history.past.length <= 100 && history.future.length <= 100,
        'Review history is malformed or exceeds 100 changes.');
      const checkedPast = history.past.map(validState), checkedFuture = history.future.map(validState);
      state = next; past = checkedPast; future = checkedFuture; revision = saved.revision;
      status('Restored review and selection for this exact context.'); changed(); return snapshot();
    }
    function setActiveTissue(id) {
      assert(context.images.some(image => image.tissue_id === id), 'Unknown active tissue identity.');
      activeTissue = id; planned = null; planGeneration++; render(); return activeTissue;
    }
    function viewState() { return {reduction_id: reductionId, scope, observation_unit: unit, cluster_field: clusterField}; }
    function restoreViewState(view) {
      assert(view && typeof view === 'object' && !Array.isArray(view), 'Review view state must be an object.');
      assert(Object.keys(view).every(name => ['reduction_id', 'scope', 'observation_unit', 'cluster_field'].includes(name)), 'Unknown review view-state field.');
      assert(['current', 'all'].includes(view.scope), 'Unknown review scope.');
      assert(units.length ? units.includes(view.observation_unit) : view.observation_unit === '', 'Unknown review observation unit.');
      assert(clusterFields.length ? clusterFields.includes(view.cluster_field) : view.cluster_field === '', 'Unknown cluster metadata field.');
      const reduction = context.reductions.find(item => item.id === view.reduction_id);
      assert(reduction ? reduction.observation_unit === view.observation_unit : view.reduction_id === '' &&
        !context.reductions.some(item => item.observation_unit === view.observation_unit), 'Unknown or incompatible review reduction.');
      scope = view.scope; unit = view.observation_unit; clusterField = view.cluster_field; reductionId = view.reduction_id;
      render(); call('onViewChange', viewState()); return viewState();
    }
    function setScope(value) { assert(['current', 'all'].includes(value), 'Unknown review scope.'); scope = value; render(); call('onViewChange', viewState()); }
    function setReduction(id) {
      const reduction = context.reductions.find(item => item.id === id);
      assert(reduction, 'Unknown reduction ID.'); reductionId = id; unit = reduction.observation_unit; render(); call('onViewChange', viewState());
    }
    function selectImageGeometry(geometry, options) {
      const opts = options || {}, tissue = opts.tissue_id || activeTissue;
      assert(text(opts.frame_id) && context.images.some(image => image.tissue_id === tissue && image.frame_id === opts.frame_id),
        'Image selection needs an exact tissue and native coordinate frame.');
      const polygons = geometryPolygons(geometry);
      const ids = rows.filter(row => row.tissue_id === tissue && row.frame_id === opts.frame_id && row.observation_unit === (opts.observation_unit || unit) &&
        polygons.some(polygon => ringContains([row.x, row.y], polygon[0]) && !polygon.slice(1).some(hole => ringContains([row.x, row.y], hole)))).map(row => row.id);
      return selectIds(ids, {mode: opts.mode || 'replace'});
    }
    function selectReductionPolygon(ring, options) {
      const opts = options || {}, reduction = context.reductions.find(item => item.id === (opts.reduction_id || reductionId));
      assert(reduction && Array.isArray(ring) && ring.length >= 3 && ring.every(p => Array.isArray(p) && p.length === 2 && p.every(finite)),
        'Choose a reduction and draw a finite selection polygon.');
      return selectIds(reduction.points.filter(point => visible(byId.get(point.id)) && ringContains([point.x, point.y], ring)).map(point => point.id), opts);
    }
    function exportSelected() {
      const selected = currentSelectedRows();
      assert(selected.length, 'No observations are selected in the current tissue. Empty selection exports nothing.');
      assert(typeof host.export === 'function', 'Tile export is not connected in this viewer.');
      return host.export(selected);
    }
    function invalidatePlan() { planned = null; planGeneration++; render(); }
    async function planSelected() {
      const selected = currentSelectedRows(), tissue = activeTissue;
      assert(selected.length, 'Select observations in the current tissue before planning work tiles.');
      assert(typeof host.plan === 'function', 'Work-tile planning is not connected in this viewer.');
      const request = ++planGeneration, selectedIds = new Set(selected.map(row => row.id));
      planned = null;
      status('Planning work tiles from ' + selected.length + ' selected observations…');
      render();
      const result = await host.plan(selected);
      assert(request === planGeneration && activeTissue === tissue && !destroyed, 'Selection or tissue changed; the old work-tile plan was discarded.');
      assert(result && Array.isArray(result.tiles), 'Work-tile planner must return a tiles array.');
      if (result.context_id != null) assert(result.context_id === context.context_id, 'Work-tile plan context differs.');
      if (result.source_signature != null) assert(result.source_signature === context.source_signature, 'Work-tile plan sources changed.');
      const seen = new Set();
      for (const tile of result.tiles) {
        const image = context.images.find(item => item.tissue_id === tile.tissue_id && item.frame_id === tile.frame_id), p = tile.footprint;
        assert(image && tile.tissue_id === tissue && text(tile.id) && !seen.has(tile.id), 'Work-tile plan contains an unknown, duplicate or foreign image tile.');
        assert(p && ['x0', 'y0', 'x1', 'y1'].every(name => finite(p[name])) && p.x0 >= 0 && p.y0 >= 0 &&
          p.x1 <= image.width && p.y1 <= image.height && p.x1 > p.x0 && p.y1 > p.y0, 'Work-tile plan has invalid native bounds.');
        assert(Array.isArray(tile.source_ids) && tile.source_ids.length && tile.source_ids.every(id => selectedIds.has(id)),
          'Work tiles must retain their selected source observation IDs.');
        seen.add(tile.id);
      }
      planned = clone(result); render();
      status(selected.length + ' selected observations → ' + result.tiles.length + ' planned work tiles. Source observation units are unchanged.');
      return clone(planned);
    }

    function element(tag, role, value, parent) {
      const node = panel.ownerDocument.createElement(tag);
      if (tag === 'button') node.type = 'button';
      if (role) node.setAttribute('data-review-' + role, '');
      if (value != null) node.textContent = value;
      if (parent) parent.appendChild(node);
      return node;
    }
    function option(select, value, label) { const node = element('option', null, label, select); node.value = value; return node; }
    function run(action) {
      try { const result = action(); if (result && typeof result.catch === 'function') result.catch(error => status(error.message, true)); }
      catch (error) { status(error.message, true); }
    }
    function mount(container) {
      assert(container && container.ownerDocument, 'Mount the review workbench in a browser element.');
      assert(!panel && !destroyed, 'This workbench is already mounted or closed.');
      panel = container.ownerDocument.createElement('section'); panel.setAttribute('data-review-workbench', '');
      panel.style.cssText = 'font:13px/1.45 system-ui,sans-serif;padding:12px;border:1px solid #9ba5b4;border-radius:8px;color:inherit;max-width:720px';
      container.appendChild(panel);
      element('h3', null, 'Review workspace', panel).style.margin = '0 0 8px';
      ui.identity = element('div', 'identity', null, panel);
      const controls = element('div', null, null, panel); controls.style.cssText = 'display:flex;gap:8px;flex-wrap:wrap;margin:8px 0';
      const scopeLabel = element('label', null, 'Scope ', controls); ui.scope = element('select', 'scope', null, scopeLabel);
      option(ui.scope, 'current', 'Current tissue'); option(ui.scope, 'all', 'All tissues (explicit)'); ui.scope.onchange = () => run(() => setScope(ui.scope.value));
      const unitLabel = element('label', null, 'Observations ', controls); ui.unit = element('select', 'unit', null, unitLabel);
      units.forEach(value => option(ui.unit, value, value)); ui.unit.onchange = () => { unit = ui.unit.value; reductionId = context.reductions.find(r => r.observation_unit === unit)?.id || ''; render(); call('onViewChange', viewState()); };
      const fieldLabel = element('label', null, 'Clusters ', controls); ui.field = element('select', 'cluster-field', null, fieldLabel);
      clusterFields.forEach(value => option(ui.field, value, value));
      ui.field.onchange = () => { clusterField = ui.field.value; render(); call('onViewChange', viewState()); };
      ui.clusters = element('div', 'clusters', null, panel); ui.clusters.style.cssText = 'display:flex;gap:10px;flex-wrap:wrap;max-height:100px;overflow:auto;margin:6px 0';
      const reductionLabel = element('label', null, 'Linked plot ', panel); ui.reduction = element('select', 'reduction', null, reductionLabel);
      ui.reduction.onchange = () => run(() => setReduction(ui.reduction.value));
      ui.plot = element('canvas', 'plot', null, panel); ui.plot.width = 620; ui.plot.height = 260;
      ui.plot.setAttribute('aria-label', 'Reduction plot. Drag to lasso observations; Shift adds, Alt subtracts.');
      ui.plot.style.cssText = 'display:block;width:100%;height:260px;touch-action:none;border:1px solid #9ba5b4;margin:6px 0;background:#fff';
      element('div', null, 'Drag to select point centres. Shift adds; Alt subtracts. Image selections use the same observation IDs.', panel);
      ui.count = element('div', 'count', null, panel);
      const decisions = element('div', null, null, panel); decisions.style.cssText = 'display:flex;gap:6px;flex-wrap:wrap;margin:8px 0';
      const classLabel = element('label', null, 'Class ', decisions); ui.label = element('input', 'label', null, classLabel); ui.label.placeholder = 'Explicit human class';
      const reviewerLabel = element('label', null, 'Reviewer ', decisions); ui.reviewer = element('input', 'reviewer', null, reviewerLabel); ui.reviewer.value = host.reviewer || 'local user';
      ui.actions = [];
      for (const [action, title] of [['confirm', 'Confirm class'], ['correct', 'Correct class'], ['exclude', 'Exclude'], ['uncertain', 'Uncertain']]) {
        const button = element('button', action, title, decisions); button.type = 'button';
        button.onclick = () => run(() => review({action, label: ui.label.value, reviewer: ui.reviewer.value,
          ids: rows.filter(row => visible(row) && state.selected_ids.includes(row.id)).map(row => row.id)})); ui.actions.push(button);
      }
      element('div', null, 'Review actions apply only to selected observations in the displayed tissue/unit scope.', panel);
      ui.records = element('div', 'records', null, panel); ui.records.style.cssText = 'max-height:130px;overflow:auto;margin:6px 0';
      const tools = element('div', null, null, panel); tools.style.cssText = 'display:flex;gap:6px;flex-wrap:wrap;margin:8px 0';
      ui.undo = element('button', 'undo', 'Undo', tools); ui.undo.onclick = () => run(undo);
      ui.redo = element('button', 'redo', 'Redo', tools); ui.redo.onclick = () => run(redo);
      const clear = element('button', 'clear', 'Clear selection', tools); clear.onclick = () => run(() => selectIds([]));
      ui.plan = element('button', 'plan', 'Preview work tiles from selection', tools); ui.plan.onclick = () => run(planSelected);
      ui.export = element('button', 'export', 'Export work tiles from current-tissue selection', tools); ui.export.onclick = () => run(exportSelected);
      ui.preview = element('div', 'preview', null, panel); ui.preview.style.cssText = 'display:flex;gap:8px;flex-wrap:wrap;max-height:250px;overflow:auto';
      ui.status = element('div', 'status', null, panel); ui.status.setAttribute('role', 'status'); ui.status.setAttribute('aria-live', 'polite');
      ui.status.style.marginTop = '8px';
      function position(event) { const bounds = ui.plot.getBoundingClientRect(); return [(event.clientX - bounds.left) * ui.plot.width / bounds.width, (event.clientY - bounds.top) * ui.plot.height / bounds.height]; }
      ui.plot.onpointerdown = event => { if (event.button !== 0) return; drag = {points: [position(event)]}; ui.plot.setPointerCapture?.(event.pointerId); event.preventDefault(); };
      ui.plot.onpointermove = event => { if (!drag) return; const p = position(event), last = drag.points[drag.points.length - 1]; if (Math.hypot(p[0] - last[0], p[1] - last[1]) >= 3) drag.points.push(p); drawPlot(); };
      ui.plot.onpointercancel = () => { drag = null; drawPlot(); };
      ui.plot.onpointerup = event => run(() => {
        if (!drag) return; const points = drag.points; drag = null;
        const mode = event.altKey ? 'subtract' : event.shiftKey ? 'add' : event.ctrlKey || event.metaKey ? 'toggle' : 'replace';
        let chosen;
        if (points.length >= 3) chosen = plotPoints.filter(p => ringContains(p.canvas, points)).map(p => p.id);
        else { const p = position(event), nearest = plotPoints.map(item => [Math.hypot(item.canvas[0] - p[0], item.canvas[1] - p[1]), item.id]).sort((a, b) => a[0] - b[0])[0]; chosen = nearest && nearest[0] <= 9 ? [nearest[1]] : []; }
        selectIds(chosen, {mode}); drawPlot();
      });
      render(); return api;
    }
    function drawPlot() {
      if (!ui.plot) return;
      const canvas = ui.plot, ctx = canvas.getContext('2d'); if (!ctx) return;
      ctx.clearRect(0, 0, canvas.width, canvas.height); plotPoints = [];
      const reduction = context.reductions.find(item => item.id === reductionId);
      const points = reduction ? reduction.points.filter(p => visible(byId.get(p.id))) : [];
      if (!points.length) { ctx.fillStyle = '#444'; ctx.fillText('No matching reduction points in this scope.', 16, 28); return; }
      let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
      for (const point of points) { minX = Math.min(minX, point.x); minY = Math.min(minY, point.y); maxX = Math.max(maxX, point.x); maxY = Math.max(maxY, point.y); }
      const dx = maxX - minX || 1, dy = maxY - minY || 1;
      const selected = new Set(state.selected_ids), reviewed = new Map(state.review_records.map(record => [record.id, record.status]));
      for (const point of points) {
        const p = [18 + (point.x - minX) / dx * (canvas.width - 36), canvas.height - 18 - (point.y - minY) / dy * (canvas.height - 36)];
        plotPoints.push({id: point.id, canvas: p});
        const stateColor = reviewed.get(point.id);
        ctx.fillStyle = selected.has(point.id) ? '#ed7900' : stateColor === 'excluded' ? '#aaa' : stateColor === 'uncertain' ? '#9b59b6' : stateColor === 'reviewed' ? '#21844a' : '#4c78a8';
        ctx.beginPath(); ctx.arc(p[0], p[1], selected.has(point.id) ? 4 : 2.5, 0, Math.PI * 2); ctx.fill();
        if (selected.has(point.id)) { ctx.strokeStyle = '#222'; ctx.stroke(); }
      }
      if (drag && drag.points.length) { ctx.strokeStyle = '#e27700'; ctx.beginPath(); drag.points.forEach((p, i) => i ? ctx.lineTo(...p) : ctx.moveTo(...p)); ctx.stroke(); }
    }
    function render() {
      if (!panel || destroyed) return;
      generation++; const currentGeneration = generation;
      const image = context.images.find(item => item.tissue_id === activeTissue);
      ui.identity.textContent = (image?.label || image?.name || activeTissue) + ' · ' + context.experiment_id + ' · native image coordinates';
      ui.scope.value = scope; ui.unit.value = unit; ui.field.value = clusterField;
      ui.clusters.replaceChildren();
      const groups = new Map(), field = clusterField;
      for (const row of rows.filter(visible)) if (row.cluster_values[field] != null) {
        const value = String(row.cluster_values[field]); if (!groups.has(value)) groups.set(value, []); groups.get(value).push(row.id);
      }
      const selected = new Set(state.selected_ids);
      for (const [value, ids] of [...groups].sort((a, b) => a[0].localeCompare(b[0], undefined, {numeric: true}))) {
        const label = element('label', null, null, ui.clusters), box = element('input', null, null, label); box.type = 'checkbox';
        const n = ids.filter(id => selected.has(id)).length; box.checked = n === ids.length; box.indeterminate = n > 0 && n < ids.length;
        box.onchange = () => run(() => selectIds(ids, {mode: box.checked ? 'add' : 'subtract'}));
        label.appendChild(panel.ownerDocument.createTextNode(' ' + value + ' (' + ids.length + ')'));
      }
      if (!groups.size) element('span', null, 'No cluster metadata in this scope.', ui.clusters);
      ui.reduction.replaceChildren();
      context.reductions.filter(item => item.observation_unit === unit).forEach(item => option(ui.reduction, item.id, item.label));
      ui.reduction.value = reductionId; ui.reduction.disabled = !ui.reduction.options.length;
      const current = currentSelectedRows(), tiles = planned ? planned.tiles : selectedTileRows();
      ui.count.textContent = state.selected_ids.length + ' selected observations total · ' + current.length + ' in current tissue · ' +
        (planned ? planned.tiles.length + ' planned work tiles · ' : '') + state.review_records.length + ' human review records';
      const visibleSelected = rows.filter(row => visible(row) && selected.has(row.id)).length;
      ui.actions.forEach(button => { button.disabled = !visibleSelected; });
      ui.records.replaceChildren();
      const human = new Map(state.review_records.map(record => [record.id, record]));
      const recordRows = rows.filter(row => visible(row) && selected.has(row.id));
      if (recordRows.length) {
        const table = element('table', null, null, ui.records); table.style.cssText = 'width:100%;text-align:left;border-collapse:collapse';
        const heading = element('tr', null, null, element('thead', null, null, table));
        ['Observation', 'Tissue / unit', 'Human review'].forEach(value => element('th', null, value, heading));
        const body = element('tbody', null, null, table);
        for (const row of recordRows.slice(0, 12)) {
          const tr = element('tr', null, null, body), record = human.get(row.id);
          element('td', null, row.original_id, tr);
          element('td', null, row.tissue_id + ' / ' + row.observation_unit, tr);
          element('td', null, record ? record.status + (record.label ? ': ' + record.label : '') + ' · ' + record.reviewer : 'Not reviewed', tr);
        }
        if (recordRows.length > 12) element('div', null, 'Showing 12 of ' + recordRows.length + ' selected observations in this scope.', ui.records);
      }
      ui.undo.disabled = !past.length; ui.redo.disabled = !future.length;
      ui.plan.disabled = !current.length || typeof host.plan !== 'function';
      ui.export.disabled = !current.length || typeof host.export !== 'function';
      ui.preview.replaceChildren();
      if (!tiles.length) element('div', null, current.length ? 'Selected observations are not image tiles. Preview their explicitly planned work tiles above.' :
        'No selected observations in this tissue. Empty selection previews and exports nothing.', ui.preview);
      for (const row of tiles.slice(0, 12)) {
        const card = element('div', null, null, ui.preview); card.style.cssText = 'width:145px;padding:5px;border:1px solid #aaa;border-radius:4px;overflow-wrap:anywhere';
        const title = row.original_id || row.id;
        const focus = element('button', null, title, card); focus.type = 'button'; focus.disabled = typeof host.focusObservation !== 'function';
        focus.title = focus.disabled ? 'Image focus is unavailable in this viewer.' : 'Show this selected/planned tile on the image';
        focus.onclick = () => call('focusObservation', clone(row));
        if (finite(row.x) && finite(row.y)) element('div', null, 'Native centre: ' + row.x + ', ' + row.y, card);
        if (planned) element('div', null, 'Work tile from ' + row.source_ids.length + ' selected source observations', card);
        if (row.footprint) element('div', null, 'Bounds: ' + ['x0', 'y0', 'x1', 'y1'].map(name => row.footprint[name]).join(', '), card);
        if (typeof host.previewTile === 'function') {
          const target = element('div', null, 'Loading selected tile…', card);
          Promise.resolve().then(() => host.previewTile(clone(row))).then(result => {
            if (destroyed || currentGeneration !== generation) return;
            const url = typeof result === 'string' ? result : result?.url;
            assert(text(url) && !/^\s*(?:javascript|file):/i.test(url), 'No usable selected-tile preview is available.');
            target.replaceChildren(); const img = element('img', null, null, target); img.src = url; img.alt = 'Selected work tile ' + title;
            img.style.cssText = 'display:block;max-width:135px;max-height:100px'; img.onerror = () => { target.textContent = 'Selected tile preview unavailable.'; };
          }).catch(error => { if (currentGeneration === generation) target.textContent = error.message; });
        } else element('div', null, typeof host.focusObservation === 'function' ? 'Use the tile ID to focus its image footprint.' : 'Native footprint retained; image preview unavailable.', card);
      }
      if (tiles.length > 12) element('div', null, 'Showing the first 12 of ' + tiles.length + ' tiles. Export uses only selected source observations in this tissue.', ui.preview);
      ui.status.textContent = message; ui.status.style.color = errorStatus ? '#a32222' : '';
      drawPlot();
    }
    function destroy() { destroyed = true; generation++; panel?.remove(); panel = null; ui = {}; }
    const api = {mount, snapshot, restore, selection, selectIds, selectCluster, selectedRows, selectedTileRows, review, undo, redo,
      setActiveTissue, setScope, setReduction, selectImageGeometry, selectReductionPolygon, exportSelected, planSelected, invalidatePlan,
      viewState, restoreViewState, workPlan: () => planned ? clone(planned) : null, destroy};
    return api;
  }
  return {create, validateContext, pointInGeometry, coordinateConvention: CONVENTION};
});
