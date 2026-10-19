// Exercise production readiness, controls and asynchronous fit guards without R fitting.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const source = fs.readFileSync(path.join(__dirname, '../../R/viewer.R'), 'utf8');
const segment = source.slice(source.indexOf('wsi_viewer_prediction_js <- function()'), source.indexOf('\nwsi_viewer_', source.indexOf('wsi_viewer_prediction_js <- function()') + 1));
const js = segment.split('\n').filter(line => /^\s+"/.test(line)).map(line => JSON.parse(line.trim().replace(/,$/, ''))).join('');
const button = () => ({disabled: false, textContent: '', attributes: {}, classList: {add() {}, remove() {}}, setAttribute(key, value) { this.attributes[key] = value; }});
const ids = Object.fromEntries(['predictionRun', 'predictionWindowOpen', 'predictionClear', 'predictionSummary', 'predictionSubtitle', 'predictionWindow'].map(id => [id, button()]));
ids.predictionTrainAnnotations = {selectedOptions: [{value: 'left'}, {value: 'right'}]};
ids.predictionFeatureSource = {value: 'experiment:pooled'};
const context = vm.createContext({
  console, cfg: {experiment: {experiment_id: 'study', manifest_sha256: 'original'}, prediction_url: '/prediction',
    prediction: {enabled: true, fastpls_installed: true, sources: [{id: 'experiment:pooled', unit: 'tile'}]}},
  projectItems: [{id: 'a', path: '/study/a.png'}, {id: 'b', path: '/study/b.png'}], activeProjectIndex: 0,
  el: id => ids[id] || null, layers: [], selectedRoi: -1, rois: [], stateCommandSeen: new Set(),
  notify() {}, handled: [], handleViewerCommands(body) { context.handled.push(body); },
  classEntries: [{roi: {id: 'left', class: 'Tumour'}, index: 0}, {roi: {id: 'right', class: 'Stroma'}, index: 1}]
});
vm.runInContext(js, context);
context.predictionSelectableRois = () => context.classEntries;
context.predictionRoiValue = entry => entry.roi.id;
context.predictionProjectSlideEntries = () => [];
context.syncPredictionReductionDimsControl = () => {};
context.syncPredictionRefineSvmControl = () => {};
context.predictionPayload = () => ({feature_source: 'experiment:pooled', train_annotations: ['left', 'right']});
context.renderPredictionLegend = () => {};
context.fillPredictionSources = () => {};
context.fillPredictionAnnotations = () => {};
function ready(value, expression) {
  context.updatePredictionControls();
  assert.equal(context.predictionReadiness().ready, value);
  assert.equal(ids.predictionRun.disabled, !value);
  if (expression) assert.match(ids.predictionSummary.textContent, expression);
}
(async () => {
  ready(true, /complete feature tile\/token footprints/);
  context.cfg.prediction.sources = []; context.cfg.prediction.enabled = false;
  ready(false, /No compatible cached predictors.*GigaTIME/);
  context.openPredictionWindow();
  assert.equal(ids.predictionWindowOpen.disabled, false, 'Missing predictors keep a discoverable explanation');
  assert.equal(ids.predictionWindow.attributes['aria-hidden'], 'false');
  context.cfg.prediction.sources = [{id: 'experiment:pooled', unit: 'tile'}]; context.cfg.prediction.enabled = true;
  context.cfg.prediction_url = ''; ready(false, /live R session/);
  context.cfg.prediction_url = '/prediction'; context.cfg.prediction.fastpls_installed = false;
  ready(false, /fastPLS.*install_github/);
  context.cfg.prediction.fastpls_installed = true;
  ids.predictionTrainAnnotations.selectedOptions = [{value: 'left'}]; ready(false, /two different classes \(1 selected\)/);
  ids.predictionTrainAnnotations.selectedOptions.push({value: 'right'});
  context.classEntries[1].roi.class = 'Tumour'; ready(false, /two different classes/);
  context.classEntries[1].roi.class = 'Stroma'; ready(true);
  let resolve, posts = 0;
  context.fetch = () => { posts++; return new Promise(yes => { resolve = yes; }); };
  const first = context.runPrediction();
  assert.equal(ids.predictionRun.disabled, true);
  assert.equal(ids.predictionRun.attributes['aria-busy'], 'true');
  await context.runPrediction(); assert.equal(posts, 1, 'Repeated activation submits only one fit');
  context.activeProjectIndex = 1;
  resolve({ok: true, json: async () => ({prediction: {count: 4, classes: ['Tumour', 'Stroma'], point_unit: 'tile'}, commands: []})});
  await first;
  assert.equal(context.handled.length, 1, 'Switching tissue does not discard legitimate multi-tissue predictions');
  assert.equal(ids.predictionRun.disabled, false);
  assert.match(ids.predictionSummary.textContent, /Predicted 4 tile labels/);
  const stale = context.runPrediction();
  context.cfg.experiment.manifest_sha256 = 'changed';
  const command = {id: 'stale-layer', type: 'add_layer', payload: {layer: {id: 'wsi_prediction_pls_lda'}}};
  resolve({ok: true, json: async () => ({prediction: {count: 9}, commands: [command]})});
  await stale;
  assert.equal(context.handled.length, 1, 'Changed scientific context prevents applying the returned prediction');
  assert.equal(context.stateCommandSeen.has('stale-layer'), true, 'The stale returned layer command cannot later replay');
  assert.match(ids.predictionSummary.textContent, /sources changed.*CPU fit may have saved a derivative/);
  context.fetch = async () => { throw new Error('Disconnected'); };
  await context.runPrediction();
  assert.match(ids.predictionSummary.textContent, /Prediction failed: Disconnected/);
  assert.equal(ids.predictionRun.disabled, false, 'Failure releases the fit guard');
  console.log('Prediction missing-data/live/backend/classes/busy/context/failure contracts passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
