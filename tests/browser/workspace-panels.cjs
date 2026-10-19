// Production panel/host orchestration with a small DOM surface; no browser, R or image inference runs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');
const root = path.resolve(__dirname, '../..');
const experimentSource = fs.readFileSync(path.join(root, 'inst/viewer/experiment-workbench.js'), 'utf8');
const reviewSource = fs.readFileSync(path.join(root, 'inst/viewer/review-host.js'), 'utf8');
const deferred = () => { let resolve; const promise = new Promise(yes => { resolve = yes; }); return {promise, resolve}; };

function domSurface() {
  const ids = new Map(), documentEvents = new Map(), windowEvents = new Map();
  function events(target, store = new Map()) {
    target.addEventListener = (name, fn) => { if (!store.has(name)) store.set(name, []); store.get(name).push(fn); };
    target.dispatch = (name, detail = {}) => {
      const event = {target, preventDefault() { this.defaultPrevented = true; }, stopPropagation() { this.propagationStopped = true; }, ...detail};
      for (const listener of store.get(name) || []) listener(event);
      target['on' + name]?.(event); return event;
    };
    return target;
  }
  const document = events({activeElement: null}, documentEvents);
  document.createElement = tag => {
    let identifier = '', html = '';
    const classes = new Set();
    const node = events({tag, ownerDocument: document, children: [], style: {}, attributes: {}, _value: null,
      checked: false, disabled: false, hidden: false, inert: false, selected: false, multiple: false,
      classList: {add: value => classes.add(value), contains: value => classes.has(value)},
      appendChild(child) { if (child.parent) child.parent.children = child.parent.children.filter(value => value !== child); this.children.push(child); child.parent = this; return child; },
      append(...children) { children.forEach(child => this.appendChild(child)); },
      replaceChildren(...children) { this.children = []; this.append(...children); },
      setAttribute(name, value) { this.attributes[name] = String(value); },
      focus() { document.activeElement = this; },
      querySelector(selector) { return selector === 'option[value=um]' ? this.options.find(option => option.value === 'um') : null; },
      getBoundingClientRect() { return {top: 0, left: 0, width: 900, height: 600, bottom: tag === 'bar' ? 96 : 600}; },
      getContext() { return {clearRect() {}, drawImage() {}, beginPath() {}, moveTo() {}, lineTo() {}, stroke() {}}; },
      toDataURL() { return 'data:image/png;base64,c3ludGhldGlj'; }});
    Object.defineProperties(node, {
      id: {get: () => identifier, set: value => { identifier = value; ids.set(value, node); }},
      options: {get: () => node.children.filter(child => child.tag === 'option')},
      selectedOptions: {get: () => node.options.filter(option => option.selected || !node.multiple && option.value === node.value)},
      value: {get: () => node._value ?? (tag === 'select' ? node.options.find(option => option.selected)?.value ?? node.options[0]?.value ?? '' : ''), set: value => { node._value = value; }},
      innerHTML: {get: () => html, set: value => {
        html = value;
        // Only the IDs, input defaults and option choices used by the host are needed here.
        for (const match of value.matchAll(/<(input|select|button|h2|div|p|datalist)\b([^>]*\bid="([^"]+)"[^>]*)>/g)) {
          const child = document.createElement(match[1]); child.id = match[3];
          child.multiple = /\bmultiple\b/.test(match[2]); child.checked = /\bchecked\b/.test(match[2]);
          const initial = match[2].match(/\bvalue="([^"]*)"/); if (initial) child.value = initial[1];
          node.appendChild(child);
        }
        for (const match of value.matchAll(/<select\b[^>]*\bid="([^"]+)"[^>]*>([\s\S]*?)<\/select>/g)) {
          const select = ids.get(match[1]);
          for (const optionMatch of match[2].matchAll(/<option\b[^>]*\bvalue="?([^" >]+)"?[^>]*>/g)) {
            const option = document.createElement('option'); option.value = optionMatch[1]; select.appendChild(option);
          }
        }
      }}
    });
    return node;
  };
  document.head = document.createElement('head'); document.body = document.createElement('body');
  const bar = document.createElement('bar');
  document.querySelector = selector => selector === '.bar' ? bar : null;
  const window = events({innerHeight: 800}, windowEvents);
  return {document, window, el: id => ids.get(id) || null};
}

function scope({bindReview = false} = {}) {
  const dom = domSurface(), requests = [], downloads = [], hosts = [], invalidations = [];
  const image = {tissue_id: 'a', frame_id: 'a-native', sha256: 'a'.repeat(64), width: 100, height: 100, mpp_x: .5, mpp_y: .5};
  const reviewContext = {context_id: 'review', source_signature: 'b'.repeat(64), images: [image], observations: [], reductions: []};
  const controller = {mount() {}, restore() {}, destroy() {}, viewState: () => ({observation_unit: 'spot', dataset_id: 'rna'}),
    selection: () => [], snapshot: () => ({}), invalidatePlan: () => invalidations.push(true)};
  const context = vm.createContext({console, ...dom, Blob, setTimeout() {},
    cfg: {experiment: {experiment_id: 'study'}, review_url: '/review', review_workbench: reviewContext},
    projectItems: [{id: 'a', tissue_id: 'a', layers: []}], activeProjectIndex: 0, layers: [],
    canvas: dom.document.createElement('canvas'),
    WsiReviewWorkbench: {create: host => { hosts.push(host); return controller; }},
    upsertViewerLayer() {}, draw() {}, buildLayerList() {}, markProjectDirty() {},
    tauriViewerInvoke: () => null, projectBrowserSnapshot: () => ({}), requestProjectSave: async () => {},
    applyProjectPreview() {}, restoreBrowserProjectInLiveSession: async () => {},
    fetch: async (url, options) => { const pending = deferred(); requests.push({payload: JSON.parse(options.body), ...pending}); return pending.promise; },
    downloadBlob: async (blob, name) => downloads.push({blob, name})});
  vm.runInContext(experimentSource, context);
  vm.runInContext(reviewSource, context);
  if (bindReview) context.wsiReviewBindWorkbench();
  return {context, ...dom, requests, downloads, hosts, invalidations, reviewContext};
}

function panelFixture(environment, name, callbacks = {}) {
  const {document, context} = environment;
  const button = document.createElement('button'), panel = document.createElement('section'), title = document.createElement('h2');
  button.id = name + '-button'; panel.id = name + '-panel'; title.id = name + '-title'; panel.hidden = true;
  document.body.appendChild(panel); panel.appendChild(title);
  const open = context.wsiWorkspaceBind(button, panel, title.id, callbacks.close, callbacks.open);
  return {button, panel, title, open};
}

test('workspace panels share one dock, use modeless dialog semantics and stay below the toolbar', () => {
  const environment = scope(), a = panelFixture(environment, 'a'), b = panelFixture(environment, 'b');
  const dock = environment.el('experimentWorkspaceTools');
  assert.equal(dock.attributes.role, 'toolbar'); assert.deepEqual(dock.children, [a.button, b.button]);
  assert.equal(environment.document.head.children.length, 1, 'Shared panel CSS is installed once.');
  assert.equal(a.panel.attributes.role, 'dialog'); assert.equal(a.panel.attributes['aria-modal'], 'false');
  assert.equal(a.panel.attributes['aria-labelledby'], a.title.id);
  assert.equal(a.button.attributes['aria-controls'], a.panel.id);
  assert.equal(a.panel.style.top, '104px');
  environment.window.innerHeight = 240; environment.window.dispatch('resize');
  assert.equal(a.panel.style.top, '60px', 'Short windows keep the panel on screen.');
});

test('opening, mutual exclusion, Escape and close return focus to the proper control', () => {
  const environment = scope(), events = [];
  const a = panelFixture(environment, 'a', {open: () => events.push('open-a'), close: () => events.push('close-a')});
  const b = panelFixture(environment, 'b', {open: () => events.push('open-b')});
  a.button.onclick(); assert.equal(a.panel.hidden, false); assert.equal(a.button.attributes['aria-expanded'], 'true');
  assert.equal(environment.document.activeElement, a.title);
  b.button.onclick(); assert.equal(a.panel.hidden, true); assert.equal(b.panel.hidden, false);
  assert.equal(a.button.attributes['aria-expanded'], 'false'); assert.equal(environment.document.activeElement, b.title);
  const escape = environment.document.dispatch('keydown', {key: 'Escape'});
  assert.equal(escape.defaultPrevented, true); assert.equal(b.panel.hidden, true);
  assert.equal(environment.document.activeElement, b.button); assert.deepEqual(events, ['open-a', 'close-a', 'open-b']);
  a.open(true); a.open(false); assert.equal(environment.document.activeElement, a.button);
});

test('a busy inert panel cannot be closed or replaced by another workspace', () => {
  const environment = scope(), a = panelFixture(environment, 'a'), b = panelFixture(environment, 'b');
  a.open(true); a.panel.inert = true;
  assert.equal(a.open(false), false); assert.equal(b.open(true), false);
  environment.document.dispatch('keydown', {key: 'Escape'});
  assert.equal(a.panel.hidden, false); assert.equal(b.panel.hidden, true);
  a.panel.inert = false; assert.equal(b.open(true), true); assert.equal(a.panel.hidden, true);
});

test('Escape consumed by a workspace does not reach the viewer and discard an unfinished trajectory', () => {
  const environment = scope(), a = panelFixture(environment, 'a'), {context} = environment;
  const line = fs.readFileSync(path.join(root, 'R/viewer.R'), 'utf8').split('\n')
    .find(line => line.includes("window.addEventListener('keydown',e=>") && line.includes('wsiEditGeneration++;measureStart=null;trajectoryDraft=[]'));
  assert.ok(line, 'Exercise the production viewer keyboard handler.');
  const decoded = JSON.parse(line.trim().replace(/,$/, ''));
  Object.assign(context, {mode: 'trajectory', trajectoryDraft: [{x: 10, y: 20}], wsiEditGeneration: 0,
    brushSubtractKeyEvent: () => false, panByKeyboard: () => false, rois: [],
    cancelScreenshotSelection() {}, setMode(value) { context.mode = value; }});
  vm.runInContext(decoded.endsWith('\\n') ? decoded.slice(0, -2) : decoded, context);
  a.open(true);
  const event = environment.document.dispatch('keydown', {key: 'Escape', target: a.title});
  if (!event.propagationStopped) environment.window.dispatch('keydown', event);
  assert.equal(a.panel.hidden, true);
  assert.equal(context.trajectoryDraft.length, 1, 'Closing a dialog must not cancel the image draft behind it.');
  assert.equal(context.mode, 'trajectory');
  a.open(true); a.panel.inert = true;
  const busyEscape = environment.document.dispatch('keydown', {key: 'Escape', target: a.title});
  if (!busyEscape.propagationStopped) environment.window.dispatch('keydown', busyEscape);
  assert.equal(a.panel.hidden, false, 'A busy panel stays open.');
  assert.equal(context.trajectoryDraft.length, 1, 'Busy-panel Escape must not reach image editing either.');
});

const reply = (request, body) => request.resolve({ok: true, json: async () => ({ok: true, ...body})});

test('review host checks the current-operation guard after planning and before any download', async () => {
  const environment = scope({bindReview: true}), {hosts, requests, downloads} = environment;
  let current = true;
  const pending = hosts[0].export([{id: 'a::spot'}], {isCurrent: () => current});
  assert.equal(requests[0].payload.action, 'tile_plan'); current = false;
  reply(requests[0], {plan: {context_id: 'review', source_signature: 'b'.repeat(64), row_count: 1, work_count: 0, excluded_ids: [], tiles: []}});
  await assert.rejects(pending, /selection, tissue or tile settings changed/); assert.equal(downloads.length, 0);
  const valid = hosts[0].export([{id: 'a::spot'}], {isCurrent: () => true});
  reply(requests[1], {plan: {context_id: 'review', source_signature: 'b'.repeat(64), row_count: 1, work_count: 0, excluded_ids: [], tiles: []}});
  await valid; assert.equal(downloads.length, 1); assert.equal(downloads[0].name, 'selected-work-tiles.json');
});

test('changing the spatial file discards pending inspection metadata and allows a fresh request', async () => {
  const environment = scope({bindReview: true}), {el, requests} = environment;
  el('reviewSpatialPath').value = '/first.rds'; el('reviewTrustSpatial').checked = true;
  const pending = el('reviewInspectSpatial').onclick();
  assert.equal(el('reviewInspectSpatial').disabled, true); assert.equal(el('reviewInspectSpatial').attributes['aria-busy'], 'true');
  el('reviewSpatialPath').value = '/second.rds'; el('reviewSpatialPath').dispatch('input');
  await el('reviewInspectSpatial').onclick(); assert.equal(requests.length, 1, 'Do not overlap inspections.');
  reply(requests[0], {inspection: {observation_count: 10, metadata_fields: ['stale_cluster'], reduction_names: ['stale_umap']}});
  await pending;
  assert.equal(el('reviewClusterColumns').options.length, 0); assert.equal(el('reviewReductionNames').options.length, 0);
  assert.equal(el('reviewInspectSpatial').disabled, false); assert.equal(el('reviewAttachSpatial').disabled, true);
  const fresh = el('reviewInspectSpatial').onclick();
  reply(requests[1], {inspection: {observation_count: 20, metadata_fields: ['new_cluster'], reduction_names: ['new_umap'], image_names: ['section-a']}});
  await fresh;
  assert.deepEqual(el('reviewClusterColumns').options.map(option => option.value), ['new_cluster']);
  assert.deepEqual(el('reviewReductionNames').options.map(option => option.value), ['new_umap']);
  assert.match(el('reviewSpatialInspection').textContent, /20 observations/);
});

test('multiple Seurat images require explicit image selection and metadata reinspection', async () => {
  const environment = scope({bindReview: true}), {el, document, requests} = environment;
  el('reviewSpatialPath').value = '/multi.rds'; el('reviewTrustSpatial').checked = true;
  const pending = el('reviewInspectSpatial').onclick();
  reply(requests[0], {inspection: {needs_image_selection: true, image_names: ['FOV1', 'FOV2']}}); await pending;
  assert.deepEqual(el('reviewSpatialImages').children.map(option => option.value), ['FOV1', 'FOV2']);
  assert.equal(document.activeElement, el('reviewImageName')); assert.equal(el('reviewAttachSpatial').disabled, true);
  assert.match(el('reviewSpatialInspection').textContent, /inspect again/);
});

test('reinspection disables an already-enabled Add button until fresh metadata has returned', async () => {
  const environment = scope({bindReview: true}), {el, requests} = environment;
  el('reviewSpatialPath').value = '/same-file.rds'; el('reviewTrustSpatial').checked = true;
  const first = el('reviewInspectSpatial').onclick();
  const inspection = {observation_count: 20, metadata_fields: ['cluster'], reduction_names: []};
  reply(requests[0], {inspection}); await first;
  el('reviewClusterColumns').options[0].selected = true; el('reviewClusterColumns').onchange();
  el('reviewConfirmMapping').checked = true; el('reviewConfirmMapping').onchange();
  assert.equal(el('reviewAttachSpatial').disabled, false, 'The previous inspection is fully ready.');
  const again = el('reviewInspectSpatial').onclick();
  try { assert.equal(el('reviewAttachSpatial').disabled, true, 'Reinspection must block attaching stale inspected data.'); }
  finally { reply(requests[1], {inspection}); await again; }
});

test('tile-size typing invalidates pending plans immediately and closing review cancels lasso mode', () => {
  const environment = scope({bindReview: true}), {context, el, invalidations, document} = environment;
  el('reviewTileSize').dispatch('input'); assert.equal(invalidations.length, 1);
  el('openReviewWorkbench').onclick(); el('reviewSelectOnTissue').onclick();
  assert.equal(vm.runInContext('wsiReviewSelecting', context), true);
  document.dispatch('keydown', {key: 'Escape'});
  assert.equal(vm.runInContext('wsiReviewSelecting', context), false);
  assert.equal(el('reviewWorkbenchWindow').hidden, true); assert.equal(document.activeElement, el('openReviewWorkbench'));
});
