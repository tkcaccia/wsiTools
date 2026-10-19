// Execute production save/export helpers: browser policy failures differ from write/encoding failures.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const lines = fs.readFileSync(path.join(__dirname, '../../R/viewer.R'), 'utf8').split('\n');
const functions = ['saveBlobWithLocation', 'saveScreenshot', 'saveScreenshotFromDialog',
  'bindScreenshotDialogControls', 'setAnnotationsDirty', 'markAnnotationsSaved',
  'setProjectDirty', 'saveProjectFile', 'confirmProjectReplacement', 'bindProjectPanelControls'].map(name => {
  const line = lines.find(value => ['"async function ', '"function '].some(prefix => value.trim().startsWith(prefix + name + '(')));
  assert.ok(line, name + ' comes from production code');
  return JSON.parse(line.trim().replace(/,$/, ''));
}).join('');
const error = (name, message = name) => Object.assign(new Error(message), {name});
function scope() {
  const notices = [], downloads = [], writes = [], encodes = [], nodes = {}, history = [], menus = [];
  const context = vm.createContext({window: {}, notices, downloads, writes, encodes, nodes, history, menus, Blob,
    tauriViewerInvoke: () => null, notify: (...args) => notices.push(args),
    downloadBlob: async (blob, name) => downloads.push({blob, name}),
    blobToBase64: async () => 'cG5n', pickerTypeFilters: () => [],
    screenshotNormaliseFormat: value => value, screenshotFormat: () => context.selectedFormat || 'png',
    screenshotFormatLabel: value => (value || 'png').toUpperCase(), screenshotFileName: () => 'tissue.png',
    screenshotPickerType: () => ({description: 'PNG', accept: {'image/png': ['.png']}}),
    screenshotBlobFromCanvas: async (canvas, format) => {encodes.push({canvas, format}); return canvas.blob || 'image-bytes';},
    el: id => nodes[id] || null, updateScreenshotDialogSummary() {}, updateProjectPanelToggle() {},
    closeContainingToolMenu() {}, closeScreenshotDialog() {},
    pendingScreenshotRect: {x: 10, y: 20, width: 100, height: 80}, pendingScreenshotPane: null,
    screenshotOptionsFromDialog: () => ({tissue: true}),
    screenshotCanvasFromRect: () => ({_wsiScreenshotBaseIncluded: true}),
    annotationsDirty: true, projectDirty: true, annotationDirtyReason: 'roi_changed', projectDirtyReason: 'view_changed',
    projectEditRevision: 0, revisionContent: 0,
    projectItems: [{id: 'a'}, {id: 'b'}], projectSnapshotName: () => 'study.wsiproject.json',
    projectBrowserSnapshot: () => ({schema: 'wsiTools-viewer-project', revision_content: context.revisionContent,
      project: {items: [{id: 'a'}, {id: 'b'}], annotation_sets: [{key: 'a'}, {key: 'b'}]}}),
    updateAnnotationDirtyIndicator() {}, projectHasUnsavedChanges: () => context.annotationsDirty || context.projectDirty,
    saveActiveProjectAnnotations: () => {context.savedAnnotationSets = true;},
    recordAnnotationHistory: (...args) => history.push(args), scheduleViewerStateSync() {},
    projectMenuStatus: message => menus.push(message)
  });
  vm.runInContext(functions, context);
  return context;
}
(async () => {
  const blob = {type: 'image/png'};
  let c = scope();
  assert.equal(await c.saveBlobWithLocation(blob, 'image.png', {}), 'downloaded');
  assert.equal(c.downloads.length, 1); assert.equal(c.downloads[0].blob, blob);
  assert.match(c.notices[0][0], /browser downloads: image.png/);
  for (const name of ['SecurityError', 'NotAllowedError', 'NotSupportedError']) {
    c = scope(); c.window.showSaveFilePicker = async () => {throw error(name);};
    assert.equal(await c.saveBlobWithLocation(blob, 'project.json', {}), 'downloaded');
    assert.equal(c.downloads.length, 1, name + ' requests a browser download');
  }
  c = scope(); c.window.showSaveFilePicker = async () => {throw error('AbortError');};
  assert.equal(await c.saveBlobWithLocation(blob, 'cancelled.png', {}), 'cancelled');
  assert.equal(c.downloads.length, 0, 'User cancellation never starts a download');
  c = scope(); c.window.showSaveFilePicker = async () => {throw error('TypeError', 'Invalid file type');};
  await assert.rejects(c.saveBlobWithLocation(blob, 'invalid.png', {}), /Invalid file type/);
  assert.equal(c.downloads.length, 0);
  for (const phase of ['createWritable', 'write', 'close']) {
    c = scope();
    c.window.showSaveFilePicker = async () => ({createWritable: async () => {
      if (phase === 'createWritable') throw error('NotAllowedError', 'File permission denied');
      return {write: async value => {if (phase === 'write') throw error('NotAllowedError', 'File permission denied'); c.writes.push(value);},
        close: async () => {if (phase === 'close') throw error('NotAllowedError', 'File permission denied');}};
    }});
    await assert.rejects(c.saveBlobWithLocation(blob, 'selected.png', {}), /File permission denied/);
    assert.equal(c.downloads.length, 0, 'A genuine ' + phase + ' error is not converted to another save');
  }
  c = scope(); c.window.showSaveFilePicker = async () => ({createWritable: async () => ({write: async value => c.writes.push(value), close: async () => {}})});
  assert.equal(await c.saveBlobWithLocation(blob, 'selected.png', {}), 'saved');
  assert.equal(c.writes[0], blob); assert.equal(c.downloads.length, 0);
  c = scope(); let command;
  c.tauriViewerInvoke = () => async (name, payload) => {command = {name, payload}; return '/tmp/native.png';};
  assert.equal(await c.saveBlobWithLocation(blob, 'native.png', {}), 'saved');
  assert.equal(command.name, 'save_viewer_file'); assert.equal(c.downloads.length, 0);
  c = scope(); c.tauriViewerInvoke = () => async () => null;
  assert.equal(await c.saveBlobWithLocation(blob, 'native-cancel.png', {}), 'cancelled');
  assert.equal(c.downloads.length, 0);
  c = scope();
  await c.saveScreenshot({_wsiScreenshotBaseIncluded: true}, null, 'png', 'section.png');
  assert.equal(c.downloads.length, 1); assert.match(c.notices.at(-1)[0], /Screenshot sent to browser downloads: section.png/);
  c = scope(); c.saveBlobWithLocation = async () => {throw error('NotAllowedError', 'Disk is read-only');};
  let fallbackCalls = 0;
  await c.saveScreenshot({_wsiScreenshotBaseIncluded: true}, () => {fallbackCalls++; return {};}, 'png');
  assert.equal(fallbackCalls, 0, 'Write failures never trigger a tissue-free screenshot fallback');
  assert.match(c.notices.at(-1)[0], /Could not save PNG screenshot: NotAllowedError: Disk is read-only/);
  assert.doesNotMatch(c.notices.at(-1)[0], /canvas access|localhost/);
  c = scope(); c.screenshotBlobFromCanvas = async () => {throw error('SecurityError', 'Canvas is tainted');};
  await c.saveScreenshot({_wsiScreenshotBaseIncluded: true}, null, 'png');
  assert.match(c.notices.at(-1)[0], /Could not encode PNG screenshot: SecurityError: Canvas is tainted/);
  assert.equal(c.downloads.length, 0);
  c = scope();
  await c.saveScreenshot({_wsiScreenshotBaseIncluded: false}, null, 'png');
  assert.equal(c.encodes.length, 0); assert.equal(c.downloads.length, 0);
  for (const format of ['png', 'jpeg', 'svg', 'pdf']) {
    c = scope(); let pickerCalls = 0;
    c.window.showSaveFilePicker = async () => {pickerCalls++; throw error('AbortError');};
    c.selectedFormat = format; const filename = 'selected-section.' + (format === 'jpeg' ? 'jpg' : format);
    c.screenshotFileName = selected => {assert.equal(selected, format); return filename;};
    await c.saveScreenshotFromDialog('download');
    assert.equal(pickerCalls, 0, 'Explicit ' + format + ' download bypasses an available picker');
    assert.equal(c.downloads.length, 1); assert.equal(c.downloads[0].name, filename);
    assert.equal(c.encodes[0].format, format); assert.equal(c.encodes[0].canvas._wsiScreenshotBaseIncluded, true);
  }
  c = scope(); c.screenshotCanvasFromRect = () => {throw error('SecurityError', 'Tissue capture blocked');};
  assert.equal(await c.saveScreenshotFromDialog('download'), false);
  assert.equal(c.downloads.length, 0, 'Failed tissue capture does not force an overlay-only download');
  assert.match(c.notices.at(-1)[0], /Tissue capture blocked.*No file was saved/);
  c = scope(); let finishDownload, pickerCalls = 0;
  c.window.showSaveFilePicker = async () => {pickerCalls++; throw error('AbortError');};
  c.downloadBlob = (blob, name) => {c.downloads.push({blob, name}); return new Promise(resolve => {finishDownload = resolve;});};
  const saving = c.saveProjectFile('download');
  assert.equal(c.annotationsDirty, true); assert.equal(c.projectDirty, true, 'Pending download preserves unsaved state');
  assert.equal(c.history.length, 0);
  finishDownload(true); assert.equal(await saving, true);
  assert.equal(pickerCalls, 0); assert.equal(c.annotationsDirty, false); assert.equal(c.projectDirty, false);
  assert.equal(c.savedAnnotationSets, true); assert.equal(c.downloads[0].name, 'study.wsiproject.json');
  assert.equal(JSON.parse(await c.downloads[0].blob.text()).project.annotation_sets.length, 2);
  assert.match(c.menus.at(-1), /browser downloads: study.wsiproject.json/);
  c = scope(); c.downloadBlob = async () => {throw error('NotAllowedError', 'Downloads blocked');};
  assert.equal(await c.saveProjectFile('download'), false);
  assert.equal(c.annotationsDirty, true); assert.equal(c.projectDirty, true); assert.equal(c.history.length, 0);
  assert.match(c.notices.at(-1)[0], /Project download failed: Downloads blocked/);
  c = scope(); c.window.showSaveFilePicker = async () => {throw error('AbortError');};
  assert.equal(await c.saveProjectFile(), false);
  assert.equal(c.downloads.length, 0); assert.equal(c.annotationsDirty, true); assert.equal(c.projectDirty, true);
  c = scope(); c.window.showSaveFilePicker = async () => ({createWritable: async () => {throw error('NotAllowedError', 'Disk is read-only');}});
  assert.equal(await c.saveProjectFile(), false);
  assert.equal(c.downloads.length, 0); assert.equal(c.annotationsDirty, true); assert.equal(c.projectDirty, true);
  // Execute the real dirty setters: repeated edits must advance the revision even if
  // both dirty booleans and the human-readable reason were already identical.
  let deferredCases = 0;
  for (const transport of ['native', 'download']) for (const initialDirty of [false, true]) {
    for (const edit of ['none', 'annotation', 'project', 'both']) {
      for (const outcome of transport === 'native' ? ['success', 'failure', 'cancel'] : ['success', 'failure']) {
        c = scope(); c.annotationsDirty = initialDirty; c.projectDirty = initialDirty;
        let settle, payload; const started = new Promise(resolve => {
          const deferred = () => new Promise((accept, reject) => {
            settle = () => outcome === 'failure' ? reject(error('NotAllowedError', 'Deferred write failed')) : accept(outcome === 'cancel' ? null : '/tmp/study.wsiproject.json');
            resolve();
          });
          if (transport === 'native') c.tauriViewerInvoke = () => (name, value) => {payload = value; return deferred();};
          else c.downloadBlob = (blob, name) => {payload = {blob, name}; return deferred();};
        });
        const pending = c.saveProjectFile(transport === 'download' ? 'download' : 'save');
        await started;
        assert.equal(c.annotationsDirty, initialDirty); assert.equal(c.projectDirty, initialDirty);
        if (edit !== 'none') c.revisionContent++;
        if (edit === 'annotation' || edit === 'both') c.setAnnotationsDirty(true, 'roi_changed');
        if (edit === 'project' || edit === 'both') c.setProjectDirty(true, 'view_changed');
        const revision = c.projectEditRevision;
        assert.equal(revision, edit === 'both' ? 2 : edit === 'none' ? 0 : 1);
        settle(); const result = await pending, savedCurrent = outcome === 'success' && edit === 'none';
        assert.equal(result, savedCurrent, [transport, initialDirty, edit, outcome].join('/'));
        assert.equal(c.projectEditRevision, revision, 'Save completion does not create or rewind an edit revision');
        assert.equal(c.annotationsDirty, savedCurrent ? false : initialDirty || edit === 'annotation' || edit === 'both');
        assert.equal(c.projectDirty, savedCurrent ? false : initialDirty || edit === 'project' || edit === 'both');
        if (outcome === 'success') {
          assert.equal(c.history.at(-1)[1].newer_changes, edit !== 'none');
          if (edit !== 'none') assert.match(c.menus.at(-1), /Newer changes remain unsaved; save again/);
        } else assert.equal(c.history.length, 0, 'Failed or cancelled exports never record a successful save');
        if (transport === 'download') assert.equal(JSON.parse(await payload.blob.text()).revision_content, 0,
          'The downloaded snapshot excludes later edits that must remain dirty');
        deferredCases++;
      }
    }
  }
  c = scope(); let finishBeforeReplacement;
  c.window.confirm = () => true;
  c.saveBlobWithLocation = () => new Promise(resolve => {finishBeforeReplacement = resolve;});
  const replacement = c.confirmProjectReplacement('opening another project');
  c.setAnnotationsDirty(true, 'roi_changed'); finishBeforeReplacement('saved');
  assert.equal(await replacement, false, 'A saved older snapshot cannot authorize discarding a newer edit');
  assert.equal(c.annotationsDirty, true); assert.match(c.notices.at(-1)[0], /still has unsaved changes/);
  c = scope(); const oldRevision = c.projectEditRevision;
  c.setProjectDirty(false, 'project_opened');
  assert.equal(c.projectEditRevision, oldRevision + 1, 'Loading another clean project invalidates an older pending save');
  const control = () => ({dataset: {}, addEventListener() {}});
  c = scope(); c.nodes.screenshotDialogDownload = control(); let screenshotDestination;
  c.saveScreenshotFromDialog = destination => {screenshotDestination = destination;};
  c.bindScreenshotDialogControls(); c.nodes.screenshotDialogDownload.onclick();
  assert.equal(screenshotDestination, 'download', 'Screenshot Download control uses the explicit download path');
  c = scope(); c.nodes.projectDownloadFile = control(); let projectDestination;
  c.saveProjectFile = destination => {projectDestination = destination;};
  c.bindProjectPanelControls(); c.nodes.projectDownloadFile.onclick({preventDefault() {}, stopPropagation() {}, currentTarget: c.nodes.projectDownloadFile});
  assert.equal(projectDestination, 'download', 'Project Download control uses the explicit download path');
  console.log('Browser save/export contracts passed, including ' + deferredCases + ' deferred native/download revision cases.');
})().catch(error => {console.error(error); process.exitCode = 1;});
