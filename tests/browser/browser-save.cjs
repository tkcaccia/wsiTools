// Execute production save/export helpers: browser policy failures differ from write/encoding failures.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const lines = fs.readFileSync(path.join(__dirname, '../../R/viewer.R'), 'utf8').split('\n');
const functions = ['saveBlobWithLocation', 'saveScreenshot'].map(name => {
  const line = lines.find(value => value.trim().startsWith('"async function ' + name + '('));
  assert.ok(line, name + ' comes from production code');
  return JSON.parse(line.trim().replace(/,$/, ''));
}).join('');
const error = (name, message = name) => Object.assign(new Error(message), {name});
function scope() {
  const notices = [], downloads = [], writes = [], encodes = [];
  const context = vm.createContext({window: {}, notices, downloads, writes, encodes,
    tauriViewerInvoke: () => null, notify: (...args) => notices.push(args),
    downloadBlob: async (blob, name) => downloads.push({blob, name}),
    blobToBase64: async () => 'cG5n', pickerTypeFilters: () => [],
    screenshotNormaliseFormat: value => value, screenshotFormat: () => 'png',
    screenshotFormatLabel: () => 'PNG', screenshotFileName: () => 'tissue.png',
    screenshotPickerType: () => ({description: 'PNG', accept: {'image/png': ['.png']}}),
    screenshotBlobFromCanvas: async canvas => {encodes.push(canvas); return canvas.blob || 'image-bytes';}
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
  console.log('Browser missing/blocked picker, cancellation, write failure, native save and screenshot diagnostics contracts passed.');
})().catch(error => {console.error(error); process.exitCode = 1;});
