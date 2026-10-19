// Launcher orchestration only: no desktop processes, windows or R sessions run.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

const source = fs.readFileSync(path.join(__dirname, '../../tools/wsiToolsDesktop/src/main.js'), 'utf8');
const start = source.indexOf('async function handleLaunch(');
const end = source.indexOf('\nshowLaunchLog.addEventListener', start);
assert.ok(start >= 0 && end > start, 'Test the actual launcher function from the desktop entry point.');

function launcherHarness({existing = true, openError, listenerError} = {}) {
  const calls = {invoke: [], statuses: [], errors: [], busy: [], polling: [], logs: [], opened: []};
  const context = vm.createContext({
    rAvailable: true, viewerWindowOpen: existing, launchCancelled: false,
    stopViewer: {disabled: !existing}, launchStage: {textContent: ''},
    clearLaunchError() {}, setStatus: (...args) => calls.statuses.push(args),
    appendLog: message => calls.logs.push(message), showLaunchError: error => calls.errors.push(String(error)),
    setBusy: value => calls.busy.push(value),
    startLaunchProgress() { context.launchCancelled = false; }, stopLaunchProgress() {},
    startLogPolling: () => calls.polling.push('start'), stopLogPolling: () => calls.polling.push('stop'),
    refreshLogs: async () => {}, errorMessage: error => error.message || String(error),
    listen: async () => { if (listenerError) throw listenerError; return () => calls.logs.push('unlisten'); },
    invoke: async command => { calls.invoke.push(command); },
    openViewerWindow: async url => {
      calls.opened.push(url); if (openError) throw openError;
      context.viewerWindowOpen = true; context.stopViewer.disabled = false; return 'browser';
    }
  });
  vm.runInContext(source.slice(start, end), context);
  return {context, calls, run: launcher => context.handleLaunch(launcher, 'mock IPC launch', 'Opened')};
}

for (const [label, error] of [
  ['invalid JSON', 'Could not read project JSON: expected value at line 1 column 1. Select the experiment or project index file.'],
  ['unsupported schema', 'This JSON is not a supported project. Choose experiment.cellphenotyper.json or a saved project.'],
  ['unsupported version', 'This CellPhenotyper experiment has an unsupported schema_version.']
]) test(`${label} preflight rejection preserves the existing viewer and Stop R control`, async () => {
  const {context, calls, run} = launcherHarness();
  await run(async () => { throw new Error(error); });
  assert.deepEqual(calls.invoke, [], 'A rejected new launch must never call global Stop R or close the existing window.');
  assert.equal(context.viewerWindowOpen, true); assert.equal(context.stopViewer.disabled, false);
  assert.deepEqual(calls.polling, ['start'], 'Keep existing viewer logs available.');
  assert.deepEqual(calls.busy, [true, false]); assert.equal(calls.errors.length, 1);
  assert.match(calls.errors[0], /project|schema/i); assert.deepEqual(calls.opened, []);
});

test('native failed-child cleanup is not followed by a global stop from the frontend', async () => {
  const {context, calls, run} = launcherHarness({existing: false});
  await run(async () => { throw new Error('R exited before returning a viewer URL; native child cleanup completed.'); });
  assert.deepEqual(calls.invoke, []);
  assert.equal(context.viewerWindowOpen, false); assert.equal(context.stopViewer.disabled, true);
  assert.deepEqual(calls.polling, ['start', 'stop']); assert.deepEqual(calls.busy, [true, false]);
});

test('a progress-listener failure before launch cannot close an existing viewer', async () => {
  const {context, calls, run} = launcherHarness({listenerError: new Error('Progress listener unavailable')});
  let launched = false; await run(async () => { launched = true; });
  assert.equal(launched, false); assert.deepEqual(calls.invoke, []);
  assert.equal(context.viewerWindowOpen, true); assert.equal(context.stopViewer.disabled, false);
});

test('window-open failure preserves the successfully launched live R session and recovery URL', async () => {
  const {context, calls, run} = launcherHarness({existing: false, openError: new Error('Window unavailable')});
  await run(async () => ({viewer_url: 'http://127.0.0.1:9999/viewer'}));
  assert.deepEqual(calls.invoke, []); assert.equal(context.viewerWindowOpen, true);
  assert.equal(context.stopViewer.disabled, false); assert.match(calls.errors[0], /127\.0\.0\.1:9999/);
});

test('explicit cancellation after a new launch is ready still stops that requested session', async () => {
  const {context, calls, run} = launcherHarness({existing: false});
  await run(async () => { context.launchCancelled = true; return {viewer_url: 'http://127.0.0.1:9999/viewer'}; });
  assert.deepEqual(calls.invoke, ['stop_r_viewer']); assert.deepEqual(calls.opened, []);
  assert.deepEqual(calls.busy, [true, false]);
});
