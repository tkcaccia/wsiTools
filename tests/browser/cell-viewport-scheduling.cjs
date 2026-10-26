// Real CellViT viewport scheduler with fake time: redraws must not postpone a
// cell query forever, and navigation/reload must use the latest viewport.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

const source = fs.readFileSync(
  process.env.WSITOOLS_CELL_SOURCE || require.resolve('../../inst/viewer/cell-workbench.js'),
  'utf8'
);

function harness() {
  let now = 0, nextTimer = 0;
  const timers = new Map(), displays = [], queries = [];
  const model = { model_id: 'pannuke', backbone: 'SAM', taxonomy: 'pannuke' };
  const entry = {
    indexed: true, tissue_id: 't1', models: [model], cells: [],
    source_sha256: 'source', index_reference: { sha256: 'index' },
    frame_id: 'frame', source_frame_id: 'frame', feature_set_id: 'features'
  };
  const nodes = {
    cellModel: { value: 'pannuke' }, cellShow: { checked: true },
    cellClass: { value: '*' }, cellSearch: { value: '' },
    cellThreshold: { value: '0' }, cellDisagreementOnly: { checked: false }
  };
  const context = vm.createContext({
    console, module: { exports: {} },
    Date: class extends Date { static now() { return now; } },
    setTimeout(fn, delay) {
      const id = ++nextTimer;
      timers.set(id, { fn, at: now + delay });
      return id;
    },
    clearTimeout(id) { timers.delete(id); },
    cfg: { experiment: { cell_classification: [entry] }, slide_width: 10000, slide_height: 10000 },
    projectItems: [{ id: 't1', tissue_id: 't1', width: 10000, height: 10000 }],
    el: id => nodes[id] || null,
    wsiExperimentTissueId: () => 't1',
    predictionUrl: () => '/prediction',
    visibleSlideBounds: () => context.testBounds,
    testBounds: { xmin: 0, ymin: 0, xmax: 1000, ymax: 1000 },
    draw() {},
    onDisplay(force) { displays.push({ at: now, force, bbox: { ...context.testBounds } }); },
    onQuery(mode, extra) { queries.push({ at: now, mode, ...JSON.parse(JSON.stringify(extra || {})) }); }
  });
  // No document is installed, so the test can exercise the production scheduler
  // without building an unrelated workbench DOM or starting real network I/O.
  vm.runInContext(source, context);
  vm.runInContext(`
    wsiCellLoadDisplay = force => { onDisplay(force); return Promise.resolve(); };
    wsiCellIndexQuery = async (mode, extra) => { onQuery(mode, extra); return {}; };
    wsiCellRenderTable = () => {};
    wsiCellRenderSummary = () => {};
    wsiCellStatus = message => { throw new Error(message); };
  `, context);

  async function advance(milliseconds) {
    const until = now + milliseconds;
    for (;;) {
      const due = [...timers.entries()]
        .filter(([, timer]) => timer.at <= until)
        .sort((a, b) => a[1].at - b[1].at || a[0] - b[0])[0];
      if (!due) break;
      const [id, timer] = due;
      timers.delete(id);
      now = timer.at;
      await timer.fn();
      await Promise.resolve();
    }
    now = until;
    await Promise.resolve();
  }
  return { context, displays, queries, advance,
    viewportQueries: () => queries.filter(query => query.mode === 'viewport') };
}

(async () => {
  {
    const h = harness();
    for (let frame = 0; frame < 20; frame++) {
      h.context.wsiCellScheduleIndex();
      await h.advance(50);
    }
    assert.ok(h.displays.length > 0,
      'Continuous unchanged redraws must start the cell display without waiting for rendering to stop');
    assert.ok(h.displays[0].at <= 350, 'The first unchanged viewport must load within the debounce interval');
    assert.equal(h.viewportQueries().length, 1, 'Unchanged redraws must reuse the bounded review query');
  }
  {
    const h = harness();
    h.context.wsiCellScheduleIndex();
    await h.advance(100);
    h.context.testBounds = { xmin: 1000, ymin: 0, xmax: 2000, ymax: 1000 };
    h.context.wsiCellScheduleIndex();
    await h.advance(100);
    h.context.testBounds = { xmin: 2000, ymin: 0, xmax: 3000, ymax: 1000 };
    h.context.wsiCellScheduleIndex();
    await h.advance(299);
    assert.equal(h.displays.length, 0, 'Navigation must replace the queued old viewport rather than query it');
    await h.advance(2);
    assert.equal(h.displays.length, 1);
    assert.equal(h.displays[0].bbox.xmin, 2000, 'Only the final navigation viewport is loaded');
    assert.equal(h.viewportQueries()[0].bbox.xmin, 2000);
  }
  {
    const h = harness();
    h.context.wsiCellScheduleIndex();
    await h.advance(100);
    h.context.wsiCellScheduleIndex(true);
    for (let frame = 0; frame < 12; frame++) {
      await h.advance(50);
      h.context.wsiCellScheduleIndex();
    }
    assert.ok(h.displays.some(display => display.force === true),
      'An ordinary redraw must not drop a queued explicit reload');
    assert.ok(h.displays.find(display => display.force === true).at <= 450,
      'An explicit reload must not be starved by subsequent unchanged redraws');
  }
  {
    const h = harness();
    h.context.wsiCellScheduleIndex();
    await h.advance(350);
    assert.equal(h.viewportQueries().length, 1);
    h.context.wsiCellScheduleIndex(true);
    for (let frame = 0; frame < 12; frame++) {
      await h.advance(50);
      h.context.wsiCellScheduleIndex();
    }
    assert.equal(h.viewportQueries().length, 2,
      'Explicit reload must refresh a previously queried viewport once, despite ordinary redraws');
  }
  console.log('Cell viewport scheduling passed: continuous redraws, latest navigation viewport, queued force preservation and explicit refresh.');
})().catch(error => { console.error(error); process.exitCode = 1; });
