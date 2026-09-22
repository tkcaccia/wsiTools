const assert = require('node:assert/strict'), fs = require('node:fs'), vm = require('node:vm');
const context = vm.createContext({ console });
vm.runInContext(fs.readFileSync('inst/viewer/sync.js','utf8'), context);
vm.runInContext(`
const a={id:'a',geometry:{coordinates:[1]}}, b={id:'b',geometry:{coordinates:[2]}};
const base={type:'FeatureCollection',features:[a,b]}, edited={id:'a',geometry:{coordinates:[11]}};
wsiSyncAcknowledged={key:'slide',revision:1,document:{rois:base}};
const capture={key:'slide',document:{rois:{type:'FeatureCollection',features:[edited,b]}},changes:{event:'roi_brush_edited'},
  explicitPatch:{upsert:[edited],remove:[]},baseRois:base,selectedIds:['a'],selectedId:'a'};
`,context);
const result = JSON.parse(vm.runInContext('JSON.stringify(wsiSyncMessage(capture))',context));
assert.equal(result.rois,undefined); assert.equal(result.sync.full,false);
assert.deepEqual(result.sync.rois_patch.upsert.map(f=>f.id),['a']);
assert.equal(result.sync.rois_patch.order,undefined,'Geometry-only patch needs no full ordering list');
// An edit captured while an earlier edit is awaiting R must include both deltas.
vm.runInContext(`capture.baseRois=capture.document.rois;
capture.document={rois:{type:'FeatureCollection',features:[edited,{id:'b',geometry:{coordinates:[22]}}]}};
capture.explicitPatch={upsert:[capture.document.rois.features[1]],remove:[]};`,context);
assert.deepEqual(JSON.parse(vm.runInContext('JSON.stringify(wsiSyncMessage(capture).sync.rois_patch.upsert.map(f=>f.id))',context)),['a','b']);
// No iteration over coordinates or features on a viewport-only message.
vm.runInContext(`const neverScan={get features(){throw new Error('Viewport scanned annotations');}};
wsiSyncAcknowledged={key:'slide',revision:3,document:{rois:neverScan}};
capture.document={rois:neverScan};capture.explicitPatch=null;capture.changes={event:'viewport_changed'};`,context);
assert.equal(vm.runInContext('wsiSyncMessage(capture).sync.rois_patch',context),undefined);
assert.equal(vm.runInContext('wsiSyncMessage(capture,true).sync.full',context),true);
console.log('Sync: changed-feature patches, queued edits, viewport fast path and full recovery passed.');
