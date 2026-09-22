const assert = require('node:assert/strict'), fs = require('node:fs'), vm = require('node:vm');
const context = vm.createContext({console,window:{addEventListener:()=>{}},
  subtractRings:r=>r.subtract_rings||[], positiveRingGroups:r=>[r.rings,...(r.add_groups||[])],
  boundsFromRings:()=>({xmin:0,ymin:0,xmax:100,ymax:100}),
  boundsFromRing:()=>({xmin:0,ymin:0,xmax:100,ymax:100}),
  refreshRoiGeometry:r=>vm.runInContext('wsiInvalidateGeometry(roi)',context)});
vm.runInContext(fs.readFileSync('inst/viewer/performance.js','utf8'),context);
const ring = x => [{x,y:0},{x:x+10,y:0},{x:x+10,y:10},{x,y:10},{x,y:0}];
context.roi = {id:'a',rings:[ring(0)],add_groups:[[ring(20)]],add_rings:[],subtract_rings:[]};
vm.runInContext('var before = wsiSnapshotRoi(roi); var parts = wsiCachedGeometry(roi).parts; parts[1].path = {cached:true};',context);
context.patch = {format:'wsi-ring-patch-1',rings:[[-1],[1]],changed:[[[[-1,0],[10,0],[10,10],[-1,10],[-1,0]]]]};
vm.runInContext('wsiSetWorkerGeometry(roi,patch); var after = wsiSnapshotRoi(roi);',context);
assert.equal(vm.runInContext('wsiCachedGeometry(roi).parts[1] === parts[1]',context),true,'Unchanged component path/index reused');
assert.equal(vm.runInContext('before.add_groups[0] === after.add_groups[0]',context),true,'Unchanged snapshot shared');
assert.equal(vm.runInContext('before.rings === after.rings',context),false,'Edited snapshot replaced');
assert.equal(vm.runInContext('after.add_groups[0][0] === roi.add_groups[0][0]',context),false,'Snapshot never aliases live points');
vm.runInContext('roi.add_groups[0][0][0].x=999; wsiInvalidateGeometry(roi); var modified = wsiSnapshotRoi(roi);',context);
assert.equal(vm.runInContext('before.add_groups[0][0][0].x',context),20,'Other editing tools cannot mutate old history');
assert.equal(vm.runInContext('modified.add_groups[0][0][0].x',context),999,'Other tools invalidate cached snapshots');
console.log('Component cache: paths, ring patches and isolated history snapshots passed.');
