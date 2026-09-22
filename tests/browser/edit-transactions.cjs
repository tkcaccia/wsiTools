const assert = require('node:assert/strict'), fs = require('node:fs'), vm = require('node:vm');
const noop = () => {};
const context = vm.createContext({ console, wsiRoiId: r=>String(r.id),
  wsiSnapshotRoi:r=>structuredClone(r), wsiProjectKey:()=> 'slide-a',
  pushHistory:(a,x)=>a.push(x), markAnnotationsDirty:noop, buildRoiList:noop, buildLayerList:noop,
  updateButtons:noop, recordAnnotationHistory:noop, scheduleViewerStateSync:noop, wsiRequestDraw:noop,
  notify:noop, wsiResetBrush:noop, annotationUndo:[], annotationRedo:[], rois:[], layers:[],
  selectedRoi:-1, newRoiCount:0, wsiEditGeneration:0, wsiAnnotationEpoch:0, wsiWorkerSourceVersions:new Map() });
vm.runInContext(fs.readFileSync('inst/viewer/edit-transactions.js','utf8'), context);
vm.runInContext(`
const a={id:'a',rings:[1]}, b={id:'b',rings:[2]}, untouched={id:'c',rings:[3]};
const layer={id:'imported',items:[b],count:1};
rois.push(a,untouched); layers.push(layer); selectedRoi=0;
let transaction=wsiBeginEditTransaction([{id:'a',item:a},{id:'layer-b',item:b,layer}],{updates:[{id:'layer-b'}]},a);
layer.items.splice(0,1); layer.count=0; rois.push(b); a.rings=[11]; b.rings=[22];
wsiCompleteEditTransaction(transaction,a,'brush');
`, context);
assert.equal(vm.runInContext('annotationUndo[0].before.length',context),2);
assert.equal(vm.runInContext('wsiRestoreEditHistory(false)',context),true);
assert.equal(vm.runInContext('JSON.stringify(rois.map(r=>[r.id,r.rings]))',context),'[["a",[1]],["c",[3]]]');
assert.equal(vm.runInContext('JSON.stringify(layers[0].items.map(r=>[r.id,r.rings]))',context),'[["b",[2]]]');
assert.equal(vm.runInContext('rois[1] === untouched',context),true,'Unchanged ROI is not cloned');
assert.equal(vm.runInContext('wsiRestoreEditHistory(true)',context),true);
assert.equal(vm.runInContext('JSON.stringify(rois.map(r=>[r.id,r.rings]))',context),'[["a",[11]],["c",[3]],["b",[22]]]');
assert.equal(vm.runInContext('layers[0].items.length',context),0);
vm.runInContext(`
transaction=wsiBeginEditTransaction([],{updates:[]},null);
const added={id:'new',rings:[7]}; rois.push(added); selectedRoi=3; newRoiCount++;
wsiCompleteEditTransaction(transaction,added,'brush'); wsiRestoreEditHistory(false);
`,context);
assert.equal(vm.runInContext('rois.some(r=>r.id==="new")',context),false);
vm.runInContext('wsiRestoreEditHistory(true)',context);
assert.equal(vm.runInContext('newRoiCount',context),1);
assert.equal(vm.runInContext('selectedRoi',context),3);
vm.runInContext(`
transaction=wsiBeginEditTransaction(rois.map(item=>({item,id:item.id})),{removed:['b']},rois[0]);
rois.splice(2,1); selectedRoi=0; wsiCompleteEditTransaction(transaction,rois[0],'merge'); wsiRestoreEditHistory(false);
`,context);
assert.equal(vm.runInContext('rois[2].id',context),'b','Deleted neighbour returns to its original position');
context.wsiProjectKey=()=> 'slide-b';
assert.equal(vm.runInContext('wsiRestoreEditHistory(true)',context),false,'Cross-slide undo is rejected');
console.log('Changed-feature undo: imported layer, neighbour deletion, creation, ordering and slide guard passed.');
