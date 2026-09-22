// Run with: node tests/browser/geometry-worker.cjs
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const root = path.resolve(__dirname, '../../inst/viewer');
let answer;
const context = vm.createContext({ performance, atob, console, navigator: { userAgent: 'chrome', appName: 'Netscape' },
  self: { postMessage: data => { answer = data; } } });
vm.runInContext(fs.readFileSync(path.join(root, 'clipper.js'), 'utf8'), context);
context.ClipperLib = context.self.ClipperLib;
vm.runInContext(fs.readFileSync(path.join(root, 'geometry-codec.js'), 'utf8'), context);
if (process.env.WSITOOLS_TEST_GEOMETRY_ENGINE !== 'js') {
  context.wsiClipper2Binary = fs.readFileSync(path.join(root, 'clipper2.wasm')).toString('base64');
  vm.runInContext(fs.readFileSync(path.join(root, 'clipper2.js'), 'utf8'), context);
}
vm.runInContext(fs.readFileSync(path.join(root, 'geometry-kernel.js'), 'utf8'), context);
vm.runInContext(fs.readFileSync(path.join(root, 'geometry-local.js'), 'utf8'), context);
vm.runInContext(fs.readFileSync(path.join(root, 'geometry-worker.js'), 'utf8'), context);
const box = (x, y, w, h) => [[[[x,y],[x+w,y],[x+w,y+h],[x,y+h],[x,y]]]];
async function run(task) {
  context.self.onmessage({ data: { id: 1, task } });
  await vm.runInContext('geometryQueue', context);
  assert.equal(answer.ok, true, answer.error);
  return JSON.parse(JSON.stringify(answer.result));
}
function area(polygons) {
  const ring = points => Math.abs(points.slice(1).reduce((sum,p,i)=>sum+points[i][0]*p[1]-p[0]*points[i][1],0))/2;
  return polygons.reduce((sum,p)=>sum+ring(p[0])-p.slice(1).reduce((sum,h)=>sum+ring(h),0),0);
}
const original = box(10,10,100,100);
(async () => {
const task = { type:'edit', width:1000, height:1000, zoom:1, radius:8,
  class_key:'tumour', target_id:'a', operation:'extend', points:[{x:105,y:40},{x:125,y:45}], active_ids:['a'],
  sources:[{id:'a',class_key:'tumour',geometry:original,locked:false}] };
let result = await run(task);
assert.equal(result.kernel, process.env.WSITOOLS_TEST_GEOMETRY_ENGINE === 'js' ? 'clipper-js' : 'clipper2-wasm', result.kernel_warning);
let edited = result.geometry;
assert.ok(area(edited)>area(original));
assert.ok(edited.flat(2).some(p=>p[0]===10&&p[1]===10), 'Untouched original vertices retained');
const precise = (await run({...task, sources:[{...task.sources[0], geometry:box(10.1234567,10.7654321,100,100)}]})).geometry;
assert.ok(precise.flat(2).some(p=>p[0]===10.1234567&&p[1]===10.7654321), 'Untouched subpixel vertices are not quantized');
for(let i=0;i<20;i++) {
  edited = (await run({...task, operation:'subtract', points:[{x:60+i*.2,y:60},{x:62,y:65}],
    sources:[{id:'a',class_key:'tumour',geometry:edited,locked:false}]})).geometry;
  assert.ok(edited.length && area(edited)>0);
}
assert.ok(edited.some(p=>p.length>1), 'Subtraction creates a real hole');
const protectedEdit = (await run({...task, target_id:null, points:[{x:60,y:50},{x:80,y:50}],
  active_ids:['other'], sources:[{id:'other',class_key:'stroma',geometry:box(0,0,200,200),locked:false}]})).geometry;
assert.equal(protectedEdit.length,0, 'New class cannot overlap another class');
const lockedEdit = (await run({...task, target_id:null, points:[{x:60,y:50},{x:80,y:50}],
  active_ids:['other'], sources:[{id:'other',class_key:'tumour',geometry:box(0,0,200,200),locked:true}]})).geometry;
assert.equal(lockedEdit.length,0, 'Same-class locked annotation is protected');
const joined = await run({...task, active_ids:['a','b'], sources:[task.sources[0],
  {id:'b',class_key:'tumour',geometry:box(118,40,30,30),locked:false}]});
assert.deepEqual(joined.removed,['b']);
assert.ok(area(joined.geometry)>10000);
const priority = await run({...task, priority_claim:true, active_ids:['a','stroma'], sources:[task.sources[0],
  {id:'stroma',class_key:'stroma',geometry:box(108,20,80,80),locked:false}]});
assert.equal(priority.updates.length,1, 'Priority brush trims an unlocked neighboring annotation');
assert.equal(priority.updates[0].id,'stroma');
assert.ok(area(priority.updates[0].geometry)<6400, 'Neighbor loses the area claimed by the selected annotation');
const lockedPriority = await run({...task, priority_claim:true, active_ids:['a','locked'], sources:[task.sources[0],
  {id:'locked',class_key:'stroma',geometry:box(108,20,80,80),locked:true}]});
assert.equal(lockedPriority.updates.length,0, 'Priority brush never modifies a locked annotation');
assert.equal(lockedPriority.removed.length,0);
const wandClaim = await run({type:'claim',target_id:'a',geometry:box(10,10,145,100),sources:[task.sources[0],
  {id:'stroma',class_key:'stroma',geometry:box(100,10,100,100),locked:false}],protection:[]});
assert.equal(wandClaim.updates.length,1, 'Priority wand uses the same neighbor-trimming operation');
assert.ok(area(wandClaim.updates[0].geometry)<10000);
const extendingWandClaim = await run({type:'claim',target_id:'a',geometry:box(105,40,35,30),sources:[task.sources[0],
  {id:'stroma',class_key:'stroma',geometry:box(108,20,80,80),locked:false}],protection:[]});
assert.ok(area(extendingWandClaim.geometry)>area(original), 'Priority wand extends rather than replaces the selected annotation');
assert.equal(extendingWandClaim.updates.length,1, 'Priority wand atomically trims the annotation beneath its extension');
const wandEdit = await run({type:'wand_edit',project_key:'wand',operation:'extend',target_id:'a',class_key:'tumour',
  geometry:box(105,40,35,30),active_ids:['a','stroma'],sources:[
    {id:'a',class_key:'tumour',geometry:original,locked:false},
    {id:'stroma',class_key:'stroma',geometry:box(120,20,80,80),locked:false}],protection:[]});
assert.ok(area(wandEdit.geometry)>area(original), 'Normal Wand extends the selected annotation locally');
assert.ok(area(wandEdit.geometry)<10900,
  'Normal Wand excludes another annotation class');
const wandSubtract = await run({type:'wand_edit',project_key:'wand-subtract',operation:'subtract',target_id:'a',class_key:'tumour',
  geometry:box(40,40,20,20),active_ids:['a'],sources:[{id:'a',class_key:'tumour',geometry:original,locked:false}]});
assert.equal(Math.round(area(wandSubtract.geometry)),9600,'Wand subtraction changes only the local selected area');
const wandMerge = await run({type:'wand_edit',project_key:'wand-merge',operation:'extend',target_id:'a',class_key:'tumour',
  geometry:box(105,40,35,30),active_ids:['a','b'],sources:[
    {id:'a',class_key:'tumour',geometry:original,locked:false},
    {id:'b',class_key:'tumour',geometry:box(125,40,30,30),locked:false}],protection:[]});
assert.deepEqual(wandMerge.removed,['b'],'Wand merges a touched annotation of the same class');
assert.ok(area(wandMerge.geometry)>area(original));
const nested = [box(0,0,100,100)[0].concat(box(20,20,60,60)[0]), box(40,40,20,20)[0]];
const topology = await run({type:'claim',target_id:'nested',geometry:box(95,0,10,10), sources:[{id:'nested',geometry:nested}],active_ids:['nested']});
assert.equal(topology.geometry.length,2,'Island inside a hole retains separate polygon identity');
assert.equal(topology.geometry.filter(p=>p.length===2).length,1,'Hole belongs to its outer ring');
assert.ok(Math.abs(area(topology.geometry)-6850)<1e-6,'Hole, island and extension areas are correct');
const resident = await run({type:'claim',target_id:'nested',geometry:box(100,0,10,10),sources:[],active_ids:['nested']});
assert.ok(area(resident.geometry)>area(topology.geometry),'Follow-up edit reuses updated resident geometry');
assert.ok(resident.resident_ids.includes('nested'));
const independent = await run({type:'claim',project_key:'other',target_id:'nested',geometry:box(0,0,10,10),sources:[{id:'nested',geometry:box(0,0,10,10)}],active_ids:['nested']});
assert.equal(area(independent.geometry),100,'Same feature ID on another slide cannot reuse old geometry');
assert.deepEqual(independent.resident_ids,['nested']);
const packed = await run({type:'claim',project_key:'other',packed:true,target_id:'nested',geometry:box(5,5,10,10),sources:[],active_ids:['nested']});
context.packedResult = answer.result.geometry;
assert.equal(area(JSON.parse(JSON.stringify(vm.runInContext('wsiUnpackPolygons(packedResult)',context)))),175,'Packed output has XY (not XYZ) and preserves topology');
const parsed=await run({type:'parse_geojson',text:'{"type":"FeatureCollection","features":[]}'});
assert.equal(parsed.type,'FeatureCollection');
// Local clipping must preserve topology even when a stroke splits a component.
context.localSubject = [box(0,0,100,100)[0].concat(box(10,10,10,10)[0],box(70,70,10,10)[0]),
  box(200,0,100,100)[0]];
for (const clip of [box(45,-10,10,120),box(-10,40,30,20),box(65,65,30,30),box(150,0,20,20)]) {
  context.localClip = clip;
  for (const op of ['ctUnion','ctDifference','ctIntersection']) {
    context.localOp = op;
    const difference = vm.runInContext('geometryClip("ctXor", geometryLocalClip(localOp,localSubject,localClip), geometryClip(localOp,localSubject,localClip))',context);
    assert.ok(area(difference)<1e-6, 'Local clipping equals complete clipping: '+op);
  }
}
context.localClip = box(-10,40,30,20);
assert.equal(vm.runInContext('geometryLocalClip("ctUnion",localSubject,localClip).includes(localSubject[1])',context),true,
  'Untouched components are retained by reference');
const patchSource = [box(0,0,100,100)[0].concat(box(10,10,10,10)[0]),box(200,0,100,100)[0]];
await run({type:'claim',project_key:'patch',target_id:'a',geometry:[],sources:[{id:'a',geometry:patchSource}],active_ids:['a']});
const patchBefore = vm.runInContext('geometrySources.get("a").geometry',context);
await run({type:'claim',project_key:'patch',packed:true,ring_patches:true,target_id:'a',geometry:box(-10,40,30,20),sources:[],active_ids:['a']});
context.ringPatch = answer.result.geometry;
context.patchBefore = patchBefore;
assert.equal(context.ringPatch.format,'wsi-ring-patch-1');
const decoded = vm.runInContext(`(() => {
  const old = patchBefore.flat(), changed = wsiUnpackPolygons(ringPatch.changed);
  return ringPatch.rings.map(p => p.map(i => i >= 0 ? old[i] : changed[-i-1][0]));
})()`, context);
assert.deepEqual(JSON.parse(JSON.stringify(decoded)),JSON.parse(JSON.stringify(vm.runInContext('geometrySources.get("a").geometry',context))),
  'Changed-ring transport reconstructs the full canonical geometry exactly');
assert.ok(context.ringPatch.rings.flat().filter(i=>i>=0).length>=2,'Untouched holes and components use references');
console.log('Geometry worker: extension, subtraction, protection, priority claiming, locking, merging and parsing passed.');
})().catch(error=>{console.error(error);process.exitCode=1;});
