// Execute the real viewer save/history functions; never serialize derived cell caches.
const assert=require('node:assert/strict'),test=require('node:test'),fs=require('node:fs'),vm=require('node:vm');
const lines=fs.readFileSync(require('node:path').join(__dirname,'../../R/viewer.R'),'utf8').split('\n');
function source(name){const line=lines.find(value=>value.includes('"function '+name+'('));assert.ok(line,name);return JSON.parse(line.trim().replace(/,$/,''));}
test('100k derived cells are excluded before cloning at every project/history boundary',()=>{
  let touched=0;
  const cells={id:'generated',metadata:{cell_display:{derived:true}},count:100000};
  Object.defineProperty(cells,'items',{enumerable:true,get(){touched++;throw Error('Derived coordinates must never be cloned');}});
  const user={id:'user-layer',items:[{x:12,y:34}],metadata:{source_type:'user'}};
  const item={id:'image',image_data_uri:'kept',layers:[cells,user],sections:[{id:'section',layers:[cells,user],image_data_uri:'section-kept'}]};
  const scope=vm.createContext({projectItems:[item],layers:[cells,user],activeProjectIndex:0,activeProjectSectionIndex:0,
    cfg:{title:'fixture'},rois:[],trajectories:[],measures:[],mode:'pan',scale:1,offsetX:0,offsetY:0,roiOpacity:1,showRois:true,showLabels:true,
    annotationsDirty:false,projectDirty:false,annotationDirtyReason:'',projectDirtyReason:'',
    cloneProjectValue:value=>JSON.parse(JSON.stringify(value)),saveActiveProjectAnnotations(){},projectAnnotationSetsFull:()=>[{key:'review',rois:[{label:'keep'}]}],predictionTrainingSnapshot:()=>[]});
  for(const name of ['projectDerivedCellDisplay','projectPersistentLayers','projectPersistentItems','projectPersistentSnapshot','projectUndoSnapshot','projectItemsForSnapshot','projectBrowserSnapshot'])vm.runInContext(source(name),scope);
  const snapshot=scope.projectBrowserSnapshot(false),history=scope.projectUndoSnapshot();
  assert.equal(touched,0);assert.deepEqual(snapshot.layers.map(x=>x.id),['user-layer']);
  for(const value of [snapshot.project.items[0],history.project_items[0]]){
    assert.deepEqual(value.layers.map(x=>x.id),['user-layer']);assert.deepEqual(value.sections[0].layers.map(x=>x.id),['user-layer']);
  }
  assert.equal(snapshot.project.items[0].image_data_uri,undefined);assert.equal(history.project_items[0].image_data_uri,'kept');
  assert.equal(item.layers.length,2);assert.equal(item.sections[0].layers.length,2);assert.equal(item.image_data_uri,'kept');
  const saved=scope.projectPersistentSnapshot({...snapshot,layers:[cells,user],cell_workbench:{view_by_tissue:{tissue:{model_id:'chosen',visible:true}}},review_checkpoint:{records:[{cell_id:'human-1',label:'accepted'}]},project:{items:[item]}});
  const roundtrip=JSON.parse(JSON.stringify(saved));assert.equal(touched,0);
  assert.equal(roundtrip.cell_workbench.view_by_tissue.tissue.model_id,'chosen');assert.equal(roundtrip.review_checkpoint.records[0].label,'accepted');
  assert.equal(roundtrip.layers.length,1);assert.equal(roundtrip.project.items[0].sections[0].layers.length,1);
});
