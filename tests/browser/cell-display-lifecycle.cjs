// Production model display + actual project ownership/upsert and single-view
// transition. Tiny data exercises the async source-refresh ordering, no browser.
const assert=require('node:assert/strict'),fs=require('node:fs'),vm=require('node:vm');
const viewer=fs.readFileSync(require.resolve('../../R/viewer.R'),'utf8').split('\n');
function source(name){const line=viewer.find(l=>l.trim().startsWith('"function '+name+'('));assert.ok(line,name);return JSON.parse(line.trim().replace(/,$/,''));}
const models=[{model_id:'pannuke',backbone:'HIPT',taxonomy:'pannuke',status:'available',class_names:['Tumour','Stroma']},
  {model_id:'midog',backbone:'HIPT',taxonomy:'midog',status:'unavailable',class_names:[],reason:'Official HIPT MIDOG unavailable'},
  {model_id:'lizard',backbone:'HIPT',taxonomy:'lizard',status:'available',class_names:['Lymphocyte','Epithelial']}];
const cells=Array.from({length:6},(_,i)=>({id:'cell-'+i,cell_id:'cell-'+i,tissue_id:'t1',x:10+i,y:15,
  predictions:{pannuke:{status:'predicted',label:models[0].class_names[i%2],probability:.9},
    midog:{status:'unavailable'},lizard:{status:'predicted',label:models[2].class_names[(i+1)%2],probability:.8}}}));
const pristine=JSON.stringify(cells),entry={tissue_id:'t1',frame_id:'f1',source_sha256:'a'.repeat(64),cell_count:6,models,cells};
const nodes={cellModel:{value:'pannuke'},cellClass:{value:'*'},cellSearch:{value:''},cellThreshold:{value:'0'},
  cellDisagreementOnly:{checked:false},cellColourMode:{value:'class'},cellShow:{checked:true}};
const rgb={id:'rgb',project_scoped:true,project_managed:true,source_type:'virchow2_rgb_image'},user={id:'user',source_type:'user',items:[]};
let pendingOpen;
const c=vm.createContext({console,setTimeout(){},cfg:{experiment:{cell_classification:[entry]}},activeProjectIndex:0,
  activeProjectSectionIndex:-1,multiViewSync:true,projectItems:[{id:'t1',tissue_id:'t1',layers:[rgb]}],layers:[rgb,user],
  el:id=>nodes[id]||null,wsiExperimentTissueId:()=> 't1',wsiReviewController:{selection:()=>[]},
  normaliseViewerLayer:l=>l,buildLayerList(){},draw(){},scheduleViewerStateSync(){},updateButtons(){},
  projectPayloadValue:(item,section,key)=>item[key],saveActiveProjectAnnotations(){},
  document:{body:{classList:{remove(){}}}},destroyMultiViewPanes(){},updateMultiViewControls(){},updateScaleBar(){},requestDraw(){}});
for(const name of ['layerFindIndex','layerCount','upsertViewerLayer','markInitialProjectManagedLayers','removeProjectManagedLayers','applyProjectLayers','setMultiViewLayout'])vm.runInContext(source(name),c);
vm.runInContext(fs.readFileSync(process.env.WSITOOLS_CELL_SOURCE||require.resolve('../../inst/viewer/cell-workbench.js'),'utf8'),c);
c.applyProjectPreview=item=>{pendingOpen=()=>c.applyProjectLayers(item);c.wsiCellDraw();return true;};
function display(){const layers=c.layers.filter(l=>l.source_type==='cell_identification');assert.equal(layers.length,1);return layers[0];}
function check(model,afterRefresh=false){
  const layer=display();assert.equal(layer.metadata.model_id,model.model_id);
  if(afterRefresh){assert.equal(layer.project_managed,true);assert.equal(layer.project_scoped,true);assert.equal(layer.project_image_index,0);}
  assert.equal(layer.items.length,6);
  assert.equal(c.projectItems[0].layers.filter(l=>l.source_type==='cell_identification').length,1);
  assert.ok(c.layers.includes(user),'Unowned user overlays are retained');
  for(let i=0;i<6;i++)assert.equal(layer.items[i].colour,c.WsiCellWorkbench.colour(cells[i],model,'class',0));
}
c.wsiCellDraw();check(models[0]);
for(let iteration=0;iteration<3;iteration++){
  c.multiViewLayout=2;
  nodes.cellModel.value='midog';c.wsiCellDraw();check(models[1]);
  nodes.cellModel.value='lizard';c.wsiCellDraw();check(models[2]);
  c.setMultiViewLayout(1);check(models[2]);
  // The real source-open callback runs after the immediate cell render wrapper.
  pendingOpen();check(models[2],true);
  nodes.cellModel.value='pannuke';c.wsiCellDraw();check(models[0]);
}
// Repair a live session which already contains a pre-fix stale copy.
c.layers.push({...display(),metadata:{...display().metadata,model_id:'stale-old-model'}});
assert.equal(c.layers.filter(l=>l.source_type==='cell_identification').length,2);
nodes.cellModel.value='lizard';c.wsiCellDraw();check(models[2],true);
assert.equal(JSON.stringify(cells),pristine,'Source classification and cell identities remain unchanged');
console.log('Cell lifecycle passed: MIDOG/Lizard/PanNuke switching, three compare→single async refreshes, exact class colours, one owned display layer, stale duplicate repair and preserved source data.');
