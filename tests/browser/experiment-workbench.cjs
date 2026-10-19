// CPU-only contract tests: node tests/browser/experiment-workbench.cjs
// Evaluates the production workbench and the viewer's own geometry primitives.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const root = path.resolve(__dirname, '../..');
const source = fs.readFileSync(path.join(root, 'inst/viewer/experiment-workbench.js'), 'utf8');
const viewerLines = fs.readFileSync(path.join(root, 'R/viewer.R'), 'utf8').split('\n');
const plain = value => JSON.parse(JSON.stringify(value));
const grid = {spacing_x_px:10, spacing_y_px:10, tile_width_px:10, tile_height_px:10, origin_x_px:0, origin_y_px:0};
const point = (x, y, id, probabilities = [0.9,0.1], tissue_id = 'a') => ({x,y,id,probabilities,tissue_id,r:100,g:20,b:200});

function scope() {
  const controls = Object.fromEntries(['experimentStatus','experimentWandSource','experimentWandReach',
    'experimentWandTolerance','experimentContrastScope','experimentTissues','predictionFeatureSource'].map(id => [id,{value:'',style:{},options:[]}]));
  controls.experimentWandSource.value = 'he';
  controls.experimentWandSource.options = [{value:'he'},{value:'feature'}];
  controls.experimentWandReach.value = '250';
  controls.experimentWandTolerance.value = '0.15';
  controls.experimentContrastScope.value = 'local';
  controls.experimentTissues.options = [{value:'a',selected:true},{value:'b',selected:true}];
  Object.defineProperty(controls.experimentTissues,'selectedOptions',{get(){return this.options.filter(o=>o.selected);}});
  controls.predictionFeatureSource.options = [{value:'experiment:pooled'}];
  const context = vm.createContext({console, performance,
    cfg:{experiment:{experiment_id:'study',manifest_sha256:'same'},slide_width:40,slide_height:40,mpp:{x:1,y:1}},
    activeProjectIndex:0, projectItems:[{id:'a',tissue_id:'a',width:40,height:40,mpp:{x:1,y:1},layers:[]},
      {id:'b',tissue_id:'b',width:40,height:40,mpp:{x:1,y:1},layers:[]}], layers:[],
    el:id=>controls[id] || null, predictionUrl:()=>'/prediction',
    predictionPayload:()=>({feature_source:'experiment:pooled',train_annotations:['one','two']}),
    selectedOptionValues:node=>Array.from(node?.selectedOptions || [],option=>option.value),
    markProjectDirty(){}, buildLayerList(){}, wsiRequestDraw(){}, setPredictionSource(){}, scheduleViewerStateSync(){},
    notify(){}, wandBusy:false, rois:[], wsiAnnotationEpoch:0,
    projectAnnotationKey:()=> 'a', pointInsideSlide:()=>true,
    wandSubtractModifier:e=>!!e.altKey, activeEditableAnnotationMaskSource:()=>null,
    brushEditableTarget:()=>-1, lockedRoi:()=>false, currentRoiClass:()=> 'tumour',
    addRoiFromBrushGroups(groups){const roi={id:'new',groups};context.rois.push(roi);return roi;},
    markAnnotationsDirty(){}, buildRoiList(){},updateButtons(){},predictionSelectedSource:()=>({unit:'tiles'})
  });
  for (const name of ['signedRingArea','maskEdgeAngle','maskTurn','chooseMaskEdge','pointInRing']) {
    const line = viewerLines.find(line => line.trim().startsWith('"function '+name+'('));
    assert.ok(line,'Viewer geometry primitive exists: '+name);
    vm.runInContext(JSON.parse(line.trim().replace(/,$/,'')).replace(/\\n$/,'\n'),context);
  }
  vm.runInContext(source, context);
  context.controls = controls;
  return context;
}

function appearance(points, scope = 'shared') {
  return {points,scope,grids:{a:grid,b:grid},recipe_id:'recipe',recipe_path:'/tmp/recipe.json',feature_source:'experiment:pooled'};
}
function ids(selection) { return plain(selection.map(record=>record.point.id)).sort(); }
let count = 0;
async function test(name, callback) { await callback(); count++; console.log('PASS '+name); }

(async()=>{
  await test('connected selection excludes different evidence, gaps and distant islands',()=>{
    const c=scope(), points=[point(5,5,'seed'),point(15,5,'near'),point(25,5,'other',[0.1,0.9]),point(35,5,'island')];
    assert.deepEqual(ids(c.wsiExperimentConnectedSelection(points,grid,{x:5,y:5},0.1,100)),['near','seed']);
    assert.deepEqual(ids(c.wsiExperimentConnectedSelection(points,grid,{x:5,y:5},0.1,8)),['seed']);
    assert.throws(()=>c.wsiExperimentConnectedSelection(points,grid,{x:5,y:30},0.1,100),/no cached features/);
  });
  await test('palette and viewport fields cannot alter probability selection',()=>{
    const c=scope(), points=[point(5,5,'seed'),point(15,5,'near')];
    const expected=ids(c.wsiExperimentConnectedSelection(points,grid,{x:5,y:5},0.01,100));
    c.cfg.zoom=200;c.cfg.scale=0.01;c.cfg.offset_x=2500;
    const recoloured=points.map(p=>({...p,r:255-p.r,g:255-p.g,b:255-p.b,colour:'#000000'}));
    assert.deepEqual(ids(c.wsiExperimentConnectedSelection(recoloured,grid,{x:5,y:5},0.01,100)),expected);
  });
  await test('invalid coordinates, duplicates and unusable evidence fail explicitly',()=>{
    const c=scope();
    assert.throws(()=>c.wsiExperimentConnectedSelection([point(5,5,'one'),point(5,5,'two')],grid,{x:5,y:5},0.1,100),/same grid/);
    assert.throws(()=>c.wsiExperimentConnectedSelection([point(NaN,5,'one')],grid,{x:5,y:5},0.1,100),/finite/);
    assert.throws(()=>c.wsiExperimentConnectedSelection([point(5,5,'one',[1])],grid,{x:5,y:5},0.1,100),/probability evidence/);
    assert.throws(()=>c.wsiExperimentConnectedSelection([point(5,5,'one')],grid,{x:5,y:5},1.1,100),/between zero and one/);
  });
  await test('native contour joins adjacent footprints, preserves holes and clips edges',()=>{
    const c=scope(), selected=[];
    for(let row=0;row<3;row++)for(let col=0;col<3;col++)if(row!==1||col!==1)selected.push({col,row,point:point(col*10+5,row*10+5,row+','+col)});
    const groups=c.wsiExperimentSelectionGroups(selected,grid,30,30);
    assert.equal(groups.length,1);assert.equal(groups[0].length,2,'Central missing observation remains a hole');
    const area=groups[0].reduce((sum,ring)=>sum+c.signedRingArea(ring),0);
    assert.equal(area,800);
    const edge=c.wsiExperimentSelectionGroups([{col:2,row:0,point:point(25,5,'edge')}],grid,26,9);
    assert.equal(c.signedRingArea(edge[0][0]),54);
    const cropped=c.wsiExperimentSelectionGroups([{col:0,row:0,point:point(0,0,'cropped')}],
      {...grid,origin_x_px:-5,origin_y_px:-5},30,30);
    assert.equal(c.signedRingArea(cropped[0][0]),25,'Negative origins clip the original footprint without shifting it');
    assert.throws(()=>c.wsiExperimentSelectionGroups(selected,{...grid,tile_width_px:11},30,30),/non-overlapping/);
  });
  await test('offset feature grid keeps native origin and exact footprints',()=>{
    const c=scope(), shifted={...grid,origin_x_px:100,origin_y_px:50};
    const selected=c.wsiExperimentConnectedSelection([point(105,55,'one'),point(115,55,'two')],shifted,{x:105,y:55},0.1,100);
    const groups=c.wsiExperimentSelectionGroups(selected,shifted,200,100);
    assert.equal(c.signedRingArea(groups[0][0]),200);
    assert.equal(Math.min(...groups[0][0].map(p=>p.x)),100);
    assert.equal(Math.min(...groups[0][0].map(p=>p.y)),50);
  });
  await test('appearance layers remain tissue scoped and retain original evidence',()=>{
    const c=scope();c.wsiExperimentInstallAppearance(appearance([point(5,5,'a1'),point(15,5,'b1',[0.2,0.8],'b')]));
    assert.equal(c.projectItems[0].layers[0].items.length,1);
    assert.equal(c.projectItems[1].layers[0].items.length,1);
    assert.deepEqual(plain(c.wsiExperimentActiveAppearance().points[0].probabilities),[0.9,0.1]);
    assert.equal(c.wsiExperimentActiveAppearance({entry:{itemIndex:1}}).points.length,0,'Other pane cannot silently use active image evidence');
    c.multiViewLayersForPane=pane=>c.projectItems[pane.entry.itemIndex].layers;
    assert.equal(c.wsiExperimentActiveAppearance({entry:{itemIndex:1}}).points[0].id,'b1');
  });
  await test('save/reopen retains per-tissue evidence, source, tolerance and physical reach',()=>{
    const c=scope();c.wsiExperimentInstallAppearance(appearance([point(5,5,'a1'),point(15,5,'b1',[0.2,0.8],'b')]));
    c.controls.experimentWandSource.value='feature';c.controls.experimentWandReach.value='123';c.controls.experimentWandTolerance.value='0.07';
    const saved=plain({experiment:c.cfg.experiment,experiment_workbench:c.wsiExperimentSnapshot(),project:{items:c.projectItems},layers:c.layers});
    c.projectItems.forEach(item=>{item.layers=[];});c.layers=[];
    c.controls.experimentWandSource.value='he';c.controls.experimentWandReach.value='250';c.controls.experimentWandTolerance.value='0.15';
    c.wsiExperimentRestoreSnapshot(saved);
    assert.equal(c.wsiExperimentActiveAppearance().points[0].id,'a1');
    assert.equal(c.projectItems[1].layers[0].items[0].id,'b1');
    assert.equal(c.controls.experimentWandSource.value,'feature');assert.equal(c.controls.experimentWandReach.value,'123');assert.equal(c.wsiExperimentTolerance(),0.07);
    assert.equal(c.controls.experimentContrastScope.value,'shared');assert.equal(c.controls.predictionFeatureSource.value,'experiment:pooled');
    assert.throws(()=>c.wsiExperimentRestoreSnapshot({...saved,experiment:{experiment_id:'different'}}),/different or revised/);
    assert.throws(()=>c.wsiExperimentRestoreSnapshot({...saved,experiment:{manifest_sha256:'changed'}}),/different or revised/);
  });
  await test('stale appearance responses never attach to a switched tissue',async()=>{
    const c=scope();let release;
    c.fetch=()=>new Promise(resolve=>{release=resolve;});
    const promise=c.wsiExperimentCreateAppearance();
    c.activeProjectIndex=1;
    release({ok:true,json:async()=>({ok:true,appearance:appearance([point(5,5,'a1')],'local')})});
    await promise;
    assert.equal(c.layers.length,0);assert.match(c.controls.experimentStatus.textContent,/active tissue changed/);
    assert.equal(vm.runInContext('wsiExperimentBusy',c),false);
  });
  await test('request-generation cancellation drops a late shared response',async()=>{
    const c=scope();c.controls.experimentContrastScope.value='shared';let release;
    c.fetch=()=>new Promise(resolve=>{release=resolve;});
    const promise=c.wsiExperimentCreateAppearance();
    vm.runInContext('wsiExperimentRequest++',c);
    release({ok:true,json:async()=>({ok:true,appearance:appearance([point(5,5,'a1')])})});await promise;
    assert.equal(c.layers.length,0);assert.equal(vm.runInContext('wsiExperimentBusy',c),false);
  });
  await test('feature wand uses physical reach and records evidence provenance',async()=>{
    const c=scope();c.projectItems[0].mpp={x:2,y:1};c.controls.experimentWandReach.value='12';c.controls.experimentWandTolerance.value='0';
    c.wsiExperimentInstallAppearance(appearance([point(5,5,'seed'),point(15,5,'x-neighbour'),point(5,15,'y-neighbour')]));
    await c.wsiExperimentRunWand({x:1000,y:2000},{x:5,y:5});
    assert.equal(c.rois.length,1);assert.equal(c.rois[0].wand_observation_count,2,'Physical circle excludes x neighbor 20µm away, retains y neighbor 10µm away');
    assert.equal(c.rois[0].wand_tolerance,0);assert.equal(c.rois[0].wand_reach_um,12);assert.equal(c.rois[0].appearance_recipe,'/tmp/recipe.json');
    assert.equal(c.rois[0].wand_source,'virchow2-probability');assert.equal(c.wandBusy,false);
    assert.equal(c.rois[0].properties.wsiTools.experiment_wand.observation_count,2);
  });
  console.log(count+' experiment workbench tests passed.');
})().catch(error=>{console.error(error);process.exitCode=1;});
