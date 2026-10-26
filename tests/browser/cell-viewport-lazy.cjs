// A 100k-cell project sends only the visible indexed positions to the viewer.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

const model = {model_id:'sam-pannuke',backbone:'SAM',taxonomy:'pannuke',
  status:'available',class_names:['Tumour','Stroma','Immune','Other','Uncertain']};
const entry = {tissue_id:'slide',frame_id:'native',source_frame_id:'native',
  source_sha256:'a'.repeat(64),index_reference:{sha256:'b'.repeat(64)},
  feature_set_id:'cellvit-1280',indexed:true,cell_count:100000,models:[model],cells:[]};
const nodes = {cellModel:{value:model.model_id},cellClass:{value:'*'},cellSearch:{value:''},
  cellThreshold:{value:'0'},cellDisagreementOnly:{checked:false},cellColourMode:{value:'class'},
  cellShow:{checked:true},cellIndexStatus:{textContent:''},cellScaleNotice:{textContent:'',hidden:true}};
let viewport = {xmin:0,ymin:0,xmax:19,ymax:19},overview = false,latest,requests = [],geometryRequests=[],expectedRegions=[viewport];
function point(i) {
  const x=i%1000,y=Math.floor(i/1000),class_id=i%5;
  return {id:'study::slide::cellvit-1280::'+i,cell_id:'cell-'+i,source_id:String(i),
    tissue_id:'slide',frame_id:'native',source_frame_id:'native',x,y,
    predictions:{[model.model_id]:{status:'predicted',class_id,
      label:model.class_names[class_id],probability:.9}}};
}
const scope = vm.createContext({console,AbortController,setTimeout(){},clearTimeout(){},
  cfg:{slide_width:1000,slide_height:100,experiment:{cell_classification:[entry]}},
  projectItems:[{tissue_id:'slide',width:1000,height:100,layers:[]}],layers:[],
  el:id=>nodes[id]||null,wsiExperimentTissueId:()=> 'slide',
  wsiReviewController:{selection:()=>[]},predictionUrl:()=>'/prediction',
  visibleSlideBounds:()=>viewport,wsiCellLayerScaleHidden:()=>overview,
  upsertViewerLayer:layer=>{latest=layer;},buildLayerList(){},draw(){},
  hexToRgba:(colour,alpha)=>colour+'@'+alpha,
  fetch:async (_url,request)=>{
    const payload=JSON.parse(request.body);assert.ok(['display_points','viewport'].includes(payload.mode));
    assert.ok(expectedRegions.some(region=>JSON.stringify(payload.bbox)===JSON.stringify(region)),'Only a visible matching-tissue pane is queried');
    const matches=[];
    for(let y=0;y<100;y++)for(let x=0;x<1000;x++)
      if(x>=payload.bbox.xmin&&x<=payload.bbox.xmax&&y>=payload.bbox.ymin&&y<=payload.bbox.ymax)
        matches.push(point(y*1000+x));
    const offset=Number(payload.cursor||0),batch=matches.slice(offset,offset+(payload.display_limit||payload.limit));
    if(payload.mode==='viewport'){
      geometryRequests.push(payload);
      return {ok:true,json:async()=>({cell_index:{source_sha256:entry.source_sha256,
        index_sha256:entry.index_reference.sha256,feature_set_id:entry.feature_set_id,frame_id:'native',
        total:matches.length,cells:batch.map(cell=>({
          ...cell,contour:Number(cell.source_id)%17===0?null:[[cell.x-.2,cell.y-.2],[cell.x+.2,cell.y-.2],[cell.x+.2,cell.y+.2],[cell.x-.2,cell.y+.2]]})),
        next_cursor:offset+batch.length<matches.length?String(offset+batch.length):null}})};
    }
    requests.push(payload);
    return {ok:true,json:async()=>({cell_index:{
      source_sha256:entry.source_sha256,index_sha256:entry.index_reference.sha256,
      feature_set_id:entry.feature_set_id,frame_id:'native',source_frame_id:'native',
      model_id:model.model_id,bbox:payload.bbox,query_scope:JSON.stringify(payload.bbox),
      total:matches.length,points:batch,
      next_cursor:offset+batch.length<matches.length?String(offset+batch.length):null}})};
  }});
vm.runInContext(fs.readFileSync(require.resolve('../../inst/viewer/cell-workbench.js'),'utf8'),scope);

(async()=>{
  await scope.wsiCellLoadDisplay();
  assert.equal(requests.length,1);
  assert.equal(latest.items.length,400);
  assert.equal(latest.metadata.cell_display.population,100000);
  assert.equal(latest.metadata.cell_display.loaded,400);
  assert.equal(new Set(latest.items.map(item=>item.colour)).size,5);
  assert.match(nodes.cellIndexStatus.textContent,/400 visible cell positions/);
  await scope.wsiCellLoadGeometry();
  assert.equal(geometryRequests.length,2,'400 native contours arrive in two bounded rich pages');
  assert.equal(latest.items.filter(item=>item.type==='polygon').length,376);
  assert.equal(latest.items.filter(item=>item.type==='point').length,24,'Missing contours remain individual centroid markers');
  assert.match(nodes.cellIndexStatus.textContent,/376 cell boundaries; 24 centroid-only/);
  assert.equal(latest.metadata.cell_display.geometry_present,true);
  await scope.wsiCellLoadDisplay();assert.equal(requests.length,1,'The unchanged viewport reuses its bounded cache');
  viewport={xmin:500,ymin:40,xmax:519,ymax:59};
  expectedRegions=[viewport];
  await scope.wsiCellLoadDisplay();
  assert.equal(requests.length,2,'Panning requests a new bounded viewport');
  assert.equal(latest.items.length,400);
  assert.ok(latest.items.every(item=>item.x>=500&&item.x<=519&&item.y>=40&&item.y<=59));
  overview=true;
  await scope.wsiCellLoadDisplay();
  assert.equal(requests.length,2,'At 1 mm the viewer does not fetch a hidden cell layer');
  assert.equal(latest.items.length,0);
  overview=false;
  await scope.wsiCellLoadDisplay();
  assert.equal(requests.length,3);
  assert.equal(latest.items.length,400,'Zooming back in restores every visible cell');
  const near={xmin:0,ymin:0,xmax:19,ymax:19},far={xmin:500,ymin:40,xmax:519,ymax:59};
  expectedRegions=[near,far];scope.multiViewLayout=2;scope.multiViewPanes=[
    {viewer:{},entry:{item:{tissue_id:'slide'}},bounds:near},
    {viewer:{},entry:{item:{tissue_id:'slide'}},bounds:far}];
  scope.multiViewVisibleSlideBounds=pane=>pane.bounds;
  await scope.wsiCellLoadDisplay();
  assert.equal(requests.length,5,'Two matching panes request only their visible regions');
  assert.equal(scope.wsiCellDisplayState().points.length,800,'Both panes retain their complete positions');
  assert.equal(latest.items.length,800);
  scope.multiViewPanes[1].entry.item.tissue_id='another-slide';expectedRegions=[near];
  await scope.wsiCellLoadDisplay();
  assert.equal(requests.length,6,'A pane assigned to another tissue is excluded');
  assert.equal(latest.items.length,400);
  console.log('Lazy 100k-cell viewport: 400 visible points, five colours, bounded pan/multi-pane requests and 1 mm hide/reopen passed.');
})().catch(error=>{console.error(error);process.exitCode=1;});
