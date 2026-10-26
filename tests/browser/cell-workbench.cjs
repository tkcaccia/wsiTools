// Production helpers + UI handlers against bounded data. No browser, GPU or model inference.
const assert=require('node:assert/strict'),fs=require('node:fs'),vm=require('node:vm');
const API=require('../../inst/viewer/cell-workbench.js'),Review=require('../../inst/viewer/review-workbench.js');
const clone=x=>JSON.parse(JSON.stringify(x));
const models=[{model_id:'hipt-pannuke',backbone:'HIPT',taxonomy:'PanNuke',class_names:['neoplastic','inflammatory'],status:'available',score_semantics:'uncalibrated_pixel_type_vote_fraction'},
  ...['CoNSeP','Lizard','NuCLS main','NuCLS super','Ocelot','MIDOG','PanopTILs'].map(taxonomy=>({model_id:'hipt-'+taxonomy,backbone:'HIPT',taxonomy,class_names:['class-a','class-b'],status:'available',score_semantics:'uncalibrated_model_probability'})),
  {model_id:'sam-midog',backbone:'SAM',taxonomy:'MIDOG',class_names:[],status:'unavailable',reason:'No cached output'}];
const entry={tissue_id:'t1',frame_id:'f1',source_frame_id:'f1',index_reference:{sha256:'d'.repeat(64)},feature_set_id:'cellvit-hipt',source_sha256:'a'.repeat(64),descriptor:{backbone:'HIPT'},models,cells:[]};
for(let i=0;i<6;i++)entry.cells.push({id:'study::t1::cellvit-hipt::'+i,cell_id:'cell-'+i,source_id:String(i),tissue_id:'t1',frame_id:'f1',x:10+i*5,y:12,segmentation_disagreement:i===1,tissue_context:{label:'stroma'},predictions:Object.fromEntries(models.filter(m=>m.status==='available').map(m=>[m.model_id,{status:i===5?'abstained':'predicted',label:m.class_names[i%2],class_id:i%2,probability:i===4?.4:.8,probabilities:m.score_semantics==='uncalibrated_model_probability'?[.8,.2]:null}]))});
const input=clone(entry);
assert.equal(API.taxonomyName("nucls_super"),"NuCLS super");assert.equal(API.taxonomyName("consep"),"CoNSeP");
assert.equal(API.displayId({source_id:'nucleus/a',cell_id:'immutable-canonical'}),'nucleus/a');
assert.equal(API.displayId({source_id:0,cell_id:'immutable-canonical'}),'0');
assert.equal(API.displayId({cell_id:'a'.repeat(80)+'::99'}),'…'+'a'.repeat(27)+'::99');
assert.equal(API.rows(entry,models[0],{class_name:'neoplastic'}).length,3);
assert.equal(API.rows(entry,models[0],{class_name:'__review__',threshold:.5}).length,2);
assert.equal(API.rows(entry,models[0],{disagreements_only:true}).length,1);
assert.equal(API.disagreement({segmentation_review_required:true}),true,'A changed boundary requires review even when no agreement gate failed');
assert.equal(API.segmentationStatus({}),'unavailable','Missing QC is not an agreement result');
assert.equal(API.segmentationStatus({segmentation_disagreement:false}),'available');
assert.notEqual(API.colour({},models[0],'disagreement'),API.colour({segmentation_disagreement:false},models[0],'disagreement'));
assert.match(API.csv({...entry,cells:[{...entry.cells[0],segmentation_review_status:'unavailable'}]},models[0]),/"unavailable","",""$/,'Unavailable QC exports no fabricated false disagreement');
assert.match(API.csv({...entry,cells:[{...entry.cells[0],segmentation_disagreement:false,segmentation_review_required:true}]},models[0]),/"available","false","true"$/,'Changed boundaries retain review-required separately from failed agreement gates');
const unavailableGeometry={...clone(entry.cells[0]),segmentation_review_status:'unavailable',segmentation_geometry_unavailable_reason:'Original contour is self-intersecting; geometry was not repaired.',segmentation_review_reason:['invalid_original_geometry'],segmentation_review_required:true,segmentation_disagreement:true,segmentation_action:'retain_original',segmentation_auto_adjustment_eligible:false,segmentation_iou:null,segmentation_cellvit_coverage:null,segmentation_stardist_coverage:null,segmentation:{segmentation_iou:.9}};
assert.equal(API.segmentationStatus(unavailableGeometry),'unavailable','Review-required flags never force unavailable geometry to available');
assert.equal(API.segmentationStatus({segmentation:{segmentation_review_status:'unavailable',segmentation_review_required:true}}),'unavailable','Explicit nested status is also preserved');
assert.match(API.segmentationSummary(unavailableGeometry),/geometry unavailable.*review required.*self-intersecting/);
assert.equal(API.rows({...entry,cells:[unavailableGeometry]},models[0],{disagreements_only:true}).length,1,'Unassessable original contours remain in the human-review queue');
const geometryCsv=API.csv({...entry,cells:[unavailableGeometry]},models[0]),csvFields=line=>[...line.matchAll(/"((?:[^"]|"")*)"(?:,|$)/g)].map(match=>match[1].replace(/""/g,'"'));
const [geometryHeaders,geometryValues]=geometryCsv.split('\n').map(csvFields),geometryExport=Object.fromEntries(geometryHeaders.map((key,index)=>[key,geometryValues[index]]));
assert.equal(geometryExport.segmentation_review_status,'unavailable');assert.equal(geometryExport.segmentation_disagreement,'true');assert.equal(geometryExport.segmentation_review_required,'true');
assert.equal(geometryExport.segmentation_geometry_unavailable_reason,unavailableGeometry.segmentation_geometry_unavailable_reason);
assert.equal(geometryExport.segmentation_auto_adjustment_eligible,'false');assert.equal(geometryExport.segmentation_action,'retain_original');
for(const key of ['segmentation_iou','segmentation_cellvit_coverage','segmentation_stardist_coverage'])assert.equal(geometryExport[key],'','Unavailable geometry exports empty/null metrics, never zero or nested stale scores');
assert.equal(geometryExport.predicted_label,'neoplastic','Unavailable geometry preserves the original class');
assert.notEqual(API.colour(entry.cells[0],models[0],'context'),API.colour(entry.cells[0],models[0],'class'));
for(const mode of ['class','context','confidence','disagreement','unknown'])assert.match(API.colour(entry.cells[0],models[0],mode),/^#[0-9A-Fa-f]{6}$/,'Every colour mode uses renderer-compatible hex');
for(const model of models.filter(model=>model.status==='available'))assert.equal(new Set(model.class_names.map(label=>API.colour({predictions:{[model.model_id]:{status:'predicted',label,probability:.8}}},model,'class'))).size,model.class_names.length,'Ordered taxonomy classes have distinct colours');
const soft={...entry.cells[0],tissue_context:{label:'stroma',coverage_fraction:.75,uncovered_fraction:.25,ambiguous_overlap_fraction:.1,boundary_crossing:true,regions:[{tissue_id:'bora:1',label:'stroma',overlap_fraction:.75,centroid_inside:true,signed_centroid_boundary_distance_um:1.25,unsigned_cell_boundary_distance_um:0}]}};
assert.match(API.contextSummary(soft),/75.0% tissue coverage.*boundary.*overlapping/);
assert.match(API.contextDetails(soft),/bora:1.*1.25 µm/);
assert.match(API.csv({...entry,cells:[soft]},models[0]),/tissue_regions_json/);
assert.match(API.csv({...entry,cells:[soft]},models[0]),/bora:1/);
assert.match(API.scoreSemantics({score_semantics:'uncalibrated_softmax_cell_class_vote_counts'}),/scores may be unavailable/);
assert.equal(API.suggestedLabel(entry.cells,models[0],[entry.cells[0].id]),'neoplastic');
assert.throws(()=>API.suggestedLabel(entry.cells,models[0],entry.cells.slice(0,2).map(cell=>cell.id)),/different or unavailable/);
assert.throws(()=>API.suggestedLabel(entry.cells,models[0],[entry.cells[5].id]),/different or unavailable/);
assert.notEqual(API.colour({predictions:{[models[0].model_id]:{status:'predicted',label:'neoplastic',probability:null}}},models[0],'confidence'),API.colour({predictions:{[models[0].model_id]:{status:'predicted',label:'neoplastic',probability:0}}},models[0],'confidence'),'Missing native scores are grey rather than fabricated zero scores');
assert.throws(()=>API.rows(entry,models[0],{threshold:2}),/threshold/);
const image={tissue_id:'t1',frame_id:'f1',sha256:'a'.repeat(64),width:100,height:100,mpp_x:.5,mpp_y:.5,coordinate_convention:'pixel-edge-origin_centres-in-native-pixels'};
const spot={id:'rna::1',original_id:'BC1',observation_unit:'spot',tissue_id:'t1',frame_id:'f1',dataset_id:'rna',x:10,y:12,cluster_values:{cluster:'RNA-cluster-A'}};
assert.deepEqual(API.guide(entry.cells,[spot],image,1),[entry.cells[0].id]);
assert.deepEqual(API.guide(entry.cells,[{...spot,tissue_id:'other'}],image,100),[]);
assert.throws(()=>API.guide(entry.cells,[spot],{...image,mpp_x:null},10),/MPP/);
assert.deepEqual(entry,input,'Filtering, colour and spatial guidance never change immutable source predictions');
const observations=entry.cells.map(cell=>({id:cell.id,original_id:cell.cell_id,tissue_id:'t1',frame_id:'f1',observation_unit:'cell',dataset_id:'experiment:cellvit-hipt',feature_set_id:'cellvit-hipt',x:cell.x,y:cell.y,cluster_values:{model:cell.predictions[models[0].model_id].label},predictions:clone(cell.predictions)})).concat([spot]);
const context={schema:'wsitools-review-context',schema_version:1,context_id:'review',source_signature:'b'.repeat(64),experiment_id:'study',images:[image],observations,reductions:[]};
const review=Review.create({context,getActiveTissueId:()=> 't1'});
review.selectIds(entry.cells.slice(0,4).map(cell=>cell.id));
review.review({action:'correct',label:'reviewed-cell-type',reviewer:'QA'});
const saved=review.snapshot(),reopened=Review.create({context,getActiveTissueId:()=> 't1'});reopened.restore(saved);
assert.equal(reopened.snapshot().review_records[0].label,'reviewed-cell-type');assert.equal(context.observations[0].predictions[models[0].model_id].label,'neoplastic');
reopened.review({action:'uncertain',reviewer:'QA'});assert.equal(reopened.snapshot().review_records[0].status,'uncertain');
reopened.undo();assert.equal(reopened.snapshot().review_records[0].status,'reviewed');reopened.redo();assert.equal(reopened.snapshot().review_records[0].status,'uncertain');
assert.match(API.csv(entry,models[0],saved.review_records),/human_label/);assert.match(API.csv(entry,models[0],saved.review_records),/reviewed-cell-type/);

class Node{constructor(tag,id){this.tagName=tag;this.id=id;this.children=[];this.style={};this.checked=false;this.disabled=false;this._value='';this.textContent='';this.classList={add(){}};this.options=[];}
 appendChild(n){this.children.push(n);if(this.tagName==='select'){this.options.push(n);if(this.options.length===1)this._value=n.value;}return n;}
 replaceChildren(...values){this.children=[];this.options=[];this._value='';for(const v of values)this.appendChild(v);}
 set value(v){this._value=String(v);}get value(){return this._value;}
 setAttribute(name,value){this.attributes??={};this.attributes[name]=value;}addEventListener(){}focus(){}
 set innerHTML(html){this.children=[];for(const match of html.matchAll(/<(\w+)\b([^>]*\bid="([^"]+)"[^>]*)>/g)){const n=new Node(match[1],match[3]);const value=match[2].match(/\bvalue="([^"]*)"/);if(value)n.value=value[1];n.checked=/\bchecked\b/.test(match[2]);nodes[n.id]=n;
  if(match[1]==='select'){const body=html.slice(match.index+match[0].length).split('</select>')[0];for(const item of body.matchAll(/<option\b[^>]*value="([^"]*)"[^>]*>([^<]*)<\/option>/g)){const opt=new Node('option');opt.value=item[1];opt.textContent=item[2];n.appendChild(opt);}}}}
}
const nodes={},updates=[],notices=[],catalog=[clone(entry)];
let cpuCalls=0,lastRequest=null;
const scope=vm.createContext({console,Blob,document:{createElement:tag=>new Node(tag),body:new Node('body')},setTimeout(){},
 cfg:{slide_width:100,slide_height:100,experiment:{manifest_sha256:'c'.repeat(64),cell_classification:catalog},prediction:{fastpls_installed:true},review_workbench:context},
 activeProjectIndex:0,projectItems:[{id:'t1',tissue_id:'t1',layers:[]}],layers:[],WsiReviewWorkbench:Review,wsiReviewController:review,wsiReviewContext:context,wsiReviewSync:Promise.resolve(),wsiExperimentClassifiers:[],
 el:id=>nodes[id]||null,wsiExperimentTissueId:()=> 't1',wsiWorkspaceBind:()=>()=>true,predictionUrl:()=>'/prediction',visibleSlideBounds:()=>({xmin:0,ymin:0,xmax:100,ymax:100}),
 upsertViewerLayer:layer=>updates.push(clone(layer)),buildLayerList(){},draw(){},markProjectDirty(){},notify:message=>notices.push(message),
 projectBrowserSnapshot:()=>({project:{items:[{id:'t1'}]},review_checkpoint:review.snapshot()}),applyProjectPreview(){},restoreBrowserProjectInLiveSession:async()=>true,handleViewerCommand(){},handleViewerCommands(){},wsiExperimentRecordPrediction(){},
 downloadBlob:async()=>true,fetch:async(url,request)=>{cpuCalls++;lastRequest=JSON.parse(request.body);return {ok:true,json:async()=>({ok:true,prediction:{count:6},commands:[]})};}});
vm.runInContext(fs.readFileSync(require.resolve('../../inst/viewer/cell-workbench.js'),'utf8'),scope);
scope.wsiCellBind();assert.equal(nodes.cellBackbone.value,'HIPT');assert.equal(nodes.cellTaxonomy.options.length,8,'All available taxonomies are discoverable');assert.equal(nodes.cellTrain.disabled,true,'One human class cannot enable training');
assert.equal(nodes.cellTableDetails.open,undefined,'The optional cell table is collapsed by default');
assert.equal(nodes.cellClassSummary.children.length,3,'Class totals are visible without opening a cell table');
const cellPanel=scope.document.body.children.find(node=>node.id==='cellWorkbench');assert.match(cellPanel.style.width,/340px/,'Compact cell panel leaves the image visible');
const navigation=[];for(const id of ['zoomIn','zoomOut','fit'])nodes[id]={click:()=>navigation.push(id)};
nodes.cellImageZoomIn.onclick();nodes.cellImageZoomOut.onclick();nodes.cellImageFit.onclick();assert.deepEqual(navigation,['zoomIn','zoomOut','fit'],'Panel navigation invokes existing viewer handlers while external navigation is covered');
nodes.cellPanelSize.onclick();assert.match(cellPanel.style.width,/680px/);nodes.cellPanelSize.onclick();assert.match(cellPanel.style.width,/340px/);
assert.match(nodes.cellTrainingStatus.textContent,/at least two classes/,'Disabled fitting explains the required reviewed labels');
assert.equal(nodes.cellTable.children[0].children[1].textContent,'0','Table shows readable source ID');
assert.equal(nodes.cellTable.children[0].children[1].title,entry.cells[0].cell_id,'Full canonical ID is retained in tooltip');
assert.equal(nodes.cellTable.children[0].children[0].children[0].attributes['aria-label'],'Select cell '+entry.cells[0].cell_id,'Accessible selection identity remains unchanged');
catalog[0].cells[0].segmentation_review_status='unavailable';scope.wsiCellRenderTable();
assert.equal(nodes.cellTable.children[0].children[6].textContent,'unavailable','Table distinguishes missing QC from no flagged disagreement');
delete catalog[0].cells[0].segmentation_review_status;
const originalGeometryCell=catalog[0].cells[0];catalog[0].cells[0]=clone(unavailableGeometry);scope.wsiCellRenderTable();
assert.match(nodes.cellTable.children[0].children[6].textContent,/geometry unavailable.*review required.*self-intersecting/,'Table explains unassessable geometry instead of presenting a failed overlap score');
assert.match(nodes.cellSelectedInspector.textContent,/self-intersecting/,'Selected cell inspector retains the reason');
assert.equal(nodes.cellTable.children[0].children[2].textContent,'neoplastic','Geometry failure does not remove class prediction');
catalog[0].cells[0]=originalGeometryCell;
nodes.cellBackbone.value='SAM';nodes.cellBackbone.onchange();assert.equal(nodes.cellTaxonomy.value,'MIDOG');assert.match(nodes.cellWorkbenchStatus.textContent,/No cached output/);
nodes.cellBackbone.value='HIPT';nodes.cellBackbone.onchange();nodes.cellTaxonomy.value='Lizard';nodes.cellTaxonomy.onchange();assert.equal(nodes.cellModel.value,'hipt-Lizard');
review.selectIds([entry.cells[0].id]);nodes.cellHumanLabel.value='manual';nodes.cellReviewer.value='QA';nodes.cellCorrect.onclick();assert.equal(review.snapshot().review_records.find(r=>r.id===entry.cells[0].id).label,'manual');
const beforeFill=clone(review.snapshot());nodes.cellUsePrediction.onclick();assert.equal(nodes.cellHumanLabel.value,'class-a');assert.deepEqual(review.snapshot(),beforeFill,'Filling a suggested class never records a human decision');
assert.equal(catalog[0].cells[0].predictions['hipt-Lizard'].label,'class-a','Actual Correct button preserves source model output');
const session=scope.projectBrowserSnapshot();assert.equal(session.cell_workbench.view_by_tissue.t1.taxonomy,'Lizard');assert.equal(session.review_checkpoint.review_records.find(r=>r.id===entry.cells[0].id).label,'manual');
(async()=>{await scope.restoreBrowserProjectInLiveSession(session);assert.equal(nodes.cellTaxonomy.value,'Lizard');
 const focused=[];scope.wsiReviewFocusObservation=async row=>focused.push(clone(row));
 review.selectIds([entry.cells[0].id]);await nodes.cellFocusSelected.onclick();
 assert.deepEqual(focused[0],{id:entry.cells[0].id,tissue_id:'t1',frame_id:'f1',x:10,y:12},'The actual Focus selected cell button reaches the registered image-focus bridge');
 nodes.cellFindId.value=entry.cells[2].source_id;await nodes.cellFind.onclick();
 assert.equal(focused.at(-1).id,entry.cells[2].id,'Exact-ID Find awaits the same image focus bridge');
 const focusCount=focused.length;catalog[0].cells[2].frame_id='foreign';await nodes.cellFocusSelected.onclick();
 assert.equal(focused.length,focusCount,'A stale cell frame cannot move the image');assert.match(nodes.cellWorkbenchStatus.textContent,/current tissue coordinate frame/);catalog[0].cells[2].frame_id='f1';
 review.selectIds([spot.id]);await scope.wsiCellFit(false);assert.equal(cpuCalls,0,'RNA selection never reaches fitting');assert.match(nodes.cellFitStatus.textContent,/RNA spots cannot train/);
 scope.wsiCellRender();assert.match(nodes.cellFitStatus.textContent,/RNA spots cannot train/,'A periodic model render cannot erase fitting errors');
 review.selectIds(entry.cells.slice(0,2).map(cell=>cell.id));review.review({action:'correct',label:'type-A',reviewer:'QA'});
 review.selectIds(entry.cells.slice(2,4).map(cell=>cell.id));review.review({action:'correct',label:'type-B',reviewer:'QA'});
 review.selectIds(entry.cells.slice(0,4).map(cell=>cell.id));let releaseReview;
 scope.wsiCellRenderTable();assert.equal(nodes.cellTrain.disabled,false,'Two explicit classes with two cells each enable fitting');
 assert.match(nodes.cellTrainingStatus.textContent,/type-A: 2.*type-B: 2.*ready/);
 const prototypeLabels=Review.create({context,getActiveTissueId:()=> 't1'});prototypeLabels.selectIds(entry.cells.slice(0,2).map(cell=>cell.id));prototypeLabels.review({action:'correct',label:'__proto__',reviewer:'QA'});
 prototypeLabels.selectIds(entry.cells.slice(2,4).map(cell=>cell.id));prototypeLabels.review({action:'correct',label:'constructor',reviewer:'QA'});
 assert.equal(API.trainingState(context.observations,prototypeLabels.snapshot().review_records,entry.cells.slice(0,4).map(cell=>cell.id),'cellvit-hipt').ready,true,'Literal user labels cannot collide with object prototype names');
 scope.wsiReviewSync=new Promise(resolve=>{releaseReview=resolve;});
 const firstFit=scope.wsiCellFit(false),duplicateFit=scope.wsiCellFit(false);
 assert.equal(nodes.cellTrain.disabled,true);assert.equal(cpuCalls,0,'Review synchronization still pending');
 review.selectIds([]);scope.wsiCellRender();
 releaseReview();await Promise.all([firstFit,duplicateFit]);assert.equal(cpuCalls,1,'A duplicate click during awaited review sync cannot start a second CPU fit');
 assert.equal(lastRequest.train_ids.length,4,'The click-time selection survives an asynchronous selection rerender');
 assert.equal(lastRequest.checkpoint.review_records.length,4,'The complete click-time reviewed checkpoint is sent');
 assert.match(nodes.cellFitStatus.textContent,/saved\/applied/);scope.wsiCellRender();assert.match(nodes.cellFitStatus.textContent,/saved\/applied/,'Completed fitting stays visible');
 await scope.wsiCellSelectGeometry({type:'Polygon',coordinates:[[[5,5],[18,5],[18,20],[5,20],[5,5]]]});
 assert.equal(review.selection().length,2,'Image-first selection uses native cell coordinates');
 const originalCells=catalog[0].cells;catalog[0].indexed=true;catalog[0].cell_count=6;catalog[0].cells=[];
 scope.projectItems[0].mpp={x:.25,y:.25};
 scope.cfg.experiment.viewer_evidence={cell_coverage:[{tissue_id:'t1',coverage:'partial',cell_count:6,completed_shards:2,planned_shards:104,
   cell_position_bounds:{xmin:10,ymin:12,xmax:22,ymax:23},focus_cell_position:{x:10,y:12}}]};
 const zooms=[];scope.zoomToSlideBounds=(bounds,pad)=>zooms.push({bounds:clone(bounds),pad});
 scope.wsiCellRender();
 assert.match(nodes.cellCoverageStatus.textContent,/6 analyzed cells in 2\/104 completed shards.*x 10–22, y 12–23 px.*Other slide regions have no CellViT prediction/);
 assert.equal(nodes.cellZoomAvailable.hidden,false,'Checked partial coverage exposes the image navigation action');
 nodes.cellZoomAvailable.onclick();assert.equal(zooms.length,1);assert.deepEqual(zooms[0].bounds,{xmin:10,ymin:12,xmax:22,ymax:23});
 assert.match(nodes.cellWorkbenchStatus.textContent,/analyzed CellViT cells.*below 1 mm/);
 scope.cfg.experiment.viewer_evidence.cell_coverage[0].cell_count=7;scope.wsiCellRefreshCoverage();
 assert.match(nodes.cellCoverageStatus.textContent,/does not match this cell source/);
 assert.equal(nodes.cellZoomAvailable.hidden,true,'Contradictory coverage cannot trigger navigation');
 scope.cfg.experiment.viewer_evidence.cell_coverage[0].cell_count=6;scope.wsiCellRefreshCoverage();
 const compact=originalCells.map(cell=>({...clone(cell),source_frame_id:'f1'}));
 scope.fetch=async(url,request)=>{lastRequest=JSON.parse(request.body);return {ok:true,json:async()=>({ok:true,cell_index:lastRequest.mode==='display_points'?{mode:'display_points',source_sha256:catalog[0].source_sha256,index_sha256:catalog[0].index_reference.sha256,feature_set_id:catalog[0].feature_set_id,frame_id:'f1',source_frame_id:'f1',model_id:'hipt-Lizard',points:compact,total:6,bbox:lastRequest.bbox,query_scope:'exact-whole-image',next_cursor:null}:{mode:lastRequest.mode,source_sha256:catalog[0].source_sha256,cells:originalCells.slice(0,2),total:100000,truncated:true,summary:[{model_id:'hipt-Lizard',label:'class-a',status:'predicted',count:60000},{model_id:'hipt-Lizard',label:'class-b',status:'predicted',count:40000}]}})};};
 await scope.wsiCellIndexQuery('viewport',{bbox:{xmin:0,ymin:0,xmax:20,ymax:20}});
 assert.equal(lastRequest.action,'experiment_cell_index');assert.equal(lastRequest.limit,500,'Rich review viewport requests remain bounded independently of display');
 assert.equal(catalog[0].cells.length,2);await scope.wsiCellLoadDisplay();scope.wsiCellRenderSummary();scope.wsiCellDraw();
 const shown=updates.filter(layer=>layer.source_type==='cell_identification').at(-1);
 assert.equal(shown.items.length,6,'A truncated rich viewport never hides the independent complete cell display');
 assert.equal(shown.metadata.cell_display.hide_at_scale_bar_um,1000);assert.equal(shown.metadata.lod.enabled,false);assert.equal(shown.metadata.lod.full_coordinates,true);assert.equal(shown.metadata.cell_display.complete,true);
 assert.equal(catalog[0].cells.length,2,'Whole-image display never replaces the rich review page');
 assert.match(nodes.cellIndexStatus.textContent,/All 6 visible cell positions loaded.*pan or zoom/);
 assert.equal(nodes.cellSelectImage.disabled,false,'Image selection is available independently of rich viewport truncation');
 assert.equal(nodes.cellClassSummary.children[0].textContent,'class-a: 60,000','Whole-slide class counts remain independent from review and display pages');
 let scaleHidden=true;scope.wsiCellLayerScaleHidden=layer=>scaleHidden&&layer.metadata.cell_display.hide_at_scale_bar_um===1000;const beforeScaleLayerCount=updates.length;
 scope.draw();assert.equal(nodes.cellScaleNotice.hidden,false);assert.match(nodes.cellScaleNotice.textContent,/Cells hidden at overview scale \(1 mm or more\)/);assert.equal(updates.length,beforeScaleLayerCount,'Scale notice refresh never rebuilds the cell population');assert.equal(nodes.cellShow.checked,true,'Automatic overview hiding preserves the manual Show cells setting');
 scaleHidden=false;scope.draw();assert.equal(nodes.cellScaleNotice.hidden,true);assert.equal(nodes.cellShow.checked,true);nodes.cellShow.checked=false;scaleHidden=true;scope.draw();assert.equal(nodes.cellScaleNotice.hidden,true,'Manual visibility remains separate from the overview notice');assert.equal(nodes.cellShow.checked,false);nodes.cellShow.checked=true;
 scaleHidden=false;review.selectIds([compact[0].id]);scope.wsiCellDraw();const selectedPoint=updates.filter(layer=>layer.source_type==='cell_identification').at(-1).items[0];
 assert.equal(selectedPoint.colour,API.colour(compact[0],models.find(model=>model.model_id==='hipt-Lizard'),'class'),'Selecting a cell preserves its cell-type colour');assert.equal(selectedPoint.radius,7);
 console.log('Cell taxonomy/filter/layer/review/persistence/spatial-guidance UI contracts passed.');
})().catch(error=>{console.error(error);process.exitCode=1;});
