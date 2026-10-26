// CPU-only contract: node tests/browser/bora-three-group-manual-review.cjs
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const vm=require('node:vm');

const source=fs.readFileSync(path.resolve(__dirname,'../../inst/viewer/experiment-workbench.js'),'utf8');
const predicted={id:'model_7',locked:true,colour:'#f97316',tissue_id:'PM016',
  properties:{value:3,predicted_not_expert_annotation:true}};
const layer={source_type:'bora_candidate_review',items:[predicted],metadata:{
  review_only:true,training_eligible:false,group_count:3,
  source_sha256:'a'.repeat(64),source_frame_id:'pm016-native',image_sha256:'b'.repeat(64)}};
const rois=[],events=[],messages=[];
const context=vm.createContext({layers:[layer],rois,projectItems:[{tissue_id:'PM016'}],
  activeProjectIndex:0,activeProjectSectionIndex:-1,
  el:()=>null,
  performance:{now:()=>1000},mode:'select',
  Blob,
  hexToRgba:(colour,alpha)=>`rgba(${colour},${alpha})`,
  roiFeature:roi=>({type:'Feature',id:roi.id,properties:roi.properties,
    geometry:{type:'Polygon',coordinates:roi.rings}}),
  denseLayerEditableGroups:()=>[[[{x:1,y:2},{x:5,y:2},{x:5,y:7},{x:1,y:2}]]],
  refreshRoiGeometry:roi=>{roi.geometry_type='Polygon';roi.edited=true;},
  selectAnnotation:i=>events.push(['select',i]),pushAnnotationUndo:x=>events.push(['undo',x]),
  ensureRoiClassOption:x=>events.push(['class',x]),
  markAnnotationsDirty:x=>events.push(['dirty',x]),
  buildRoiList:()=>{},updateButtons:()=>{},
  scheduleViewerStateSync:(name,detail)=>events.push(['sync',name,detail]),
  recordAnnotationHistory:()=>{},setMode:x=>events.push(['mode',x]),draw:()=>{},
  notify:text=>messages.push(text),setTimeout:()=>{},document:undefined});
vm.runInContext(source,context);
assert.equal(context.wsiExperimentCopyBoraBoundaryForReview(0,0),true);
assert.equal(rois.length,1);
assert.equal(predicted.locked,true);
assert.equal(layer.items.length,1);
assert.equal(rois[0].locked,false);
assert.equal(rois[0].fill,'rgba(#f97316,0.12)');
assert.equal(rois[0].class,'KODAMA Group 3');
assert.equal(rois[0].properties.manual_review_status,'draft_from_prediction');
assert.equal(rois[0].properties.review_only,true);
assert.equal(rois[0].properties.training_eligible,false);
assert.equal(rois[0].properties.source_prediction_id,'model_7');
assert.equal(rois[0].properties.source_prediction_sha256,'a'.repeat(64));
assert.equal(predicted.visible,false,'model geometry is visually superseded by draft');
predicted.visible=true;
context.wsiExperimentSyncBoraReviewVisibility();
assert.equal(predicted.visible,false,'saved draft hides its matching prediction on reload');
predicted._boraShowOriginalForCompare=true;
context.wsiExperimentSyncBoraReviewVisibility();
assert.equal(predicted.visible,true,'reviewer can compare the unchanged original');
predicted._boraShowOriginalForCompare=false;
context.wsiExperimentSyncBoraReviewVisibility();
assert.deepEqual(Array.from(events.filter(x=>x[0]==='mode'),x=>x[1]),['brush']);
assert.equal(context.wsiExperimentCopyBoraBoundaryForReview(0,0),true,'reselect existing draft');
assert.equal(rois.length,1,'reselect does not duplicate draft');
context.deleteRoi=index=>{rois.splice(index,1);events.push(['delete',index]);};
assert.equal(context.wsiExperimentRevertBoraDraft(0),true);
assert.equal(rois.length,0);
assert.equal(predicted.visible,true,'reverting restores original prediction');
const canvas={};context.canvas=canvas;
context.pointerToSlide=()=>({x:3,y:4});
context.layerObjectAt=()=>({layer,layerIndex:0,item:predicted,itemIndex:0});
let clickStopped=false;
context.wsiExperimentHandleBoraCanvasClick({button:0,target:canvas,
  preventDefault:()=>{},stopImmediatePropagation:()=>{clickStopped=true;}});
assert.equal(clickStopped,true,'direct slide click takes the prediction into review');
assert.equal(rois.length,1);
assert.equal(predicted.visible,false);
assert.equal(context.wsiExperimentRevertBoraDraft(0),true);
context.mode='brush';
let pointerStopped=false;
context.wsiExperimentHandleBoraCanvasPointerDown({button:0,target:canvas,
  preventDefault:()=>{},stopImmediatePropagation:()=>{pointerStopped=true;}});
assert.equal(pointerStopped,true,'brush pointer down selects a new predicted component before painting');
assert.equal(rois.length,1);
context.wsiExperimentHandleBoraCanvasClick({button:0,target:canvas,
  preventDefault:()=>{},stopImmediatePropagation:()=>{}});
assert.equal(rois.length,1,'trailing click does not duplicate the draft');
layer.metadata.group_count=2;
assert.equal(context.wsiExperimentCopyBoraBoundaryForReview(0,0),false);
assert.equal(rois.length,1);
assert.ok(messages.some(x=>x.includes('three-group')));
let saved=null;
context.saveBlobWithLocation=async (blob,name)=>{saved={blob,name};return 'saved';};
context.savePickerAvailable=()=>true;
context.wsiExperimentConfig=()=>({manifest_sha256:'c'.repeat(64)});
(async()=>{
  assert.equal(await context.wsiExperimentExportBoraDrafts(),true);
  assert.equal(saved.name,'PM016_KODAMA3_Bora_manual_corrections.geojson');
  const exported=JSON.parse(await saved.blob.text());
  assert.equal(exported.features.length,1,'model prediction is excluded');
  assert.equal(exported.features[0].id,rois[0].id);
  assert.equal(exported.features[0].properties.source_prediction_id,'model_7');
  assert.equal(exported.review_status,'manual_correction_draft_not_yet_expert_validated');
  console.log('Three-group Bora manual review copy/export contract passed.');
})().catch(error=>{console.error(error);process.exitCode=1;});
