// CPU-only contract: node tests/browser/bora-review-lock.cjs
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const vm=require('node:vm');
const lines=fs.readFileSync(path.resolve(__dirname,'../../R/viewer.R'),'utf8').split('\n');
const layer={id:'source-bound-candidate',items:[{id:'candidate_1',locked:true,
  properties:{review_only:true}}],
  metadata:{review_only:true}};
const warnings=[];
const context=vm.createContext({layers:[layer],notify:(text)=>warnings.push(text),
  layerFindIndex:()=>0,selectedLayerObject:()=>({layer,item:layer.items[0],itemIndex:0}),
  isDrawable:()=>true,layerItemIsPoint:()=>false});
for(const name of ['removeViewerLayer','deleteSelectedLayerObject','promoteSelectedLayerAnnotationForEdit']){
  const line=lines.find(value=>value.trim().startsWith('"function '+name+'('));
  assert.ok(line,name+' is defined');
  vm.runInContext(JSON.parse(line.trim().replace(/,$/,'')),context);
  if(name!=='promoteSelectedLayerAnnotationForEdit')
    assert.equal(context[name](),false,name+' blocks source-bound review geometry');
}
layer.items[0].locked=false; // Even a corrupted/unlocked browser snapshot cannot promote it.
assert.equal(context.promoteSelectedLayerAnnotationForEdit(),-2);
assert.equal(context.layers.length,1);
assert.equal(layer.items.length,1);
assert.equal(warnings.length,3);

const browserLine = name => {
  const line=lines.find(value=>value.trim().startsWith('"function '+name+'('));
  assert.ok(line,name+' is defined');
  return JSON.parse(line.trim().replace(/,$/,''));
};
class Node {
  constructor(tag='div'){this.tag=tag;this.children=[];this.dataset={};this.style={setProperty(){}};
    this.classList={add(){},toggle(){}};this.textContent='';this.visible=true;}
  append(...children){this.children.push(...children);}
  appendChild(child){this.children.push(child);}
  querySelectorAll(){return [];}
  querySelector(){return null;}
}
const list=new Node(),summary=new Node(),reviewItem={id:'candidate_1',name:'anonymous group A',
  visible:true,locked:true,properties:{review_only:true},bbox:{xmin:1,ymin:1,xmax:9,ymax:9}},
  semanticItem={id:'annotation_1',name:'Manual ROI',bbox:{xmin:10,ymin:10,xmax:20,ymax:20}};
const uiLayers=[{id:'bora',source_type:'bora_candidate_review',visible:true,
  metadata:{review_only:true,training_eligible:false},items:[reviewItem]},
  {id:'semantic',source_type:'tissue_annotation',visible:true,items:[semanticItem]}];
let zoomed=0,drawn=0;
const ui=vm.createContext({layers:uiLayers,rois:[],selectedLayerIndex:-1,
  selectedLayerItemIndex:-1,document:{createElement:tag=>new Node(tag)},
  el:id=>({roiList:list,roiSummary:summary})[id],
  layerItemMatchesActiveProject:()=>true,annotationFilterValue:()=> 'all',
  annotationSearchQuery:()=>'',roiListSearchText:()=>'',visibleRoi:item=>item.visible!==false,
  lockedRoi:item=>!!item.locked,roiClassHighlighted:()=>false,
  classColour:()=> '#fff',addDetail:(node,label,value)=>{
    const detail=new Node('span');detail.textContent=label+': '+value;node.append(detail);
  },setRoiPanelOpen:()=>{},
  updateRoiList:()=>{},buildRoiList:()=>{},draw:()=>{drawn++;},
  scheduleViewerStateSync:()=>{},selectLayerObject:()=>{},
  zoomToSlideBounds:()=>{zoomed++;},roiBounds:item=>item.bbox,
  annotationListActive:()=>false,isDrawable:()=>true});
for(const name of ['overlayFocusLayerKind','tissueAnnotationLayerEntries',
  'boraSemanticReviewKind',
  'appendImportedTissueAnnotationList'])vm.runInContext(browserLine(name),ui);
ui.overlayFocusText=()=> 'bora prediction';ui.overlayFocusKindFromText=()=> 'prediction';
assert.equal(ui.overlayFocusLayerKind(uiLayers[0]),'annotation',
  'Stains > Annotation controls Bora review geometry');
const pls=vm.createContext({rois:[{id:'review',properties:{review_only:true}},
  {id:'manual',properties:{}}],predictionProjectAnnotationEntries:()=>null,
  isDrawable:()=>true,roiIsCellLike:()=>false});
vm.runInContext(browserLine('predictionSelectableRois'),pls);
assert.deepEqual(Array.from(pls.predictionSelectableRois(),entry=>entry.roi.id),['manual'],
  'PLS-LDA does not offer review geometry for training');
const layerList=new Node(),layerSummary=new Node();
const layerUi=vm.createContext({layers:[uiLayers[0]],
  el:id=>({layerList,layerSummary})[id],syncAnnotationClassOptions:()=>{},
  renderProximityLegend:()=>{},selectedLayerObject:()=>null,
  setRoiPanelOpen:()=>{throw Error('Bora review belongs in Annotations');}});
vm.runInContext(browserLine('buildLayerList'),layerUi);
layerUi.buildLayerList();
assert.equal(layerList.children.length,0,'Bora review has no duplicate Layers controls');
ui.appendImportedTissueAnnotationList();
assert.equal(list.children.length,2);
const boraRow=list.children[0],semanticRow=list.children[1];
assert.equal(boraRow.children[0].children[1].textContent,
  'Predicted by Bora · exploratory · locked');
assert.deepEqual(boraRow.children[2].children.map(child=>child.textContent),['Hide','Zoom']);
assert.deepEqual(semanticRow.children[2].children.map(child=>child.textContent),['Edit','Zoom']);
assert.match(summary.textContent,/Bora exploratory boundaries: 1/);
boraRow.children[2].children[0].onclick({stopPropagation(){}});
assert.equal(reviewItem.visible,false);
uiLayers[0].visible=false;
boraRow.children[2].children[0].onclick({stopPropagation(){}});
assert.equal(reviewItem.visible,true);
assert.equal(uiLayers[0].visible,true);
boraRow.children[2].children[1].onclick({stopPropagation(){}});
assert.equal(zoomed,1);assert.equal(drawn,3);
list.children=[];
uiLayers[0].metadata.producer='bora-wsi-semantic-chain-v1';
reviewItem.predicted_tissue='tumor';
ui.appendImportedTissueAnnotationList();
assert.equal(list.children[0].children[0].children[1].textContent,
  'Predicted by Bora · same-slide holdout · locked');
assert.equal(list.children[0].children[0].children[2].textContent,'Predicted tumor');
assert.match(list.children[0].children[1].children[0].textContent,
  /same-slide holdout; exploratory prediction, not ground truth or PLS training/);
assert.deepEqual(list.children[0].children[2].children.map(child=>child.textContent),
  ['Hide','Zoom']);
list.children=[];
uiLayers[0].metadata.producer='bora-wsi-native-he-refined-semantic-chain-v1';
ui.appendImportedTissueAnnotationList();
assert.equal(list.children[0].children[0].children[1].textContent,
  'Predicted by Bora · H&E-refined · same-slide holdout · locked');
assert.equal(list.children[0].children[0].children[2].textContent,'Predicted tumor');
assert.match(list.children[0].children[1].children[0].textContent,
  /native H&E-refined, expert-trained same-slide holdout; exploratory prediction, not ground truth or PLS training/);
assert.deepEqual(list.children[0].children[2].children.map(child=>child.textContent),
  ['Hide','Zoom']);
console.log('Bora review layer lock contracts passed.');
