/* Read-only pretrained cell evidence plus separate, explicit human review. No encoder or job starts here. */
(function(root,factory){const api=factory();if(typeof module==='object'&&module.exports)module.exports=api;else root.WsiCellWorkbench=api;})(typeof globalThis!=='undefined'?globalThis:this,function(){
  'use strict';
  const clone=x=>JSON.parse(JSON.stringify(x));
  function displayId(cell){const source=String(cell.source_id??'').trim();if(source)return source;const id=String(cell.cell_id||cell.id||'');return id.length>32?'…'+id.slice(-31):id;}
  function taxonomyName(value){return ({pannuke:"PanNuke",consep:"CoNSeP",lizard:"Lizard",nucls_main:"NuCLS main",nucls_super:"NuCLS super",ocelot:"Ocelot",midog:"MIDOG",panoptils:"PanopTILs"})[String(value).toLowerCase()]||String(value);}
  function prediction(cell,model){return cell.predictions?.[model.model_id]||{status:'unavailable',reason:model.reason||'No model output registered'};}
  function isUnknown(value,threshold){return value.status!=='predicted'||!Number.isFinite(value.probability)||value.probability<threshold;}
  function segmentationStatus(cell){const evidence=[cell,cell.segmentation,cell.disagreement],explicit=evidence.map(value=>value?.segmentation_review_status).find(value=>value==='available'||value==='unavailable');if(explicit)return explicit;return evidence.some(value=>value&&(typeof value.segmentation_disagreement==='boolean'||typeof value.segmentation_review_required==='boolean'))?'available':'unavailable';}
  function segmentationGeometryReason(cell){const reason=cell.segmentation_geometry_unavailable_reason??cell.segmentation?.segmentation_geometry_unavailable_reason??cell.disagreement?.segmentation_geometry_unavailable_reason;return typeof reason==='string'?reason.trim():'';}
  function segmentationSummary(cell){if(segmentationStatus(cell)!=='available'){const reason=segmentationGeometryReason(cell);return reason?'geometry unavailable - review required: '+reason:'unavailable';}return disagreement(cell)?'review required':'no flagged disagreement';}
  function disagreement(cell){return cell.segmentation_disagreement===true||cell.segmentation_review_required===true||cell.segmentation?.segmentation_disagreement===true||cell.segmentation?.segmentation_review_required===true||cell.disagreement?.segmentation_disagreement===true;}
  function scoreSemantics(model){return model?.score_semantics==='uncalibrated_pixel_type_vote_fraction'?'pixel-type vote fraction (uncalibrated)':model?.score_semantics==='uncalibrated_softmax_cell_class_vote_counts'?'native class-vote evidence (uncalibrated; scores may be unavailable)':'model probabilities (uncalibrated)';}
  function contextSummary(cell){const context=cell.tissue_context;if(!context?.label)return 'unavailable';
    const coverage=Number.isFinite(context.coverage_fraction)?' · '+(100*context.coverage_fraction).toFixed(1)+'% tissue coverage':'';
    return context.label+' (soft)'+coverage+(context.boundary_crossing?' · boundary':'')+(context.partial_image_boundary?' · partial image boundary':'')+(context.ambiguous_overlap_fraction>0?' · overlapping regions':'');}
  function contextDetails(cell){const context=cell.tissue_context;if(!context?.label)return 'No checked tissue context is available.';
    const lines=['Descriptive tissue context; cell types and training targets are unchanged.'];
    const number=(value,digits=2)=>Number.isFinite(value)?value.toFixed(digits):'unavailable';
    for(const region of Array.isArray(context.regions)?context.regions:[])lines.push(region.label+' ['+region.tissue_id+']: '+number(100*region.overlap_fraction,1)+'% overlap; centroid '+number(region.signed_centroid_boundary_distance_um)+' µm from boundary (positive inside); cell boundary gap '+number(region.unsigned_cell_boundary_distance_um)+' µm');
    if(Number.isFinite(context.uncovered_fraction))lines.push((100*context.uncovered_fraction).toFixed(1)+'% outside registered tissue regions.');
    if(Number.isFinite(context.visible_area_fraction))lines.push((100*context.visible_area_fraction).toFixed(1)+'% of the original cell contour is inside the image; overlaps use this visible area.');return lines.join('\n');}
  function trainingState(observations,records,ids,featureSet){
    const selected=new Map(observations.map(row=>[row.id,row])),reviewed=new Map(records.map(row=>[row.id,row])),counts=Object.create(null);
    if(!ids.length)return {ready:false,reason:'Select confirmed/corrected cells to train. At least two classes need two cells each.',counts,selected:0};
    for(const id of ids){const row=selected.get(id),record=reviewed.get(id),label=String(record?.label||'').trim();
      if(!row||row.observation_unit!=='cell'||row.feature_set_id!==featureSet||record?.status!=='reviewed'||!label)
        return {ready:false,reason:'Select only cells with confirmed/corrected human labels from this exact feature set. Uncertain/excluded cells and RNA spots cannot train.',counts,selected:ids.length};
      counts[label]=(Object.hasOwn(counts,label)?counts[label]:0)+1;}
    const classes=Object.entries(counts),summary=classes.map(([label,count])=>label+': '+count).join(' · '),ready=classes.length>=2&&classes.every(([,count])=>count>=2);
    return {ready,reason:summary+(ready?' · ready for CPU fitting.':' · at least two classes need two cells each.'),counts,selected:ids.length};
  }
  function suggestedLabel(cells,model,ids){const selected=new Set(ids),predictions=cells.filter(cell=>selected.has(cell.id)).map(cell=>prediction(cell,model));
    if(!predictions.length)throw new Error('Select cells on the current tissue first.');
    const labels=new Set(predictions.filter(p=>p.status==='predicted').map(p=>p.label));
    if(predictions.some(p=>p.status!=='predicted')||labels.size!==1)throw new Error('Selected cells have different or unavailable predictions. Enter the class you intend to confirm.');
    return [...labels][0];}
  function rows(entry,model,options={}){
    if(!entry||!model)return [];
    const threshold=Number(options.threshold??0),query=String(options.query||'').toLowerCase();
    if(!Number.isFinite(threshold)||threshold<0||threshold>1)throw new Error('Review threshold must be between zero and one. It does not calibrate scores.');
    return entry.cells.filter(cell=>{const p=prediction(cell,model);return (!options.class_name||options.class_name==='*'||(options.class_name==='__review__'?isUnknown(p,threshold):p.status==='predicted'&&p.label===options.class_name))&&
      (!options.disagreements_only||disagreement(cell))&&(!query||[cell.id,cell.cell_id,cell.source_id,p.label,p.reason,cell.tissue_context?.label].some(v=>String(v||'').toLowerCase().includes(query)));});
  }
  function summary(entry,model){if(!entry||!model)return [];
    if(Array.isArray(entry.summary))return entry.summary.filter(row=>!row.model_id||row.model_id===model.model_id);
    const counts=new Map();for(const cell of entry.cells){const p=prediction(cell,model),label=p.status==='predicted'?p.label:p.status,key=p.status+'\0'+label;
      if(!counts.has(key))counts.set(key,{label,status:p.status,count:0,model_id:model.model_id});counts.get(key).count++;}return [...counts.values()];}
  const classPalette=['#E6194B','#3CB44B','#4363D8','#F58231','#911EB4','#42D4F4','#F032E6','#BFEF45','#FABED4','#469990','#DCBEFF','#9A6324','#FFFAC8','#800000','#AAFFC3','#808000','#FFD8B1','#000075','#A9A9A9','#FFE119'];
  const classColours=new WeakMap();
  function rgbHex(r,g,b){return '#'+[r,g,b].map(value=>Math.max(0,Math.min(255,Math.round(value))).toString(16).padStart(2,'0')).join('').toUpperCase();}
  function hslHex(h,s=70,l=60){h=((h%360)+360)%360/360;s/=100;l/=100;const a=s*Math.min(l,1-l),channel=n=>{const k=(n+h*12)%12;return 255*(l-a*Math.max(-1,Math.min(k-3,9-k,1)));};return rgbHex(channel(0),channel(8),channel(4));}
  function classColour(model,label){let colours=classColours.get(model);if(!colours){colours=new Map();const used=new Set();for(const [index,name] of (model.class_names||[]).entries()){let value=classPalette[index]||hslHex(index*137.507764,70,45+10*(Math.floor(index/64)%3));while(used.has(value))value=rgbHex((parseInt(value.slice(1),16)+1)>>16,((parseInt(value.slice(1),16)+1)>>8)&255,(parseInt(value.slice(1),16)+1)&255);used.add(value);colours.set(name,value);}classColours.set(model,colours);}return colours.get(label)||'#94A3B8';}
  function colour(cell,model,mode,threshold=0){
    const p=prediction(cell,model),unknown=isUnknown(p,threshold);
    if(mode==='disagreement')return segmentationStatus(cell)!=='available'?'#94a3b8':disagreement(cell)?'#f97316':'#475569';
    if(mode==='unknown')return unknown?'#facc15':'#475569';
    if(mode==='confidence'){if(!Number.isFinite(p.probability))return '#94a3b8';const value=Math.max(0,Math.min(1,p.probability));return rgbHex(255*(1-value),210*value,180);}
    const label=mode==='context'?(cell.tissue_context?.label||'context unavailable'):(p.status==='predicted'?p.label:p.status);
    if(mode!=='context')return p.status==='predicted'?classColour(model,label):p.status==='abstained'?'#FACC15':'#94A3B8';
    let hash=0;for(const c of label)hash=(hash*31+c.charCodeAt(0))>>>0;
    return !cell.tissue_context?.label?'#64748B':hslHex(hash%360);
  }
  function guide(cells,spots,image,radiusUm){
    if(!Number.isFinite(radiusUm)||radiusUm<=0||radiusUm>1000)throw new Error('Choose a review radius between 0 and 1,000 µm.');
    if(!(image.mpp_x>0&&image.mpp_y>0))throw new Error('Spatial guidance requires verified image MPP.');
    const bins=new Map(),key=(x,y)=>x+':'+y;
    for(const spot of spots){if(spot.observation_unit!=='spot'||spot.tissue_id!==image.tissue_id||spot.frame_id!==image.frame_id)continue;
      const x=spot.x*image.mpp_x,y=spot.y*image.mpp_y,k=key(Math.floor(x/radiusUm),Math.floor(y/radiusUm));if(!bins.has(k))bins.set(k,[]);bins.get(k).push({x,y});}
    return cells.filter(cell=>{const x=cell.x*image.mpp_x,y=cell.y*image.mpp_y,cx=Math.floor(x/radiusUm),cy=Math.floor(y/radiusUm);
      for(let dy=-1;dy<=1;dy++)for(let dx=-1;dx<=1;dx++)if((bins.get(key(cx+dx,cy+dy))||[]).some(p=>(p.x-x)**2+(p.y-y)**2<=radiusUm**2))return true;return false;}).map(cell=>cell.id);
  }
  function csv(entry,model,records=[]){const byId=new Map(records.map(r=>[r.id,r])),escape=v=>'"'+String(v??'').replace(/"/g,'""')+'"';
    const values=[['cell_id','source_id','tissue_id','backbone','taxonomy','model_id','predicted_label','status','score','score_semantics','human_status','human_label','reviewer','tissue_context','tissue_coverage_fraction','uncovered_fraction','ambiguous_tissue_overlap_fraction','tissue_boundary_crossing','partial_image_boundary','visible_cell_area_fraction','tissue_regions_json','segmentation_geometry_unavailable_reason','segmentation_review_reason','segmentation_action','segmentation_auto_adjustment_eligible','segmentation_iou','segmentation_cellvit_coverage','segmentation_stardist_coverage','segmentation_review_status','segmentation_disagreement','segmentation_review_required']];
    for(const cell of entry.cells){const p=prediction(cell,model),r=byId.get(cell.id)||{},status=segmentationStatus(cell),qc=cell.segmentation||cell.disagreement||{},value=key=>Object.hasOwn(cell,key)?cell[key]:qc[key],context=cell.tissue_context||{},geometryReason=segmentationGeometryReason(cell),hasEvidence=status==='available'||!!geometryReason;
      values.push([cell.cell_id,cell.source_id,entry.tissue_id,model.backbone,model.taxonomy,model.model_id,p.label,p.status,p.probability,model.score_semantics,r.status,r.label,r.reviewer,context.label,context.coverage_fraction,context.uncovered_fraction,context.ambiguous_overlap_fraction,context.boundary_crossing,context.partial_image_boundary,context.visible_area_fraction,context.regions?JSON.stringify(context.regions):'',geometryReason,value('segmentation_review_reason')?JSON.stringify(value('segmentation_review_reason')):'',value('segmentation_action'),value('segmentation_auto_adjustment_eligible'),...['segmentation_iou','segmentation_cellvit_coverage','segmentation_stardist_coverage'].map(key=>status==='available'?value(key):null),status,hasEvidence?value('segmentation_disagreement'):'',hasEvidence?value('segmentation_review_required'):'']);}
    return values.map(row=>row.map(escape).join(',')).join('\n');}
  return {prediction,isUnknown,disagreement,rows,colour,guide,csv,clone,taxonomyName,displayId,segmentationStatus,segmentationGeometryReason,segmentationSummary,contextSummary,contextDetails,scoreSemantics,trainingState,suggestedLabel,summary};
});

let wsiCellViewByTissue={},wsiCellBusy=false,wsiCellPage=0,wsiCellRenderedTissue=null,wsiCellFitOutcome={text:'',error:false},wsiCellIndexTimer=null,wsiCellIndexPending=null,wsiCellLassoActive=false;
let wsiCellDisplayCache=new WeakMap(),wsiCellDisplaySerial=0,wsiCellActiveDisplay=null;
let wsiCellGeometryCache=new WeakMap(),wsiCellActiveGeometry=null;
function wsiCellCatalog(){return cfg.experiment?.cell_classification||[];}
function wsiCellEntry(){return wsiCellCatalog().find(entry=>entry.tissue_id===wsiExperimentTissueId());}
function wsiCellCoverage(entry=wsiCellEntry()){
  const records=cfg.experiment?.viewer_evidence?.cell_coverage;
  return Array.isArray(records)&&entry?records.find(item=>item.tissue_id===entry.tissue_id&&item.coverage==='partial')||null:null;
}
function wsiCellCoverageBounds(item){const b=item?.cell_position_bounds;if(!b)return null;
  const values=['xmin','ymin','xmax','ymax'].map(key=>Number(b[key]));
  if(!values.every(Number.isFinite)||values[0]>values[2]||values[1]>values[3])return null;
  return {xmin:values[0],ymin:values[1],xmax:values[2],ymax:values[3]};
}
function wsiCellRefreshCoverage(){const node=el('cellCoverageStatus'),button=el('cellZoomAvailable');if(!node)return;
  const entry=wsiCellEntry(),item=wsiCellCoverage(entry),bounds=wsiCellCoverageBounds(item);
  if(!item){node.textContent='';if(button)button.hidden=true;return;}
  const count=Number(item.cell_count),done=Number(item.completed_shards),planned=Number(item.planned_shards),consistent=Number.isSafeInteger(count)&&count>=0&&count===Number(entry.cell_count??entry.cells?.length)&&Number.isSafeInteger(done)&&done>=0&&Number.isSafeInteger(planned)&&planned>0&&done<=planned;
  if(!consistent){node.textContent='CellViT coverage metadata does not match this cell source. Reopen the project before interpreting coverage.';if(button)button.hidden=true;return;}
  const range=bounds?` Native cell-position bounds: x ${Math.floor(bounds.xmin).toLocaleString()}–${Math.ceil(bounds.xmax).toLocaleString()}, y ${Math.floor(bounds.ymin).toLocaleString()}–${Math.ceil(bounds.ymax).toLocaleString()} px.`:'';
  node.textContent=`Partial CellViT: ${count.toLocaleString()} analyzed cells in ${done}/${planned} completed shards.${range} Other slide regions have no CellViT prediction.`;
  if(button){button.hidden=!bounds||count===0;button.disabled=!bounds||count===0;}
}
function wsiCellZoomAvailable(){const entry=wsiCellEntry(),item=wsiCellCoverage(entry),bounds=wsiCellCoverageBounds(item);
  if(!entry||!bounds||Number(item.cell_count)!==Number(entry.cell_count??entry.cells?.length))throw new Error('No checked CellViT coverage bounds are available for this tissue.');
  if(typeof zoomToSlideBounds!=='function')throw new Error('Image zoom is unavailable in this viewer.');
  const image=projectItems?.find(value=>value.tissue_id===entry.tissue_id),mpp=image?.mpp||cfg.mpp;
  const mppx=Number(typeof mpp==='number'?mpp:mpp?.x??mpp?.mpp_x),mppy=Number(typeof mpp==='number'?mpp:mpp?.y??mpp?.mpp_y);
  let target=bounds;
  // A widely separated partial population should open at one real cell,
  // instead of fitting an overview where the 1 mm visibility rule hides it.
  if(Number.isFinite(mppx)&&mppx>0&&Number.isFinite(mppy)&&mppy>0&&
    ((bounds.xmax-bounds.xmin)*mppx>1800||(bounds.ymax-bounds.ymin)*mppy>1800)){
    const point=item.focus_cell_position,x=Number(point?.x),y=Number(point?.y);
    if(Number.isFinite(x)&&Number.isFinite(y)&&x>=bounds.xmin&&x<=bounds.xmax&&y>=bounds.ymin&&y<=bounds.ymax)
      target={xmin:x-450/mppx,ymin:y-450/mppy,xmax:x+450/mppx,ymax:y+450/mppy};
  }
  if(target===bounds&&(bounds.xmin===bounds.xmax||bounds.ymin===bounds.ymax))
    target={xmin:bounds.xmin===bounds.xmax?bounds.xmin-100:bounds.xmin,
      ymin:bounds.ymin===bounds.ymax?bounds.ymin-100:bounds.ymin,
      xmax:bounds.xmin===bounds.xmax?bounds.xmax+100:bounds.xmax,
      ymax:bounds.ymin===bounds.ymax?bounds.ymax+100:bounds.ymax};
  zoomToSlideBounds(target,1.15);wsiCellScheduleIndex(true);
  wsiCellStatus('Zoomed to an area with analyzed CellViT cells. Cell colours appear when the scale bar is below 1 mm.');
}
function wsiCellStatus(text,error=false){const node=el('cellWorkbenchStatus');if(node){node.textContent=text;node.style.color=error?'#fca5a5':'';}}
function wsiCellFitStatus(text,error=false){wsiCellFitOutcome={text,error};const node=el('cellFitStatus');if(node){node.textContent=text;node.style.color=error?'#fca5a5':'';}}
function wsiCellSelectedModel(){return wsiCellEntry()?.models.find(model=>model.model_id===el('cellModel')?.value);}
async function wsiCellFocusSelected(){const entry=wsiCellEntry(),cell=wsiCellDisplayRows(entry).concat(entry?.cells||[]).find(cell=>(wsiReviewController?.selection()||[]).includes(cell.id));
  if(!cell)throw new Error('Select a cell on the current tissue first.');
  if((cell.tissue_id&&cell.tissue_id!==entry.tissue_id)||(cell.frame_id&&cell.frame_id!==entry.frame_id))throw new Error('The selected cell does not match the current tissue coordinate frame.');
  if(typeof wsiReviewFocusObservation!=='function')throw new Error('Image focus is unavailable in this viewer.');
  await wsiReviewFocusObservation({id:cell.id,tissue_id:entry.tissue_id,frame_id:entry.frame_id,x:cell.x,y:cell.y});wsiCellScheduleIndex(true);}
function wsiCellOptions(){return {class_name:el('cellClass')?.value||'*',query:el('cellSearch')?.value||'',threshold:Number(el('cellThreshold')?.value||0),disagreements_only:!!el('cellDisagreementOnly')?.checked};}
function wsiCellViewport(){return typeof visibleSlideBounds==='function'?visibleSlideBounds():{xmin:0,ymin:0,xmax:Number(cfg.slide_width||1),ymax:Number(cfg.slide_height||1)};}
async function wsiCellIndexQuery(mode,extra={}){const entry=wsiCellEntry(),model=wsiCellSelectedModel();if(!entry?.indexed)return null;
  const token=(entry.request_serial||0)+1;if(mode!=='summary')entry.request_serial=token;
  const response=await fetch(predictionUrl(),{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({action:'experiment_cell_index',mode,tissue_id:entry.tissue_id,model_id:model?.model_id,limit:500,...extra})});
  const body=await response.json();if(!response.ok||body.ok===false)throw new Error(body.error||'Cell index request failed.');
  if(entry!==wsiCellEntry()||(mode!=='summary'&&token!==entry.request_serial)||(mode==='summary'&&model?.model_id!==wsiCellSelectedModel()?.model_id))return null;
  const data=body.cell_index||body;if(data.source_sha256&&data.source_sha256!==entry.source_sha256)throw new Error('Cell index source changed; reopen the current experiment.');if(Array.isArray(data.summary))entry.summary=data.summary;
  if(Array.isArray(data.cells)&&!['summary','review_subset'].includes(mode)){entry.cells=data.cells;entry.query_result=data;entry.query_mode=mode;entry.query_arguments=extra;if(extra.cursor==null)entry.query_history=[];wsiCellPage=0;}
  if(Number.isFinite(data.cell_count))entry.cell_count=data.cell_count;
  return body;
}
function wsiCellDisplayRegions(entry=wsiCellEntry()){
  if(!entry||!el('cellShow')?.checked)return [];
  const scaleLayer={source_type:'cell_identification',metadata:{cell_display:{hide_at_scale_bar_um:1000}}};
  const regions=[];
  const add=b=>{if(!b)return;const width=Number(projectItems?.find(item=>item.tissue_id===entry.tissue_id)?.width||cfg.slide_width||Infinity),
    height=Number(projectItems?.find(item=>item.tissue_id===entry.tissue_id)?.height||cfg.slide_height||Infinity);
    const bbox={xmin:Math.max(0,Math.floor(Number(b.xmin))),ymin:Math.max(0,Math.floor(Number(b.ymin))),
      xmax:Math.min(width,Math.ceil(Number(b.xmax))),ymax:Math.min(height,Math.ceil(Number(b.ymax)))};
    if(Object.values(bbox).every(Number.isFinite)&&bbox.xmin<bbox.xmax&&bbox.ymin<bbox.ymax)regions.push(bbox);
  };
  if(typeof multiViewLayout!=='undefined'&&multiViewLayout>1&&Array.isArray(multiViewPanes)){
    for(const pane of multiViewPanes){
      if(!pane||pane.blank||!pane.viewer||String(pane.entry?.item?.tissue_id||pane.entry?.item?.id||'')!==entry.tissue_id||
        (typeof wsiCellLayerScaleHidden==='function'&&wsiCellLayerScaleHidden(scaleLayer,pane)))continue;
      add(typeof multiViewVisibleSlideBounds==='function'?multiViewVisibleSlideBounds(pane):null);
    }
  }else if(typeof wsiCellLayerScaleHidden!=='function'||!wsiCellLayerScaleHidden(scaleLayer))add(wsiCellViewport());
  return regions;
}
function wsiCellDisplayKey(entry,model,regions=wsiCellDisplayRegions(entry)){return JSON.stringify([entry.source_sha256,entry.index_reference?.sha256,entry.frame_id,entry.source_frame_id,entry.feature_set_id,model?.model_id,regions]);}
function wsiCellDisplayState(entry=wsiCellEntry(),model=wsiCellSelectedModel()){if(!entry?.indexed||!model)return null;const state=wsiCellDisplayCache.get(entry);return state?.key===wsiCellDisplayKey(entry,model)?state:null;}
function wsiCellDisplayRows(entry=wsiCellEntry(),model=wsiCellSelectedModel()){return entry?.indexed?wsiCellDisplayState(entry,model)?.points||[]:entry?.cells||[];}
function wsiCellGeometryState(entry=wsiCellEntry(),model=wsiCellSelectedModel()){
  if(!entry?.indexed||!model)return null;
  const state=wsiCellGeometryCache.get(entry);
  return state?.key===wsiCellDisplayKey(entry,model)?state:null;
}
function wsiCellOutline(cell,colour){
  const source=cell?.contour;
  if(cell?.segmentation_geometry_unavailable_reason||!Array.isArray(source)||source.length<3||source.length>2048)return null;
  const ring=[];
  for(const pair of source){
    if(!Array.isArray(pair)||pair.length!==2||!Number.isFinite(pair[0])||!Number.isFinite(pair[1]))return null;
    ring.push({x:pair[0],y:pair[1]});
  }
  if(ring[0].x!==ring[ring.length-1].x||ring[0].y!==ring[ring.length-1].y)ring.push({...ring[0]});
  return {type:'polygon',rings:[ring],colour,fill:hexToRgba(colour,.16),drawable:true};
}
async function wsiCellLoadGeometry(display=wsiCellDisplayState()){
  const entry=wsiCellEntry(),model=wsiCellSelectedModel();
  if(!entry?.indexed||!model||!display?.complete||!predictionUrl())return null;
  const key=wsiCellDisplayKey(entry,model),existing=wsiCellGeometryState(entry,model);
  if(existing&&(existing.loading||existing.complete||existing.skipped))return existing.promise||existing;
  if(wsiCellActiveGeometry?.controller)wsiCellActiveGeometry.controller.abort();
  // Point markers still represent every visible cell when the viewport is too
  // dense for bounded full-resolution contours. Zooming in requests outlines.
  const limit=3000,state={key,parts:new Map(),loading:false,complete:false,outlined:0,centroidOnly:0,skipped:null,error:null,
    controller:typeof AbortController==='function'?new AbortController():null};
  wsiCellActiveGeometry=state;wsiCellGeometryCache.set(entry,state);
  if(display.points.length>limit){state.skipped='Zoom in to show cell outlines ('+display.points.length.toLocaleString()+' cells in view).';wsiCellDisplayStatus();return state;}
  const positions=new Map(display.points.map(point=>[point.id,point]));
  const current=()=>wsiCellActiveGeometry===state&&entry===wsiCellEntry()&&
    model.model_id===wsiCellSelectedModel()?.model_id&&key===wsiCellDisplayKey(entry,model);
  state.loading=true;
  state.promise=(async()=>{try{
    const seen=new Set();
    for(const bbox of display.regions){let cursor=null,total=null,received=0;const cursors=new Set();
      do{
        if(!current())return null;
        const response=await fetch(predictionUrl(),{method:'POST',headers:{'Content-Type':'application/json'},
          ...(state.controller?{signal:state.controller.signal}:{}),
          body:JSON.stringify({action:'experiment_cell_index',mode:'viewport',tissue_id:entry.tissue_id,
            model_id:model.model_id,limit:250,bbox,...(cursor===null?{}:{cursor})})});
        const body=await response.json();if(!current())return null;
        if(!response.ok||body.ok===false)throw new Error(body.error||'Cell outline request failed.');
        const page=body.cell_index||body;
        if(page.source_sha256!==entry.source_sha256||page.index_sha256!==entry.index_reference?.sha256||
           page.feature_set_id!==entry.feature_set_id||page.frame_id!==entry.frame_id||
           !Number.isSafeInteger(page.total)||page.total<0||!Array.isArray(page.cells)||page.cells.length>250)
          throw new Error('Cell outline page changed source, frame or population.');
        if(total===null)total=page.total;else if(total!==page.total)throw new Error('Cell outline population changed between pages.');
        if(total>limit)throw new Error('Cell outline viewport exceeds the bounded geometry limit; zoom in.');
        received+=page.cells.length;if(received>total)throw new Error('Cell outline page exceeds the declared viewport count.');
        for(const cell of page.cells){
          const point=positions.get(cell.id);
          if(!point||cell.cell_id!==point.cell_id||cell.source_id!==point.source_id||
             cell.tissue_id!==entry.tissue_id||cell.frame_id!==entry.frame_id||
             cell.x!==point.x||cell.y!==point.y)throw new Error('Cell outline contradicts an indexed display position.');
          if(!seen.has(cell.id)){seen.add(cell.id);state.parts.set(cell.id,cell);}
        }
        cursor=page.next_cursor??null;
        if(cursor!==null){if(typeof cursor!=='string'||!cursor||cursors.has(cursor)||!page.cells.length)throw new Error('Cell outline cursor did not advance.');cursors.add(cursor);}
        if(cursor===null&&received!==total)throw new Error('Cell outline viewport is incomplete.');
      }while(cursor!==null);
    }
    if(state.parts.size!==positions.size)throw new Error('Cell outline pages did not cover every visible cell.');
    state.outlined=[...state.parts.values()].filter(cell=>!!wsiCellOutline(cell,'#38bdf8')).length;
    state.centroidOnly=positions.size-state.outlined;
    state.complete=true;wsiCellDraw();return state;
  }catch(error){if(!current()||error.name==='AbortError')return null;state.error=error.message;wsiCellDisplayStatus();return state;
  }finally{state.loading=false;if(current())wsiCellDisplayStatus();}})();
  return state.promise;
}
function wsiCellRefreshScaleNotice(){const node=el('cellScaleNotice');if(!node)return;const entry=wsiCellEntry(),pane=typeof multiViewLayout!=='undefined'&&multiViewLayout>1&&typeof multiViewPanes!=='undefined'?multiViewPanes[typeof multiViewActiveIndex==='undefined'?0:multiViewActiveIndex]:null,
  hidden=!!entry&&!!el('cellShow')?.checked&&typeof wsiCellLayerScaleHidden==='function'&&wsiCellLayerScaleHidden({source_type:'cell_identification',metadata:{cell_display:{hide_at_scale_bar_um:1000}}},pane);
  const text=hidden?'Cells hidden at overview scale (1 mm or more); zoom in below 1 mm to see cells in analyzed regions only.':'';node.hidden=!hidden;if(node.textContent!==text)node.textContent=text;}
function wsiCellDisplayStatus(){const entry=wsiCellEntry(),node=el('cellIndexStatus');if(!node)return;if(!entry?.indexed){node.textContent='';return;}const state=wsiCellDisplayState(entry),shown=state?.points.length||0,total=state?.regions.length===1?state?.total:null;
  const geometry=wsiCellGeometryState(entry);
  node.textContent=!wsiCellDisplayRegions(entry).length?'Cell positions hidden at overview scale; zoom below 1 mm to see them.':
    (state?.error?'Cell display stopped: '+state.error+' · ':'')+(state?.complete?'All '+shown.toLocaleString()+' visible cell positions loaded':(state?.loading?'Loading visible cell positions: ':'Visible cell positions: ')+shown.toLocaleString()+(Number.isFinite(total)?' / '+total.toLocaleString():''))+(state?.complete?' · pan or zoom to load another area.':' · incomplete display; loading does not imply empty tissue.')+
    (geometry?.skipped?' '+geometry.skipped:geometry?.error?' Cell outlines unavailable: '+geometry.error:geometry?.loading?' Loading cell outlines…':geometry?.complete?' '+geometry.outlined.toLocaleString()+' cell boundaries; '+geometry.centroidOnly.toLocaleString()+' centroid-only (missing or unavailable contour).':'')+' Review details stay in bounded pages.';}
async function wsiCellLoadDisplay(force=false){const entry=wsiCellEntry(),model=wsiCellSelectedModel();if(!entry?.indexed||!model||!predictionUrl())return null;
  const regions=wsiCellDisplayRegions(entry),key=wsiCellDisplayKey(entry,model,regions),existing=wsiCellDisplayState(entry,model);
  if(existing&&!force&&(existing.loading||existing.complete||existing.error)){
    // A viewport animation can cancel outlines after its positions completed.
    // Returning to that cached viewport must resume the unfinished contours.
    if(existing.complete&&!wsiCellGeometryState(entry,model)?.error)void wsiCellLoadGeometry(existing);
    wsiCellDisplayStatus();return existing.promise||existing;
  }
  if(wsiCellActiveDisplay?.controller)wsiCellActiveDisplay.controller.abort();
  const state={key,serial:++wsiCellDisplaySerial,regions,points:[],total:null,complete:false,loading:true,error:null,source_sha256:entry.source_sha256,index_sha256:entry.index_reference?.sha256,model_id:model.model_id,frame_id:entry.frame_id,source_frame_id:entry.source_frame_id,controller:typeof AbortController==='function'?new AbortController():null};
  wsiCellActiveDisplay=state;wsiCellDisplayCache.set(entry,state);const current=()=>wsiCellActiveDisplay===state&&entry===wsiCellEntry()&&model?.model_id===wsiCellSelectedModel()?.model_id&&state.key===wsiCellDisplayKey(entry,model);
  state.promise=(async()=>{const ids=new Set();try{for(const bbox of regions){const cursors=new Set();let cursor=null,received=0,total=null,scope=null;do{if(!current())return null;const response=await fetch(predictionUrl(),{method:'POST',headers:{'Content-Type':'application/json'},...(state.controller?{signal:state.controller.signal}:{}),body:JSON.stringify({action:'experiment_cell_index',mode:'display_points',tissue_id:entry.tissue_id,model_id:model.model_id,display_limit:5000,bbox,...(cursor===null?{}:{cursor})})});
      const body=await response.json();if(!current())return null;if(!response.ok||body.ok===false)throw new Error(body.error||'Cell position request failed.');const page=body.cell_index||body;
      for(const [field,value] of Object.entries({source_sha256:entry.source_sha256,index_sha256:entry.index_reference?.sha256,feature_set_id:entry.feature_set_id,frame_id:entry.frame_id,source_frame_id:entry.source_frame_id,model_id:model.model_id}))if(value==null||page[field]!==value)throw new Error('Cell display '+field+' changed; reopen the current experiment.');
      if(!Number.isSafeInteger(page.total)||page.total<0||!Array.isArray(page.points)||page.points.length>5000||typeof page.query_scope!=='string'||!page.query_scope||
        !['xmin','ymin','xmax','ymax'].every(field=>Number.isFinite(page.bbox?.[field])&&Math.abs(page.bbox[field]-bbox[field])<1e-7))throw new Error('Cell display page has invalid viewport bounds or identity.');
      if(total===null){total=page.total;scope=page.query_scope;}else if(total!==page.total||scope!==page.query_scope)throw new Error('Cell display population changed between viewport pages.');
      received+=page.points.length;if(received>total)throw new Error('Cell display exceeds its declared viewport population.');
      for(const point of page.points){if(typeof point.id!=='string'||!point.id||point.tissue_id!==entry.tissue_id||point.frame_id!==entry.frame_id||point.source_frame_id!==entry.source_frame_id||!Number.isFinite(point.x)||!Number.isFinite(point.y)||!point.predictions?.[model.model_id])throw new Error('Cell display page contains mismatched cell positions.');if(!ids.has(point.id)){ids.add(point.id);state.points.push(point);}}
      cursor=page.next_cursor??null;
      if(cursor!==null){if(typeof cursor!=='string'||!cursor||cursors.has(cursor)||!page.points.length)throw new Error('Cell display cursor did not advance.');cursors.add(cursor);}
      if(cursor===null&&received!==total)throw new Error('Cell display viewport is incomplete.');
      if(regions.length===1)state.total=total;
      wsiCellDisplayStatus();wsiCellDraw();
    }while(cursor!==null);}state.complete=true;wsiCellDisplayStatus();wsiCellDraw();void wsiCellLoadGeometry(state);return state;}catch(error){if(!current()||error.name==='AbortError')return null;state.error=error.message;state.complete=false;wsiCellDisplayStatus();wsiCellDraw();return state;}finally{state.loading=false;if(current())wsiCellDisplayStatus();}})();return state.promise;
}
function wsiCellScheduleIndex(force=false){const entry=wsiCellEntry();if(!entry?.indexed||!predictionUrl())return;
  const model=wsiCellSelectedModel(),options=wsiCellOptions(),bbox=wsiCellViewport(),
    scope=JSON.stringify([wsiCellDisplayKey(entry,model),bbox,options.class_name,options.threshold]);
  // Paints and sync messages can arrive continuously without a viewport change.
  // Keep the original deadline for that scope so redraws cannot starve loading.
  if(wsiCellIndexPending?.entry===entry&&wsiCellIndexPending.scope===scope){
    wsiCellIndexPending.force=wsiCellIndexPending.force||force;return;
  }
  if(wsiCellIndexTimer)clearTimeout(wsiCellIndexTimer);
  const pending={entry,scope,force};wsiCellIndexPending=pending;
  wsiCellIndexTimer=setTimeout(async()=>{
    if(wsiCellIndexPending!==pending)return;
    wsiCellIndexPending=null;wsiCellIndexTimer=null;
    if(entry!==wsiCellEntry())return;
    const force=pending.force;
    try{void wsiCellLoadDisplay(force);if(!wsiCellDisplayRegions(entry).length){entry.cells=[];wsiCellRenderTable();return;}
      const model=wsiCellSelectedModel(),bbox=wsiCellViewport(),options=wsiCellOptions(),key=JSON.stringify([model?.model_id,bbox,options.class_name,options.threshold]);
      if(!force&&entry.query_key===key)return;entry.query_key=key;
      await wsiCellIndexQuery('viewport',{bbox,class_name:options.class_name,threshold:options.threshold});
      if(entry.summary_model!==model?.model_id){await wsiCellIndexQuery('summary');entry.summary_model=model?.model_id;}
      wsiCellRenderTable();wsiCellRenderSummary();
    }catch(error){entry.query_key=null;wsiCellStatus(error.message,true);}
  },300);
}
async function wsiCellPrepareReview(ids){const entry=wsiCellEntry();if(!entry?.indexed)return;
  if(ids.length>500)throw new Error('Select at most 500 cells for one review batch. Zoom in or draw a smaller selection; every cell remains displayed.');
  const checkpoint=wsiReviewController?.snapshot();await wsiReviewSync;
  const result=await wsiCellIndexQuery('review_subset',{cell_ids:ids,checkpoint});
  if(!result)return;if(!result.review_context)throw new Error('Cell index did not return a bounded review context.');
  wsiReviewInstall(result.review_context,result.review_checkpoint);
}
async function wsiCellSelectGeometry(geometry,mode='replace'){const entry=wsiCellEntry();if(!entry)return;
  const model=wsiCellSelectedModel(),visible=WsiCellWorkbench.rows({cells:wsiCellDisplayRows(entry,model)},model,wsiCellOptions());
  const hits=visible.filter(cell=>WsiReviewWorkbench.pointInGeometry(cell.x,cell.y,geometry)).map(cell=>cell.id),previous=wsiReviewController?.selectedRows({tissue_id:entry.tissue_id,observation_unit:'cell'}).map(row=>row.id)||[];
  const ids=mode==='add'?[...new Set(previous.concat(hits))]:mode==='subtract'?previous.filter(id=>!hits.includes(id)):hits;
  await wsiCellSelect(ids);wsiCellDraw();wsiCellStatus('Selected '+ids.length+' cells on the image for review.');
}
function wsiCellPreference(){return {backbone:el('cellBackbone')?.value,taxonomy:el('cellTaxonomy')?.value,model_id:el('cellModel')?.value,class_name:el('cellClass')?.value,
  mode:el('cellColourMode')?.value||'class',threshold:Number(el('cellThreshold')?.value||0),visible:!!el('cellShow')?.checked};}
function wsiCellSavePreference(){const tissue=wsiExperimentTissueId();wsiCellViewByTissue[tissue]=wsiCellPreference();if(typeof markProjectDirty==='function')markProjectDirty('cell_display_updated');}
async function wsiCellSelect(ids){if(!wsiReviewController)throw new Error('Cell review is unavailable; open a bounded experiment review context.');
  if(wsiCellEntry()?.indexed)await wsiCellPrepareReview(ids);
  const known=new Set((wsiReviewContext?.observations||[]).filter(row=>row.observation_unit==='cell'&&row.tissue_id===wsiExperimentTissueId()).map(row=>row.id));
  if(ids.some(id=>!known.has(id)))throw new Error('Some cells are outside the loaded review context; use a bounded derivative.');
  wsiReviewController.selectIds(ids);wsiCellRenderTable();wsiCellDraw();}
function wsiCellDraw(){const entry=wsiCellEntry(),model=wsiCellSelectedModel(),id='cell_identification_'+(entry?.tissue_id||'');if(!entry||!model)return;
  const display=wsiCellDisplayState(entry,model),geometry=wsiCellGeometryState(entry,model),
    shapes=entry.indexed?(geometry?.complete?geometry.parts:null):
      (entry.cells.some(cell=>Array.isArray(cell.contour)&&cell.contour.length>=3)?new Map(entry.cells.map(cell=>[cell.id,cell])):null),
    rows=WsiCellWorkbench.rows({cells:wsiCellDisplayRows(entry,model)},model,wsiCellOptions()),mode=el('cellColourMode').value,threshold=Number(el('cellThreshold').value),index=projectItems.findIndex((item,i)=>wsiExperimentTissueId(i)===entry.tissue_id),selected=new Set(wsiReviewController?.selection()||[]);
  const layer={id,name:'Cells / '+model.backbone+' / '+WsiCellWorkbench.taxonomyName(model.taxonomy)+' / '+mode,type:'vector',source_type:'cell_identification',project_managed:true,project_scoped:true,project_image_index:index,project_section_index:-1,visible:!!el('cellShow').checked,opacity:.9,radius:4,
    metadata:{tissue_id:entry.tissue_id,frame_id:entry.frame_id,cell_classification_sha256:entry.source_sha256,model_id:model.model_id,score_semantics:model.score_semantics,lod:{enabled:false,full_coordinates:true},cell_display:{derived:true,hide_at_scale_bar_um:1000,geometry_present:!!shapes,complete:!entry.indexed||!!display?.complete,population:entry.indexed?entry.cell_count:entry.cells.length,viewport_total:entry.indexed?display?.total??null:entry.cells.length,loaded:entry.indexed?display?.points.length||0:entry.cells.length,index_sha256:entry.index_reference?.sha256||null,source_sha256:entry.source_sha256,frame_id:entry.frame_id,source_frame_id:entry.source_frame_id||entry.frame_id,model_id:model.model_id}},
    items:rows.map(cell=>{const colour=WsiCellWorkbench.colour(cell,model,mode,threshold),outline=shapes?wsiCellOutline(shapes.get(cell.id),colour):null;
      return {id:cell.id,original_id:cell.cell_id,label:WsiCellWorkbench.prediction(cell,model).label||WsiCellWorkbench.prediction(cell,model).status,
        type:'point',x:cell.x,y:cell.y,colour,radius:selected.has(cell.id)?7:4,project_image_index:index,project_section_index:-1,
        ...(outline||{})};}),count:rows.length};
  if(projectItems[index])projectItems[index].layers=(projectItems[index].layers||[]).filter(value=>value.id!==id&&value.id!=='cell_density_'+entry.tissue_id).concat([layer]);
  // A previous project-source refresh could prepend this derived layer while
  // leaving its unowned global copy behind. Repair existing live duplicates as
  // well as marking every replacement as owned by the tissue lifecycle.
  if(typeof layers!=='undefined'){
    const first=layers.findIndex(value=>value.id===id);
    for(let i=layers.length-1;i>first;i--)if(layers[i].id===id)layers.splice(i,1);
  }
  upsertViewerLayer(layer);
  // Remove a restored historical density layer; individual positions are never replaced by aggregates.
  const densityId='cell_density_'+entry.tissue_id;if(typeof layers!=='undefined')for(let i=layers.length-1;i>=0;i--)if(layers[i].id===densityId)layers.splice(i,1);
  wsiCellDisplayStatus();wsiCellRefreshScaleNotice();if(typeof buildLayerList==='function')buildLayerList();if(typeof draw==='function')draw();}
function wsiCellFill(id,values,previous){const select=el(id);select.replaceChildren();for(const [value,label] of values){const option=document.createElement('option');option.value=value;option.textContent=label;select.appendChild(option);}if(values.some(([value])=>value===previous))select.value=previous;}
function wsiCellTrainingState(){return WsiCellWorkbench.trainingState(wsiReviewContext?.observations||[],wsiReviewController?.snapshot().review_records||[],wsiReviewController?.selection()||[],el('cellFeatureSet')?.value);}
function wsiCellRenderTraining(){if(!el('cellTrainingStatus'))return;const state=wsiCellTrainingState();
  el('cellTrain').disabled=wsiCellBusy||!predictionUrl()||!cfg.prediction?.fastpls_installed||!state.ready;
  el('cellTrainingStatus').textContent=!predictionUrl()?'CPU fitting requires a live R viewer.':!cfg.prediction?.fastpls_installed?'Install the optional fastPLS R package to enable CPU fitting.':state.reason;
  el('cellTrain').title=el('cellTrainingStatus').textContent;}
function wsiCellRender(reset=false){if(!el('cellModel'))return;const entry=wsiCellEntry(),tissue=wsiExperimentTissueId(),changed=tissue!==wsiCellRenderedTissue,saved=wsiCellViewByTissue[tissue]||{};
  wsiCellRefreshCoverage();
  wsiCellFitStatus(wsiCellFitOutcome.text,wsiCellFitOutcome.error);
  if(changed){wsiCellRenderedTissue=tissue;wsiCellPage=0;el('cellSearch').value='';el('cellThreshold').value=String(saved.threshold??0);el('cellColourMode').value=saved.mode||'class';el('cellShow').checked=saved.visible!==false;}
  const models=entry?.models||[],backbone=(changed?saved.backbone:null)||el('cellBackbone').value;
  wsiCellFill('cellBackbone',[...new Set(models.map(model=>model.backbone))].map(value=>[value,value]),backbone);
  const byBackbone=models.filter(model=>model.backbone===el('cellBackbone').value),taxonomy=(changed?saved.taxonomy:null)||el('cellTaxonomy').value;
  wsiCellFill('cellTaxonomy',[...new Set(byBackbone.map(model=>model.taxonomy))].map(value=>[value,WsiCellWorkbench.taxonomyName(value)]),taxonomy);
  const filtered=byBackbone.filter(model=>model.taxonomy===el('cellTaxonomy').value);
  wsiCellFill('cellModel',filtered.map(model=>[model.model_id,model.model_id+(model.status==='unavailable'?' (unavailable)':'')]),(changed?saved.model_id:null)||el('cellModel').value);
  const model=wsiCellSelectedModel(),oldClass=(changed?saved.class_name:null)||el('cellClass').value;
  wsiCellFill('cellClass',[['*','All classes'],['__review__','Abstained / unavailable / below review threshold'],...(model?.class_names||[]).map(value=>[value,value])],oldClass);
  if(el('cellLabelSuggestions')){el('cellLabelSuggestions').replaceChildren();for(const label of model?.class_names||[]){const option=document.createElement('option');option.value=label;el('cellLabelSuggestions').appendChild(option);}}
  const features=[...new Map(wsiCellCatalog().map(item=>[item.feature_set_id,item])).values()];
  wsiCellFill('cellFeatureSet',features.map(item=>[item.feature_set_id,item.descriptor.backbone+(item.descriptor.detector==='HoVer-NeXt'?' morphology':' embedding')+' / '+item.feature_set_id]),el('cellFeatureSet').value);
  const savedModels=(typeof wsiExperimentClassifiers==='undefined'?[]:wsiExperimentClassifiers).filter(item=>features.some(feature=>'experiment:'+feature.feature_set_id===item.feature_source));
  wsiCellFill('cellSavedClassifier',savedModels.map(item=>[item.path,item.label]),el('cellSavedClassifier').value);
  el('cellTrain').disabled=wsiCellBusy||!features.length||!predictionUrl()||!cfg.prediction?.fastpls_installed;
  el('cellApply').disabled=wsiCellBusy||!savedModels.length||!predictionUrl();
  el('cellExport').disabled=!entry||!model;if(el('cellReloadDisplay'))el('cellReloadDisplay').hidden=!entry?.indexed;
  if(reset)wsiCellPage=0;wsiCellRenderTable();wsiCellDraw();
  wsiCellRenderSummary();wsiCellScheduleIndex();
  if(!entry)wsiCellStatus('No cell classification output is registered for this tissue. Opening the project does not run a model.');
  else if(model?.status==='unavailable')wsiCellStatus(model.reason||'This pretrained head is unavailable.',true);
  else wsiCellStatus((entry.cell_count??entry.cells.length).toLocaleString()+' cells · '+(model?WsiCellWorkbench.taxonomyName(model.taxonomy):'choose a taxonomy')+' · '+WsiCellWorkbench.scoreSemantics(model)+'. Scores are model evidence; confirm cell types separately.');
}
function wsiCellRenderSummary(){const node=el('cellClassSummary'),entry=wsiCellEntry(),model=wsiCellSelectedModel();if(!node)return;node.replaceChildren();
  for(const row of WsiCellWorkbench.summary(entry,model)){const button=document.createElement('button');button.textContent=row.label+': '+Number(row.count).toLocaleString();button.title='Filter cells and the review queue';button.style.borderLeft='8px solid '+WsiCellWorkbench.colour({predictions:{[model.model_id]:{status:row.status,label:row.label,probability:null}}},model,'class');button.onclick=()=>{el('cellClass').value=row.status==='predicted'?row.label:'__review__';wsiCellRender(true);};node.appendChild(button);}
  wsiCellDisplayStatus();
  if(el('cellSelectImage'))el('cellSelectImage').disabled=!!(entry?.indexed&&!wsiCellDisplayRows(entry,model).length);
}
function wsiCellTableRows(){const entry=wsiCellEntry(),model=wsiCellSelectedModel();
  const scope=el('cellQueueScope')?.value,selection=new Set(wsiReviewController?.selection()||[]),bounds=scope==='viewport'?wsiCellViewport():null;
  return WsiCellWorkbench.rows(entry,model,wsiCellOptions()).filter(cell=>scope==='selected'?selection.has(cell.id):!bounds||(cell.x>=bounds.xmin&&cell.x<=bounds.xmax&&cell.y>=bounds.ymin&&cell.y<=bounds.ymax));}
function wsiCellRenderTable(){const entry=wsiCellEntry(),model=wsiCellSelectedModel();if(!el('cellTable'))return;
  const rows=wsiCellTableRows(),pages=Math.max(1,Math.ceil(rows.length/50));wsiCellPage=Math.min(wsiCellPage,pages-1);
  const start=wsiCellPage*50,selected=new Set(wsiReviewController?.selection()||[]),records=new Map((wsiReviewController?.snapshot().review_records||[]).map(record=>[record.id,record]));
  el('cellPage').textContent=rows.length?'Showing '+(start+1)+'–'+Math.min(start+50,rows.length)+' of '+rows.length.toLocaleString()+' cells':'No matching cells';
  el('cellPrevious').disabled=wsiCellPage===0&&!entry?.query_history?.length;el('cellNext').disabled=wsiCellPage>=pages-1&&entry?.query_result?.next_cursor==null;const body=el('cellTable');body.replaceChildren();
  for(const cell of rows.slice(start,start+50)){const tr=document.createElement('tr'),p=WsiCellWorkbench.prediction(cell,model),record=records.get(cell.id),box=document.createElement('input');box.type='checkbox';box.checked=selected.has(cell.id);box.setAttribute('aria-label','Select cell '+cell.cell_id);
    box.onchange=async()=>{try{const current=new Set((wsiReviewContext?.observations||[]).filter(row=>row.observation_unit==='cell'&&row.tissue_id===entry.tissue_id).map(row=>row.id));const ids=new Set((wsiReviewController?.selection()||[]).filter(id=>current.has(id)));if(box.checked)ids.add(cell.id);else ids.delete(cell.id);await wsiCellSelect([...ids]);wsiCellRenderTable();}catch(error){wsiCellStatus(error.message,true);}};
    const select=document.createElement('td');select.appendChild(box);tr.appendChild(select);
    const info=[WsiCellWorkbench.displayId(cell),p.status==='predicted'?p.label:p.status,Number.isFinite(p.probability)?p.probability.toFixed(3):'unavailable',record?record.status+(record.label?' / '+record.label:''):'unreviewed',
      WsiCellWorkbench.contextSummary(cell),WsiCellWorkbench.segmentationSummary(cell)];
    for(const [index,text] of info.entries()){const td=document.createElement('td');td.textContent=text;if(index===0){td.title=cell.cell_id||cell.id;td.style.whiteSpace='nowrap';td.style.maxWidth='14rem';td.style.overflow='hidden';td.style.textOverflow='ellipsis';}if(index===4)td.title=WsiCellWorkbench.contextDetails(cell);tr.appendChild(td);}body.appendChild(tr);}
  if(el('cellSelectionStatus'))el('cellSelectionStatus').textContent=wsiCellDisplayRows(entry,model).filter(cell=>selected.has(cell.id)).length+' cells selected on this tissue.';
  if(el('cellSelectedInspector')){const cell=(entry?.cells||[]).concat(wsiCellDisplayRows(entry,model)).find(cell=>selected.has(cell.id));el('cellSelectedInspector').textContent=cell?'Selected cell '+WsiCellWorkbench.displayId(cell)+' · '+(WsiCellWorkbench.prediction(cell,model).label||'prediction unavailable')+' · '+WsiCellWorkbench.contextSummary(cell)+' · '+WsiCellWorkbench.segmentationSummary(cell):'Select cells directly on the image, or open the optional review queue.';}
  wsiCellRenderTraining();
}
async function wsiCellFit(apply=false){if(wsiCellBusy)return;const entry=wsiCellEntry(),source='experiment:'+el('cellFeatureSet').value,signature=cfg.review_workbench?.source_signature;
  // Capture the user's complete decision before an asynchronous sync can rerender controls.
  const controller=wsiReviewController,context=wsiReviewContext,featureSet=el('cellFeatureSet').value,
    checkpoint=controller?.snapshot(),ids=controller?.selection().slice()||[],savedPath=el('cellSavedClassifier').value;
  wsiCellBusy=true;el('cellTrain').disabled=true;el('cellApply').disabled=true;
  try{if(!entry)throw new Error('No cell features are registered for the current tissue.');if(!controller||!signature)throw new Error('Open a bounded review context first.');wsiCellFitStatus('Synchronizing reviewed cell labels…');if(typeof wsiReviewFlush==='function')await wsiReviewFlush();else await wsiReviewSync;
    if(controller!==wsiReviewController||context!==wsiReviewContext||signature!==cfg.review_workbench?.source_signature)throw new Error('Review context changed while synchronizing. Select your reviewed cells again.');
    let payload;if(apply){const model=wsiExperimentClassifiers.find(model=>model.path===savedPath);if(!model)throw new Error('Choose a saved cell classifier.');payload={action:'experiment_apply_classifier',model_path:savedPath,feature_source:model.feature_source,tissue_ids:[entry.tissue_id]};}
    else{const records=new Map(checkpoint.review_records.map(record=>[record.id,record]));
      const eligible=new Set(context.observations.filter(row=>row.observation_unit==='cell'&&row.feature_set_id===featureSet).map(row=>row.id));
      if(!ids.length||ids.some(id=>!eligible.has(id)||records.get(id)?.status!=='reviewed'))throw new Error('Select only cells with confirmed/corrected human labels from this exact feature set. Uncertain/excluded cells and RNA spots cannot train.');
      const readiness=WsiCellWorkbench.trainingState(context.observations,checkpoint.review_records,ids,featureSet);if(!readiness.ready)throw new Error(readiness.reason);
      payload={action:'experiment_train_reviewed_classifier',feature_source:source,train_ids:ids,checkpoint,context_id:context.context_id,source_signature:signature,tissue_ids:[entry.tissue_id],ncomp:Number(el('cellComponents').value),max_features:Number(el('cellMaxFeatures').value)};}
    wsiCellFitStatus(apply?'Applying saved cell classifier…':'Fitting '+ids.length+' reviewed cells on CPU…');
    const response=await fetch(predictionUrl(),{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(payload)}),body=await response.json();if(!response.ok||body.ok===false)throw new Error(body.error||'Cell classifier request failed.');
    if(signature!==cfg.review_workbench?.source_signature)throw new Error('Review source changed during fitting; the completed derivative was not applied.');
    handleViewerCommands(body);wsiExperimentRecordPrediction(body.prediction);if(typeof revealPredictionOverlay==='function')revealPredictionOverlay();wsiCellFitStatus('Cell classifier saved/applied'+(body.prediction?.classifier_path?' · '+body.prediction.classifier_path:'')+'. Predictions remain separate from human labels.');
  }catch(error){wsiCellFitStatus(error.message,true);if(typeof recordViewerLog==='function')recordViewerLog('Cell classifier: '+error.message,'error',{feature_source:source,selected_count:ids.length,checkpoint_revision:checkpoint?.revision},'prediction');}finally{wsiCellBusy=false;wsiCellRender();}}
function wsiCellBind(){if(!wsiCellCatalog().length||el('openCellWorkbench'))return;
  const button=document.createElement('button');button.id='openCellWorkbench';button.textContent='Cell identification';
  const panel=document.createElement('section');panel.id='cellWorkbench';panel.hidden=true;
  panel.innerHTML='<div class="wsiWorkspaceHead"><h2 id="cellWorkbenchTitle" tabindex="-1">Cell review</h2><button id="cellPanelSize" aria-expanded="false">Expand</button><button id="closeCellWorkbench" aria-label="Close cell identification">Close</button></div>'+
    '<details id="cellDisplaySettings"><summary>Model, colours and filters</summary><p>Taxonomies stay separate. Scores are uncalibrated. Human decisions never overwrite source predictions.</p>'+
    '<label>Backbone <select id="cellBackbone"></select></label> <label>Taxonomy <select id="cellTaxonomy"></select></label> <label>Model <select id="cellModel"></select></label><br>'+
    '<label>Class <select id="cellClass"></select></label> <label>Search cells <input id="cellSearch" type="search"></label><br>'+
    '<label>Colour <select id="cellColourMode"><option value="class">Predicted cell type</option><option value="confidence">Model score</option><option value="unknown">Abstention / low score</option><option value="context">Separate tissue context</option><option value="disagreement">StarDist disagreement</option></select></label> '+
    '<label>Review score threshold <input id="cellThreshold" type="number" min="0" max="1" step="0.05" value="0" style="width:75px"></label> <label><input id="cellShow" type="checkbox" checked> Show cells</label> <label><input id="cellDisagreementOnly" type="checkbox"> Disagreement only</label></details>'+
    '<p id="cellCoverageStatus" role="status"></p><p id="cellWorkbenchStatus" role="status"></p><div id="cellClassSummary" aria-label="Cell class counts"></div><p id="cellScaleNotice" role="status" aria-live="polite"></p><p id="cellIndexStatus" role="status"></p><div role="group" aria-label="Image navigation"><button id="cellImageZoomOut" aria-label="Zoom image out">−</button> <button id="cellImageZoomIn" aria-label="Zoom image in">+</button> <button id="cellImageFit" aria-label="Fit image in viewer">Fit image</button> <button id="cellZoomAvailable" hidden>Zoom to available cells</button></div><button id="cellReloadDisplay" hidden>Reload cell positions</button> <button id="cellSelectImage">Select on image</button> <button id="cellFocusSelected">Focus selected cell</button><p id="cellSelectedInspector"></p><p id="cellSelectionStatus" role="status"></p>'+
    '<details id="cellTableDetails"><summary>Optional cell table and review queue · 50 cells per page</summary><label>Queue <select id="cellQueueScope"><option value="matching">Matching loaded cells</option><option value="selected">Selected cells only</option><option value="viewport">Current image view only</option></select></label> <button id="cellRefreshViewport">Refresh image view</button> <button id="cellLoadQueue">Load review queue</button><label>Find exact cell ID <input id="cellFindId"></label> <button id="cellFind">Find cell</button>'+
    '<div class="wsiTableScroll"><table><thead><tr><th>Select</th><th>Cell</th><th>Prediction</th><th>Score</th><th>Human review</th><th>Tissue context</th><th>Segmentation</th></tr></thead><tbody id="cellTable"></tbody></table></div>'+
    '<button id="cellPrevious">Previous</button> <span id="cellPage"></span> <button id="cellNext">Next</button><br><button id="cellSelectFiltered">Select loaded matching cells</button> <button id="cellReviewWorkspace">Open review workspace</button> <button id="cellExport">Download loaded cell evidence CSV</button></details>'+
    '<fieldset><legend>Human cell labels</legend><label>Class <input id="cellHumanLabel" list="cellLabelSuggestions"></label><datalist id="cellLabelSuggestions"></datalist> <button id="cellUsePrediction">Use selected prediction as class</button> <label>Reviewer <input id="cellReviewer" placeholder="Your name"></label><br><button id="cellConfirm">Confirm class</button> <button id="cellCorrect">Correct class</button> <button id="cellUncertain">Uncertain</button> <button id="cellExclude">Exclude</button> <button id="cellUndo">Undo review</button> <button id="cellRedo">Redo review</button><p>Applies only to selected cells on this tissue. Filling a class is separate from recording a human decision. Save Project retains decisions and history.</p></fieldset>'+
    '<p id="cellFitStatus" role="status" aria-live="polite"></p><details id="cellClassifierDetails"><summary>Reviewed CPU classifier</summary><fieldset><label>Feature set <select id="cellFeatureSet"></select></label> <details><summary>CPU fitting settings</summary><label>PLS components <input id="cellComponents" type="number" min="1" value="2"></label> <label>Maximum features <input id="cellMaxFeatures" type="number" min="1" value="256"></label></details><p id="cellTrainingStatus" role="status"></p><button id="cellTrain">Train selected reviewed cells</button><br><label>Saved classifier <select id="cellSavedClassifier"></select></label> <button id="cellApply">Apply to current tissue</button><p>At least two confirmed classes with two cells each. Only the declared feature columns are predictors. Applying preserves the feature descriptor and training recipe.</p></fieldset></details>'+
    '<details><summary>Spatial transcriptomics guided review</summary><p>Select same-section RNA spots in Review workspace, then find nearby cells. This selects candidates and never assigns RNA cluster labels to cells.</p><label>Radius µm <input id="cellSpatialRadius" type="number" min="1" max="1000" value="25"></label> <button id="cellGuideSpatial">Select cells near selected RNA spots</button></details>';
  document.body.appendChild(panel);const setOpen=wsiWorkspaceBind(button,panel,'cellWorkbenchTitle',()=>{wsiCellLassoActive=false;if(typeof wsiReviewSelecting!=='undefined')wsiReviewSelecting=false;el('cellSelectImage').textContent='Select on image';},()=>wsiCellRender());el('closeCellWorkbench').onclick=()=>setOpen(false);
  panel.style.width='min(340px,calc(100vw - 32px))';panel.style.maxHeight='min(75vh,calc(100vh - 150px))';let expanded=false;
  el('cellPanelSize').onclick=()=>{expanded=!expanded;panel.style.width=expanded?'min(680px,calc(100vw - 24px))':'min(340px,calc(100vw - 32px))';el('cellPanelSize').textContent=expanded?'Compact':'Expand';el('cellPanelSize').setAttribute('aria-expanded',String(expanded));};
  document.addEventListener?.('keydown',event=>{if(event.key==='Escape'){wsiCellLassoActive=false;el('cellSelectImage').textContent='Select on image';}});
  const run=fn=>()=>{try{fn();}catch(error){wsiCellStatus(error.message,true);}};
  const runAsync=fn=>async()=>{try{await fn();}catch(error){wsiCellStatus(error.message,true);}};
  for(const [control,target] of [['cellImageZoomOut','zoomOut'],['cellImageZoomIn','zoomIn'],['cellImageFit','fit']])el(control).onclick=run(()=>{const button=el(target);if(typeof button?.click!=='function')throw new Error('Image navigation is unavailable in this viewer.');button.click();wsiCellScheduleIndex(true);});
  el('cellZoomAvailable').onclick=run(wsiCellZoomAvailable);
  for(const id of ['cellBackbone','cellTaxonomy','cellModel','cellClass','cellColourMode','cellThreshold','cellShow','cellDisagreementOnly'])el(id).onchange=run(()=>{wsiCellRender(true);wsiCellSavePreference();});
  el('cellSearch').oninput=run(()=>{wsiCellPage=0;wsiCellRenderTable();wsiCellDraw();});
  el('cellPrevious').onclick=runAsync(async()=>{const entry=wsiCellEntry();if(wsiCellPage>0)wsiCellPage--;else if(entry?.query_history?.length){const history=entry.query_history.slice(),previous=history.pop();await wsiCellIndexQuery(previous.mode,previous.arguments);entry.query_history=history;wsiCellPage=Math.max(0,Math.ceil(wsiCellTableRows().length/50)-1);}wsiCellRenderTable();});
  el('cellNext').onclick=runAsync(async()=>{const entry=wsiCellEntry(),pages=Math.max(1,Math.ceil(wsiCellTableRows().length/50));if(wsiCellPage<pages-1)wsiCellPage++;else if(entry?.query_result?.next_cursor!=null){const history=(entry.query_history||[]).concat([{mode:entry.query_mode,arguments:entry.query_arguments}]);await wsiCellIndexQuery(entry.query_mode,{...entry.query_arguments,cursor:entry.query_result.next_cursor});entry.query_history=history;}wsiCellRenderTable();});
  el('cellSelectFiltered').onclick=runAsync(()=>wsiCellSelect(WsiCellWorkbench.rows(wsiCellEntry(),wsiCellSelectedModel(),wsiCellOptions()).map(cell=>cell.id)));
  el('cellQueueScope').onchange=()=>wsiCellRenderTable();el('cellRefreshViewport').onclick=()=>{if(wsiCellEntry()?.indexed)wsiCellScheduleIndex(true);else{el('cellQueueScope').value='viewport';wsiCellRenderTable();}};
  el('cellFind').onclick=runAsync(async()=>{const entry=wsiCellEntry(),id=el('cellFindId').value.trim();if(!id)throw new Error('Enter the exact canonical or source cell ID.');if(entry?.indexed)await wsiCellIndexQuery('cell',{cell_id:id});const cell=entry.cells.find(cell=>[cell.id,cell.cell_id,cell.source_id].includes(id));if(!cell)throw new Error('No cell matches that exact ID.');await wsiCellSelect([cell.id]);await wsiCellFocusSelected();wsiCellDraw();});
  el('cellSelectImage').onclick=()=>{wsiCellLassoActive=!wsiCellLassoActive;wsiReviewSelecting=wsiCellLassoActive;el('cellSelectImage').textContent=wsiCellLassoActive?'Finish image selection':'Select on image';wsiCellStatus('Drag around cells on the image. Review up to 500 per batch. Shift adds; Alt subtracts; Escape finishes.');};
  el('cellFocusSelected').onclick=runAsync(wsiCellFocusSelected);el('cellReloadDisplay').onclick=runAsync(()=>wsiCellLoadDisplay(true));
  el('cellLoadQueue').onclick=runAsync(async()=>{const entry=wsiCellEntry();if(entry?.indexed){await wsiCellIndexQuery('queue',{class_name:el('cellClass').value,threshold:Number(el('cellThreshold').value)});wsiCellRenderSummary();}wsiCellRenderTable();});
  el('cellReviewWorkspace').onclick=()=>{setOpen(false);el('openReviewWorkbench')?.click();};
  el('cellUsePrediction').onclick=run(()=>{el('cellHumanLabel').value=WsiCellWorkbench.suggestedLabel(wsiCellEntry()?.cells||[],wsiCellSelectedModel(),wsiReviewController?.selection()||[]);wsiCellStatus('Class filled from the selected model. Inspect the cells, then record your own confirmation or correction.');});
  for(const [id,action] of [['cellConfirm','confirm'],['cellCorrect','correct'],['cellUncertain','uncertain'],['cellExclude','exclude']])el(id).onclick=run(()=>{
    const ids=wsiReviewController?.selectedRows({tissue_id:wsiExperimentTissueId(),observation_unit:'cell'}).map(row=>row.id)||[];if(!ids.length)throw new Error('Select cells on the current tissue first.');
    wsiReviewController.review({action,ids,label:el('cellHumanLabel').value,reviewer:el('cellReviewer').value});wsiCellRenderTable();});
  el('cellUndo').onclick=run(()=>{wsiReviewController?.undo();wsiCellRenderTable();});el('cellRedo').onclick=run(()=>{wsiReviewController?.redo();wsiCellRenderTable();});
  el('cellTrain').onclick=()=>wsiCellFit(false);el('cellApply').onclick=()=>wsiCellFit(true);
  el('cellFeatureSet').onchange=()=>wsiCellRenderTraining();
  el('cellGuideSpatial').onclick=runAsync(async()=>{const entry=wsiCellEntry(),image=wsiReviewContext.images.find(image=>image.tissue_id===entry.tissue_id),spots=wsiReviewController.selectedRows({tissue_id:entry.tissue_id,observation_unit:'spot'});
    if(!spots.length)throw new Error('Select RNA spots on the current tissue first.');const ids=WsiCellWorkbench.guide(entry.cells,spots,image,Number(el('cellSpatialRadius').value));await wsiCellSelect(ids);wsiCellStatus('Selected '+ids.length+' nearby loaded cells for human review. RNA labels were not copied.');});
  el('cellExport').onclick=async()=>{try{const entry=wsiCellEntry(),model=wsiCellSelectedModel();if(!entry||!model)return;await downloadBlob(new Blob([WsiCellWorkbench.csv(entry,model,wsiReviewController?.snapshot().review_records||[])],{type:'text/csv;charset=utf-8'}),'cell-evidence.csv');wsiCellStatus('Cell evidence sent to browser downloads. Human labels remain separate columns.');}catch(error){wsiCellStatus(error.message,true);}};
  const originalSnapshot=projectBrowserSnapshot;projectBrowserSnapshot=function(...args){return {...originalSnapshot(...args),cell_workbench:{version:1,source_manifest_sha256:cfg.experiment.manifest_sha256,view_by_tissue:WsiCellWorkbench.clone(wsiCellViewByTissue)}};};
  const originalPreview=applyProjectPreview;applyProjectPreview=function(...args){const result=originalPreview(...args);wsiCellRender();return result;};
  const originalDraw=draw;draw=function(...args){const result=originalDraw(...args);wsiCellRefreshScaleNotice();wsiCellScheduleIndex();return result;};
  const originalRestore=restoreBrowserProjectInLiveSession;restoreBrowserProjectInLiveSession=async function(snapshot,...args){const saved=snapshot.cell_workbench;if(saved&&saved.source_manifest_sha256!==cfg.experiment.manifest_sha256)throw new Error('Cell review display belongs to a different experiment source.');
    const result=await originalRestore(snapshot,...args);wsiCellViewByTissue=saved?.view_by_tissue||{};wsiCellRenderedTissue=null;wsiCellRender();return result;};
  const originalCommand=handleViewerCommand;handleViewerCommand=function(command){if(command?.type!=='open_browser_project')return originalCommand(command);const saved=command.payload?.cell_workbench;if(saved&&saved.source_manifest_sha256!==cfg.experiment.manifest_sha256)throw new Error('Cell review display belongs to a different experiment source.');const result=originalCommand(command);wsiCellViewByTissue=saved?.view_by_tissue||{};wsiCellRenderedTissue=null;wsiCellRender();return result;};
  wsiCellRender();
}
if(typeof document!=='undefined')setTimeout(wsiCellBind,0);
