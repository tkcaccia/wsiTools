/* Review metadata stays independent of source clusters and predictor matrices. */
let wsiReviewController = null;
let wsiReviewContext = null;
let wsiReviewSelecting = false;
let wsiReviewSetOpen = null;
let wsiReviewSync = Promise.resolve();
let wsiReviewPendingCheckpoint = null;
let wsiReviewSyncRunning = false;
let wsiReviewSyncError = null;
let wsiReviewTransport = {pending:null,inflight:null,failed:null,running:false,error:null};
const wsiReviewPreviewImages = new Map();
let wsiBoraReviewController = null;
let wsiBoraReviewSelected = null;
let wsiBoraReviewRestoring = false;

function wsiBoraReviewRevealFocusedImage() {
  if(window.innerWidth>=800)return;
  const project=el('projectPanel'),annotations=el('roiPanel'),layerPanel=el('layerPanel'),history=el('annotationHistory'),log=el('viewerLogPanel');
  if(project&&!project.classList.contains('closed')&&typeof setProjectPanelClosed==='function')
    setProjectPanelClosed(true,false);
  if(annotations&&annotations.classList.contains('open')&&typeof setRoiPanelOpen==='function')
    setRoiPanelOpen(false,{save:false});
  if(layerPanel&&!layerPanel.classList.contains('closed')&&typeof setLayerPanelClosed==='function')
    setLayerPanelClosed(true,false);
  if(history&&!history.classList.contains('closed')&&typeof setHistoryPanelClosed==='function')
    setHistoryPanelClosed(true,false);
  if(log&&!log.classList.contains('closed')&&typeof setViewerLogPanelClosed==='function')
    setViewerLogPanelClosed(true,false);
}

function wsiBoraReviewInstall() {
  const sources=projectItems.map(item=>item.bora_review_regions).filter(Boolean);
  if(!sources.length){wsiBoraReviewController=null;wsiBoraReviewRender();return;}
  const experiment=cfg.experiment||{};
  wsiBoraReviewController=WsiBoraReview.create({experiment:{experiment_id:experiment.experiment_id||experiment.id,
    manifest_sha256:experiment.manifest_sha256},images:wsiReviewContext.images,sources,onChange:()=>{
      if(wsiBoraReviewRestoring)return;
      if(typeof markProjectDirty==='function')markProjectDirty('bora_human_review_updated');
      wsiBoraReviewRender();
    }});
  window.wsiBoraReviewController=wsiBoraReviewController;
  wsiBoraReviewRender();
}
function wsiBoraReviewRender() {
  const section=el('boraReviewSection');if(!section)return;
  const tissue=wsiReviewActiveTissue(),source=wsiBoraReviewController?.source(tissue),list=el('boraReviewQueue');
  section.hidden=!source;if(!source||!list){const reopen=el('openReviewWorkbench');if(reopen)reopen.textContent='Review workspace';return;}
  const all=wsiBoraReviewController.queue(tissue,true),pending=wsiBoraReviewController.queue(tissue,false),
    shown=el('boraReviewShowCompleted')?.checked?all:pending;
  const summary=el('boraReviewSummary');if(summary)summary.textContent=all.length+' ranked regions · '+pending.length+
    ' awaiting review. '+(source.exact_annotation_ids?.length?
      'Pixel-exact predicted boundaries are locked entries in Annotations. ':'' )+
    'The orange fitted contour is a separate review-only approximation.';
  const identity=el('boraReviewIdentity');if(identity)identity.textContent='Frame '+source.frame_id+
    ' · candidate '+source.prediction_id+' · exact mask '+source.exact_mask_sha256.slice(0,12)+'…';
  const focused=wsiBoraReviewSelected?.tissue_id===tissue?wsiBoraReviewSelected.id:null;
  const selected=el('boraReviewSelected');if(selected)selected.textContent=focused?
    'Focused region: '+focused+'. Inspect the H&E, then choose a decision here.':'Select a ranked region to inspect it on the H&E.';
  const reopen=el('openReviewWorkbench');if(reopen)reopen.textContent=focused?'Resume Bora review':'Review workspace';
  const show=el('boraReviewShowGeometry');if(show){
    const layer=(typeof layers!=='undefined'&&Array.isArray(layers)?layers:[]).find(item=>item.id===source.geometry_layer_id);show.disabled=!layer;
    show.checked=!!layer?.visible;
  }
  for(const [id,key] of [['boraReviewShowExactMask','exact_mask_channel_id'],
                         ['boraReviewShowProbability','probability_channel_id'],
                         ['boraReviewShowUncertainty','uncertainty_channel_id']]){
    const input=el(id),channel=typeof channelSourceById==='function'?channelSourceById(source[key]):null;
    if(input){input.disabled=!channel;input.checked=!!channel?.visible;}
  }
  list.replaceChildren();
  for(const row of shown.slice(0,25)){
    const decision=wsiBoraReviewController.decision(tissue,row.id),button=document.createElement('button');
    button.type='button';button.className='boraReviewRegion';button.style.cssText='display:block;width:100%;text-align:left;margin:3px 0;padding:6px';
    button.textContent=(row.priority_score*100).toFixed(0)+'% priority · '+row.reason.replaceAll('_',' ')+
      ' · '+row.id+(decision?' · '+(decision.label||decision.action):'');
    button.title='Focus the registered H&E image on this review region';button.onclick=async()=>{
      try{await wsiReviewFocusObservation(row);wsiBoraReviewSelected={tissue_id:tissue,id:row.id};
        const p=row.footprint;upsertViewerLayer({id:'bora_review_region_focus',name:'Bora review target (temporary)',
          type:'vector',source_type:'review_points',visible:true,replace:true,opacity:1,
          metadata:{lod:{enabled:false},review_display:{prediction_id:source.prediction_id,source_sha256:source.source_sha256}},
          items:[{id:'bora_review_region_focus',type:'polygon',rings:[[{x:p.x0,y:p.y0},{x:p.x1,y:p.y0},{x:p.x1,y:p.y1},{x:p.x0,y:p.y1}]],
            colour:'#FACC15',fill:'rgba(0,0,0,0)',line_width:3,
            project_image_index:activeProjectIndex,project_section_index:-1}],count:1});
        if(typeof draw==='function')draw();
        wsiBoraReviewRender();
        wsiBoraReviewRevealFocusedImage();
        if(typeof wsiReviewSetOpen==='function')wsiReviewSetOpen(false);
        wsiReviewStatus('Review target '+row.id+' focused on the H&E. Inspect it, then choose Resume Bora review to decide. The left panels can be reopened from the toolbar.');
      }catch(error){wsiReviewStatus(error.message,true);}
    };
    list.appendChild(button);
  }
  if(shown.length>25){const more=document.createElement('p');more.textContent='Showing 25 of '+shown.length+
    ' regions. Review these first; the next ranked regions appear automatically.';list.appendChild(more);}
  if(!shown.length){const empty=document.createElement('p');empty.textContent='No regions in this queue view.';list.appendChild(empty);}
}
function wsiBoraReviewDecide(action,label=null) {
  try{
    const chosen=wsiBoraReviewSelected,tissue=wsiReviewActiveTissue();
    if(!chosen||chosen.tissue_id!==tissue)throw new Error('Focus a ranked region on this tissue before recording a decision.');
    const reviewer=el('boraReviewReviewer')?.value||'',note=el('boraReviewNote')?.value||'';
    wsiBoraReviewController.decide(tissue,chosen.id,action,label,reviewer,note);
    if(el('boraReviewNote'))el('boraReviewNote').value='';
    wsiReviewStatus('Saved human review for '+chosen.id+'. Source probabilities and masks are unchanged.');
  }catch(error){wsiReviewStatus(error.message,true);}
}

// Every checkpoint contains the complete history. Replace only snapshots that
// have not been sent; retain the in-flight write and the newest complete state.
function wsiReviewQueueCheckpoint(checkpoint) {
  const transport=wsiReviewTransport;
  transport.pending=checkpoint;transport.error=null;wsiReviewPendingCheckpoint=checkpoint;wsiReviewSyncError=null;
  if(transport.running)return wsiReviewSync;
  transport.running=true;wsiReviewSyncRunning=true;
  wsiReviewSync=(async()=>{
    try{
      while(transport.pending){const next=transport.pending;transport.pending=null;transport.inflight=next;
        if(transport===wsiReviewTransport)wsiReviewPendingCheckpoint=null;
        try{await wsiReviewRequest({action:'checkpoint',checkpoint:next});transport.failed=null;transport.error=null;}
        catch(error){transport.failed=next;transport.error=error;}
        finally{transport.inflight=null;}
        if(transport!==wsiReviewTransport)return;
        // A newer complete snapshot can recover an earlier failed write.
        if(transport.error&&!transport.pending)throw transport.error;
      }
    }catch(error){if(transport===wsiReviewTransport){wsiReviewSyncError=error;wsiReviewStatus('Review not synchronized: '+error.message,true);throw error;}}
    finally{transport.running=false;if(transport===wsiReviewTransport)wsiReviewSyncRunning=false;}
  })();
  // Keep the rejecting promise available to explicit fitting/saving callers.
  wsiReviewSync.catch(()=>{});
  return wsiReviewSync;
}

function wsiReviewAdoptCheckpoint(checkpoint) {
  const stable=value=>JSON.stringify((function sort(item){if(Array.isArray(item))return item.map(sort);if(item&&typeof item==='object')return Object.fromEntries(Object.keys(item).sort().map(key=>[key,sort(item[key])]));return item;})(value));
  const empty=state=>!(state?.selected_ids||[]).length&&!(state?.review_records||[]).length&&
    ['past','future'].every(key=>(state?.history?.[key]||[]).every(empty));
  const content=state=>({selected_ids:state.selected_ids,review_records:state.review_records,history:state.history||{past:[],future:[]}});
  const covered=local=>!local||empty(local)||(checkpoint&&local.context_id===checkpoint.context_id&&local.source_signature===checkpoint.source_signature&&
    local.revision<=checkpoint.revision&&stable(content(local))===stable(content(checkpoint)));
  if(![wsiReviewTransport.pending,wsiReviewTransport.inflight,wsiReviewTransport.failed].every(covered))
    throw new Error('Unsynchronized human review changes remain. Save or retry those decisions before restoring another review state.');
  // The incoming state is authoritative. Obsolete empty initialization requests
  // may finish later, but cannot reject or modify this new transport generation.
  wsiReviewTransport={pending:null,inflight:null,failed:null,running:false,error:null};
  wsiReviewPendingCheckpoint=null;wsiReviewSyncRunning=false;wsiReviewSyncError=null;wsiReviewSync=Promise.resolve();
}

async function wsiReviewFlush() {
  if(wsiReviewSyncError&&wsiReviewController)wsiReviewQueueCheckpoint(wsiReviewController.snapshot());
  await wsiReviewSync;
}

function wsiReviewRectangleInside(tile,roi) {
  const e=Math.min(tile.width,tile.height)*1e-8,l=tile.x+e,r=tile.x+tile.width-e,t=tile.y+e,b=tile.y+tile.height-e;
  if(![[l,t],[r,t],[r,b],[l,b]].every(p=>roiContainsPoint(roi,{x:p[0],y:p[1]})))return false;
  // Any polygon boundary entering the rectangle interior detects enclosed holes
  // and concave notches even when all four corners lie inside.
  for(const ring of (typeof roiDrawGroups==='function'?roiDrawGroups(roi).flatMap(g=>g.rings.concat(g.holes)):roi.rings||[]))for(let i=0;i<ring.length;i++){
    const a=ring[i],z=ring[(i+1)%ring.length],ax=Number(a.x??a[0]),ay=Number(a.y??a[1]),dx=Number(z.x??z[0])-ax,dy=Number(z.y??z[1])-ay;
    let lo=0,hi=1;
    for(const [v,d,min,max] of [[ax,dx,l,r],[ay,dy,t,b]]){
      if(Math.abs(d)<Number.EPSILON){if(v<min||v>max){lo=1;hi=0;break;}}
      else{const p=(min-v)/d,q=(max-v)/d;lo=Math.max(lo,Math.min(p,q));hi=Math.min(hi,Math.max(p,q));}
    }
    if(lo<=hi)return false;
  }
  return true;
}

async function wsiReviewRequest(payload) {
  if (!cfg.review_url) throw new Error('This action needs a live R viewer.');
  const response = await fetch(cfg.review_url, {method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(payload)});
  const result = await response.json();
  if (!response.ok || !result.ok) throw new Error(result.error || 'Review request failed.');
  return result;
}
function wsiReviewActiveTissue() { return wsiExperimentTissueId(); }
async function wsiReviewFocusObservation(row) {
  const native=wsiReviewContext?.images.find(image=>image.tissue_id===row.tissue_id);
  if(!native||row.frame_id!==native.frame_id)throw new Error('Image focus requires the registered tissue and native coordinate frame.');
  const index=projectItems.findIndex((item,i)=>wsiExperimentTissueId(i)===row.tissue_id);
  if(index<0)throw new Error('The selected tissue is not available in this viewer.');
  const footprint=row.footprint||{x0:row.x-40,y0:row.y-40,x1:row.x+40,y1:row.y+40};
  if(!['x0','y0','x1','y1'].every(key=>Number.isFinite(footprint[key]))||footprint.x1<=footprint.x0||footprint.y1<=footprint.y0)
    throw new Error('The selected observation has no valid native image footprint.');
  const bounds={xmin:Math.max(0,footprint.x0),ymin:Math.max(0,footprint.y0),xmax:Math.min(native.width,footprint.x1),ymax:Math.min(native.height,footprint.y1)};
  if(bounds.xmax<=bounds.xmin||bounds.ymax<=bounds.ymin)throw new Error('The selected footprint falls outside its registered image.');
  if(wsiReviewActiveTissue()!==row.tissue_id){
    if(typeof switchProjectItem!=='function')throw new Error('Switch to the selected tissue before focusing its image.');
    await new Promise((resolve,reject)=>{
      const tiled=typeof osdViewer!=='undefined'&&osdViewer&&typeof osdViewer.addHandler==='function',target=tiled?osdViewer:typeof image!=='undefined'?image:null;
      if(!target||(!tiled&&typeof target.addEventListener!=='function')){reject(new Error('Image switching is unavailable in this viewer.'));return;}
      const clean=()=>{clearTimeout(timer);if(tiled){target.removeHandler('open',ready);target.removeHandler('open-failed',failed);}else{target.removeEventListener('load',ready);target.removeEventListener('error',failed);}};
      const ready=()=>{clean();resolve();},failed=()=>{clean();reject(new Error('The selected tissue image could not be opened.'));};
      const timer=setTimeout(()=>{clean();reject(new Error('The selected tissue image is still loading. Retry image focus when it is ready.'));},5000);
      if(tiled){target.addHandler('open',ready);target.addHandler('open-failed',failed);}else{target.addEventListener('load',ready);target.addEventListener('error',failed);}
      try{switchProjectItem(index);if(wsiReviewActiveTissue()!==row.tissue_id)failed();}catch(error){clean();reject(error);}
    });
  }
  if(wsiReviewActiveTissue()!==row.tissue_id||Number(cfg.slide_width)!==native.width||Number(cfg.slide_height)!==native.height)
    throw new Error('The displayed tissue changed. Select the observation again before focusing.');
  if(typeof zoomToSlideBounds==='function'){
    if((typeof osdReady!=='undefined'&&!osdReady)||(typeof osdItem==='function'&&!osdItem()))throw new Error('The tissue image is still loading. Retry image focus when it is ready.');
    zoomToSlideBounds(bounds,1.3);
  }else{
    if(typeof slideToCanvas!=='function'||typeof zoomAt!=='function'||typeof panByPixels!=='function')throw new Error('Image focus is unavailable in this viewer.');
    if(typeof image!=='undefined'&&(!image.complete||!(image.naturalWidth>0)))throw new Error('The tissue image is still loading. Retry image focus when it is ready.');
    const corners=[{x:bounds.xmin,y:bounds.ymin},{x:bounds.xmax,y:bounds.ymin},{x:bounds.xmax,y:bounds.ymax},{x:bounds.xmin,y:bounds.ymax}].map(slideToCanvas);
    if(!corners.every(point=>Number.isFinite(point.x)&&Number.isFinite(point.y)))throw new Error('The tissue image is not ready for image focus.');
    const width=Math.max(...corners.map(point=>point.x))-Math.min(...corners.map(point=>point.x)),height=Math.max(...corners.map(point=>point.y))-Math.min(...corners.map(point=>point.y));
    if(!(width>0&&height>0&&innerWidth>0&&innerHeight>0))throw new Error('The tissue image is not ready for image focus.');
    zoomAt(Math.min(innerWidth/(width*1.3),innerHeight/(height*1.3)),innerWidth/2,innerHeight/2);
    const centre=slideToCanvas({x:(bounds.xmin+bounds.xmax)/2,y:(bounds.ymin+bounds.ymax)/2});
    panByPixels(innerWidth/2-centre.x,innerHeight/2-centre.y);
  }
  return {tissue_id:row.tissue_id,frame_id:row.frame_id,bounds};
}
function wsiReviewStatus(message, error=false) {
  for(const id of ['reviewHostStatus','reviewSpatialStatus']){const node=el(id);if(node){node.textContent=message;node.style.color=error?'#fca5a5':'';}}
}
function wsiReviewKodamaAvailability() {
  if(typeof WsiReviewWorkbench.kodamaAvailability!=='function')return {maps:[],reason:'KODAMA display metadata is unavailable in this viewer.'};
  return WsiReviewWorkbench.kodamaAvailability(wsiReviewContext||cfg.review_workbench,wsiReviewActiveTissue());
}
function wsiReviewRefreshKodamaButton() {
  const button=el('openKodamaMap');if(!button)return;
  const available=wsiReviewKodamaAvailability();button.disabled=!available.maps.length;
  const map=available.maps[0],virchow2=map?.observation_unit==='tile'&&map.source_feature_set_id==='virchow2-pooled';
  button.textContent=virchow2?'Open KODAMA map from Virchow2 tile features':'Open KODAMA map';
  button.title=available.maps.length?virchow2?
    'Open the registered KODAMA embedding computed from Virchow2 tile features':'Open the registered KODAMA embedding for this tissue':available.reason;
  const description=el('analysisKodamaDescription');if(description)description.textContent=available.maps.length?
    (virchow2?'The registered map uses Virchow2 tile features. Its groups and colours link to the H&E image; Bora tissue predictions are separate.':
      'The registered KODAMA map links its groups and colours to the tissue image.'):
    available.reason;
}
function wsiReviewOpenKodama(id) {
  try {
    const available=wsiReviewKodamaAvailability();if(!available.maps.length)throw new Error(available.reason);
    if(!wsiReviewController||!wsiReviewSetOpen)throw new Error('The review workspace is still loading. Try Open KODAMA map again.');
    if(!wsiReviewSetOpen(true))throw new Error('Finish the active workspace operation before opening the KODAMA map.');
    return wsiReviewController.openKodamaMap(id);
  }catch(error){wsiReviewStatus(error.message,true);if(typeof notify==='function')notify(error.message,'warning',5500);return null;}
}
function wsiReviewIsTissuePrediction(item) {
  const properties=item?.properties||{};
  if(item?.source==='virchow2-rgb-review'||item?.human_review_overlay||properties.human_review_overlay)return false;
  return ['tissue_annotations','bora_local_exact_semantic_vector'].includes(properties.source_role)&&
    (properties.predicted_tissue_id!=null||typeof properties.predicted_tissue==='string');
}
function wsiReviewPredictionClasses() {
  const entries=typeof rois==='undefined'?[]:[...rois],seen=new Set(),classes=new Set();
  if(typeof layers!=='undefined')for(const layer of layers||[])if(['annotation','tissue_annotation'].includes(String(layer.source_type||'')))for(const item of layer.items||[])
    if(typeof layerItemMatchesActiveProject!=='function'||layerItemMatchesActiveProject(item,layer))entries.push(item);
  let count=0;
  for(const item of entries){
    if(!wsiReviewIsTissuePrediction(item))continue;
    const p=item.properties||{},index=p.project_image_index??item.project_image_index;
    if(index!=null&&Number(index)!==activeProjectIndex)continue;
    const id=String(item.id||item.roi_id||'');if(id&&seen.has(id))continue;if(id)seen.add(id);
    classes.add(String(p.predicted_tissue||item.class||p.predicted_tissue_id));count++;
  }
  return {classes:[...classes].sort(),count};
}
function wsiReviewRefreshMapControls() {
  if(!wsiReviewController)return;
  const view=wsiReviewController.viewState(),legend=typeof wsiReviewController.clusterLegend==='function'?wsiReviewController.clusterLegend():[],
    predictions=wsiReviewPredictionClasses(),availability=wsiReviewKodamaAvailability();
  const panel=el('reviewImageGroups');if(!panel)return;
  const title=el('reviewImageGroupTitle');
  if(title)title.textContent=(availability.maps.some(map=>map.id===view.reduction_id)?'KODAMA groups':'Observation groups')+
    ' · '+legend.length+' groups · '+legend.reduce((n,group)=>n+group.count,0)+' '+view.observation_unit+' observations';
  const box=el('reviewImageGroupLegend');if(box){box.replaceChildren();for(const group of legend){
    const label=document.createElement('span');label.style.cssText='display:inline-block;margin-right:12px';
    const swatch=document.createElement('span');swatch.textContent='■ ';swatch.style.color=group.colour;swatch.setAttribute('aria-hidden','true');label.appendChild(swatch);
    label.appendChild(document.createTextNode('Group '+group.value+' ('+group.count+')'));box.appendChild(label);
  }}
  const groups=el('reviewImageShowGroups');if(groups){groups.checked=view.show_cluster_overlay;groups.disabled=!legend.length;}
  const groupLabel=el('reviewImageShowGroupsLabel');if(groupLabel)groupLabel.textContent=
    (availability.maps.some(map=>map.id===view.reduction_id)?'Show KODAMA groups':'Show observation groups')+
    (view.observation_unit==='tile'?' on processing tiles':' as points');
  const groupDescription=el('reviewImageGroupDescription');if(groupDescription)groupDescription.textContent=
    view.observation_unit==='tile'?'Colours fill registered processing tile footprints; these are analysis tiles, not tissue boundaries.':
      'Colours mark observation positions as points; no processing tile footprints are shown.';
  const tissue=el('reviewImagePredictionSummary');if(tissue)tissue.textContent=predictions.count?
    'Bora predicted annotations · '+predictions.count+' entries · '+predictions.classes.join(', ')+
      '. Use Annotations to show or hide each locked prediction. These are not ground truth.':
    'No Bora predicted annotations are registered for this image. KODAMA groups are anonymous feature groups.';
}
function wsiReviewBindMapControls() {
  const menu=document.querySelector('.stainMenu .menuBody');if(!menu||el('reviewImageGroups'))return;
  const section=document.createElement('section');section.id='reviewImageGroups';section.style.cssText='display:grid;gap:7px;border-bottom:1px solid #64748b;padding:10px 0';
  section.innerHTML='<strong id="reviewImageGroupTitle">Observation groups</strong><div id="reviewImageGroupLegend" aria-label="Image group legend"></div>'+
    '<label><input id="reviewImageShowGroups" type="checkbox" checked> <span id="reviewImageShowGroupsLabel">Show groups on image</span></label>'+
    '<p id="reviewImageGroupDescription" style="margin:0;font-size:12px"></p>'+
    '<p id="reviewImagePredictionSummary" role="status" style="margin:0;font-size:12px"></p>'+
    '<p style="margin:0;font-size:12px">Exact Bora predictions are imported as annotations from the Bora class mask, not Virchow2 RGB. RGB is unlabelled feature contrast.</p>';
  menu.appendChild(section);
  for(const [id,key] of [['reviewImageShowGroups','show_cluster_overlay']])
    el(id).onchange=event=>wsiReviewController.setDisplayOptions({[key]:event.target.checked});
  wsiReviewRefreshMapControls();
}
function wsiReviewOverlay(rows, selected=false) {
  const id=selected?'review_selected_observations':'review_observations';
  const native=wsiReviewContext?.images.find(image=>image.tissue_id===wsiReviewActiveTissue());
  const view=wsiReviewController?.viewState()||{},tileRows=rows.length&&rows.every(row=>row.observation_unit==='tile');
  const groupName=wsiReviewKodamaAvailability().maps.some(map=>map.id===view.reduction_id)?'KODAMA groups':'Observation groups';
  const items=rows.map(row=>{
    const scope={id:row.id,label:row.original_id,project_image_index:projectItems.findIndex((item,i)=>wsiExperimentTissueId(i)===row.tissue_id),project_section_index:-1};
    const colour=wsiReviewController?.observationColour?wsiReviewController.observationColour(row):'#22D3EE';
    const footprint=row.footprint||(row.footprint_width_px&&row.footprint_height_px?{x0:row.x-row.footprint_width_px/2,y0:row.y-row.footprint_height_px/2,x1:row.x+row.footprint_width_px/2,y1:row.y+row.footprint_height_px/2}:null);
    if(row.observation_unit==='tile'&&footprint&&view.show_processing_tile_footprints===true){
      const bounds=row.valid_bounds||footprint,x0=Math.max(0,bounds.x0??bounds.valid_x0??footprint.x0),y0=Math.max(0,bounds.y0??bounds.valid_y0??footprint.y0),
        x1=Math.min(native?.width??Infinity,bounds.x1??bounds.valid_x1??footprint.x1),y1=Math.min(native?.height??Infinity,bounds.y1??bounds.valid_y1??footprint.y1);
      if(x1>x0&&y1>y0)return {...scope,type:'polygon',rings:[[{x:x0,y:y0},{x:x1,y:y0},{x:x1,y:y1},{x:x0,y:y1}]],
        colour:selected?'#FACC15':colour,fill:selected?'rgba(0,0,0,0)':colour+'44',line_width:selected?3:1};
    }
    return {...scope,type:'point',x:row.x,y:row.y,radius:selected?8:3,colour:selected?'#FACC15':colour,fill:selected?'rgba(250,204,21,.5)':colour};
  });
  const metadata={review_display:{cluster_field:view.cluster_field,dataset_id:view.dataset_id,reduction_id:view.reduction_id,
    observation_unit:view.observation_unit,geometry_role:tileRows?(view.show_processing_tile_footprints?'processing_footprint':'tile_representative'):'observation_representative',semantic_tissue_labels_assigned:false},
    ...(rows.length&&rows.every(row=>row.observation_unit==='cell')?{cell_display:{hide_at_scale_bar_um:1000}}:{lod:{enabled:false}})};
  upsertViewerLayer({id,name:selected?'Review selection':tileRows?groupName+(view.show_processing_tile_footprints?' · processing tile footprints':' · tile representatives'):'Review observations',type:'vector',source_type:'review_points',
    metadata,visible:view.show_cluster_overlay!==false,replace:true,opacity:1,items,count:items.length});
  if(typeof buildLayerList==='function')buildLayerList();
  if(typeof draw==='function')draw();
}
function wsiReviewRefreshOverlays() {
  if(!wsiReviewController||!wsiReviewContext)return;
  const view=wsiReviewController.viewState(),selected=new Set(wsiReviewController.selection());
  const rows=wsiReviewContext.observations.filter(row=>row.tissue_id===wsiReviewActiveTissue()&&
    row.observation_unit===view.observation_unit&&(view.dataset_id==='*'||(row.dataset_id||'')===view.dataset_id));
  wsiReviewOverlay(rows);wsiReviewOverlay(rows.filter(row=>selected.has(row.id)),true);
  wsiReviewRefreshMapControls();
}
async function wsiReviewPlan(rows) {
  const result=await wsiReviewRequest({action:'tile_plan',context_id:wsiReviewContext.context_id,
    source_signature:wsiReviewContext.source_signature,selected_ids:rows.map(r=>r.id),
    tile_size:Number(el('reviewTileSize').value),units:el('reviewTileUnits').value,bounds:el('reviewTileBounds').value});
  const plan=result.plan;
  wsiReviewStatus(plan.row_count+' selected observations → '+plan.work_count+' work tiles; '+plan.excluded_ids.length+' outside the bounds policy.');
  return {context_id:plan.context_id,source_signature:plan.source_signature,backend_plan:plan,
    tiles:plan.tiles.map(t=>({id:t.tile_id,tissue_id:t.tissue_id,frame_id:t.frame_id,
      footprint:{x0:t.x,y0:t.y,x1:t.x+t.width,y1:t.y+t.height},source_ids:t.observation_ids}))};
}
async function wsiReviewPreview(row) {
  const index=projectItems.findIndex((item,i)=>wsiExperimentTissueId(i)===row.tissue_id),item=projectItems[index];
  const src=item&&(item.image_data_uri||item.navigator_image_data_uri||item.preview?.image_data_uri);
  if(!src) return {caption:'Preview unavailable; native coordinates retained.'};
  if(!wsiReviewPreviewImages.has(src)){
    const pending=new Promise((resolve,reject)=>{const im=new Image();im.onload=()=>resolve(im);im.onerror=()=>reject(new Error('Could not decode this tissue thumbnail.'));im.src=src;});
    wsiReviewPreviewImages.set(src,pending);pending.catch(()=>wsiReviewPreviewImages.delete(src));
  }
  const image=await wsiReviewPreviewImages.get(src);
  const native=wsiReviewContext.images.find(i=>i.tissue_id===row.tissue_id),p=row.footprint||{x0:row.x-28,y0:row.y-28,x1:row.x+28,y1:row.y+28};
  const crop=document.createElement('canvas');crop.width=100;crop.height=100;
  crop.getContext('2d').drawImage(image,p.x0/native.width*image.width,p.y0/native.height*image.height,
    (p.x1-p.x0)/native.width*image.width,(p.y1-p.y0)/native.height*image.height,0,0,100,100);
  return {url:crop.toDataURL(),caption:'Thumbnail preview · '+(row.original_id||row.id)};
}
function wsiReviewStoredObjects() {
  const node=el('reviewStoredObjects');if(!node||!wsiReviewContext)return;
  const objects=(wsiReviewContext.provenance?.dataset_catalog||[]).filter(item=>item.role==='spatial_transcriptomics_object'&&item.tissue_id===wsiReviewActiveTissue());
  node.replaceChildren();for(const item of objects){const row=document.createElement('p');row.style.margin='4px 0';row.textContent=item.dataset_id+' · '+item.row_count+' '+item.observation_unit+' observations · saved in project';row.title=item.object_path||'';node.appendChild(row);}
  if(!objects.length)node.textContent='No saved spatial transcriptomics object for this tissue.';
}
function wsiReviewInstall(context,checkpoint) {
  let installing=true;
  const candidate=WsiReviewWorkbench.create({context,getActiveTissueId:wsiReviewActiveTissue,
    selectOverlay:()=>{if(!installing)wsiReviewRefreshOverlays();},
    onViewChange:()=>{if(installing)return;wsiReviewRefreshOverlays();if(typeof markProjectDirty==='function')markProjectDirty('review_view_updated');},
    draw:()=>{if(!installing&&typeof draw==='function')draw();},
    sync:checkpoint=>{
      if(installing)return;
      cfg.review_checkpoint=checkpoint;
      if(typeof markProjectDirty==='function')markProjectDirty('review_updated');
      if(cfg.review_url)wsiReviewQueueCheckpoint(checkpoint);
    },
    plan:wsiReviewPlan,
    export:async (rows,operation)=>{
      const result=await wsiReviewPlan(rows);
      if(operation&&!operation.isCurrent())throw new Error('The selection, tissue or tile settings changed. Preview the tiles again before exporting.');
      const blob=new Blob([JSON.stringify(result.backend_plan,null,2)],{type:'application/json'});
      await downloadBlob(blob,'selected-work-tiles.json');
      wsiReviewStatus('Exported '+result.tiles.length+' planned work tiles. This file starts no processing job.');return result;
    },previewTile:wsiReviewPreview,focusObservation:wsiReviewFocusObservation});
  try{if(checkpoint)candidate.restore(checkpoint);wsiReviewAdoptCheckpoint(candidate.snapshot());}
  catch(error){candidate.destroy();wsiReviewStatus(error.message,true);throw error;}
  if(wsiReviewController)wsiReviewController.destroy();
  const mount=el('reviewWorkbenchMount');mount.replaceChildren();
  wsiReviewContext=context;cfg.review_workbench=context;window.wsiReviewContext=context;
  wsiReviewController=candidate;wsiReviewController.mount(mount);installing=false;
  cfg.review_checkpoint=wsiReviewController.snapshot();
  window.wsiReviewWorkbench=wsiReviewController;
  wsiReviewRefreshOverlays();wsiReviewStoredObjects();wsiReviewRefreshKodamaButton();
  const activeImage=context.images.find(i=>i.tissue_id===wsiReviewActiveTissue());
  const physicalOption=el('reviewTileUnits').querySelector('option[value=um]');physicalOption.disabled=!(activeImage?.mpp_x>0&&activeImage?.mpp_y>0);
  if(physicalOption.disabled&&el('reviewTileUnits').value==='um')el('reviewTileUnits').value='px';
  wsiReviewStatus(context.provenance?.availability?.reason||'Select a cluster or lasso observations, preview tiles, then export the plan.');
}
function wsiReviewBindWorkbench() {
  if(!cfg.review_workbench||el('reviewWorkbenchWindow'))return;
  const button=document.createElement('button');button.id='openReviewWorkbench';button.type='button';button.textContent='Review workspace';
  const panel=document.createElement('section');panel.id='reviewWorkbenchWindow';panel.hidden=true;
  panel.innerHTML='<div class="wsiWorkspaceHead"><h2 id="reviewWorkbenchTitle" tabindex="-1">Human review · spatial &amp; morphology</h2><button id="closeReviewWorkbench">Close</button></div>'+
    '<p>Select clusters to choose work. Confirm a class only after examining the tissue. Source cluster labels stay unchanged.</p>'+
    '<div style="display:flex;gap:6px;flex-wrap:wrap"><button id="reviewSelectOnTissue">Lasso on tissue</button><button id="reviewSelectAnnotation">Select inside annotation</button></div>'+
    '<p id="reviewHostStatus" role="status"></p><div id="reviewWorkbenchMount"></div>'+
    '<section id="boraReviewSection" hidden><h3>Bora regions needing review</h3>'+
    '<p id="boraReviewSummary"></p><p id="boraReviewIdentity" style="font-size:11px;overflow-wrap:anywhere"></p>'+
    '<label><input id="boraReviewShowCompleted" type="checkbox"> Show reviewed regions</label> '+
    '<label><input id="boraReviewShowExactMask" type="checkbox"> Exact class mask (rose tumor / blue stroma)</label> '+
    '<label><input id="boraReviewShowGeometry" type="checkbox"> Fitted contour (orange; review only)</label> '+
    '<label><input id="boraReviewShowProbability" type="checkbox"> Soft tumor probability (rose; display only)</label> '+
    '<label><input id="boraReviewShowUncertainty" type="checkbox"> Soft uncertainty (amber; display only)</label>'+
    '<p>Opacity and visibility change only the viewer. Bora’s registered exact mask determines the exported GeoJSON boundary.</p>'+
    '<div id="boraReviewQueue" aria-label="Ranked Bora review regions" style="max-height:230px;overflow:auto"></div>'+
    '<p id="boraReviewSelected" role="status">Select a ranked region to inspect it on the H&amp;E.</p>'+
    '<label>Reviewer <input id="boraReviewReviewer" value="local user" maxlength="120"></label> '+
    '<label>Note <input id="boraReviewNote" maxlength="2000" placeholder="Why this label?"></label>'+
    '<div style="display:flex;gap:6px;flex-wrap:wrap"><button id="boraReviewTumor">Label tumor</button><button id="boraReviewStroma">Label stroma</button><button id="boraReviewUncertain">Uncertain</button><button id="boraReviewExclude">Exclude</button></div>'+
    '<p>These are separate human decisions for model improvement. Virchow2 RGB colours are feature contrast, not tissue labels. Review the H&amp;E and exact mask before deciding.</p></section>'+
    '<fieldset><legend>Work tile plan</legend><label>Tile size <input id="reviewTileSize" type="number" value="112" min="1" style="width:80px"></label> '+
    '<select id="reviewTileUnits" aria-label="Tile units"><option value="px">native pixels</option><option value="um">µm (image calibration required)</option></select> '+
    '<select id="reviewTileBounds" aria-label="Tile bounds"><option value="drop">Drop edge tiles</option><option value="trim">Trim at image edge</option><option value="error">Reject edge tiles</option></select>'+
    '<p>One image-anchored grid; overlapping selections share a work tile. The plan records selected source IDs and image hashes. Review labels are saved with your viewer project.</p></fieldset>'+
    '<details><summary>Add spatial transcriptomics to this project</summary><p>Choose a trusted local RDS. Coordinates must refer to this exact H&amp;E image. Large datasets need a bounded metadata export (10,000 observations per view).</p>'+
    '<strong>Saved spatial objects</strong><div id="reviewStoredObjects" aria-label="Saved spatial transcriptomics objects"></div>'+
    '<input id="reviewSpatialPath" placeholder="Local .rds path" aria-label="Spatial RDS path"><button id="reviewChooseSpatial">Choose RDS…</button> '+
    '<label>Dataset <input id="reviewDatasetId" value="spatial"></label><label>Seurat image <input id="reviewImageName" list="reviewSpatialImages" placeholder="Inspect to list Seurat images"></label><datalist id="reviewSpatialImages"></datalist>'+
    '<p><label><input id="reviewTrustSpatial" type="checkbox"> I trust this local RDS file</label></p>'+
    '<button id="reviewInspectSpatial">Inspect metadata</button><p id="reviewSpatialInspection" role="status"></p>'+
    '<label>Cluster fields <select id="reviewClusterColumns" multiple aria-label="Spatial cluster fields"></select></label> '+
    '<label>Reductions <select id="reviewReductionNames" multiple aria-label="Spatial reductions"></select></label><p>Inspect first, then explicitly choose cluster fields and reductions to attach. Ctrl/⌘ selects several.</p>'+
    '<label>Coordinates <select id="reviewCoordinateSpace"><option value="native_pixels">Full-resolution H&amp;E pixels</option><option value="source_pixels">Source pixels with explicit affine</option></select></label>'+
    '<label>Affine JSON <input id="reviewAffine" value="[[1,0,0],[0,1,0],[0,0,1]]" style="width:100%"></label>'+
    '<p><label><input id="reviewConfirmMapping" type="checkbox"> I verified the same section, image orientation and full-resolution coordinate mapping</label></p>'+
    '<p><label><input id="reviewPersistSpatial" type="checkbox" checked> Save the original object and selected display data in this CellPhenotyper project</label></p>'+
    '<p>The project keeps its own RDS copy and a small metadata/reduction file. A previous-index backup is retained. Opening the project loads display data; it does not automatically deserialize the RDS.</p>'+
    '<button id="reviewAttachSpatial">Add to CellPhenotyper project</button><p id="reviewSpatialStatus" class="wsiSpatialStatus" role="status">Inspect a trusted file, select display fields and confirm the mapping.</p></details>';
  document.body.appendChild(panel);
  const setOpen=wsiWorkspaceBind(button,panel,'reviewWorkbenchTitle',()=>{wsiReviewSelecting=false;drag=null;erase();el('reviewSelectOnTissue').textContent='Lasso on tissue';});
  wsiReviewSetOpen=setOpen;
  const analysisMenu=document.querySelector('.analysisMenu .menuBody');
  if(analysisMenu){
    const kodamaSection=document.createElement('section');kodamaSection.id='analysisKodamaMap';
    kodamaSection.style.cssText='border-top:1px solid #64748b;padding:10px 0 0';
    kodamaSection.innerHTML='<div class="menuTitle">KODAMA tissue map</div>'+
      '<button id="openKodamaMap" type="button">Open KODAMA map</button>'+
      '<p id="analysisKodamaDescription" class="menuHint" role="status"></p>';
    analysisMenu.appendChild(kodamaSection);
    el('openKodamaMap').onclick=()=>{wsiReviewOpenKodama();if(typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(el('openKodamaMap'));};
  }
  el('closeReviewWorkbench').onclick=()=>setOpen(false);
  wsiReviewInstall(cfg.review_workbench,cfg.review_checkpoint);
  wsiBoraReviewInstall();
  el('boraReviewShowCompleted').onchange=wsiBoraReviewRender;
  el('boraReviewShowGeometry').onchange=event=>{
    const source=wsiBoraReviewController?.source(wsiReviewActiveTissue());
    if(source&&typeof setViewerLayerVisible==='function')setViewerLayerVisible(source.geometry_layer_id,event.target.checked);
    wsiBoraReviewRender();
  };
  for(const [id,key] of [['boraReviewShowExactMask','exact_mask_channel_id'],
                         ['boraReviewShowProbability','probability_channel_id'],
                         ['boraReviewShowUncertainty','uncertainty_channel_id']])el(id).onchange=event=>{
    const source=wsiBoraReviewController?.source(wsiReviewActiveTissue());
    if(source&&typeof setChannelSettings==='function')setChannelSettings(source[key],{visible:event.target.checked});
    wsiBoraReviewRender();
  };
  el('boraReviewTumor').onclick=()=>wsiBoraReviewDecide('human_label','tumor');
  el('boraReviewStroma').onclick=()=>wsiBoraReviewDecide('human_label','stroma');
  el('boraReviewUncertain').onclick=()=>wsiBoraReviewDecide('uncertain');
  el('boraReviewExclude').onclick=()=>wsiBoraReviewDecide('exclude');
  wsiReviewBindMapControls();
  for(const id of ['reviewTileSize','reviewTileUnits','reviewTileBounds'])el(id).addEventListener(id==='reviewTileSize'?'input':'change',()=>wsiReviewController.invalidatePlan());
  el('reviewSelectOnTissue').onclick=()=>{wsiReviewSelecting=!wsiReviewSelecting;el('reviewSelectOnTissue').textContent=wsiReviewSelecting?'Finish: drag on tissue (Esc cancels)':'Lasso on tissue';wsiReviewStatus('Drag on the tissue; Shift adds and Alt subtracts.');};
  el('reviewSelectAnnotation').onclick=()=>{
    try{const roi=rois[selectedRoi];if(!roi)throw new Error('Select an annotation first.');
      const im=wsiReviewContext.images.find(i=>i.tissue_id===wsiReviewActiveTissue());
      const geometry=roiCompositeGeometry(roi);
      wsiReviewController.selectImageGeometry(geometry,{tissue_id:im.tissue_id,frame_id:im.frame_id});
    }catch(error){wsiReviewStatus(error.message,true);}
  };
  let drag=null;
  const overlay=document.createElement('canvas');overlay.style.cssText='position:fixed;pointer-events:none;z-index:1490';document.body.appendChild(overlay);
  function erase(){overlay.getContext('2d').clearRect(0,0,overlay.width,overlay.height);}
  canvas.addEventListener('pointerdown',event=>{if(!wsiReviewSelecting||event.button!==0)return;event.stopImmediatePropagation();event.preventDefault();const rect=canvas.getBoundingClientRect();overlay.width=rect.width;overlay.height=rect.height;overlay.style.left=rect.left+'px';overlay.style.top=rect.top+'px';drag=[pointerToSlide(event)];canvas.setPointerCapture?.(event.pointerId);},true);
  canvas.addEventListener('pointermove',event=>{if(!drag)return;event.stopImmediatePropagation();event.preventDefault();drag.push(pointerToSlide(event));erase();const c=overlay.getContext('2d');c.strokeStyle='#facc15';c.lineWidth=2;c.beginPath();drag.forEach((p,i)=>{const q=slideToCanvas(p);i?c.lineTo(q.x,q.y):c.moveTo(q.x,q.y);});c.stroke();},true);
  canvas.addEventListener('pointerup',async event=>{if(!drag)return;event.stopImmediatePropagation();event.preventDefault();const points=drag;drag=null;erase();if(points.length>=3){const im=wsiReviewContext.images.find(i=>i.tissue_id===wsiReviewActiveTissue()),ring=points.map(p=>[p.x,p.y]);ring.push(ring[0]);try{const geometry={type:'Polygon',coordinates:[ring]},mode=event.altKey?'subtract':event.shiftKey?'add':'replace';if(typeof wsiCellLassoActive!=='undefined'&&wsiCellLassoActive&&typeof wsiCellSelectGeometry==='function')await wsiCellSelectGeometry(geometry,mode);else wsiReviewController.selectImageGeometry(geometry,{tissue_id:im.tissue_id,frame_id:im.frame_id,mode});}catch(error){wsiReviewStatus(error.message,true);}}},true);
  canvas.addEventListener('pointercancel',()=>{drag=null;erase();});
  document.addEventListener('keydown',event=>{if(event.key==='Escape'){drag=null;erase();wsiReviewSelecting=false;el('reviewSelectOnTissue').textContent='Lasso on tissue';}});
  for(const type of ['mousedown','mousemove','mouseup','click','dblclick'])canvas.addEventListener(type,event=>{if(wsiReviewSelecting){event.stopImmediatePropagation();event.preventDefault();}},true);
  const run=fn=>async()=>{try{await fn();}catch(error){wsiReviewStatus(error.message,true);}};
  el('reviewChooseSpatial').onclick=run(async()=>{const invoke=tauriViewerInvoke();if(!invoke)throw new Error('Paste the local RDS path when using a browser.');const path=await invoke('choose_viewer_review_data');if(path){el('reviewSpatialPath').value=path;invalidateInspection();invalidateMapping();}});
  const attachment=()=>({path:el('reviewSpatialPath').value,trusted:el('reviewTrustSpatial').checked,image_name:el('reviewImageName').value||null});
  let inspectedAttachment=null,inspectionGeneration=0,inspecting=false;
  const invalidateInspection=()=>{
    inspectedAttachment=null;inspectionGeneration++;el('reviewSpatialInspection').textContent='Inspect this file and image to choose its display data.';
    for(const id of ['reviewClusterColumns','reviewReductionNames'])el(id).replaceChildren();
    updateAttach();
  };
  el('reviewInspectSpatial').onclick=run(async()=>{
    if(inspecting)return;
    invalidateInspection();invalidateMapping();
    const chosen=attachment(),identity=JSON.stringify(chosen),generation=++inspectionGeneration;
    inspecting=true;const button=el('reviewInspectSpatial');button.disabled=true;button.textContent='Inspecting…';button.setAttribute('aria-busy','true');
    updateAttach();
    wsiReviewStatus('Inspecting this trusted object’s metadata…');
    try{
      const data=await wsiReviewRequest({action:'inspect_spatial',...chosen});
      if(generation!==inspectionGeneration||identity!==JSON.stringify(attachment()))return;
      const info=data.inspection;
      const catalogue=el('reviewSpatialImages');catalogue.replaceChildren();
      for(const name of info.image_names||[]){const option=document.createElement('option');option.value=name;catalogue.appendChild(option);}
      if(info.needs_image_selection){
        inspectedAttachment=null;
        const message='Choose a Seurat image ('+(info.image_names||[]).join(', ')+'), then inspect again to verify its coordinates.';
        el('reviewSpatialInspection').textContent=message;wsiReviewStatus(message);el('reviewImageName').focus();return;
      }
      inspectedAttachment=identity;
      el('reviewSpatialInspection').textContent=Number(info.observation_count).toLocaleString()+' observations · '+(info.metadata_fields||[]).length+' metadata fields · '+(info.reduction_names||[]).length+' reductions.';
      for(const [id,values] of [['reviewClusterColumns',info.metadata_fields],['reviewReductionNames',info.reduction_names]]){
        const select=el(id);select.replaceChildren();for(const value of values||[]){const option=document.createElement('option');option.value=option.textContent=value;select.appendChild(option);}
      }
      wsiReviewStatus('Metadata inspected. Select cluster fields or reductions, then verify the mapping onto this tissue.');
    }catch(error){if(generation===inspectionGeneration&&identity===JSON.stringify(attachment())){inspectedAttachment=null;wsiReviewStatus(error.message,true);}}
    finally{inspecting=false;button.disabled=false;button.textContent='Inspect metadata';button.setAttribute('aria-busy','false');updateAttach();}
  });
  let verifiedMapping=null,attaching=false;
  const mappingFingerprint=()=>{const im=wsiReviewContext.images.find(i=>i.tissue_id===wsiReviewActiveTissue());return JSON.stringify({
    tissue_id:im?.tissue_id,frame_id:im?.frame_id,image_sha256:im?.sha256,source_signature:wsiReviewContext.source_signature,
    path:el('reviewSpatialPath').value,image_name:el('reviewImageName').value,space:el('reviewCoordinateSpace').value,affine:el('reviewAffine').value});};
  const updateAttach=()=>{
    const selected=el('reviewClusterColumns').selectedOptions.length+el('reviewReductionNames').selectedOptions.length;
    el('reviewAttachSpatial').disabled=attaching||inspecting||inspectedAttachment!==JSON.stringify(attachment())||!selected||!el('reviewConfirmMapping').checked||verifiedMapping!==mappingFingerprint();
  };
  const invalidateMapping=()=>{verifiedMapping=null;el('reviewConfirmMapping').checked=false;updateAttach();};
  el('reviewConfirmMapping').onchange=()=>{verifiedMapping=el('reviewConfirmMapping').checked?mappingFingerprint():null;updateAttach();};
  for(const id of ['reviewSpatialPath','reviewImageName','reviewCoordinateSpace','reviewAffine'])el(id).addEventListener('input',invalidateMapping);
  for(const id of ['reviewSpatialPath','reviewImageName'])el(id).addEventListener('input',invalidateInspection);
  el('reviewTrustSpatial').onchange=()=>{invalidateInspection();invalidateMapping();};
  for(const id of ['reviewClusterColumns','reviewReductionNames'])el(id).onchange=updateAttach;
  updateAttach();
  el('reviewPersistSpatial').onchange=()=>{el('reviewAttachSpatial').textContent=el('reviewPersistSpatial').checked?'Add to CellPhenotyper project':'Attach to this viewer only';};
  el('reviewAttachSpatial').onclick=run(async()=>{
    if(attaching||inspecting)return;
    if(inspectedAttachment!==JSON.stringify(attachment()))throw new Error('Inspect this file and image first, then choose the cluster fields and reductions.');
    const clusterColumns=Array.from(el('reviewClusterColumns').selectedOptions,o=>o.value),reductionNames=Array.from(el('reviewReductionNames').selectedOptions,o=>o.value);
    if(!clusterColumns.length&&!reductionNames.length)throw new Error('Choose at least one cluster field or reduction to attach.');
    if(!/^[A-Za-z0-9][A-Za-z0-9_.:-]*$/.test(el('reviewDatasetId').value))throw new Error('Dataset ID must begin with a letter/digit and use letters, digits, underscores, dots, colons or hyphens.');
    if(!el('reviewConfirmMapping').checked||verifiedMapping!==mappingFingerprint())throw new Error('Verify and confirm the coordinate mapping for this exact tissue and file first.');
    const verified=verifiedMapping,im=wsiReviewContext.images.find(i=>i.tissue_id===wsiReviewActiveTissue()),space=el('reviewCoordinateSpace').value;
    const persist=el('reviewPersistSpatial').checked,button=el('reviewAttachSpatial');
    const request={action:'attach_spatial',...attachment(),persist_to_experiment:persist,cluster_columns:clusterColumns,reduction_names:reductionNames,
      tissue_id:im.tissue_id,dataset_id:el('reviewDatasetId').value,checkpoint:wsiReviewController.snapshot(),
      coordinate_mapping:{confirmed:true,same_section:true,image_sha256:im.sha256,frame_id:im.frame_id,coordinate_space:space,
        affine:space==='source_pixels'?JSON.parse(el('reviewAffine').value):null}};
    attaching=true;button.disabled=true;button.textContent=persist?'Saving object…':'Attaching…';button.setAttribute('aria-busy','true');panel.inert=true;wsiReviewSelecting=false;drag=null;erase();el('reviewSelectOnTissue').textContent='Lasso on tissue';
    try{
      await wsiReviewSync;
      if(verified!==mappingFingerprint())throw new Error('The target tissue or mapping changed. Confirm the mapping again before adding this object.');
      wsiReviewStatus(persist?'Adding object and saving the CellPhenotyper project…':'Attaching display data to this viewer…');
      const data=await wsiReviewRequest(request);
      if(data.experiment_reference){
        cfg.experiment={...cfg.experiment,...data.experiment_reference};cfg.session_inputs=cfg.session_inputs||{};
        cfg.session_inputs.cellphenotyper_experiment=data.experiment_reference;wsiExperimentRequest++;
      }
      wsiReviewInstall(data.context,data.checkpoint);invalidateMapping();
      const linked=data.context.reductions.find(r=>r.dataset_id===request.dataset_id&&r.frame_id===im.frame_id);
      const fallbackReduction=data.context.reductions.find(r=>r.observation_unit==='spot'&&r.dataset_id===request.dataset_id);
      const fields=[...new Set(data.context.observations.flatMap(row=>Object.keys(row.cluster_values||{})))].sort();
      wsiReviewController.restoreViewState({scope:'current',observation_unit:'spot',dataset_id:request.dataset_id,cluster_field:clusterColumns[0]||fields[0]||'',reduction_id:linked?.id||fallbackReduction?.id||''});
      wsiReviewStatus(data.persisted?'Added to the CellPhenotyper project. Reopen its experiment index to find this object and its selected metadata/reductions. Use Save Project as well to retain human review decisions.':'Attached selected metadata and reductions to this viewer. Inspect the cyan points on H&E before selecting tiles.');
    }finally{attaching=false;panel.inert=false;button.setAttribute('aria-busy','false');button.textContent=el('reviewPersistSpatial').checked?'Add to CellPhenotyper project':'Attach to this viewer only';updateAttach();}
  });
  const originalSnapshot=projectBrowserSnapshot;
  projectBrowserSnapshot=function(...args){return {...originalSnapshot(...args),review_workbench:wsiReviewContext,review_checkpoint:wsiReviewController.snapshot(),review_view_state:wsiReviewController.viewState(),
    bora_review_state:wsiBoraReviewController?.snapshot()||null};};
  // Checkpoints have one ordered transport. Reusing them in the annotation
  // delta document would let a pre-attachment snapshot overwrite newer labels.
  const originalSave=requestProjectSave;
  requestProjectSave=async function(...args){await wsiReviewFlush();return originalSave(...args);};
  const originalPreview=applyProjectPreview;
  applyProjectPreview=function(...args){const result=originalPreview(...args);invalidateMapping();wsiReviewSelecting=false;drag=null;erase();wsiReviewController.setActiveTissue(wsiReviewActiveTissue());wsiReviewRefreshOverlays();wsiReviewStoredObjects();wsiReviewRefreshKodamaButton();wsiBoraReviewSelected=null;wsiBoraReviewRender();const im=wsiReviewContext.images.find(i=>i.tissue_id===wsiReviewActiveTissue());el('reviewTileUnits').querySelector('option[value=um]').disabled=!(im?.mpp_x>0&&im?.mpp_y>0);if(!(im?.mpp_x>0&&im?.mpp_y>0))el('reviewTileUnits').value='px';return result;};
  const originalRestore=restoreBrowserProjectInLiveSession;
  restoreBrowserProjectInLiveSession=async function(snapshot,...args){
    await wsiReviewFlush();
    if(!snapshot||snapshot.schema!=='wsiTools-viewer-project'||!Array.isArray(snapshot.project?.items)||snapshot.project.items.length!==projectItems.length)
      throw new Error('Saved project images do not match this live session.');
    const samePath=(a,b)=>String(a.path||a.image_path||'')===String(b.path||b.image_path||'');
    if(snapshot.project.items.some((item,i)=>!samePath(item,projectItems[i])))throw new Error('Saved project refers to different image paths. Reopen it through Desktop Open Project.');
    if(typeof wsiExperimentValidateSnapshot==='function')wsiExperimentValidateSnapshot(snapshot);
    if(snapshot.review_workbench){
      const probe=WsiReviewWorkbench.create({context:snapshot.review_workbench,getActiveTissueId:wsiReviewActiveTissue});
      if(snapshot.review_checkpoint)probe.restore(snapshot.review_checkpoint);
      if(snapshot.review_view_state)probe.restoreViewState(snapshot.review_view_state);probe.destroy();
      const data=await wsiReviewRequest({action:'restore',context:snapshot.review_workbench,checkpoint:snapshot.review_checkpoint});
      wsiReviewInstall(data.context,data.checkpoint);
    }
    const result=await originalRestore(snapshot,...args);wsiReviewController.setActiveTissue(wsiReviewActiveTissue());wsiReviewRefreshOverlays();
    if(snapshot.review_view_state)wsiReviewController.restoreViewState(snapshot.review_view_state);
    if(snapshot.bora_review_state&&wsiBoraReviewController){wsiBoraReviewRestoring=true;try{wsiBoraReviewController.restore(snapshot.bora_review_state);}finally{wsiBoraReviewRestoring=false;}wsiBoraReviewRender();}
    return result;
  };
  if(typeof handleViewerCommand==='function'){
    const originalCommand=handleViewerCommand;
    handleViewerCommand=function(command){const result=originalCommand(command);
      if(command?.type==='restore_project_state'&&command.payload?.bora_review_state&&wsiBoraReviewController){
        wsiBoraReviewRestoring=true;try{wsiBoraReviewController.restore(command.payload.bora_review_state);}finally{wsiBoraReviewRestoring=false;}
        wsiBoraReviewRender();
      }
      return result;};
  }
}
if(typeof document!=='undefined')setTimeout(wsiReviewBindWorkbench,0);
