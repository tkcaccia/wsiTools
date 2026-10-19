/* Review metadata stays independent of source clusters and predictor matrices. */
let wsiReviewController = null;
let wsiReviewContext = null;
let wsiReviewSelecting = false;
let wsiReviewSync = Promise.resolve();

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
function wsiReviewStatus(message, error=false) {
  const node=el('reviewHostStatus'); if(node) {node.textContent=message;node.style.color=error?'#fca5a5':'';}
}
function wsiReviewOverlay(rows, selected=false) {
  const id=selected?'review_selected_observations':'review_observations';
  const items=rows.map(row=>({id:row.id,label:row.original_id,type:'point',x:row.x,y:row.y,
    radius:selected?8:3,colour:selected?'#facc15':'#22d3ee',fill:selected?'rgba(250,204,21,.5)':'rgba(34,211,238,.5)',
    project_image_index:projectItems.findIndex((item,i)=>wsiExperimentTissueId(i)===row.tissue_id),
    project_section_index:-1}));
  upsertViewerLayer({id,name:selected?'Review selection':'Review observations',type:'vector',source_type:'review_points',
    visible:true,replace:true,opacity:1,items,count:items.length});
  if(typeof buildLayerList==='function')buildLayerList();
  if(typeof draw==='function')draw();
}
function wsiReviewRefreshOverlays() {
  if(!wsiReviewController||!wsiReviewContext)return;
  const view=wsiReviewController.viewState(),selected=new Set(wsiReviewController.selection());
  const rows=wsiReviewContext.observations.filter(row=>row.tissue_id===wsiReviewActiveTissue()&&
    row.observation_unit===view.observation_unit&&(view.dataset_id==='*'||(row.dataset_id||'')===view.dataset_id));
  wsiReviewOverlay(rows);wsiReviewOverlay(rows.filter(row=>selected.has(row.id)),true);
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
  const image=await new Promise((resolve,reject)=>{const im=new Image();im.onload=()=>resolve(im);im.onerror=reject;im.src=src;});
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
  if(wsiReviewController)wsiReviewController.destroy();
  const mount=el('reviewWorkbenchMount');mount.replaceChildren();
  wsiReviewContext=context;cfg.review_workbench=context;window.wsiReviewContext=context;
  wsiReviewController=WsiReviewWorkbench.create({context,getActiveTissueId:wsiReviewActiveTissue,
    selectOverlay:()=>wsiReviewRefreshOverlays(),
    onViewChange:()=>{wsiReviewRefreshOverlays();if(typeof markProjectDirty==='function')markProjectDirty('review_view_updated');},
    draw:()=>{if(typeof draw==='function')draw();},
    sync:checkpoint=>{
      cfg.review_checkpoint=checkpoint;
      if(typeof markProjectDirty==='function')markProjectDirty('review_updated');
      if(cfg.review_url)wsiReviewSync=wsiReviewSync.catch(()=>{}).then(()=>wsiReviewRequest({action:'checkpoint',checkpoint}))
        .catch(error=>wsiReviewStatus('Review not synchronized: '+error.message,true));
    },
    plan:wsiReviewPlan,
    export:async rows=>{
      const result=await wsiReviewPlan(rows),blob=new Blob([JSON.stringify(result.backend_plan,null,2)],{type:'application/json'});
      await downloadBlob(blob,'selected-work-tiles.json');
      wsiReviewStatus('Exported '+result.tiles.length+' planned work tiles. This file starts no processing job.');return result;
    },previewTile:wsiReviewPreview});
  wsiReviewController.mount(mount);
  if(checkpoint)wsiReviewController.restore(checkpoint);
  window.wsiReviewWorkbench=wsiReviewController;
  wsiReviewRefreshOverlays();wsiReviewStoredObjects();
  const activeImage=context.images.find(i=>i.tissue_id===wsiReviewActiveTissue());
  const physicalOption=el('reviewTileUnits').querySelector('option[value=um]');physicalOption.disabled=!(activeImage?.mpp_x>0&&activeImage?.mpp_y>0);
  if(physicalOption.disabled&&el('reviewTileUnits').value==='um')el('reviewTileUnits').value='px';
  wsiReviewStatus(context.provenance?.availability?.reason||'Select a cluster or lasso observations, preview tiles, then export the plan.');
}
function wsiReviewBindWorkbench() {
  if(!cfg.review_workbench||el('reviewWorkbenchWindow'))return;
  const button=document.createElement('button');button.id='openReviewWorkbench';button.type='button';button.textContent='Review workspace';
  button.style.cssText='position:fixed;right:18px;top:54px;z-index:1100';document.body.appendChild(button);
  const panel=document.createElement('section');panel.id='reviewWorkbenchWindow';panel.hidden=true;
  panel.style.cssText='position:fixed;right:16px;top:92px;width:min(680px,90vw);max-height:80vh;overflow:auto;resize:both;z-index:1500;background:#172033;color:#e2e8f0;padding:12px;border:1px solid #64748b;border-radius:10px;box-shadow:0 10px 40px #0008';
  panel.innerHTML='<div style="display:flex;justify-content:space-between"><strong>Human review · spatial &amp; morphology</strong><button id="closeReviewWorkbench">Close</button></div>'+
    '<p>Select clusters to choose work. Confirm a class only after examining the tissue. Source cluster labels stay unchanged.</p>'+
    '<div style="display:flex;gap:6px;flex-wrap:wrap"><button id="reviewSelectOnTissue">Lasso on tissue</button><button id="reviewSelectAnnotation">Select inside annotation</button></div>'+
    '<p id="reviewHostStatus" role="status"></p><div id="reviewWorkbenchMount"></div>'+
    '<fieldset><legend>Work tile plan</legend><label>Tile size <input id="reviewTileSize" type="number" value="112" min="1" style="width:80px"></label> '+
    '<select id="reviewTileUnits" aria-label="Tile units"><option value="px">native pixels</option><option value="um">µm (image calibration required)</option></select> '+
    '<select id="reviewTileBounds" aria-label="Tile bounds"><option value="drop">Drop edge tiles</option><option value="trim">Trim at image edge</option><option value="error">Reject edge tiles</option></select>'+
    '<p>One image-anchored grid; overlapping selections share a work tile. The plan records selected source IDs and image hashes. Review labels are saved with your viewer project.</p></fieldset>'+
    '<details><summary>Add spatial transcriptomics to this project</summary><p>Choose a trusted local RDS. Coordinates must refer to this exact H&amp;E image. Large datasets need a bounded metadata export (10,000 observations per view).</p>'+
    '<strong>Saved spatial objects</strong><div id="reviewStoredObjects" aria-label="Saved spatial transcriptomics objects"></div>'+
    '<input id="reviewSpatialPath" placeholder="Local .rds path" aria-label="Spatial RDS path"><button id="reviewChooseSpatial">Choose RDS…</button> '+
    '<label>Dataset <input id="reviewDatasetId" value="spatial"></label><label>Seurat image <input id="reviewImageName" placeholder="required for multiple images"></label>'+
    '<p><label><input id="reviewTrustSpatial" type="checkbox"> I trust this local RDS file</label></p>'+
    '<button id="reviewInspectSpatial">Inspect metadata</button><pre id="reviewSpatialInspection" style="white-space:pre-wrap"></pre>'+
    '<label>Cluster fields <select id="reviewClusterColumns" multiple aria-label="Spatial cluster fields"></select></label> '+
    '<label>Reductions <select id="reviewReductionNames" multiple aria-label="Spatial reductions"></select></label><p>Inspect first, then explicitly choose cluster fields and reductions to attach. Ctrl/⌘ selects several.</p>'+
    '<label>Coordinates <select id="reviewCoordinateSpace"><option value="native_pixels">Full-resolution H&amp;E pixels</option><option value="source_pixels">Source pixels with explicit affine</option></select></label>'+
    '<label>Affine JSON <input id="reviewAffine" value="[[1,0,0],[0,1,0],[0,0,1]]" style="width:100%"></label>'+
    '<p><label><input id="reviewConfirmMapping" type="checkbox"> I verified the same section, image orientation and full-resolution coordinate mapping</label></p>'+
    '<p><label><input id="reviewPersistSpatial" type="checkbox" checked> Save the original object and selected display data in this CellPhenotyper project</label></p>'+
    '<p>The project keeps its own RDS copy and a small metadata/reduction file. A previous-index backup is retained. Opening the project loads display data; it does not automatically deserialize the RDS.</p>'+
    '<button id="reviewAttachSpatial">Add to CellPhenotyper project</button></details>';
  document.body.appendChild(panel);button.onclick=()=>{panel.hidden=!panel.hidden;};el('closeReviewWorkbench').onclick=()=>{panel.hidden=true;};
  wsiReviewInstall(cfg.review_workbench,cfg.review_checkpoint);
  for(const id of ['reviewTileSize','reviewTileUnits','reviewTileBounds'])el(id).onchange=()=>wsiReviewController.invalidatePlan();
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
  canvas.addEventListener('pointerup',event=>{if(!drag)return;event.stopImmediatePropagation();event.preventDefault();const points=drag;drag=null;erase();if(points.length>=3){const im=wsiReviewContext.images.find(i=>i.tissue_id===wsiReviewActiveTissue()),ring=points.map(p=>[p.x,p.y]);ring.push(ring[0]);try{wsiReviewController.selectImageGeometry({type:'Polygon',coordinates:[ring]},{tissue_id:im.tissue_id,frame_id:im.frame_id,mode:event.altKey?'subtract':event.shiftKey?'add':'replace'});}catch(error){wsiReviewStatus(error.message,true);}}},true);
  canvas.addEventListener('pointercancel',()=>{drag=null;erase();});
  document.addEventListener('keydown',event=>{if(event.key==='Escape'){drag=null;erase();wsiReviewSelecting=false;el('reviewSelectOnTissue').textContent='Lasso on tissue';}});
  for(const type of ['mousedown','mousemove','mouseup','click','dblclick'])canvas.addEventListener(type,event=>{if(wsiReviewSelecting){event.stopImmediatePropagation();event.preventDefault();}},true);
  const run=fn=>async()=>{try{await fn();}catch(error){wsiReviewStatus(error.message,true);}};
  el('reviewChooseSpatial').onclick=run(async()=>{const invoke=tauriViewerInvoke();if(!invoke)throw new Error('Paste the local RDS path when using a browser.');const path=await invoke('choose_viewer_review_data');if(path){el('reviewSpatialPath').value=path;el('reviewConfirmMapping').checked=false;}});
  const attachment=()=>({path:el('reviewSpatialPath').value,trusted:el('reviewTrustSpatial').checked,image_name:el('reviewImageName').value||null});
  let inspectedAttachment=null;
  el('reviewInspectSpatial').onclick=run(async()=>{
    const chosen=attachment(),data=await wsiReviewRequest({action:'inspect_spatial',...chosen});
    inspectedAttachment=JSON.stringify(chosen);el('reviewSpatialInspection').textContent=JSON.stringify(data.inspection,null,2);
    for(const [id,values] of [['reviewClusterColumns',data.inspection.metadata_fields],['reviewReductionNames',data.inspection.reduction_names]]){
      const select=el(id);select.replaceChildren();for(const value of values||[]){const option=document.createElement('option');option.value=option.textContent=value;select.appendChild(option);}
    }
  });
  let verifiedMapping=null,attaching=false;
  const mappingFingerprint=()=>{const im=wsiReviewContext.images.find(i=>i.tissue_id===wsiReviewActiveTissue());return JSON.stringify({
    tissue_id:im?.tissue_id,frame_id:im?.frame_id,image_sha256:im?.sha256,source_signature:wsiReviewContext.source_signature,
    path:el('reviewSpatialPath').value,image_name:el('reviewImageName').value,space:el('reviewCoordinateSpace').value,affine:el('reviewAffine').value});};
  const invalidateMapping=()=>{verifiedMapping=null;el('reviewConfirmMapping').checked=false;};
  el('reviewConfirmMapping').onchange=()=>{verifiedMapping=el('reviewConfirmMapping').checked?mappingFingerprint():null;};
  for(const id of ['reviewSpatialPath','reviewImageName','reviewCoordinateSpace','reviewAffine'])el(id).addEventListener('input',invalidateMapping);
  el('reviewPersistSpatial').onchange=()=>{el('reviewAttachSpatial').textContent=el('reviewPersistSpatial').checked?'Add to CellPhenotyper project':'Attach to this viewer only';};
  el('reviewAttachSpatial').onclick=run(async()=>{
    if(attaching)return;
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
    attaching=true;button.disabled=true;panel.inert=true;wsiReviewSelecting=false;drag=null;erase();el('reviewSelectOnTissue').textContent='Lasso on tissue';
    try{
      await wsiReviewSync;
      if(verified!==mappingFingerprint())throw new Error('The target tissue or mapping changed. Confirm the mapping again before adding this object.');
      wsiReviewStatus(persist?'Adding object and saving the CellPhenotyper project…':'Attaching display data to this viewer…');
      const data=await wsiReviewRequest(request);
      if(data.experiment_reference){
        cfg.experiment=data.experiment_reference;cfg.session_inputs=cfg.session_inputs||{};
        cfg.session_inputs.cellphenotyper_experiment=data.experiment_reference;wsiExperimentRequest++;
      }
      wsiReviewInstall(data.context,data.checkpoint);invalidateMapping();
      const linked=data.context.reductions.find(r=>r.dataset_id===request.dataset_id&&r.frame_id===im.frame_id);
      const fallbackReduction=data.context.reductions.find(r=>r.observation_unit==='spot'&&r.dataset_id===request.dataset_id);
      const fields=[...new Set(data.context.observations.flatMap(row=>Object.keys(row.cluster_values||{})))].sort();
      wsiReviewController.restoreViewState({scope:'current',observation_unit:'spot',dataset_id:request.dataset_id,cluster_field:clusterColumns[0]||fields[0]||'',reduction_id:linked?.id||fallbackReduction?.id||''});
      wsiReviewStatus(data.persisted?'Added to the CellPhenotyper project. Reopen its experiment index to find this object and its selected metadata/reductions. Use Save Project as well to retain human review decisions.':'Attached selected metadata and reductions to this viewer. Inspect the cyan points on H&E before selecting tiles.');
    }finally{attaching=false;button.disabled=false;panel.inert=false;}
  });
  const originalSnapshot=projectBrowserSnapshot;
  projectBrowserSnapshot=function(...args){return {...originalSnapshot(...args),review_workbench:wsiReviewContext,review_checkpoint:wsiReviewController.snapshot(),review_view_state:wsiReviewController.viewState()};};
  // Checkpoints have one ordered transport. Reusing them in the annotation
  // delta document would let a pre-attachment snapshot overwrite newer labels.
  const originalSave=requestProjectSave;
  requestProjectSave=async function(...args){await wsiReviewSync;return originalSave(...args);};
  const originalPreview=applyProjectPreview;
  applyProjectPreview=function(...args){const result=originalPreview(...args);invalidateMapping();wsiReviewSelecting=false;drag=null;erase();wsiReviewController.setActiveTissue(wsiReviewActiveTissue());wsiReviewRefreshOverlays();wsiReviewStoredObjects();const im=wsiReviewContext.images.find(i=>i.tissue_id===wsiReviewActiveTissue());el('reviewTileUnits').querySelector('option[value=um]').disabled=!(im?.mpp_x>0&&im?.mpp_y>0);if(!(im?.mpp_x>0&&im?.mpp_y>0))el('reviewTileUnits').value='px';return result;};
  const originalRestore=restoreBrowserProjectInLiveSession;
  restoreBrowserProjectInLiveSession=async function(snapshot,...args){
    await wsiReviewSync;
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
    if(snapshot.review_view_state)wsiReviewController.restoreViewState(snapshot.review_view_state);return result;
  };
}
if(typeof document!=='undefined')setTimeout(wsiReviewBindWorkbench,0);
