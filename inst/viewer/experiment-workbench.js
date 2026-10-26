/* CellPhenotyper experiments use cached, typed observations. No encoder runs here. */
// Shared modeless panels leave the tissue and the Project menu accessible.
const wsiWorkspacePanels = new Map();
function wsiWorkspaceBind(button,panel,titleId,onClose=()=>{},onOpen=()=>{}) {
  let dock=el('experimentWorkspaceTools');
  if(!dock){
    const style=document.createElement('style');style.textContent=`
      #experimentWorkspaceTools{position:fixed;right:16px;bottom:16px;z-index:1100;display:flex;gap:8px;flex-wrap:wrap;justify-content:flex-end;max-width:calc(100vw - 32px)}
      .wsiExperimentPanel{box-sizing:border-box;position:fixed;right:12px;bottom:66px;width:min(680px,calc(100vw - 24px));overflow:auto;z-index:1490;background:#172033;color:#e2e8f0;padding:12px;border:1px solid #64748b;border-radius:10px;box-shadow:0 10px 40px #0008;overflow-wrap:anywhere}
      .wsiExperimentPanel[hidden]{display:none}.wsiExperimentPanel *{box-sizing:border-box;min-width:0}
      .wsiExperimentPanel select,.wsiExperimentPanel input:not([type=checkbox]){max-width:100%}
      .wsiExperimentPanel select{white-space:nowrap;text-overflow:ellipsis}
      .wsiExperimentPanel fieldset{margin:12px 0}.wsiExperimentPanel fieldset label{display:inline-block;margin:4px 0}
      .wsiWorkspaceHead{position:sticky;top:-12px;z-index:3;display:flex;align-items:center;justify-content:space-between;gap:12px;background:#172033;padding:8px 0;border-bottom:1px solid #64748b}
      .wsiWorkspaceHead h2{font-size:16px;margin:0}.wsiExperimentPanel :focus-visible{outline:2px solid #67e8f9;outline-offset:2px}
      .wsiTableScroll{max-width:100%;overflow-x:auto}.wsiTableScroll td{padding:4px 8px}.wsiSpatialStatus{position:sticky;bottom:-12px;z-index:2;background:#172033;padding:10px 0;margin-bottom:0}
      #reviewSpatialInspection{white-space:pre-wrap;max-height:150px;overflow:auto}
      @media(max-width:900px){.wsiExperimentWorkspaces #workspacePanel{right:auto;width:min(340px,calc(100vw - 24px))}}
    `;document.head.appendChild(style);
    document.body.classList.add('wsiExperimentWorkspaces');
    dock=document.createElement('div');dock.id='experimentWorkspaceTools';dock.setAttribute('role','toolbar');dock.setAttribute('aria-label','Experiment workspaces');document.body.appendChild(dock);
  }
  dock.appendChild(button);panel.classList.add('wsiExperimentPanel');
  panel.setAttribute('role','dialog');panel.setAttribute('aria-modal','false');panel.setAttribute('aria-labelledby',titleId);
  button.setAttribute('aria-controls',panel.id);button.setAttribute('aria-expanded','false');
  const updatePosition=()=>{const bar=document.querySelector('.bar'),bottom=bar?.getBoundingClientRect().bottom||80;
    panel.style.top=Math.max(12,Math.min(window.innerHeight-180,bottom+8))+'px';};
  window.addEventListener('resize',updatePosition);if(typeof ResizeObserver!=='undefined'&&document.querySelector('.bar'))new ResizeObserver(updatePosition).observe(document.querySelector('.bar'));updatePosition();
  const setOpen=(open,focus=true)=>{
    if(panel.inert)return false;
    if(open){for(const [other,close] of wsiWorkspacePanels)if(other!==panel&&!other.hidden&&!close(false,false))return false;}
    panel.hidden=!open;button.setAttribute('aria-expanded',String(open));
    if(open){onOpen();if(focus)el(titleId)?.focus();}else{onClose();if(focus)button.focus();}return true;
  };
  wsiWorkspacePanels.set(panel,setOpen);button.onclick=()=>setOpen(panel.hidden);
  document.addEventListener('keydown',event=>{
    if(event.key==='Escape'&&!panel.hidden){setOpen(false);event.preventDefault();event.stopPropagation();}
  });
  return setOpen;
}

let wsiExperimentAppearance = null;
let wsiExperimentBusy = false;
let wsiExperimentRequest = 0;
let wsiExperimentClassifiers = [];

function wsiExperimentConfig() { return cfg.experiment || null; }
function wsiExperimentTissueId(index = activeProjectIndex) {
  const item = projectItems[index] || {};
  return String(item.tissue_id || item.id || '');
}
function wsiExperimentStatus(message, error = false) {
  const node = el('experimentStatus');
  if (node) { node.textContent = message; node.style.color = error ? '#fca5a5' : ''; }
}
function wsiExperimentSnapshot() {
  const appearance = wsiExperimentAppearance || wsiExperimentAppearanceFromLayers();
  return { version: 1, appearance: appearance ? {
    recipe_path: appearance.recipe_path,
    recipe_id: appearance.recipe_id,
    feature_source: appearance.feature_source,
    scope: appearance.scope,
    grids: appearance.grids
  } : null, wand_source: el('experimentWandSource')?.value || 'he',
  reach_um: Number(el('experimentWandReach')?.value || 250),
  feature_tolerance: wsiExperimentTolerance(),
  tissue_ids: Array.from(el('experimentTissues')?.selectedOptions || [], option => option.value),
  classifiers: wsiExperimentClassifiers.map(model => Object.assign({},model)) };
}

function wsiExperimentFillClassifiers() {
  const select=el('experimentSavedClassifier');if(!select)return;
  const previous=select.value;select.innerHTML='';
  if(!wsiExperimentClassifiers.length){const opt=document.createElement('option');opt.textContent='Run PLS-LDA above to save a classifier';opt.value='';select.appendChild(opt);}
  wsiExperimentClassifiers.forEach(model=>{const opt=document.createElement('option');opt.value=model.path;opt.textContent=model.label;select.appendChild(opt);});
  if(wsiExperimentClassifiers.some(model=>model.path===previous))select.value=previous;
  const apply=el('experimentApplyClassifier');if(apply)apply.disabled=!wsiExperimentClassifiers.length||wsiExperimentBusy;
}

function wsiExperimentRecordPrediction(info) {
  if(!wsiExperimentConfig()||!info?.classifier_path)return;
  if(!wsiExperimentClassifiers.some(model=>model.path===info.classifier_path))
    wsiExperimentClassifiers.push({path:info.classifier_path,feature_source:info.feature_source,
      label:'Saved '+(String(info.point_unit||'feature'))+' classifier · '+new Date().toLocaleTimeString()});
  wsiExperimentFillClassifiers();
  wsiExperimentStatus('Classifier saved with its training settings. You can apply it to other tissues below without retraining.');
}

async function wsiExperimentApplyClassifier() {
  const path=el('experimentSavedClassifier')?.value;
  if(!path||wsiExperimentBusy)return;
  const model=wsiExperimentClassifiers.find(model=>model.path===path),tissue_ids=selectedOptionValues(el('experimentTissues'));
  if(!tissue_ids.length){wsiExperimentStatus('Choose at least one output tissue.',true);return;}
  wsiExperimentBusy=true;wsiExperimentFillClassifiers();
  wsiExperimentStatus('Applying the saved classifier to cached features…');
  try{
    const response=await fetch(predictionUrl(),{method:'POST',headers:{'Content-Type':'application/json'},
      body:JSON.stringify({action:'experiment_apply_classifier',model_path:path,feature_source:model.feature_source,tissue_ids})});
    const body=await response.json();if(!response.ok||body.ok===false)throw new Error(body.error||'Classifier application failed.');
    handleViewerCommands(body);if(typeof revealPredictionOverlay==='function')revealPredictionOverlay();
    const count=Number(body.prediction?.count||0);
    wsiExperimentStatus('Applied the saved classifier to '+count.toLocaleString()+' observations. Training settings were held fixed.');
  }catch(error){wsiExperimentStatus(error.message,true);}
  finally{wsiExperimentBusy=false;wsiExperimentFillClassifiers();}
}

function wsiExperimentTolerance() {
  const value = Number(el('experimentWandTolerance')?.value ?? 0.15);
  if (!Number.isFinite(value) || value < 0 || value > 1)
    throw new Error('Feature tolerance must be between zero and one.');
  return value;
}

function wsiExperimentValidateSnapshot(snapshot) {
  const saved = snapshot?.experiment, current = wsiExperimentConfig();
  if (!saved || !current) return;
  const oldId = saved.id || saved.experiment_id, newId = current.id || current.experiment_id;
  if ((oldId && newId && oldId !== newId) ||
      (saved.manifest_sha256 && current.manifest_sha256 && saved.manifest_sha256 !== current.manifest_sha256))
    throw new Error('This session belongs to a different or revised experiment. Open its experiment index first.');
}

function wsiExperimentRestoreSnapshot(snapshot) {
  if (!wsiExperimentConfig()) return;
  wsiExperimentValidateSnapshot(snapshot);
  const state = snapshot?.experiment_workbench || {}, savedItems = snapshot?.project?.items || [];
  wsiExperimentClassifiers = Array.isArray(state.classifiers) ? state.classifiers.filter(model =>
    model && typeof model.path === 'string' && typeof model.label === 'string' &&
    typeof model.feature_source === 'string' && model.feature_source.startsWith('experiment:')) : [];
  wsiExperimentFillClassifiers();
  const clone = value => JSON.parse(JSON.stringify(value));
  // Restore only derived appearance layers. Fresh image sources, calibration and
  // the scientific experiment backend remain owned by the opened experiment.
  projectItems.forEach((item, index) => {
    const tissue = wsiExperimentTissueId(index);
    const saved = savedItems.find(entry => String(entry.tissue_id || entry.id || '') === tissue);
    const appearance = (saved?.layers || []).find(layer => layer.id === 'wsi_experiment_appearance');
    if (appearance) item.layers = (item.layers || []).filter(layer => layer.id !== appearance.id).concat([clone(appearance)]);
  });
  const activeLayer = (projectItems[activeProjectIndex]?.layers || []).find(layer => layer.id === 'wsi_experiment_appearance');
  if (activeLayer) {
    const index = layers.findIndex(layer => layer.id === activeLayer.id);
    if (index >= 0) layers.splice(index, 1, clone(activeLayer)); else layers.push(clone(activeLayer));
  }
  wsiExperimentAppearance = wsiExperimentAppearanceFromLayers();
  const reach = Number(state.reach_um), tolerance = Number(state.feature_tolerance);
  if (el('experimentWandReach')) el('experimentWandReach').value = String(Number.isFinite(reach) && reach >= 1 && reach <= 5000 ? reach : 250);
  if (el('experimentWandTolerance')) el('experimentWandTolerance').value = String(Number.isFinite(tolerance) && tolerance >= 0 && tolerance <= 1 ? tolerance : 0.15);
  const scope = el('experimentContrastScope'), tissues = el('experimentTissues');
  if (scope) scope.value = state.appearance?.scope === 'shared' ? 'shared' : 'local';
  if (tissues) {
    tissues.disabled = false;
    if (Array.isArray(state.tissue_ids)) for (const option of tissues.options) option.selected = state.tissue_ids.includes(option.value);
  }
  const source = el('predictionFeatureSource'), requestedSource = state.appearance?.feature_source;
  if (source && requestedSource && Array.from(source.options).some(option => option.value === requestedSource)) {
    source.value = requestedSource;
    if (typeof setPredictionSource === 'function') setPredictionSource(requestedSource);
  }
  if (el('experimentWandSource')) el('experimentWandSource').value = state.wand_source === 'feature' ? 'feature' : 'he';
  if (typeof buildLayerList === 'function') buildLayerList();
  if (typeof overlayFocusMode !== 'undefined') {
    overlayFocusMode='all';
    if(typeof updateOverlayFocusControls==='function')updateOverlayFocusControls();
    if(typeof applyOverlayFocus==='function')applyOverlayFocus();
  }
  wsiExperimentRefreshControls();
  if (typeof wsiRequestDraw === 'function') wsiRequestDraw();
  if (typeof scheduleViewerStateSync === 'function')
    scheduleViewerStateSync('viewer_state', {reason:'experiment_restored'});
}

// Similarity uses retained probability vectors. Display palette and screen zoom
// are deliberately absent from this operation.
function wsiExperimentConnectedSelection(points, grid, seed, tolerance, reachPx) {
  const sx = Number(grid.spacing_x_px), sy = Number(grid.spacing_y_px);
  const ox = Number(grid.origin_x_px || 0), oy = Number(grid.origin_y_px || 0);
  const tw = Number(grid.tile_width_px || sx), th = Number(grid.tile_height_px || sy);
  if (![sx, sy, tw, th, reachPx].every(v => Number.isFinite(v) && v > 0))
    throw new Error('The feature grid or physical reach is missing. Re-export this experiment.');
  if (!Number.isFinite(tolerance) || tolerance < 0 || tolerance > 1)
    throw new Error('Feature tolerance must be between zero and one.');
  const records = new Map(); let closest = null, distance = Infinity;
  for (const p of points) {
    if (![p.x, p.y].every(Number.isFinite)) throw new Error('Feature coordinates must be finite.');
    const col = Math.floor((p.x - ox) / sx), row = Math.floor((p.y - oy) / sy);
    const key = col + ',' + row;
    if (records.has(key)) throw new Error('Two feature observations occupy the same grid location.');
    const probabilities = p.probabilities;
    if (!Array.isArray(probabilities) || probabilities.length < 2 ||
      !probabilities.every(v => Number.isFinite(v) && v >= 0 && v <= 1))
      throw new Error('This appearance has no usable probability evidence. Recreate the contrast.');
    const record = { point: p, col, row, key };
    records.set(key, record);
    if (Math.abs(p.x - seed.x) <= tw / 2 && Math.abs(p.y - seed.y) <= th / 2) {
      const d = (p.x - seed.x) ** 2 + (p.y - seed.y) ** 2;
      if (d < distance) { closest = record; distance = d; }
    }
  }
  if (!closest) throw new Error('There are no cached features at this position. Click a coloured tissue region.');
  const reference = closest.point.probabilities, visited = new Set(), selected = [], queue = [closest];
  for (let head = 0; head < queue.length; head++) {
    const current = queue[head];
    if (visited.has(current.key)) continue;
    visited.add(current.key);
    const p = current.point, values = p.probabilities;
    if (values.length !== reference.length) throw new Error('Appearance class order is inconsistent.');
    if ((p.x - seed.x) ** 2 + (p.y - seed.y) ** 2 > reachPx ** 2) continue;
    const delta = Math.sqrt(values.reduce((sum, v, i) => sum + (v - reference[i]) ** 2, 0) / values.length);
    if (delta > tolerance) continue;
    selected.push(current);
    if (selected.length > 50000) throw new Error('The selection is too large. Reduce the physical reach.');
    for (const [dc, dr] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) {
      const next = records.get((current.col + dc) + ',' + (current.row + dr));
      if (next && !visited.has(next.key)) queue.push(next);
    }
  }
  if (!selected.length) throw new Error('The selected region is smaller than the feature spacing. Increase physical reach.');
  return selected;
}

function wsiExperimentSelectionGroups(selected, grid, width, height) {
  // Cancel shared footprint edges, then trace native-coordinate rings. Unlike
  // a screen mask this preserves the same geometry at every zoom and keeps holes.
  const edges = new Map(), sx = Number(grid.spacing_x_px), sy = Number(grid.spacing_y_px);
  const tw = Number(grid.tile_width_px || sx), th = Number(grid.tile_height_px || sy);
  if (tw > sx + 1e-7 || th > sy + 1e-7)
    throw new Error('The feature wand needs a non-overlapping observation grid.');
  const key = (x,y) => [x,y].map(v => Math.round(v*1e7)/1e7).join(',');
  function edge(a,b) {
    const forward=key(a.x,a.y)+'>'+key(b.x,b.y), reverse=key(b.x,b.y)+'>'+key(a.x,a.y);
    if (edges.has(reverse)) edges.delete(reverse);
    else edges.set(forward,{x1:a.x,y1:a.y,x2:b.x,y2:b.y,used:false});
  }
  for (const record of selected) {
    const rawX=Number(grid.origin_x_px||0)+record.col*sx,
      rawY=Number(grid.origin_y_px||0)+record.row*sy,
      x0=Math.max(0,rawX),y0=Math.max(0,rawY),
      x1=Math.min(width,rawX+tw),y1=Math.min(height,rawY+th);
    if (!(x1>x0&&y1>y0)) continue;
    const p=[{x:x0,y:y0},{x:x1,y:y0},{x:x1,y:y1},{x:x0,y:y1}];
    p.forEach((a,i)=>edge(a,p[(i+1)%4]));
  }
  const starts=new Map();
  for (const e of edges.values()) {
    const k=key(e.x1,e.y1);if(!starts.has(k))starts.set(k,[]);starts.get(k).push(e);
  }
  const rings=[];
  for (const first of edges.values()) {
    if(first.used)continue;
    let e=first,ring=[{x:e.x1,y:e.y1}],guard=0;
    while(e&&!e.used&&guard++<=edges.size+1) {
      e.used=true;ring.push({x:e.x2,y:e.y2});
      if(key(e.x2,e.y2)===key(first.x1,first.y1))break;
      const candidates=(starts.get(key(e.x2,e.y2))||[]).filter(next=>!next.used);
      e=candidates.length>1?chooseMaskEdge(e,candidates):candidates[0];
    }
    if(ring.length>=4&&key(ring[0].x,ring[0].y)===key(ring.at(-1).x,ring.at(-1).y))rings.push(ring);
  }
  const groups=rings.filter(r=>signedRingArea(r)>0).map(r=>[r]);
  for(const hole of rings.filter(r=>signedRingArea(r)<0)) {
    const owner=groups.filter(g=>pointInRing(hole[0],g[0]))
      .sort((a,b)=>Math.abs(signedRingArea(a[0]))-Math.abs(signedRingArea(b[0])))[0];
    if(owner)owner.push(hole);
  }
  if(!groups.length)throw new Error('No valid feature boundary was found.');
  return groups;
}

function wsiExperimentAppearanceFromLayers() {
  const layer = (layers || []).find(l => l.id === 'wsi_experiment_appearance');
  if (!layer?.metadata?.experiment_appearance) return null;
  return Object.assign({}, layer.metadata.experiment_appearance, {
    points: (layer.items || []).map(item => Object.assign({}, item.evidence, {
      id: item.id, tissue_id: item.tissue_id
    }))
  });
}
function wsiExperimentActiveAppearance(pane = null) {
  const index = pane?.entry ? Number(pane.entry.itemIndex) : activeProjectIndex;
  const tissue = wsiExperimentTissueId(index);
  const sourceLayers = pane && typeof multiViewLayersForPane === 'function' ? multiViewLayersForPane(pane) : layers;
  const layer = (sourceLayers || []).find(l => l.id === 'wsi_experiment_appearance');
  if (!layer?.metadata?.experiment_appearance) return null;
  const appearance = layer.metadata.experiment_appearance;
  return { tissue, index, grid: appearance.grids?.[tissue], appearance,
    points: (layer.items || []).filter(item => String(item.tissue_id) === tissue).map(item => item.evidence) };
}

function wsiExperimentInstallAppearance(appearance) {
  if (!appearance || !Array.isArray(appearance.points) || !appearance.points.length)
    throw new Error('No appearance observations were returned. Check the selected tissues and annotations.');
  const grids = appearance.grids || appearance.grid_by_tissue || {};
  if (appearance.points.length > 20000) throw new Error('This preview exceeds 20,000 observations. Choose a smaller feature cache.');
  wsiExperimentAppearance = Object.assign({}, appearance, { grids });
  projectItems.forEach((item, index) => {
    const tissue = wsiExperimentTissueId(index), grid = grids[tissue];
    const points = appearance.points.filter(p => String(p.tissue_id) === tissue);
    if (!points.length || !grid) return;
    const metadata = Object.assign({}, wsiExperimentSnapshot().appearance);
    const entries = points.map(p => {
      const sx=Number(grid.spacing_x_px),sy=Number(grid.spacing_y_px),
        ox=Number(grid.origin_x_px||0),oy=Number(grid.origin_y_px||0),
        x0=ox+Math.floor((p.x-ox)/sx)*sx,y0=oy+Math.floor((p.y-oy)/sy)*sy,
        x1=x0+Number(grid.tile_width_px||sx),y1=y0+Number(grid.tile_height_px||sy);
      const colour = '#' + [p.r,p.g,p.b].map(v => Math.max(0,Math.min(255,Math.round(Number(v)))).toString(16).padStart(2,'0')).join('');
      const ring = [{x:Math.max(0,x0),y:Math.max(0,y0)},
        {x:Math.min(Number(item.width),x1),y:Math.max(0,y0)},
        {x:Math.min(Number(item.width),x1),y:Math.min(Number(item.height),y1)},
        {x:Math.max(0,x0),y:Math.min(Number(item.height),y1)}];
      ring.push(ring[0]);
      return {id:String(p.id),name:'Virchow2 evidence',type:'polygon',drawable:true,rings:[ring],
        tissue_id:tissue,project_image_index:index,project_section_index:-1,project_scoped:true,
        colour,fill:colour,evidence:p};
    });
    const layer = {id:'wsi_experiment_appearance',name:appearance.scope === 'shared' ?
      'Virchow2 contrast · shared reference' : 'Virchow2 contrast · local',type:'vector',source_type:'feature_appearance',
      visible:true,opacity:0.75,outline:false,project_managed:true,count:entries.length,items:entries,
      metadata:{experiment_appearance:metadata,coordinate_overlay:false}};
    item.layers = (item.layers || []).filter(l => l.id !== layer.id).concat([layer]);
    if (index === activeProjectIndex) {
      const prior = layers.findIndex(l => l.id === layer.id);
      if (prior >= 0) layers.splice(prior,1,layer); else layers.push(layer);
    }
  });
  if (typeof buildLayerList === 'function') buildLayerList();
  if (typeof markProjectDirty === 'function') markProjectDirty('experiment_appearance');
  if (typeof wsiRequestDraw === 'function') wsiRequestDraw();
  wsiExperimentRefreshControls();
}

async function wsiExperimentCreateAppearance(recipePath = null) {
  if (wsiExperimentBusy) return;
  const payload = predictionPayload(), mode = el('experimentContrastScope')?.value || 'local';
  if (!String(payload.feature_source).startsWith('experiment:')) {
    wsiExperimentStatus('Choose a cached Virchow2 feature source above.', true); return;
  }
  if (!recipePath && !payload.train_annotations.length) {
    wsiExperimentStatus('Select training annotations from at least two tissue classes above, then create the contrast.', true); return;
  }
  const requestedTissues = selectedOptionValues(el('experimentTissues'));
  const tissue = wsiExperimentTissueId();
  payload.action = 'experiment_appearance'; payload.mode = mode;
  payload.active_tissue_id = tissue;
  payload.tissue_ids = mode === 'local' ? [tissue] : requestedTissues;
  if (!payload.tissue_ids.length) { wsiExperimentStatus('Choose at least one output tissue.',true); return; }
  if (recipePath) payload.recipe_path = recipePath;
  const generation = ++wsiExperimentRequest;
  wsiExperimentBusy = true; wsiExperimentRefreshControls();
  wsiExperimentStatus(recipePath ? 'Reopening saved feature contrast…' : 'Creating contrast from cached features on CPU…');
  try {
    const response = await fetch(predictionUrl(), {method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(payload)});
    const body = await response.json();
    if (!response.ok || body.ok === false) throw new Error(body.error || 'The contrast could not be created.');
    if (generation !== wsiExperimentRequest) {
      wsiExperimentStatus('The tissue changed during processing. Create the contrast for the current view.');return;
    }
    // Do not attach a local result to another tissue after a view switch.
    if (mode === 'local' && tissue !== wsiExperimentTissueId()) throw new Error('The active tissue changed. Return to the original tissue and create the contrast again.');
    const appearance = body.appearance;
    appearance.scope = appearance.scope || mode; appearance.feature_source = appearance.feature_source || payload.feature_source;
    wsiExperimentInstallAppearance(appearance);
    wsiExperimentStatus((mode === 'shared' ? 'Shared reference colours are fixed across the selected tissues. ' : 'Local colours describe this tissue only. ') +
      'The contrast is ready. Choose “Use feature wand”, then click a coloured region.');
  } catch (error) { wsiExperimentStatus(error.message, true); }
  finally { wsiExperimentBusy = false; wsiExperimentRefreshControls(); }
}

async function wsiExperimentRunWand(canvasPointValue, slidePointValue, pane = null, event = {}) {
  if (wandBusy || wsiExperimentBusy) return;
  const active = wsiExperimentActiveAppearance(pane);
  if (!active?.grid || !active.points.length) { notify('Create a Virchow2 contrast for this tissue first.','warning'); return; }
  if (pane?.entry && Number(pane.entry.itemIndex) !== activeProjectIndex) {
    notify('Select this tissue in the Project panel before editing it with the feature wand.','info',4200); return;
  }
  const subtract = wandSubtractModifier(event), maskSource = activeEditableAnnotationMaskSource();
  if (!pointInsideSlide(slidePointValue)) return;
  const target = brushEditableTarget(slidePointValue), targetRoi = target >= 0 ? rois[target] : null;
  if (targetRoi && lockedRoi(targetRoi)) { notify('Unlock the selected annotation before editing.','warning'); return; }
  if (!maskSource && subtract && !targetRoi) { notify('Select an annotation before using Alt + Wand.','warning'); return; }
  const startKey = projectAnnotationKey(), epoch = wsiAnnotationEpoch;
  const mpp = projectItems[active.index]?.mpp || cfg.mpp;
  const mppx = Number(typeof mpp === 'number' ? mpp : mpp?.x || mpp?.mpp_x),
    mppy = Number(typeof mpp === 'number' ? mpp : mpp?.y || mpp?.mpp_y || mppx);
  if (!(mppx > 0 && mppy > 0)) { notify('Physical calibration is missing. Re-export the experiment before feature selection.','warning'); return; }
  const reach = Number(el('experimentWandReach')?.value || 250);
  wandBusy = true;
  try {
    // Scale coordinates to micrometres so anisotropic images use a physical circle.
    const physical = active.points.map(p => Object.assign({},p,{x:p.x*mppx,y:p.y*mppy}));
    const physicalGrid = Object.assign({},active.grid,{spacing_x_px:active.grid.spacing_x_px*mppx,
      spacing_y_px:active.grid.spacing_y_px*mppy,tile_width_px:active.grid.tile_width_px*mppx,
      tile_height_px:active.grid.tile_height_px*mppy,origin_x_px:active.grid.origin_x_px*mppx,
      origin_y_px:active.grid.origin_y_px*mppy});
    const selected = wsiExperimentConnectedSelection(physical,physicalGrid,
      {x:slidePointValue.x*mppx,y:slidePointValue.y*mppy},wsiExperimentTolerance(),reach);
    selected.forEach(r => { r.point = Object.assign({},r.point,{x:r.point.x/mppx,y:r.point.y/mppy}); });
    let groups = wsiExperimentSelectionGroups(selected,active.grid,Number(cfg.slide_width),Number(cfg.slide_height));
    if (startKey !== projectAnnotationKey() || epoch !== wsiAnnotationEpoch)
      throw new Error('The annotations changed during selection. Please click again.');
    if (maskSource) {
      if (!paintAnnotationMaskGroups(groups,currentRoiClass(),subtract?'subtract':'paint')) throw new Error('The annotation mask could not be updated.');
      finishAnnotationMaskBrush(subtract?'subtract':'paint',currentRoiClass()); return;
    }
    let roi;
    if (targetRoi) roi = !subtract && brushPriorityModifier(event) ?
      await wsiApplyPriorityClaim(groups,target,'wand',wandGeometryOptions(pane)) :
      await wsiApplyWandEdit(groups,target,subtract?'subtract':'extend',wandGeometryOptions(pane));
    else roi = addRoiFromBrushGroups(groups,'wand','Feature wand ROI',currentRoiClass());
    if (!roi) throw new Error('The selected region overlaps a protected annotation.');
    roi.wand = true; roi.wand_source = 'virchow2-probability';
    roi.appearance_recipe = active.appearance.recipe_path || active.appearance.recipe_id;
    roi.wand_tolerance = wsiExperimentTolerance(); roi.wand_reach_um = reach;
    roi.wand_observation_count = selected.length;
    roi.properties=Object.assign({},roi.properties,{wsiTools:Object.assign({},roi.properties?.wsiTools,{
      experiment_wand:{source:'virchow2-probability',experiment_id:wsiExperimentConfig()?.experiment_id,
        tissue_id:active.tissue,recipe:roi.appearance_recipe,tolerance:roi.wand_tolerance,
        reach_um:reach,observation_count:selected.length,coordinate_space:'level0_slide_pixels'}})});
    if(typeof wsiInvalidateGeometry==='function')wsiInvalidateGeometry(roi);
    markAnnotationsDirty(targetRoi?'roi_brush_edited':'roi_added'); buildRoiList(); updateButtons(); wsiRequestDraw();
    scheduleViewerStateSync('roi_brush_edited',{id:roi.id,source:'virchow2-probability',recipe:roi.appearance_recipe});
    notify('Selected '+selected.length+' '+(String(predictionSelectedSource().unit||'observations'))+'.','success',2200);
  } catch (error) { notify('Feature selection: '+error.message,'error',6200); }
  finally { wandBusy = false; }
}

function wsiExperimentRefreshControls() {
  if (!wsiExperimentConfig()) return;
  const create = el('experimentContrastCreate'), use = el('experimentUseWand'), source = el('experimentWandSource');
  if (create) create.disabled = wsiExperimentBusy || !predictionUrl();
  const available = !!wsiExperimentActiveAppearance()?.points.length;
  if (use) use.disabled = !available || wsiExperimentBusy;
  if (source) { source.options[1].disabled = !available; if (!available) source.value = 'he'; }
  const physical = source?.value === 'feature';
  const reach = el('experimentWandReachControl'); if (reach) reach.hidden = !physical;
  const tolerance = el('experimentWandToleranceControl'); if (tolerance) tolerance.hidden = !physical;
  const originalReach = el('wandReach')?.closest('label'); if (originalReach) originalReach.hidden = physical;
  const originalTolerance = el('wandTolerance')?.closest('label'); if (originalTolerance) originalTolerance.hidden = physical;
  const button = el('toolWand');
  if (button) {
    if (!button.dataset.experimentOriginalTitle) button.dataset.experimentOriginalTitle = button.title || 'Magic wand';
    button.title = physical ? 'Feature wand: connected cached evidence in native tissue coordinates. Reach is in micrometres; tolerance is 0–1. Alt-click subtracts.' : button.dataset.experimentOriginalTitle;
  }
}

function wsiExperimentBoraReviewEntry(){
  if(typeof tissueAnnotationLayerEntries!=='function')return null;
  return tissueAnnotationLayerEntries().find(entry=>entry?.review===true&&
    entry?.layer?.metadata?.review_only===true&&
    entry?.layer?.metadata?.training_eligible===false)||null;
}
function wsiExperimentBoraDraftFor(item,layer){
  const sourceId=String(item?.id||'');
  const sourceHash=String(layer?.metadata?.source_sha256||'');
  return Array.isArray(rois)?rois.find(roi=>roi?.properties?.manual_review_status==='draft_from_prediction'&&
    String(roi.properties.source_prediction_id||'')===sourceId&&
    String(roi.properties.source_prediction_sha256||'')===sourceHash):null;
}
function wsiExperimentSyncBoraReviewVisibility(){
  if(typeof layers==='undefined'||!Array.isArray(layers))return;
  layers.filter(layer=>layer?.source_type==='bora_candidate_review'&&
    Number(layer?.metadata?.group_count)===3).forEach(layer=>{
    (layer.items||[]).forEach(item=>{
      const draft=wsiExperimentBoraDraftFor(item,layer);
      if(draft){
        item._boraHiddenByDraft=true;
        item.visible=!!item._boraShowOriginalForCompare;
      }else if(item._boraHiddenByDraft){
        item._boraHiddenByDraft=false;
        item._boraShowOriginalForCompare=false;
        item.visible=true;
      }
    });
  });
}
function wsiExperimentBoraAnnotationMessage(){
  if(wsiExperimentConfig()?.bora_annotations_mode==='ordinary_editable')return '';
  const evidence=wsiExperimentConfig()?.viewer_evidence||{};
  const interpretation=evidence.bora_interpretation;
  const review=wsiExperimentBoraReviewEntry();
  if(review&&Number(review.layer.metadata.group_count)===3)return 'Three anonymous KODAMA groups were refined by Bora. The technical seam check failed: grid-aligned boundary artifacts may remain. Click a Bora boundary on the slide or in Annotations to edit a separate autosaved copy with Brush or polygon tools. The original prediction remains available for comparison or reset. These predictions are not ground truth.';
  if(review)return review.layer.metadata.native_he_boundary_refinement_applied===true?
    'Bora predicted tissue boundaries are available for exploratory review after native H&E refinement. They are not ground truth or PLS training regions.':
    'Bora predicted tissue boundaries are available for exploratory review. They are not ground truth or PLS training regions.';
  if(typeof interpretation!=='string'||!interpretation.trim())return '';
  const withheld=/^(withheld|pending)/i.test(String(evidence.bora_status||''));
  const qualified=typeof rois!=='undefined'&&Array.isArray(rois)&&rois.some(roi=>
    roi?.properties?.source_role==='tissue_annotations');
  const imported=typeof tissueAnnotationLayerEntries==='function'&&
    tissueAnnotationLayerEntries().some(entry=>entry?.item?.properties?.source_role==='tissue_annotations');
  if(!withheld&&(qualified||imported))return '';
  return 'No Bora tissue boundaries are available in this project. Predictions are withheld pending review. '+interpretation;
}
function wsiExperimentEvidenceSummary(){
  const evidence=wsiExperimentConfig()?.viewer_evidence||{},coverage=evidence.cell_coverage||[];
  const partial=coverage.map(item=>`${item.tissue_id}: CellViT ${item.completed_shards}/${item.planned_shards} shards · ${Number(item.cell_count).toLocaleString()} cells`);
  if(wsiExperimentConfig()?.bora_annotations_mode==='ordinary_editable')
    return ['Bora tissue annotations · edit with standard tools',...partial].join(' | ');
  const review=wsiExperimentBoraReviewEntry();
  return [review?'Bora prediction · exploratory review':evidence.bora_interpretation?(wsiExperimentBoraAnnotationMessage()?'Bora tissue predictions withheld':'Bora prediction · exploratory'):'',...partial].filter(Boolean).join(' | ');
}
function wsiExperimentRefreshBoraAnnotationStatus(){
  const summary=el('experimentEvidenceSummary');if(summary)summary.textContent=wsiExperimentEvidenceSummary();
  const notice=el('experimentBoraAnnotationStatus');if(!notice)return;
  const message=wsiExperimentBoraAnnotationMessage();notice.hidden=!message;
  if(notice.textContent!==message)notice.textContent=message;
  const save=el('experimentManualReviewExport');
  if(save){const three=Number(wsiExperimentBoraReviewEntry()?.layer?.metadata?.group_count)===3;
    save.hidden=!three;save.disabled=!three||!Array.isArray(rois)||
      !rois.some(roi=>roi?.properties?.manual_review_status==='draft_from_prediction');}
  wsiExperimentDecorateBoraReviewRows();
  wsiExperimentDecorateBoraDraftRows();
}
function wsiExperimentBindBoraAnnotationStatus(){
  if(wsiExperimentConfig()?.bora_annotations_mode==='ordinary_editable')return;
  if(!wsiExperimentConfig()||!el('roiPanelBody')||el('experimentBoraAnnotationStatus'))return;
  const notice=document.createElement('p');notice.id='experimentBoraAnnotationStatus';
  notice.setAttribute('role','status');notice.style.cssText='border:1px solid #f59e0b;border-radius:6px;background:#422006;color:#fef3c7;padding:10px;margin:6px 0;font-size:13px;line-height:1.45';
  const summary=el('roiSummary');if(summary&&typeof summary.after==='function')summary.after(notice);
  else el('roiPanelBody').prepend(notice);
  const save=document.createElement('button');save.id='experimentManualReviewExport';
  save.type='button';save.textContent='Export manual corrections · GeoJSON';
  save.title='Export separate editable review drafts. The locked Bora predictions are not included.';
  save.style.cssText='margin:0 0 8px 0;padding:7px 10px';
  save.onclick=()=>wsiExperimentExportBoraDrafts();
  notice.after(save);
  if(typeof buildRoiList==='function'){
    const original=buildRoiList;
    buildRoiList=function(...args){wsiExperimentSyncBoraReviewVisibility();const result=original(...args);wsiExperimentRefreshBoraAnnotationStatus();return result;};
  }
  wsiExperimentSyncBoraReviewVisibility();
  wsiExperimentRefreshBoraAnnotationStatus();
}

// Model boundaries stay immutable. A reviewer can copy one into a separate
// draft ROI, edit it with the ordinary polygon/Brush/Wand tools, and export
// only the draft ROIs as GeoJSON. Copying is not an expert validation event.
function wsiExperimentCopyBoraBoundaryForReview(layerIndex,itemIndex){
  const layer=layers?.[Number(layerIndex)],item=layer?.items?.[Number(itemIndex)];
  const group=Number(item?.properties?.value);
  if(layer?.source_type!=='bora_candidate_review'||
     layer?.metadata?.review_only!==true||layer?.metadata?.training_eligible!==false||
     Number(layer?.metadata?.group_count)!==3||![1,2,3].includes(group)||
     !item||typeof denseLayerEditableGroups!=='function'){
    notify('Select a three-group Bora prediction to copy for manual review.','warning');return false;
  }
  const existing=wsiExperimentBoraDraftFor(item,layer);
  if(existing){
    wsiExperimentSyncBoraReviewVisibility();
    selectAnnotation(rois.indexOf(existing),false);
    if(typeof setMode==='function')setMode('brush');
    if(typeof buildRoiList==='function')buildRoiList();
    if(typeof draw==='function')draw();
    notify('Continuing the saved review draft.','info',2400);
    return true;
  }
  const groups=denseLayerEditableGroups(item);
  if(!groups.length){notify('This boundary has no editable polygon.','warning');return false;}
  const sourceId=String(item.id||itemIndex),sourceHash=String(layer.metadata.source_sha256||'');
  const id='manual_bora_review_'+Date.now().toString(36)+'_'+Math.random().toString(36).slice(2,9);
  const label='KODAMA Group '+group;
  const draftColour=item.colour||'#5eead4';
  const roi={id,name:'Review draft · '+label,class:label,colour:draftColour,
    original_colour:draftColour,fill:hexToRgba(draftColour,0.12),
    rings:groups[0],add_groups:groups.slice(1),add_rings:[],subtract_rings:[],
    raw_coordinates:null,dense_static_geometry:false,dense_geometry:false,
    drawable:true,locked:false,isLocked:false,visible:true,source:'Manual Bora review draft',
    source_type:'manual_review_draft',tissue_annotation:false,
    properties:{class:label,review_only:true,training_eligible:false,
      predicted_not_expert_annotation:true,manual_review_status:'draft_from_prediction',
      source_prediction_id:sourceId,source_prediction_sha256:sourceHash,
      source_frame_id:layer.metadata.source_frame_id||null,
      source_image_sha256:layer.metadata.image_sha256||null,
      tissue_id:item.tissue_id||projectItems?.[activeProjectIndex]?.tissue_id||null,
      project_image_index:activeProjectIndex,project_section_index:activeProjectSectionIndex,
      coordinate_space:'level0_slide_pixels'}};
  if(typeof pushAnnotationUndo==='function')pushAnnotationUndo('bora_review_draft_copied');
  refreshRoiGeometry(roi);
  rois.push(roi);
  wsiExperimentSyncBoraReviewVisibility();
  if(typeof ensureRoiClassOption==='function')ensureRoiClassOption(label);
  selectAnnotation(rois.length-1,false);
  if(typeof markAnnotationsDirty==='function')markAnnotationsDirty('bora_review_draft_copied');
  if(typeof buildRoiList==='function')buildRoiList();
  if(typeof updateButtons==='function')updateButtons();
  if(typeof scheduleViewerStateSync==='function')scheduleViewerStateSync('roi_added',
    {id,source_prediction_id:sourceId,source_prediction_sha256:sourceHash,
      manual_review_status:'draft_from_prediction',training_eligible:false});
  if(typeof recordAnnotationHistory==='function')recordAnnotationHistory('roi_added',
    {id,name:roi.name,source:'Bora prediction copy for manual review'});
  if(typeof setMode==='function')setMode('brush');
  if(typeof draw==='function')draw();
  notify('Editable review copy created. Refine it with Brush or polygon editing, then export the annotations as GeoJSON. The prediction remains unchanged.','success',6000);
  return true;
}
function wsiExperimentBoraPredictionForDraft(roi){
  if(roi?.properties?.manual_review_status!=='draft_from_prediction')return null;
  for(let li=0;li<(layers?.length||0);li++){
    const layer=layers[li];if(layer?.source_type!=='bora_candidate_review'||
      String(layer.metadata?.source_sha256||'')!==String(roi.properties.source_prediction_sha256||''))continue;
    const ii=(layer.items||[]).findIndex(item=>String(item.id||'')===String(roi.properties.source_prediction_id||''));
    if(ii>=0)return {layer,item:layer.items[ii],layerIndex:li,itemIndex:ii};
  }
  return null;
}
function wsiExperimentRevertBoraDraft(index){
  const roi=rois?.[Number(index)];if(!roi||roi.properties?.manual_review_status!=='draft_from_prediction')return false;
  const prediction=wsiExperimentBoraPredictionForDraft(roi);
  if(typeof deleteRoi==='function')deleteRoi(index);
  else return false;
  if(prediction){prediction.item._boraShowOriginalForCompare=false;prediction.item.visible=true;}
  wsiExperimentSyncBoraReviewVisibility();
  if(typeof buildRoiList==='function')buildRoiList();
  if(typeof draw==='function')draw();
  notify('Review draft removed; original Bora prediction is visible again.','info',3400);
  return true;
}
function wsiExperimentDecorateBoraDraftRows(){
  const list=el('roiList');if(!list)return;
  list.querySelectorAll('.roiItem[data-index]').forEach(row=>{
    const index=Number(row.dataset.index),roi=rois?.[index];
    if(roi?.properties?.manual_review_status!=='draft_from_prediction'||row.querySelector('.boraReviewRevert'))return;
    const controls=row.querySelector('.roiControls');if(!controls)return;
    const revert=document.createElement('button');revert.type='button';
    revert.className='boraReviewRevert';revert.textContent='Revert to prediction';
    revert.title='Discard this manual draft and reveal the unchanged Bora prediction';
    revert.onclick=event=>{event.stopPropagation();wsiExperimentRevertBoraDraft(index);};
    controls.prepend(revert);
    const prediction=wsiExperimentBoraPredictionForDraft(roi);
    if(prediction){
      const compare=document.createElement('button');compare.type='button';
      compare.className='boraReviewCompare';
      compare.textContent=prediction.item._boraShowOriginalForCompare?'Hide original':'Show original';
      compare.title='Toggle the unchanged model prediction for comparison';
      compare.onclick=event=>{
        event.stopPropagation();prediction.item._boraShowOriginalForCompare=!prediction.item._boraShowOriginalForCompare;
        wsiExperimentSyncBoraReviewVisibility();
        if(typeof buildRoiList==='function')buildRoiList();if(typeof draw==='function')draw();
      };
      controls.prepend(compare);
    }
  });
}
function wsiExperimentBoraCanvasHit(event){
  if(event.button!==0||event.target!==canvas||typeof pointerToSlide!=='function'||
     typeof layerObjectAt!=='function')return null;
  const hit=layerObjectAt(pointerToSlide(event),(item,layer)=>
    layer?.source_type==='bora_candidate_review'&&Number(layer?.metadata?.group_count)===3);
  return hit;
}
let wsiExperimentBoraSuppressClickUntil=0;
function wsiExperimentHandleBoraCanvasPointerDown(event){
  if(!['brush','wand','polygon','draw'].includes(String(mode)))return;
  const hit=wsiExperimentBoraCanvasHit(event);if(!hit)return;
  event.preventDefault();event.stopImmediatePropagation();
  wsiExperimentBoraSuppressClickUntil=performance.now()+700;
  wsiExperimentCopyBoraBoundaryForReview(hit.layerIndex,hit.itemIndex);
}
function wsiExperimentHandleBoraCanvasMouseDown(event){
  if(event.target!==canvas||performance.now()>wsiExperimentBoraSuppressClickUntil)return;
  event.preventDefault();event.stopImmediatePropagation();
}
function wsiExperimentHandleBoraCanvasClick(event){
  if(event.target===canvas&&performance.now()<=wsiExperimentBoraSuppressClickUntil){
    event.preventDefault();event.stopImmediatePropagation();
    wsiExperimentBoraSuppressClickUntil=0;return;
  }
  const hit=wsiExperimentBoraCanvasHit(event);
  if(!hit)return;
  event.preventDefault();event.stopImmediatePropagation();
  wsiExperimentCopyBoraBoundaryForReview(hit.layerIndex,hit.itemIndex);
}

async function wsiExperimentExportBoraDrafts(){
  const drafts=rois.map((roi,index)=>({roi,index})).filter(entry=>
    entry.roi?.properties?.manual_review_status==='draft_from_prediction');
  if(!drafts.length){notify('Copy and edit a Bora boundary first.','warning');return false;}
  const features=drafts.map(entry=>roiFeature(entry.roi,entry.index)).filter(Boolean);
  if(features.length!==drafts.length){notify('A review draft has no exportable geometry.','error');return false;}
  const payload={type:'FeatureCollection',coordinate_space:'level0_slide_pixels',
    review_status:'manual_correction_draft_not_yet_expert_validated',
    experiment_manifest_sha256:wsiExperimentConfig()?.manifest_sha256||null,
    features};
  const name='PM016_KODAMA3_Bora_manual_corrections.geojson';
  const blob=new Blob([JSON.stringify(payload,null,2)],{type:'application/geo+json'});
  try{
    const format={description:'GeoJSON manual corrections',
      accept:{'application/geo+json':['.geojson'],'application/json':['.json']}};
    const result=typeof saveBlobWithLocation==='function'&&
      typeof savePickerAvailable==='function'&&savePickerAvailable()?
      await saveBlobWithLocation(blob,name,format):'unsupported';
    if(result==='unsupported'){
      if(typeof downloadBlob!=='function')throw new Error('No browser download is available.');
      await downloadBlob(blob,name);
    }
    else if(result==='cancelled')return false;
    else if(result!=='saved'&&result!=='unsupported')throw new Error('Save did not complete.');
    if(typeof scheduleViewerStateSync==='function')scheduleViewerStateSync('roi_exported',
      {count:features.length,source:'manual_bora_review_draft'});
    notify('Exported '+features.length+' editable review draft'+(features.length===1?'':'s')+' as GeoJSON.','success');
    return true;
  }catch(error){notify('Manual correction export failed: '+error.message,'error');return false;}
}

function wsiExperimentDecorateBoraReviewRows(){
  const list=el('roiList');if(!list)return;
  list.querySelectorAll('.roiItem[data-layer-index][data-item-index]').forEach(row=>{
      const layerIndex=Number(row.dataset.layerIndex),itemIndex=Number(row.dataset.itemIndex),
        layer=layers?.[layerIndex],group=Number(layer?.items?.[itemIndex]?.properties?.value);
      if(layer?.source_type!=='bora_candidate_review'||
         layer?.metadata?.review_only!==true||Number(layer?.metadata?.group_count)!==3||
         ![1,2,3].includes(group)||row.querySelector('.boraReviewCopy'))return;
      const name=row.querySelector('.roiName'),klass=row.querySelector('.roiClass');
      if(name)name.textContent='Bora prediction · click to edit';
      if(klass)klass.textContent='KODAMA Group '+group;
      const controls=row.querySelector('.roiControls');if(!controls)return;
      const copy=document.createElement('button');copy.type='button';
      copy.className='boraReviewCopy';copy.textContent='Edit boundary';
      copy.title='Edit a separate review draft while preserving the model prediction';
      copy.onclick=event=>{event.stopPropagation();wsiExperimentCopyBoraBoundaryForReview(layerIndex,itemIndex);};
      controls.prepend(copy);
      const originalClick=row.onclick;
      row.onclick=event=>{
        event.stopPropagation();wsiExperimentCopyBoraBoundaryForReview(layerIndex,itemIndex);
      };
  });
}
function wsiExperimentBindBoraManualReview(){
  if(wsiExperimentConfig()?.bora_annotations_mode==='ordinary_editable')return;
  if(typeof appendImportedTissueAnnotationList==='function'&&
     !appendImportedTissueAnnotationList._boraManualReviewBound){
    const original=appendImportedTissueAnnotationList;
    appendImportedTissueAnnotationList=function(...args){
      const result=original(...args);wsiExperimentDecorateBoraReviewRows();return result;
    };
    appendImportedTissueAnnotationList._boraManualReviewBound=true;
  }
  if(typeof window!=='undefined'&&!wsiExperimentBindBoraManualReview._canvasBound){
    window.addEventListener('pointerdown',wsiExperimentHandleBoraCanvasPointerDown,true);
    window.addEventListener('mousedown',wsiExperimentHandleBoraCanvasMouseDown,true);
    window.addEventListener('click',wsiExperimentHandleBoraCanvasClick,true);
    wsiExperimentBindBoraManualReview._canvasBound=true;
  }
  wsiExperimentDecorateBoraReviewRows();
  wsiExperimentDecorateBoraDraftRows();
}

function wsiExperimentBindWorkbench() {
  if (!wsiExperimentConfig() || el('experimentWorkbench')) return;
  const evidence=wsiExperimentConfig().viewer_evidence||{},coverage=evidence.cell_coverage||[];
  wsiExperimentBindBoraManualReview();
  wsiExperimentBindBoraAnnotationStatus();
  if((coverage.length||evidence.bora_interpretation||wsiExperimentBoraReviewEntry())&&el('projectPanelBody')&&!el('experimentEvidenceNotice')){
    const notice=document.createElement('details');notice.id='experimentEvidenceNotice';
    notice.style.cssText='border:1px solid #64748b;border-radius:6px;padding:6px;margin:4px 0 8px;color:#e2e8f0;font-size:12px';
    const summary=document.createElement('summary');summary.id='experimentEvidenceSummary';summary.style.cursor='pointer';
    summary.textContent=wsiExperimentEvidenceSummary();
    const detail=document.createElement('p');detail.style.margin='6px 0 0';
    detail.textContent=(wsiExperimentConfig()?.bora_annotations_mode==='ordinary_editable'?
      'Bora polygons are editable tissue annotations. Their KODAMA group labels describe model output, not pathology ground truth.':
      (wsiExperimentBoraReviewEntry()?wsiExperimentBoraAnnotationMessage():(evidence.bora_interpretation||'')))+
      (coverage.length?' CellViT covers only the listed completed shards; unprocessed tissue has no cell prediction.':'');
    notice.append(summary,detail);el('projectPanelBody').prepend(notice);
  }
  const panel = el('predictionWindow'); if (!panel) return;
  const section = document.createElement('section'); section.id = 'experimentWorkbench';
  section.style.cssText = 'border-top:1px solid #475569;margin-top:12px;padding-top:12px;display:grid;gap:8px';
  section.innerHTML = '<strong>Virchow2 contrast &amp; feature wand</strong>'+
    '<p class="menuHint" style="margin:0">1. Choose cached features above. 2. Select annotations from two or more classes. 3. Create a contrast, then use it to guide the wand.</p>'+
    '<label class="control">Colour scope <select id="experimentContrastScope"><option value="local">Local · current tissue</option><option value="shared">Shared · compare selected tissues</option></select></label>'+
    '<label class="control wide">Apply to tissues <select id="experimentTissues" multiple aria-label="Tissues for shared contrast and saved classifiers"></select></label>'+
    '<p class="menuHint" style="margin:0">Local contrast uses the current tissue. Shared contrast and saved classifiers use the tissues selected here.</p>'+
    '<label class="control wide">Saved classifier <select id="experimentSavedClassifier" aria-label="Saved classifiers"></select></label>'+
    '<button id="experimentApplyClassifier" type="button" disabled>Apply saved classifier to selected tissues</button>'+
    '<div class="predictionTools"><button id="experimentContrastCreate" type="button">Create feature contrast</button><button id="experimentUseWand" type="button" disabled>Use feature wand</button></div>'+
    '<div id="experimentStatus" class="menuHint" role="status" aria-live="polite">Local colours emphasize this tissue. Shared colours use one fixed reference. No encoder or GPU job starts here.</div>';
  panel.insertBefore(section,el('predictionLegend'));
  const tissues = el('experimentTissues');
  projectItems.forEach((item,index) => { const opt = document.createElement('option'); opt.value=wsiExperimentTissueId(index);
    opt.textContent=String(item.label||item.name||opt.value); opt.selected=true; tissues.appendChild(opt); });
  const guide = document.createElement('p'); guide.className='menuHint'; guide.style.margin='4px 0';
  guide.textContent='Cached experiment: '+projectItems.length+' tissue'+(projectItems.length===1?'':'s')+'. Draw and label training regions, then select them below. Tile and spatial-token sources have different resolutions.';
  panel.querySelector('.predictionHead')?.after(guide);
  el('experimentContrastCreate').onclick=()=>wsiExperimentCreateAppearance();
  el('experimentApplyClassifier').onclick=wsiExperimentApplyClassifier;
  wsiExperimentFillClassifiers();
  el('experimentUseWand').onclick=()=>{el('experimentWandSource').value='feature';wsiExperimentRefreshControls();closePredictionWindow();setMode('wand');notify('Feature wand ready. Click a coloured region; Alt-click subtracts from a selected annotation.','info',4400);};
  tissues.disabled=false;
  const wand = el('wandTolerance')?.closest('label');
  if (wand) {
    const label=document.createElement('label');label.className='control';label.textContent='Wand source ';
    const select=document.createElement('select');select.id='experimentWandSource';
    select.innerHTML='<option value="he">H&amp;E image</option><option value="feature" disabled>Virchow2 contrast</option>';
    label.appendChild(select);wand.before(label);
    const reach=document.createElement('label');reach.id='experimentWandReachControl';reach.className='control';reach.hidden=true;
    reach.innerHTML='Feature reach (µm) <input id="experimentWandReach" type="number" min="1" max="5000" step="10" value="250" style="width:75px">';
    wand.after(reach);select.onchange=wsiExperimentRefreshControls;
    const tolerance=document.createElement('label');tolerance.id='experimentWandToleranceControl';tolerance.className='control';tolerance.hidden=true;
    tolerance.title='Distance from the clicked observation in retained feature evidence. Lower values select more similar tissue; this is not a confidence threshold.';
    tolerance.innerHTML='Feature tolerance <input id="experimentWandTolerance" type="number" min="0" max="1" step="0.01" value="0.15" style="width:65px">';
    reach.after(tolerance);
  }
  const originalWand = runWandAt;
  runWandAt = async function(...args) { return el('experimentWandSource')?.value === 'feature' ? wsiExperimentRunWand(...args) : originalWand(...args); };
  const originalPreview = applyProjectPreview;
  applyProjectPreview = function(...args) { const result=originalPreview(...args);wsiExperimentRequest++;wsiExperimentRefreshControls();wsiExperimentRefreshBoraAnnotationStatus();return result; };
  if (typeof restoreBrowserProjectInLiveSession === 'function' && !restoreBrowserProjectInLiveSession._experimentBound) {
    const originalRestore = restoreBrowserProjectInLiveSession;
    restoreBrowserProjectInLiveSession = function(snapshot, ...args) {
      wsiExperimentValidateSnapshot(snapshot);
      const result = originalRestore(snapshot, ...args);
      wsiExperimentRestoreSnapshot(snapshot);
      return result;
    };
    restoreBrowserProjectInLiveSession._experimentBound = true;
  }
  wsiExperimentRefreshControls();
  if(wsiExperimentConfig()?.bora_annotations_mode==='ordinary_editable'&&
     typeof setMode==='function')setMode('pan');
}

// Start with the actual project capabilities, rather than unexplained disabled tools.
function wsiExperimentBindGuide() {
  if(!wsiExperimentConfig()||el('experimentGuideWindow'))return;
  const button=document.createElement('button');button.id='openExperimentGuide';button.type='button';button.textContent='Experiment guide';
  const panel=document.createElement('section');panel.id='experimentGuideWindow';panel.hidden=true;
  panel.innerHTML='<div class="wsiWorkspaceHead"><h2 id="experimentGuideTitle" tabindex="-1">Start with this experiment</h2><button id="closeExperimentGuide">Close</button></div>'+
    '<p id="experimentGuideSummary"></p><div id="experimentGuideCapabilities"></div>'+
    '<p id="experimentGuideTissue" role="status"></p>'+
    '<p><strong>PLS training:</strong> choose cached features, then select annotations from at least two classes. A tile is eligible only when its whole footprint lies inside the annotation. Pilot labels in demonstration projects are software examples, not biological truth.</p>'+
    '<p>Spatial review bundles contain clusters and reductions. Gene expression, gene statistics and expression-based PLS additionally need an explicitly linked expression object; storing an RDS does not automatically load it.</p>'+
    '<p>Use Save Project to keep display settings and review decisions. This viewer uses existing results and does not start GigaTIME, feature extraction or remote processing.</p>';
  document.body.appendChild(panel);
  const render=()=>{
    const sources=Array.isArray(cfg.prediction?.sources)?cfg.prediction.sources:[];
    const markers=(typeof channelSources==='undefined'?[]:channelSources).filter(source=>source.metadata?.gigatime);
    const tissueIds=new Set(markers.map(source=>source.metadata.gigatime.tissue_id));
    const review=cfg.review_workbench||{},rows=review.observations||[];
    el('experimentGuideSummary').textContent=String(wsiExperimentConfig().name||cfg.title||'Opened experiment')+' · '+projectItems.length+' tissues. Choose a workflow below; availability is based on the data actually registered in this project.';
    const container=el('experimentGuideCapabilities');container.replaceChildren();
    const add=(title,description,label,action,enabled=true)=>{
      const section=document.createElement('section');section.style.cssText='border:1px solid #64748b;border-radius:6px;margin:10px 0;padding:10px';
      const heading=document.createElement('h3');heading.style.margin='0 0 6px';heading.textContent=title;
      const text=document.createElement('p');text.textContent=description;section.append(heading,text);
      const control=document.createElement('button');control.textContent=label;control.disabled=!enabled;control.onclick=action;section.appendChild(control);container.appendChild(section);
    };
    add('GigaTIME marker images and cell scores',markers.length?markers.length+' registered image channels across '+tissueIds.size+' tissues. Images start hidden so H&E stays visible. Show a marker to overlay its predicted signal; cell scores require a registered segmentation.':'No GigaTIME marker images are registered in this experiment. Register existing outputs and reopen the new index.',
      'Show marker on current tissue',()=>{setOpen(false);el('openGigatimeWorkbench')?.click();const image=el('experimentGigatimeImage');if(image&&!image.disabled&&!image.checked)image.click();},markers.some(source=>source.metadata.gigatime.tissue_id===wsiExperimentTissueId()));
    add('PLS classification and Virchow2 contrast',sources.length?sources.length+' predictor sources available. '+(cfg.prediction?.fastpls_installed?'fastPLS is installed.':'fastPLS is missing; Prediction explains how to install it.')+' Choose a source and two labelled training classes.':'This project has no cached predictor features. GigaTIME marker scores alone do not enable Virchow2 PLS. Open an experiment containing registered Virchow2 features or link a supported expression/cell table.',
      sources.length?'Open PLS classification':'Why PLS is unavailable',()=>{setOpen(false);if(typeof openPredictionWindow==='function')openPredictionWindow();});
    const spots=rows.filter(row=>row.observation_unit==='spot').length;
    add('Spatial clusters, KODAMA and human review',rows.length?rows.length.toLocaleString()+' registered observations, '+spots.toLocaleString()+' spots and '+(review.reductions||[]).length+' linked reductions. Select a dataset to keep equally named clusters separate.':'No review observations are registered. Add a trusted same-section spatial object through the Review workspace.',
      'Open Review workspace',()=>{setOpen(false);el('openReviewWorkbench')?.click();},!!cfg.review_workbench);
    const active=wsiExperimentTissueId(),item=projectItems[activeProjectIndex];
    el('experimentGuideTissue').textContent='Current tissue: '+String(item?.label||item?.name||active)+(tissueIds.has(active)?' · GigaTIME output available.':' · no registered GigaTIME output on this tissue.');
  };
  const setOpen=wsiWorkspaceBind(button,panel,'experimentGuideTitle',()=>{},render);
  el('closeExperimentGuide').onclick=()=>setOpen(false);
  // This is a modeless introduction, closed by its action buttons or Escape.
  setOpen(true,false);
}

// The scientific index owns GigaTIME sources. This panel only changes their
// display settings and exports already-quantified values; it runs no model.
let wsiGigatimeFreshSources=null;
let wsiGigatimeSelection={};
let wsiGigatimePage=0;
let wsiGigatimeRenderedTissue=null;
let wsiGigatimeExporting=false;
function wsiGigatimeSource(value) { return !!value?.metadata?.gigatime||String(value?.id||'').startsWith('gigatime_'); }
function wsiGigatimeCaptureSources() {
  const clone=value=>JSON.parse(JSON.stringify(value));
  wsiGigatimeFreshSources={tissues:projectItems.map((item,index)=>wsiExperimentTissueId(index)),items:projectItems.map(item=>clone((item.layers||[]).filter(wsiGigatimeSource))),
    itemSources:projectItems.map(item=>{const {layers,...source}=item;return clone(source);}),
    channels:clone((typeof channelSources==='undefined'?[]:channelSources).filter(wsiGigatimeSource))};
}
function wsiGigatimePrepareSnapshot(input) {
  const clone=value=>JSON.parse(JSON.stringify(value)),snapshot=clone(input);
  if(!wsiGigatimeFreshSources)return snapshot;
  if(Array.isArray(snapshot.project?.items)&&(snapshot.project.items.length!==wsiGigatimeFreshSources.tissues.length||
    snapshot.project.items.some((item,index)=>String(item.tissue_id||item.id||'')!==wsiGigatimeFreshSources.tissues[index])))
    throw new Error('Saved GigaTIME tissue identities/order differ from this experiment. Open the scientific index first.');
  const saved=[...(snapshot.layers||[]),...(snapshot.project?.items||[]).flatMap(item=>item.layers||[]),
    ...(snapshot.channel_sources||[]),...(snapshot.channel_settings||[]),...layers,
    ...projectItems.flatMap(item=>item.layers||[]),...(typeof channelSources==='undefined'?[]:channelSources)];
  const preference=source=>{
    const result=clone(source),setting=saved.find(item=>item.id===source.id)||{};
    if(typeof setting.visible==='boolean')result.visible=setting.visible;
    if(source.metadata?.gigatime?.available===false)result.visible=false;
    for(const key of ['opacity','gain'])if(Number.isFinite(setting[key])&&setting[key]>=0&&(key!=='opacity'||setting[key]<=1))result[key]=setting[key];
    if(typeof setting.colour==='string'&&/^#[0-9a-f]{6}$/i.test(setting.colour))result.colour=setting.colour;
    const lo=setting.contrast_min,hi=setting.contrast_max;
    if(Number.isFinite(lo)&&Number.isFinite(hi)&&lo>=0&&hi<=1&&lo<hi){result.contrast_min=lo;result.contrast_max=hi;}
    return result;
  };
  const known=wsiGigatimeFreshSources.items.map(items=>items.map(preference));
  Object.defineProperty(snapshot,'_gigatimeDisplayItems',{value:known,enumerable:false});
  if(Array.isArray(snapshot.project?.items))snapshot.project.items.forEach((item,index)=>{
    snapshot.project.items[index]={...clone(wsiGigatimeFreshSources.itemSources[index]),
      layers:(item.layers||[]).filter(layer=>!wsiGigatimeSource(layer)).concat(known[index]||[])};});
  const active=Number(snapshot.project?.active_index??activeProjectIndex);
  snapshot.layers=(snapshot.layers||[]).filter(layer=>!wsiGigatimeSource(layer)).concat(known[active]||[]);
  snapshot.channel_sources=(snapshot.channel_sources||[]).filter(src=>!wsiGigatimeSource(src)).concat(wsiGigatimeFreshSources.channels.map(preference));
  // A later generic restore command must not overwrite the checked settings.
  const channelIds=new Set(wsiGigatimeFreshSources.channels.map(src=>src.id));
  snapshot.channel_settings=(snapshot.channel_settings||[]).filter(setting=>!channelIds.has(setting.id)&&!wsiGigatimeSource(setting));
  return snapshot;
}
function wsiGigatimeRestoreDisplay(input) {
  if(!wsiGigatimeFreshSources)return;
  const checked=wsiGigatimePrepareSnapshot(input);
  projectItems.forEach((item,index)=>{item.layers=(item.layers||[]).filter(layer=>!wsiGigatimeSource(layer))
    .concat(checked._gigatimeDisplayItems[index]||[]);});
  layers.splice(0,layers.length,...layers.filter(layer=>!wsiGigatimeSource(layer)),...(projectItems[activeProjectIndex]?.layers||[]).filter(wsiGigatimeSource));
  if(typeof channelSources!=='undefined'){
    for(const src of channelSources.filter(wsiGigatimeSource))if(typeof removeChannelItem==='function')removeChannelItem(src.id);
    channelSources.splice(0,channelSources.length,...channelSources.filter(src=>!wsiGigatimeSource(src)),...checked.channel_sources.filter(wsiGigatimeSource));
    if(typeof syncChannelSourcesForActiveImage==='function')syncChannelSourcesForActiveImage();
  }
  const saved=input?.gigatime_workbench?.selected_by_tissue;
  wsiGigatimeSelection=saved&&typeof saved==='object'&&!Array.isArray(saved)?Object.assign({},saved):{};
  wsiGigatimeRenderedTissue=null;
  if(typeof buildLayerList==='function')buildLayerList();if(typeof draw==='function')draw();wsiGigatimeRender();
}
function wsiGigatimeEntries() {
  const tissue=wsiExperimentTissueId(),sources=typeof channelSources==='undefined'?[]:channelSources;
  const images=sources.filter(src=>src.metadata?.gigatime?.tissue_id===tissue);
  const cells=(layers||[]).filter(layer=>layer.metadata?.gigatime?.tissue_id===tissue);
  const entries=cells.map(layer=>{
    const meta=layer.metadata.gigatime;
    return {key:JSON.stringify([meta.artifact_id,meta.marker]),marker:meta.marker,cell:layer,
      image:images.find(src=>src.metadata.gigatime.marker===meta.marker&&
        src.metadata.gigatime.artifact_id===meta.source_artifact_id)||null};
  });
  for(const image of images){const meta=image.metadata.gigatime;
    if(!entries.some(entry=>entry.image?.id===image.id))entries.push({
      key:JSON.stringify([meta.artifact_id,meta.marker]),marker:meta.marker,image,cell:null});
  }
  return entries;
}
function wsiGigatimeRows(entry) {
  const tissue=wsiExperimentTissueId();
  return (entry?.cell?.items||[]).filter(item=>(item.tissue_id==null||item.tissue_id===tissue)&&
    item.project_image_index===activeProjectIndex);
}
function wsiGigatimeSetCellVisible(entry,visible) {
  for(const candidate of wsiGigatimeEntries())if(candidate.cell){candidate.cell.visible=!!visible&&candidate.cell.metadata.gigatime.available!==false&&candidate.cell.id===entry?.cell?.id;
    const item=projectItems[activeProjectIndex];if(item)item.layers=(item.layers||[]).map(layer=>layer.id===candidate.cell.id?candidate.cell:layer);}
  if(typeof overlayFocusMode!=='undefined')overlayFocusMode='all';
  if(typeof applyOverlayFocus==='function')applyOverlayFocus();
  if(typeof buildLayerList==='function')buildLayerList();if(typeof draw==='function')draw();
  if(typeof markProjectDirty==='function')markProjectDirty('gigatime_display');
}
function wsiGigatimeCsv(entry) {
  const quote=value=>{let text=String(value??'');if(typeof value==='string'&&/^[=+\-@\t\r]/.test(text))text="'"+text;return '"'+text.replaceAll('"','""')+'"';};
  const meta=entry?.cell?.metadata?.gigatime||{};
  const rows=[['observation_id','original_id','tissue_id','marker','value','source_column','mask_name','segmentation_unit','score_semantics'],
    ...wsiGigatimeRows(entry).map(item=>[item.id,item.original_id,wsiExperimentTissueId(),entry.marker,
      item.value,meta.source_column,meta.mask_name,meta.segmentation_unit,meta.score_semantics])];
  return rows.map(row=>row.map(quote).join(',')).join('\r\n')+'\r\n';
}
function wsiGigatimeImageSettings(entry,lower,upper,opacity) {
  const maximum=Number(entry?.image?.metadata?.gigatime?.display_max);
  lower=Number(lower);upper=Number(upper);opacity=Number(opacity);
  if(!Number.isFinite(maximum)||maximum<=0||![lower,upper,opacity].every(Number.isFinite)||lower<0||upper<=lower||upper>maximum||opacity<0||opacity>1)
    throw new Error('Choose an image display range with 0 ≤ minimum < maximum ≤ '+maximum+' and opacity between 0 and 1. Exported scores remain unchanged.');
  return {contrast_min:lower/maximum,contrast_max:upper/maximum,opacity};
}
function wsiGigatimeRender() {
  const select=el('experimentGigatimeMarker');if(!select)return;
  const tissue=wsiExperimentTissueId(),entries=wsiGigatimeEntries(),sameTissue=tissue===wsiGigatimeRenderedTissue;
  const visible=entries.find(entry=>entry.cell&&entry.cell.visible!==false&&entry.cell.metadata.gigatime.available!==false);
  const previous=wsiGigatimeSelection[tissue]||(sameTissue?select.value:null)||visible?.key;
  if(!sameTissue){wsiGigatimePage=0;el('experimentGigatimeSearch').value='';}
  wsiGigatimeRenderedTissue=tissue;select.replaceChildren();
  for(const entry of entries){const option=document.createElement('option');option.value=entry.key;
    const meta=entry.cell?.metadata.gigatime;
    option.textContent=entry.marker+(meta?' · '+meta.mask_name+' ('+meta.segmentation_unit+')':' · image');select.appendChild(option);}
  if(entries.some(entry=>entry.key===previous))select.value=previous;
  select.disabled=!entries.length;select.title=select.selectedOptions[0]?.textContent||'No GigaTIME output for this tissue';
  const entry=entries.find(entry=>entry.key===select.value),allRows=wsiGigatimeRows(entry),meta=entry?.cell?.metadata.gigatime;
  if(entry)wsiGigatimeSelection[tissue]=entry.key;
  const image=el('experimentGigatimeImage'),cells=el('experimentGigatimeCells'),available=!!entry?.cell&&meta?.available!==false;
  image.disabled=!entry?.image;image.checked=!!entry?.image&&entry.image.visible!==false;
  el('experimentGigatimeBackground').checked=typeof baseImageState==='undefined'?true:baseImageState.visible!==false;
  el('experimentGigatimeResetDisplay').disabled=!entry?.image;
  cells.disabled=!available;cells.checked=available&&entry.cell.visible!==false;
  const maximum=Number(entry?.image?.metadata?.gigatime?.display_max);
  for(const id of ['experimentGigatimeMin','experimentGigatimeMax','experimentGigatimeOpacity'])el(id).disabled=!entry?.image;
  el('experimentGigatimeMin').value=String(Number(entry?.image?.contrast_min??0)*maximum||0);
  el('experimentGigatimeMax').value=String(Number(entry?.image?.contrast_max??1)*maximum||0);
  el('experimentGigatimeOpacity').value=String(Number(entry?.image?.opacity??1));
  el('experimentGigatimeDisplayRange').textContent=entry?.image?'Display units: exported image scale (0–'+maximum+'). Lower the display maximum to reveal weak signal; numeric cell scores are unchanged.':'';
  el('experimentGigatimeExport').disabled=!allRows.length||!available||wsiGigatimeExporting;
  el('experimentGigatimeSearch').disabled=!allRows.length;
  const query=el('experimentGigatimeSearch').value.trim().toLocaleLowerCase();
  const rows=query?allRows.filter(item=>String(item.original_id??item.id).toLocaleLowerCase().includes(query)):allRows;
  const pages=Math.max(1,Math.ceil(rows.length/50));wsiGigatimePage=Math.min(wsiGigatimePage,pages-1);
  const offset=wsiGigatimePage*50;
  el('experimentGigatimePrevious').disabled=wsiGigatimePage===0;
  el('experimentGigatimeNext').disabled=wsiGigatimePage>=pages-1;
  const name=projectItems[activeProjectIndex]?.label||projectItems[activeProjectIndex]?.name||tissue;
  el('experimentGigatimeTissue').textContent='Tissue: '+name+' ('+tissue+')';
  el('experimentGigatimeSummary').textContent=!entry?'No registered GigaTIME output for this tissue. Register an existing marker image in the CellPhenotyper experiment, then reopen the index.':meta?.available===false?
    'Marker image remains available. Per-object display unavailable: '+meta.reason+(meta.row_count!=null?' Declared objects: '+Number(meta.row_count).toLocaleString()+'.':''):
    meta?allRows.length.toLocaleString()+' segmented '+(meta.segmentation_unit==='nucleus'?'nuclei':'cells')+' · '+entry.marker+(meta.source_column?' · score field '+meta.source_column:'')+
      ' · colour scale '+meta.min+' to '+meta.max+' (this dataset).':
    'Marker image available. No linked single-cell quantification was registered.';
  const visibility=entry?.image?(entry.image.visible===false?' Marker image is hidden; enable Marker image above to see it.':' Marker image is shown'+(typeof baseImageState!=='undefined'&&baseImageState.visible===false?' (H&E background is hidden).':' on H&E.')):'';
  el('experimentGigatimeSummary').textContent+=visibility;
  el('experimentGigatimePage').textContent=rows.length?'Showing '+(offset+1)+'–'+Math.min(offset+50,rows.length)+' of '+rows.length.toLocaleString()+(query?' matching':'')+' objects. Export includes all '+allRows.length.toLocaleString()+' registered rows for this tissue, marker and segmentation.':query?'No object IDs match this search.':'No per-object rows to display.';
  const body=el('experimentGigatimeTable');body.replaceChildren();
  for(const item of rows.slice(offset,offset+50)){const tr=document.createElement('tr'),identity=document.createElement('td'),focus=document.createElement('button');
    focus.textContent=String(item.original_id??item.id);focus.title='Select this segmented object on the tissue';
    focus.onclick=()=>{const index=layers.findIndex(layer=>layer.id===entry.cell.id);if(index<0)return;
      if(typeof selectLayerObject==='function')selectLayerObject(index,entry.cell.items.indexOf(item),true);
      if(typeof setMode==='function')setMode('select');if(typeof draw==='function')draw();};
    identity.appendChild(focus);tr.appendChild(identity);
    for(const value of [item.value,item.x,item.y]){const td=document.createElement('td');td.textContent=String(value??'');tr.appendChild(td);}body.appendChild(tr);}
}
function wsiGigatimeBindWorkbench() {
  if(!wsiExperimentConfig()||el('gigatimeWorkbenchWindow'))return;
  const hasSources=(typeof channelSources!=='undefined'&&channelSources.some(src=>src.metadata?.gigatime))||
    (layers||[]).some(layer=>layer.metadata?.gigatime)||projectItems.some(item=>(item.layers||[]).some(layer=>layer.metadata?.gigatime));
  if(!hasSources)return;
  wsiGigatimeCaptureSources();
  const button=document.createElement('button');button.id='openGigatimeWorkbench';button.type='button';button.textContent='GigaTIME markers';
  const panel=document.createElement('section');panel.id='gigatimeWorkbenchWindow';panel.hidden=true;
  panel.innerHTML='<div class="wsiWorkspaceHead"><h2 id="gigatimeWorkbenchTitle" tabindex="-1">Predicted GigaTIME markers</h2><button id="closeGigatimeWorkbench">Close</button></div>'+
    '<p id="experimentGigatimeTissue"></p><p>Virtual marker scores are uncalibrated predictions from H&amp;E. They are not measured protein abundance or calibrated probabilities.</p>'+
    '<label style="display:block">Marker / segmentation <select style="display:block;width:100%" id="experimentGigatimeMarker" aria-label="GigaTIME marker and segmentation"></select></label>'+
    '<p><label><input type="checkbox" id="experimentGigatimeImage"> Marker image (mIHC layer)</label> '+
    '<label><input type="checkbox" id="experimentGigatimeCells"> Per-cell scores</label> '+
    '<label><input type="checkbox" id="experimentGigatimeBackground"> H&amp;E background</label></p>'+
    '<p>Combine marker images using their colour and opacity controls in Layers. Per-cell display shows one marker at a time; nucleus masks retain their nucleus label.</p>'+
    '<fieldset><legend>Marker image display</legend><label>Display minimum <input id="experimentGigatimeMin" type="number" min="0" step="any"></label> '+
    '<label>Display maximum <input id="experimentGigatimeMax" type="number" min="0" step="any"></label> '+
    '<label>Image opacity <input id="experimentGigatimeOpacity" type="range" min="0" max="1" step="0.05"></label>'+
    '<button id="experimentGigatimeResetDisplay">Reset image display</button><p id="experimentGigatimeDisplayRange"></p><p id="experimentGigatimeDisplayStatus" role="status"></p></fieldset>'+
    '<p id="experimentGigatimeSummary" role="status"></p><button id="experimentGigatimeExport">Export all cell scores for this marker</button><p id="experimentGigatimeExportStatus" role="status"></p>'+
    '<label>Find object ID <input id="experimentGigatimeSearch" type="search" placeholder="Search object IDs"></label>'+
    '<p id="experimentGigatimePage" role="status"></p><div><button id="experimentGigatimePrevious">Previous 50</button> <button id="experimentGigatimeNext">Next 50</button></div>'+
    '<div class="wsiTableScroll"><table style="width:100%;text-align:left"><thead><tr><th>Object ID</th><th>Predicted score</th><th>X</th><th>Y</th></tr></thead><tbody id="experimentGigatimeTable"></tbody></table></div>';
  document.body.appendChild(panel);
  const setOpen=wsiWorkspaceBind(button,panel,'gigatimeWorkbenchTitle',()=>{},wsiGigatimeRender);
  el('closeGigatimeWorkbench').onclick=()=>setOpen(false);
  const selected=()=>wsiGigatimeEntries().find(entry=>entry.key===el('experimentGigatimeMarker').value);
  el('experimentGigatimeMarker').onchange=()=>{
    wsiGigatimeSelection[wsiExperimentTissueId()]=el('experimentGigatimeMarker').value;wsiGigatimePage=0;
    el('experimentGigatimeSearch').value='';
    if(wsiGigatimeEntries().some(entry=>entry.cell&&entry.cell.visible!==false))wsiGigatimeSetCellVisible(selected(),true);
    if(typeof markProjectDirty==='function')markProjectDirty('gigatime_marker');wsiGigatimeRender();};
  el('experimentGigatimeSearch').oninput=()=>{wsiGigatimePage=0;wsiGigatimeRender();};
  el('experimentGigatimePrevious').onclick=()=>{wsiGigatimePage=Math.max(0,wsiGigatimePage-1);wsiGigatimeRender();};
  el('experimentGigatimeNext').onclick=()=>{wsiGigatimePage++;wsiGigatimeRender();};
  el('experimentGigatimeImage').onchange=()=>{const entry=selected();if(!entry?.image)return;
    setChannelSettings(entry.image.id,{visible:el('experimentGigatimeImage').checked});
    if(typeof markProjectDirty==='function')markProjectDirty('gigatime_display');wsiGigatimeRender();};
  el('experimentGigatimeBackground').onchange=()=>{if(typeof setBaseImageVisible==='function')setBaseImageVisible(el('experimentGigatimeBackground').checked);};
  const applyImageDisplay=()=>{
    const entry=selected();if(!entry?.image)return;
    try{
      const settings=wsiGigatimeImageSettings(entry,el('experimentGigatimeMin').value,el('experimentGigatimeMax').value,el('experimentGigatimeOpacity').value);
      setChannelSettings(entry.image.id,settings);el('experimentGigatimeDisplayStatus').textContent='Image display updated. Exported cell scores are unchanged.';
      if(typeof markProjectDirty==='function')markProjectDirty('gigatime_contrast');
    }catch(error){el('experimentGigatimeDisplayStatus').textContent=error.message;}
  };
  for(const id of ['experimentGigatimeMin','experimentGigatimeMax','experimentGigatimeOpacity'])el(id).onchange=applyImageDisplay;
  el('experimentGigatimeResetDisplay').onclick=()=>{const entry=selected();if(!entry?.image)return;setChannelSettings(entry.image.id,{contrast_min:0,contrast_max:1,gain:1,opacity:.65});wsiGigatimeRender();};
  el('experimentGigatimeCells').onchange=()=>{const entry=selected();if(!entry?.cell||entry.cell.metadata.gigatime.available===false)return;
    wsiGigatimeSetCellVisible(entry,el('experimentGigatimeCells').checked);wsiGigatimeRender();};
  el('experimentGigatimeExport').onclick=async()=>{const entry=selected();if(!entry?.cell||entry.cell.metadata.gigatime.available===false||wsiGigatimeExporting)return;
    const tissue=wsiExperimentTissueId(),safe=value=>String(value).replace(/[^a-zA-Z0-9_.-]/g,'_').slice(0,80);
    wsiGigatimeExporting=true;el('experimentGigatimeExportStatus').textContent='Preparing cell score export…';wsiGigatimeRender();
    try{await downloadBlob(new Blob([wsiGigatimeCsv(entry)],{type:'text/csv;charset=utf-8'}),[tissue,entry.marker,entry.cell.metadata.gigatime.mask_name,'cell-scores'].map(safe).join('-')+'.csv');
      el('experimentGigatimeExportStatus').textContent='Exported all registered rows for '+tissue+' · '+entry.marker+'.';
    }catch(error){el('experimentGigatimeExportStatus').textContent='Export failed: '+error.message;}
    finally{wsiGigatimeExporting=false;wsiGigatimeRender();}};
  const originalSnapshot=projectBrowserSnapshot;
  projectBrowserSnapshot=function(...args){return {...originalSnapshot(...args),gigatime_workbench:{selected_by_tissue:{...wsiGigatimeSelection}}};};
  const originalPayloads=applyProjectPayloads;
  applyProjectPayloads=function(...args){const result=originalPayloads(...args);wsiGigatimeRender();return result;};
  const originalRestore=restoreBrowserProject;
  restoreBrowserProject=function(snapshot,...args){wsiExperimentValidateSnapshot(snapshot);
    const result=originalRestore(wsiGigatimePrepareSnapshot(snapshot),...args);wsiGigatimeRestoreDisplay(snapshot);return result;};
  const originalLiveRestore=restoreBrowserProjectInLiveSession;
  restoreBrowserProjectInLiveSession=async function(snapshot,...args){const result=await originalLiveRestore(snapshot,...args);
    wsiGigatimeRestoreDisplay(snapshot);return result;};
  const originalCommand=handleViewerCommand;
  handleViewerCommand=function(command){if(command?.type!=='restore_project_state')return originalCommand(command);
    const checked={...command,payload:wsiGigatimePrepareSnapshot(command.payload||{})};
    const result=originalCommand(checked);wsiGigatimeRestoreDisplay(command.payload||{});return result;};
  wsiGigatimeRender();
}

if (typeof document !== 'undefined') setTimeout(()=>{wsiExperimentBindWorkbench();wsiGigatimeBindWorkbench();wsiExperimentBindGuide();},0);
