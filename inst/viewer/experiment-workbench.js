/* CellPhenotyper experiments use cached, typed observations. No encoder runs here. */
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

function wsiExperimentBindWorkbench() {
  if (!wsiExperimentConfig() || el('experimentWorkbench')) return;
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
  applyProjectPreview = function(...args) { const result=originalPreview(...args);wsiExperimentRequest++;wsiExperimentRefreshControls();return result; };
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
}

// The scientific index owns GigaTIME sources. This panel only changes their
// display settings and exports already-quantified values; it runs no model.
let wsiGigatimeFreshSources=null;
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
  for(const candidate of wsiGigatimeEntries())if(candidate.cell){candidate.cell.visible=!!visible&&candidate.cell.id===entry?.cell?.id;
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
function wsiGigatimeRender() {
  const select=el('experimentGigatimeMarker');if(!select)return;
  const previous=select.value,entries=wsiGigatimeEntries();select.replaceChildren();
  for(const entry of entries){const option=document.createElement('option');option.value=entry.key;
    const meta=entry.cell?.metadata.gigatime;
    option.textContent=entry.marker+(meta?' · '+meta.mask_name+' ('+meta.segmentation_unit+')':' · image');select.appendChild(option);}
  if(entries.some(entry=>entry.key===previous))select.value=previous;
  const entry=entries.find(entry=>entry.key===select.value),rows=wsiGigatimeRows(entry),meta=entry?.cell?.metadata.gigatime;
  const image=el('experimentGigatimeImage'),cells=el('experimentGigatimeCells');
  image.disabled=!entry?.image;image.checked=!!entry?.image&&entry.image.visible!==false;
  cells.disabled=!entry?.cell;cells.checked=!!entry?.cell&&entry.cell.visible!==false;
  el('experimentGigatimeExport').disabled=!rows.length;
  el('experimentGigatimeSummary').textContent=!entry?'No registered GigaTIME output for this tissue.':meta?
    rows.length+' segmented '+(meta.segmentation_unit==='nucleus'?'nuclei':'cells')+' · '+entry.marker+(meta.source_column?' · score field '+meta.source_column:'')+
      ' · colour scale '+meta.min+' to '+meta.max+' (this dataset) · showing '+Math.min(50,rows.length)+' rows.':
    'Marker image available. No linked single-cell quantification was registered.';
  const body=el('experimentGigatimeTable');body.replaceChildren();
  for(const item of rows.slice(0,50)){const tr=document.createElement('tr'),identity=document.createElement('td'),focus=document.createElement('button');
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
  button.style.cssText='position:fixed;right:18px;top:88px;z-index:1100';document.body.appendChild(button);
  const panel=document.createElement('section');panel.id='gigatimeWorkbenchWindow';panel.hidden=true;
  panel.style.cssText='position:fixed;right:16px;top:126px;width:min(600px,90vw);max-height:72vh;overflow:auto;z-index:1490;background:#172033;color:#e2e8f0;padding:12px;border:1px solid #64748b;border-radius:10px';
  panel.innerHTML='<div style="display:flex;justify-content:space-between"><strong>Predicted GigaTIME markers</strong><button id="closeGigatimeWorkbench">Close</button></div>'+
    '<p>Virtual marker scores are uncalibrated predictions from H&amp;E. They are not measured protein abundance or calibrated probabilities.</p>'+
    '<label>Marker / segmentation <select id="experimentGigatimeMarker" aria-label="GigaTIME marker and segmentation"></select></label>'+
    '<p><label><input type="checkbox" id="experimentGigatimeImage"> Marker image (mIHC layer)</label> '+
    '<label><input type="checkbox" id="experimentGigatimeCells"> Per-cell scores</label></p>'+
    '<p>Combine marker images using their colour and opacity controls in Layers. Per-cell display shows one marker at a time; nucleus masks retain their nucleus label.</p>'+
    '<p id="experimentGigatimeSummary" role="status"></p><button id="experimentGigatimeExport">Export this marker’s cell scores</button>'+
    '<table style="width:100%;text-align:left"><thead><tr><th>Object ID</th><th>Predicted score</th><th>X</th><th>Y</th></tr></thead><tbody id="experimentGigatimeTable"></tbody></table>';
  document.body.appendChild(panel);
  button.onclick=()=>{panel.hidden=!panel.hidden;wsiGigatimeRender();};el('closeGigatimeWorkbench').onclick=()=>{panel.hidden=true;};
  const selected=()=>wsiGigatimeEntries().find(entry=>entry.key===el('experimentGigatimeMarker').value);
  el('experimentGigatimeMarker').onchange=()=>{
    if(wsiGigatimeEntries().some(entry=>entry.cell&&entry.cell.visible!==false))wsiGigatimeSetCellVisible(selected(),true);
    wsiGigatimeRender();};
  el('experimentGigatimeImage').onchange=()=>{const entry=selected();if(!entry?.image)return;
    setChannelSettings(entry.image.id,{visible:el('experimentGigatimeImage').checked});
    if(typeof markProjectDirty==='function')markProjectDirty('gigatime_display');wsiGigatimeRender();};
  el('experimentGigatimeCells').onchange=()=>{const entry=selected();if(!entry?.cell)return;
    wsiGigatimeSetCellVisible(entry,el('experimentGigatimeCells').checked);wsiGigatimeRender();};
  el('experimentGigatimeExport').onclick=async()=>{const entry=selected();if(!entry?.cell)return;
    await downloadBlob(new Blob([wsiGigatimeCsv(entry)],{type:'text/csv;charset=utf-8'}),'gigatime-cell-scores.csv');};
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

if (typeof document !== 'undefined') setTimeout(()=>{wsiExperimentBindWorkbench();wsiGigatimeBindWorkbench();},0);
