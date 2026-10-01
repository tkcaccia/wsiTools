let annotationRegistrationDrag = null;
let annotationRegistrationPanelDrag = null;
let annotationRegistrationOriginals = new Map();
let annotationRegistrationKey = '';

function annotationRegistrationPanel() { return el('annotationRegistrationWindow'); }
function annotationRegistrationScope() { return el('annotationRegistrationScope')?.value || 'selected'; }
function annotationRegistrationEligible(roi) {
  return !!(roi && isDrawable(roi) && editableRoi(roi) && tissueAnnotationRoi(roi) && !roiIsCellLike(roi));
}

function annotationRegistrationPromoteLayerItem(layer, item) {
  if (!layer || !item || !annotationRegistrationEligible(item)) return -1;
  const groups = denseLayerEditableGroups(item);
  if (!groups.length) return -1;
  const roi = Object.assign({}, item, {
    rings: groups[0], add_groups: groups.slice(1), add_rings: [], subtract_rings: [],
    raw_coordinates: null, dense_static_geometry: false, dense_geometry: false,
    properties: clonePlain(item.properties || {})
  });
  refreshRoiGeometry(roi);
  const sourceId = String(layer.metadata?.source_id || layer.id || '').replace(/^dense_geojson_viewport_/, '');
  denseGeojsonPromotedIds.add(denseGeojsonPromotionKey(sourceId, item.id));
  const itemIndex = layer.items.indexOf(item);
  if (itemIndex >= 0) layer.items.splice(itemIndex, 1);
  layer.count = layer.items.length;
  const placeholder = rois.findIndex(other => other !== item &&
    String(other.id || '') === String(item.id || '') && !isDrawable(other));
  if (placeholder >= 0) { rois[placeholder] = roi; return placeholder; }
  rois.push(roi);
  return rois.length - 1;
}

function annotationRegistrationPromoteStaticLayers() {
  let added = 0;
  for (const layer of layers) {
    if (!layer?.metadata?.static_source || !Array.isArray(layer.items)) continue;
    for (let i = layer.items.length - 1; i >= 0; i--) {
      const item = layer.items[i];
      if (!annotationRegistrationEligible(item) ||
          typeof layerItemMatchesActiveProject === 'function' && !layerItemMatchesActiveProject(item, layer)) continue;
      if (annotationRegistrationPromoteLayerItem(layer, item) >= 0) added++;
    }
  }
  if (added) { buildLayerList(); buildRoiList(); updateButtons(); }
  return added;
}

function annotationRegistrationTargets(promote = false) {
  if (annotationRegistrationScope() === 'all') {
    if (promote) annotationRegistrationPromoteStaticLayers();
    const current = rois.filter(annotationRegistrationEligible);
    if (promote) return current;
    for (const layer of layers) {
      if (!layer?.metadata?.static_source) continue;
      current.push(...(layer.items || []).filter(item => annotationRegistrationEligible(item) &&
        (typeof layerItemMatchesActiveProject !== 'function' || layerItemMatchesActiveProject(item, layer))));
    }
    return current;
  }
  if (selectedRoi >= 0 && annotationRegistrationEligible(rois[selectedRoi])) return [rois[selectedRoi]];
  const selected = typeof selectedLayerObject === 'function' ? selectedLayerObject() : null;
  if (selected?.item && annotationRegistrationEligible(selected.item)) {
    if (!promote) return [selected.item];
    const index = annotationRegistrationPromoteLayerItem(selected.layer, selected.item);
    if (index < 0) return [];
    clearSelectedLayerObject(false);
    selectAnnotation(index, false);
    buildLayerList(); buildRoiList(); updateButtons(); draw();
    return [rois[index]];
  }
  return [];
}

function annotationRegistrationStatus(message) {
  const summary = el('annotationRegistrationSummary');
  if (summary) summary.textContent = message;
}

function updateAnnotationRegistrationControls() {
  const targets = annotationRegistrationTargets(false);
  const subtitle = el('annotationRegistrationSubtitle');
  const scope = annotationRegistrationScope();
  if (subtitle) subtitle.textContent = targets.length ?
    (scope === 'all' ? targets.length + ' tissue regions on this slide' : 'Selected: ' + (targets[0].name || targets[0].label || 'ROI')) :
    (scope === 'all' ? 'No editable tissue annotations on this slide' : 'Select a tissue annotation first');
  const drag = el('annotationRegistrationDrag');
  if (drag) drag.classList.toggle('active', mode === 'annotation_register');
}

function annotationRegistrationStep(id, fallback, minimum, maximum) {
  const value = Number(el(id)?.value);
  return Number.isFinite(value) ? Math.min(maximum, Math.max(minimum, value)) : fallback;
}

function annotationRegistrationBounds(targets) {
  let bounds = null;
  for (const roi of targets) {
    const b = roiBounds(roi);
    if (b) bounds = bounds ? unionBounds(bounds, b) : b;
  }
  return bounds;
}

function annotationRegistrationGeometry(roi, fn) {
  const groups = positiveRingGroups(roi);
  const holes = subtractRings(roi);
  const mapRing = ring => ring.map(point => {
    const next = fn(Number(point.x), Number(point.y));
    return { x: Math.round(next.x * 1000) / 1000, y: Math.round(next.y * 1000) / 1000 };
  });
  roi.rings = groups[0]?.map(mapRing) || [];
  roi.add_groups = groups.slice(1).map(group => group.map(mapRing));
  roi.add_rings = [];
  roi.subtract_rings = holes.map(mapRing);
  refreshRoiGeometry(roi);
}

function annotationRegistrationRemember(roi) {
  const id = wsiRoiId(roi);
  if (!annotationRegistrationOriginals.has(id)) {
    annotationRegistrationOriginals.set(id, {
      rings: JSON.parse(JSON.stringify(roi.rings || [])),
      add_groups: JSON.parse(JSON.stringify(roi.add_groups || [])),
      add_rings: JSON.parse(JSON.stringify(roi.add_rings || [])),
      subtract_rings: JSON.parse(JSON.stringify(roi.subtract_rings || []))
    });
  }
}

function annotationRegistrationCommit(action, transform, onlyTargets = null) {
  if (annotationRegistrationKey && annotationRegistrationKey !== wsiProjectKey()) {
    annotationRegistrationStatus('This registration window belongs to another tissue. Reopen it for the current slide.');
    return false;
  }
  const targets = onlyTargets || annotationRegistrationTargets(true);
  if (!targets.length) {
    notify('Select an unlocked tissue annotation first, or choose All tissue annotations', 'warning', 3600);
    updateAnnotationRegistrationControls();
    return false;
  }
  const transaction = {
    wsi_delta: true, key: wsiProjectKey(), before: targets.map(roi => wsiEditEntry(roi)),
    selectedBefore: selectedRoi >= 0 ? wsiRoiId(rois[selectedRoi]) : null, countBefore: newRoiCount
  };
  targets.forEach(annotationRegistrationRemember);
  try {
    transform(targets);
  } catch (error) {
    annotationRegistrationStatus('Registration failed: ' + error.message);
    notify('Annotation registration failed: ' + error.message, 'error', 5200);
    return false;
  }
  const delta = wsiCompleteEditTransaction(transaction, null, 'roi_registered');
  wsiRefreshEditedAnnotations(delta);
  updateButtons();
  draw();
  if (typeof saveActiveProjectAnnotations === 'function') saveActiveProjectAnnotations();
  recordAnnotationHistory('roi_registered', { action, count: targets.length, ids: targets.map(wsiRoiId) });
  scheduleViewerStateSync('roi_updated', { action: 'registration', transform: action, count: targets.length, ids: targets.map(wsiRoiId) });
  annotationRegistrationStatus(action + ' applied to ' + targets.length + ' tissue annotation' + (targets.length === 1 ? '' : 's') + '. Save annotations or the project to keep the alignment.');
  updateAnnotationRegistrationControls();
  return true;
}

function annotationRegistrationMove(dx, dy) {
  if (!dx && !dy) return false;
  return annotationRegistrationCommit('Move', targets => targets.forEach(roi =>
    annotationRegistrationGeometry(roi, (x, y) => ({ x: x + dx, y: y + dy }))));
}

function annotationRegistrationAffine(action, matrix) {
  return annotationRegistrationCommit(action, targets => {
    const b = annotationRegistrationBounds(targets);
    if (!b) throw new Error('No polygon geometry is available.');
    const cx = (b.xmin + b.xmax) / 2, cy = (b.ymin + b.ymax) / 2;
    targets.forEach(roi => annotationRegistrationGeometry(roi, (x, y) => {
      const px = x - cx, py = y - cy;
      return { x: cx + matrix[0] * px + matrix[1] * py, y: cy + matrix[2] * px + matrix[3] * py };
    }));
  });
}

function annotationRegistrationScale(axis, up) {
  const step = annotationRegistrationStep('annotationRegistrationScaleStep', 1.05, 1.001, 10);
  const factor = up ? step : 1 / step;
  return annotationRegistrationAffine('Scale ' + axis, [axis === 'y' ? 1 : factor, 0, 0, axis === 'x' ? 1 : factor]);
}

function annotationRegistrationRotate(degrees) {
  const radians = degrees * Math.PI / 180, c = Math.cos(radians), s = Math.sin(radians);
  return annotationRegistrationAffine('Rotate ' + degrees + ' degrees', [c, -s, s, c]);
}

function annotationRegistrationReset() {
  const targets = annotationRegistrationTargets(false).filter(roi =>
    rois.includes(roi) && annotationRegistrationOriginals.has(wsiRoiId(roi)));
  if (!targets.length) { notify('No registration changes to reset', 'info', 2200); return; }
  const ids = new Set(targets.map(wsiRoiId));
  annotationRegistrationCommit('Reset', current => current.filter(roi => ids.has(wsiRoiId(roi))).forEach(roi => {
    const original = annotationRegistrationOriginals.get(wsiRoiId(roi));
    roi.rings = JSON.parse(JSON.stringify(original.rings));
    roi.add_groups = JSON.parse(JSON.stringify(original.add_groups));
    roi.add_rings = JSON.parse(JSON.stringify(original.add_rings));
    roi.subtract_rings = JSON.parse(JSON.stringify(original.subtract_rings));
    refreshRoiGeometry(roi);
  }), targets);
}

function openAnnotationRegistrationWindow() {
  const panel = annotationRegistrationPanel();
  if (!panel) return;
  const key = wsiProjectKey();
  if (annotationRegistrationKey !== key || !panel.classList.contains('open')) {
    annotationRegistrationOriginals = new Map();
    annotationRegistrationKey = key;
  }
  panel.classList.add('open');
  panel.setAttribute('aria-hidden', 'false');
  updateAnnotationRegistrationControls();
  annotationRegistrationStatus('Select a region to align it independently, or choose All tissue annotations. Changes use slide-pixel coordinates.');
}

function closeAnnotationRegistrationWindow() {
  const panel = annotationRegistrationPanel();
  if (panel) { panel.classList.remove('open'); panel.setAttribute('aria-hidden', 'true'); }
  annotationRegistrationDrag = null;
  updateAnnotationRegistrationGhost();
  if (mode === 'annotation_register') setMode('pan');
  updateAnnotationRegistrationControls();
}

function annotationRegistrationGhost() {
  let ghost = el('annotationRegistrationGhost');
  if (!ghost) {
    ghost = document.createElement('div');
    ghost.id = 'annotationRegistrationGhost';
    document.body.appendChild(ghost);
  }
  return ghost;
}

function updateAnnotationRegistrationGhost() {
  const drag = annotationRegistrationDrag, b = drag?.bounds, ghost = el('annotationRegistrationGhost');
  if (!drag || !b) { if (ghost) ghost.remove(); return; }
  const rect = drag.pane ? drag.pane.overlay.getBoundingClientRect() : canvas.getBoundingClientRect();
  const map = drag.pane ? p => multiViewSlideToCanvas(p, drag.pane) : slideToCanvas;
  const points = [
    { x: b.xmin, y: b.ymin }, { x: b.xmax, y: b.ymin },
    { x: b.xmax, y: b.ymax }, { x: b.xmin, y: b.ymax }
  ].map(p => map({ x: p.x + drag.dx, y: p.y + drag.dy }));
  const xs = points.map(p => p.x), ys = points.map(p => p.y);
  const left = Math.min(...xs), top = Math.min(...ys), right = Math.max(...xs), bottom = Math.max(...ys);
  const display = annotationRegistrationGhost();
  display.style.left = rect.left + left + 'px';
  display.style.top = rect.top + top + 'px';
  display.style.width = Math.max(2, right - left) + 'px';
  display.style.height = Math.max(2, bottom - top) + 'px';
  display.title = 'Annotation registration preview';
}

function startAnnotationRegistrationPointer(_event, point, pane = null) {
  if (!annotationRegistrationPanel()?.classList.contains('open')) return false;
  const targets = annotationRegistrationTargets(true);
  if (!targets.length) { notify('Select a tissue annotation to drag', 'warning', 2600); return false; }
  annotationRegistrationDrag = { start: point, dx: 0, dy: 0, bounds: annotationRegistrationBounds(targets), pane };
  updateAnnotationRegistrationGhost();
  return true;
}

function moveAnnotationRegistrationPointer(point) {
  if (!annotationRegistrationDrag) return false;
  annotationRegistrationDrag.dx = point.x - annotationRegistrationDrag.start.x;
  annotationRegistrationDrag.dy = point.y - annotationRegistrationDrag.start.y;
  updateAnnotationRegistrationGhost();
  return true;
}

function finishAnnotationRegistrationPointer(point) {
  if (!annotationRegistrationDrag) return false;
  const drag = annotationRegistrationDrag;
  annotationRegistrationDrag = null;
  updateAnnotationRegistrationGhost();
  const dx = point.x - drag.start.x, dy = point.y - drag.start.y;
  if (Math.hypot(dx, dy) > 0.5) annotationRegistrationMove(dx, dy);
  else draw();
  return true;
}

function drawAnnotationRegistrationOverlay() {
  const drag = annotationRegistrationDrag, b = drag?.bounds;
  if (!b) return;
  const corners = [
    { x: b.xmin, y: b.ymin }, { x: b.xmax, y: b.ymin },
    { x: b.xmax, y: b.ymax }, { x: b.xmin, y: b.ymax }
  ];
  ctx.save();
  ctx.beginPath();
  corners.forEach((corner, i) => {
    const p = slideToCanvas({ x: corner.x + drag.dx, y: corner.y + drag.dy });
    if (i) ctx.lineTo(p.x, p.y); else ctx.moveTo(p.x, p.y);
  });
  ctx.closePath();
  ctx.setLineDash([7, 5]);
  ctx.strokeStyle = '#facc15';
  ctx.lineWidth = 2;
  ctx.stroke();
  ctx.restore();
}

function bindAnnotationRegistrationWindow() {
  const panel = annotationRegistrationPanel(), open = el('annotationRegistrationOpen');
  if (open) open.onclick = event => {
    openAnnotationRegistrationWindow();
    if (typeof closeMenuAfterToolAction === 'function') closeMenuAfterToolAction(event.currentTarget);
  };
  if (!panel) return;
  const bind = (suffix, fn) => { const button = el('annotationRegistration' + suffix); if (button) button.onclick = fn; };
  bind('Close', closeAnnotationRegistrationWindow);
  bind('MoveUp', () => annotationRegistrationMove(0, -annotationRegistrationStep('annotationRegistrationMoveStep', 50, 1, 1e6)));
  bind('MoveDown', () => annotationRegistrationMove(0, annotationRegistrationStep('annotationRegistrationMoveStep', 50, 1, 1e6)));
  bind('MoveLeft', () => annotationRegistrationMove(-annotationRegistrationStep('annotationRegistrationMoveStep', 50, 1, 1e6), 0));
  bind('MoveRight', () => annotationRegistrationMove(annotationRegistrationStep('annotationRegistrationMoveStep', 50, 1, 1e6), 0));
  bind('Drag', () => { setMode('annotation_register'); updateAnnotationRegistrationControls(); annotationRegistrationStatus('Drag the annotation across the tissue, then release to commit.'); });
  bind('ScaleBothUp', () => annotationRegistrationScale('both', true));
  bind('ScaleBothDown', () => annotationRegistrationScale('both', false));
  bind('ScaleXUp', () => annotationRegistrationScale('x', true));
  bind('ScaleXDown', () => annotationRegistrationScale('x', false));
  bind('ScaleYUp', () => annotationRegistrationScale('y', true));
  bind('ScaleYDown', () => annotationRegistrationScale('y', false));
  bind('FlipH', () => annotationRegistrationAffine('Flip horizontally', [-1, 0, 0, 1]));
  bind('FlipV', () => annotationRegistrationAffine('Flip vertically', [1, 0, 0, -1]));
  bind('RotateCcw', () => annotationRegistrationRotate(-90));
  bind('RotateCw', () => annotationRegistrationRotate(90));
  bind('RotateSmallCcw', () => annotationRegistrationRotate(-annotationRegistrationStep('annotationRegistrationRotationStep', 5, 0.1, 45)));
  bind('RotateSmallCw', () => annotationRegistrationRotate(annotationRegistrationStep('annotationRegistrationRotationStep', 5, 0.1, 45)));
  bind('Undo', restoreAnnotationUndo);
  bind('Redo', restoreAnnotationRedo);
  bind('Reset', annotationRegistrationReset);
  bind('Save', () => saveGeojson());
  el('annotationRegistrationScope').onchange = updateAnnotationRegistrationControls;
  const head = panel.querySelector('.seuratRegistrationHead');
  if (head) {
    head.addEventListener('mousedown', event => {
      if (event.button !== 0 || event.target.closest('button,input,select,textarea')) return;
      const rect = panel.getBoundingClientRect();
      panel.style.left = rect.left + 'px'; panel.style.top = rect.top + 'px';
      panel.style.right = 'auto'; panel.style.bottom = 'auto';
      annotationRegistrationPanelDrag = { dx: event.clientX - rect.left, dy: event.clientY - rect.top };
      panel.classList.add('moving');
      event.preventDefault();
    });
    window.addEventListener('mousemove', event => {
      if (!annotationRegistrationPanelDrag) return;
      const x = Math.max(6, Math.min(innerWidth - panel.offsetWidth - 6, event.clientX - annotationRegistrationPanelDrag.dx));
      const y = Math.max(6, Math.min(innerHeight - panel.offsetHeight - 6, event.clientY - annotationRegistrationPanelDrag.dy));
      panel.style.left = x + 'px'; panel.style.top = y + 'px';
    });
    window.addEventListener('mouseup', () => { annotationRegistrationPanelDrag = null; panel.classList.remove('moving'); });
  }
  window.addEventListener('mousedown', event => {
    if (mode !== 'annotation_register' || event.button !== 0) return;
    const pane = typeof multiViewPanes !== 'undefined' ? multiViewPanes.find(item => item.overlay === event.target || item.overlay?.contains(event.target)) : null;
    if (pane) {
      const index = multiViewPanes.indexOf(pane);
      if (pane.blank) return;
      multiViewFocusPane(index, true);
      if (!multiViewEnsureEditingContext(pane, index)) return;
    } else if (event.target !== canvas) return;
    const point = pane ? multiViewPointerToSlide(event, pane) : pointerToSlide(event);
    if (!startAnnotationRegistrationPointer(event, point, pane)) return;
    event.preventDefault();
    event.stopImmediatePropagation();
  }, true);
  window.addEventListener('mousemove', event => {
    const drag = annotationRegistrationDrag;
    if (!drag) return;
    const point = drag.pane ? multiViewPointerToSlide(event, drag.pane) : pointerToSlide(event);
    moveAnnotationRegistrationPointer(point);
    event.preventDefault();
    event.stopImmediatePropagation();
  }, true);
  window.addEventListener('mouseup', event => {
    const drag = annotationRegistrationDrag;
    if (!drag) return;
    const point = drag.pane ? multiViewPointerToSlide(event, drag.pane) : pointerToSlide(event);
    finishAnnotationRegistrationPointer(point);
    event.preventDefault();
    event.stopImmediatePropagation();
  }, true);
  updateAnnotationRegistrationControls();
}

setTimeout(bindAnnotationRegistrationWindow, 0);
