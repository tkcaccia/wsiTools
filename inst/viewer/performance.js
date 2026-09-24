const wsiGeometryCache = new WeakMap();
const wsiReusableGeometryParts = new WeakMap();
const wsiSnapshotCache = new WeakMap();
const wsiWorkerSourceVersions = new Map();
const wsiGeometryJobs = new Map();
let wsiGeometryWorker = null, wsiGeometryJobId = 0, wsiEditPromise = null;
let wsiBrushPriorityClaim = false, wsiBrushPriorityDown = false;
let wsiEditGeneration = 0;
let wsiInteractionCanvas = null, wsiInteractionQueued = false, wsiRoiSequence = 0;
let wsiAnnotationEpoch = 0;
let wsiDirtyPanes = new Set(), wsiAllPanesDirty = false;
const wsiRuntimeMetrics = { cursor_frames: 0, cursor_max_ms: 0, geometry_jobs: 0,
  geometry_worker_ms: 0, geometry_commit_ms: 0, path_builds: 0, pane_draws: 0,
  geometry_kernel: 'initializing', geometry_cache_bytes: 0, geometry_transfer_bytes: 0 };
let wsiKernelWarningShown = false;

// Shared viewer controls may request a queued redraw even in thumbnail mode,
// where the OpenSeadragon-specific requestDraw() function is not installed.
if (typeof window !== 'undefined' && typeof window.requestDraw !== 'function') {
  window.requestDraw = function() {
    if (typeof draw === 'function') draw();
  };
}

function wsiRequestDraw() {
  if (typeof requestDraw === 'function') requestDraw();
  else if (typeof draw === 'function') draw();
}

function wsiProjectKey() {
  return typeof projectAnnotationKey === 'function' ? projectAnnotationKey() : 'slide';
}

function wsiRoiId(roi) {
  if (!roi.id) roi.id = 'annotation_' + Date.now().toString(36) + '_' + (++wsiRoiSequence);
  return String(roi.id);
}

function wsiInvalidateGeometry(roi) {
  if (!roi) return;
  Object.defineProperty(roi, '_wsiRevision', { value: (roi._wsiRevision || 0) + 1,
    writable: true, configurable: true, enumerable: false });
  wsiGeometryCache.delete(roi);
  wsiReusableGeometryParts.delete(roi);
  wsiAnnotationEpoch++;
}

function wsiGeometrySignature(roi) {
  return [roi._wsiRevision || 0, roi.rings, roi.add_groups, roi.add_rings,
    roi.subtract_rings, roi.raw_coordinates, roi.subtract_rings && roi.subtract_rings.length,
    roi.dense_static_geometry && !denseStaticUsesFullResolution(roi) ? denseStaticDisplayCap(roi) : 0];
}

function wsiSameSignature(a, b) {
  return a && b && a.length === b.length && a.every((v, i) => v === b[i]);
}

function wsiCachedGeometry(roi) {
  const signature = wsiGeometrySignature(roi), existing = wsiGeometryCache.get(roi);
  if (existing && wsiSameSignature(existing.signature, signature)) return existing;
  const holes = subtractRings(roi), holeBounds = holes.map(boundsFromRing);
  const groups = roi.dense_static_geometry ? denseStaticRoiDrawGroups(roi) : positiveRingGroups(roi).map(rings => {
    if (!holes.length) return { rings, holes: [] };
    const bounds = boundsFromRings(rings);
    return { rings, holes: holes.map((hole, i) => boundsOverlap(bounds, holeBounds[i])
      ? clippedHoleForGroup(hole, rings) : null).filter(Boolean) };
  });
  const reusable = wsiReusableGeometryParts.get(roi);
  const data = { signature, groups, parts: groups.map(group => {
    const old = reusable && reusable.get(group.rings[0]);
    if (old && wsiSameSignature(old.group.rings, group.rings) && wsiSameSignature(old.group.holes, group.holes)) return old;
    return { group, bounds: boundsFromRings(group.rings), path: null };
  }), coordinates: null, segmentTree: null };
  wsiReusableGeometryParts.delete(roi);
  wsiGeometryCache.set(roi, data);
  return data;
}

function wsiRoiCoordinates(roi) {
  const data = wsiCachedGeometry(roi);
  if (!data.coordinates) data.coordinates = data.groups.map(group => group.rings.concat(group.holes)
    .map(ring => ring.map(p => [Number(p.x), Number(p.y)])));
  return data.coordinates;
}

function wsiCachedPath(part) {
  if (part.path) return part.path;
  const path = new Path2D();
  for (const ring of part.group.rings.concat(part.group.holes)) {
    ring.forEach((p, i) => i ? path.lineTo(p.x, p.y) : path.moveTo(p.x, p.y));
    path.closePath();
  }
  wsiRuntimeMetrics.path_builds++;
  return part.path = path;
}

function wsiCanvasTransform(project) {
  const p = project({ x: 0, y: 0 }), x = project({ x: 1, y: 0 }), y = project({ x: 0, y: 1 });
  return [x.x - p.x, x.y - p.y, y.x - p.x, y.y - p.y, p.x, p.y];
}

function wsiPaintRoi(cx, roi, index, project, viewBounds, selected, highlighted, dimmed) {
  const transform = wsiCanvasTransform(project), unit = Math.max(1e-9, Math.hypot(transform[0], transform[1]));
  const parts = wsiCachedGeometry(roi).parts.filter(part => !viewBounds || boundsOverlap(part.bounds, viewBounds));
  if (!parts.length) return;
  const locked = lockedRoi(roi), colour = selected ? '#ffffff' : locked ? '#facc15' : roi.colour || '#5eead4';
  cx.save(); cx.transform(...transform); cx.lineJoin = 'round'; cx.lineCap = 'round';
  for (const part of parts) {
    const path = wsiCachedPath(part);
    cx.globalAlpha = dimmed ? Math.min(.12, roiOpacity * .35) : roiOpacity;
    cx.fillStyle = roi.fill; cx.fill(path, 'evenodd');
    cx.globalAlpha = dimmed ? .32 : 1;
    cx.setLineDash([]); cx.strokeStyle = highlighted ? 'rgba(255,255,255,.96)' : 'rgba(0,0,0,.72)';
    cx.lineWidth = (highlighted ? 10 : selected ? 7 : locked ? 6 : 5) / unit;
    cx.stroke(path); cx.strokeStyle = colour;
    cx.lineWidth = (highlighted ? 5 : selected ? 4 : locked ? 3 : 2) / unit;
    if (!highlighted && !selected) { cx.setLineDash([7 / unit, 4 / unit]); cx.lineDashOffset = -(index % 9) * 2 / unit; }
    cx.stroke(path);
  }
  cx.restore();
}

function wsiDrawRois() {
  if (!screenshotIncludeComponent('annotations') || !showRois || !rois.length) return;
  const viewBounds = roiVisibleSlideBounds(.10), labels = [], highlightActive = annotationHighlightActive();
  ctx.save(); ctx.font = '600 12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif'; ctx.textBaseline = 'top';
  rois.forEach((roi, index) => {
    if (!visibleRoi(roi) || !isDrawable(roi) || !denseGeometryVisible(roi) || !overlayFocusRoiAllowed(roi)) return;
    const bounds = roiBounds(roi);
    if (!bounds || !boundsOverlap(bounds, viewBounds)) return;
    const selected = index === selectedRoi || !!roi.export_selected;
    const highlighted = roiClassHighlighted(roi), dimmed = highlightActive && !highlighted;
    const metrics = roiCanvasMetrics(bounds), lod = roiLodMode(roi, index, bounds, metrics, highlighted);
    if (lod === 'detail') wsiPaintRoi(ctx, roi, index, slideToCanvas, viewBounds, selected, highlighted, dimmed);
    else drawRoiLodMarker(roi, index, bounds, metrics, lod, dimmed, highlighted, selected);
    if (showLabels && !dimmed && (lod === 'detail' || highlighted || selected)) {
      const anchor = roiLabelPoint(roi), text = roiLabelText(roi, index);
      if (anchor && text) labels.push({ anchor, text, w: ctx.measureText(text).width + 18, h: 22,
        colour: roi.colour || '#5eead4', priority: highlighted ? 20 : selected ? 10 : 0 });
    }
  });
  if (showLabels) drawRoiLabels(labels);
  ctx.restore();
}

function wsiPaintLayerPolygon(cx, pane, layer, item, selected) {
  const project = pane ? p => multiViewSlideToCanvas(p, pane) : slideToCanvas;
  const bounds = pane ? multiViewVisibleSlideBounds(pane) : roiVisibleSlideBounds(.12);
  const transform = wsiCanvasTransform(project), unit = Math.max(1e-9, Math.hypot(transform[0], transform[1]));
  const colour = normaliseHexColour(item.colour || item.color || layer.colour || '#38bdf8');
  cx.save(); cx.transform(...transform);
  for (const part of wsiCachedGeometry(item).parts) {
    if (bounds && !boundsOverlap(part.bounds, bounds)) continue;
    const path = wsiCachedPath(part);
    cx.globalAlpha = layerOpacity(layer); cx.fillStyle = item.fill || hexToRgba(colour, .12);
    cx.strokeStyle = selected ? '#ffffff' : colour;
    cx.lineWidth = (selected ? Math.max(3, Number(layer.line_width || item.line_width || 2) + 2) : Number(layer.line_width || item.line_width || 2)) / unit;
    cx.fill(path, 'evenodd'); cx.stroke(path);
    if (selected) { cx.globalAlpha = 1; cx.strokeStyle = '#facc15'; cx.lineWidth = 1.5 / unit; cx.stroke(path); }
  }
  cx.restore();
}

function wsiDrawPaneRois(cx, pane, state, rect) {
  if (!screenshotIncludeComponent('annotations') || !showRois) return;
  const list = state.rois || [], bounds = multiViewVisibleSlideBounds(pane), labels = [];
  const highlightActive = annotationHighlightActive();
  cx.save(); cx.font = '600 12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif'; cx.textBaseline = 'top';
  list.forEach((roi, index) => {
    if (!visibleRoi(roi) || !isDrawable(roi) || !overlayFocusRoiAllowed(roi)) return;
    if (!boundsOverlap(roiBounds(roi), bounds)) return;
    const selected = index === Number(state.selectedRoi), highlighted = roiClassHighlighted(roi), dimmed = highlightActive && !highlighted;
    wsiPaintRoi(cx, roi, index, p => multiViewSlideToCanvas(p, pane), bounds, selected, highlighted, dimmed);
    if (showLabels && !dimmed) {
      const anchor = multiViewRoiLabelPoint(roi, pane), text = roiLabelText(roi, index);
      if (anchor && text) labels.push({ anchor, text, w: cx.measureText(text).width + 22, h: 24,
        colour: roi.colour || '#5eead4', priority: highlighted ? 20 : selected ? 10 : 0 });
    }
  });
  if (showLabels) multiViewDrawLabels(cx, labels, rect);
  cx.restore();
}

function wsiRequestPaneDraw(pane = null) {
  if (multiViewLayout <= 1) return;
  if (pane) wsiDirtyPanes.add(pane); else wsiAllPanesDirty = true;
  if (multiViewOverlayDrawQueued) return;
  multiViewOverlayDrawQueued = true;
  requestAnimationFrame(() => {
    multiViewOverlayDrawQueued = false;
    const targets = wsiAllPanesDirty ? null : wsiDirtyPanes;
    wsiDirtyPanes = new Set(); wsiAllPanesDirty = false;
    drawMultiViewOverlays(targets); wsiRequestInteraction();
    if (!targets || targets.has(multiViewPanes[multiViewActiveIndex])) drawMiniNavigator();
  });
}

function wsiInteractionSurface(pane = null) {
  let surface = pane ? pane.interactionCanvas : wsiInteractionCanvas;
  if (!surface) {
    surface = document.createElement('canvas'); surface.className = 'wsiInteractionCanvas';
    surface.setAttribute('aria-hidden', 'true');
    Object.assign(surface.style, { position: pane ? 'absolute' : 'fixed', inset: '0', pointerEvents: 'none', zIndex: '4', background: 'transparent' });
    (pane ? pane.element : document.body).appendChild(surface);
    if (pane) pane.interactionCanvas = surface; else wsiInteractionCanvas = surface;
  }
  const rect = pane ? pane.overlay.getBoundingClientRect() : { width: innerWidth, height: innerHeight };
  const dpr = window.devicePixelRatio || 1, w = Math.max(1, Math.round(rect.width * dpr)), h = Math.max(1, Math.round(rect.height * dpr));
  if (surface.width !== w) surface.width = w;
  if (surface.height !== h) surface.height = h;
  surface.style.width = rect.width + 'px'; surface.style.height = rect.height + 'px';
  const context = surface.getContext('2d'); context.setTransform(dpr, 0, 0, dpr, 0, 0);
  context.clearRect(0, 0, rect.width, rect.height);
  return context;
}

function wsiDrawInteraction() {
  const started = performance.now();
  if (typeof multiViewLayout !== 'undefined' && multiViewLayout > 1) {
    if (wsiInteractionCanvas) wsiInteractionCanvas.style.display = 'none';
    for (const pane of multiViewPanes) {
      if (!pane || !pane.overlay) continue;
      const context = wsiInteractionSurface(pane);
      if (pane.blank) continue;
      multiViewDrawDraft(context, pane);
      if (!wsiAnnotationPickMode()) multiViewDrawBrushPreview(context, pane);
      multiViewDrawEditHandles(context, pane); multiViewDrawCrosshair(context, pane, pane.overlay.getBoundingClientRect());
      multiViewDrawScreenshotSelection(context, pane);
      if (multiViewPanes[multiViewActiveIndex] === pane) {
        if (mode === 'measure' && measureStart && lastPointer && pointInsideSlide(lastPointer)) multiViewDrawMeasureLine(context, pane, measurementRecord(measureStart, lastPointer), true, false);
        if (mode === 'trajectory' && trajectoryDraft.length) multiViewDrawTrajectoryPath(context, pane, currentTrajectoryPreview(), { control_points: trajectoryDraft, colour: '#facc15' }, true);
      }
    }
  } else {
    const context = wsiInteractionSurface(), previous = ctx;
    wsiInteractionCanvas.style.display = 'block'; ctx = context;
    try {
      drawDraft();
      if (!wsiAnnotationPickMode()) drawBrushPreview();
      drawEditHandles(); drawCrosshair(); drawScreenshotSelection();
      if (mode === 'measure' && measureStart && lastPointer && pointInsideSlide(lastPointer)) drawMeasureLine(measurementRecord(measureStart, lastPointer), true, false);
      if (mode === 'trajectory' && trajectoryDraft.length) drawTrajectoryPath(currentTrajectoryPreview(), { control_points: trajectoryDraft, colour: '#facc15' }, true);
    }
    finally { ctx = previous; }
  }
  wsiRuntimeMetrics.cursor_frames++;
  wsiRuntimeMetrics.cursor_max_ms = Math.max(wsiRuntimeMetrics.cursor_max_ms, performance.now() - started);
}

function wsiRequestInteraction() {
  if (wsiInteractionQueued) return;
  wsiInteractionQueued = true;
  requestAnimationFrame(() => { wsiInteractionQueued = false; wsiDrawInteraction(); });
}

function wsiSegmentTree(entries) {
  if (!entries.length) return null;
  const bounds = entries.reduce((b, e) => unionBounds(b, e.bounds), null);
  if (entries.length <= 12) return { bounds, entries };
  const axis = bounds.xmax - bounds.xmin >= bounds.ymax - bounds.ymin ? 'x' : 'y';
  entries.sort((a, b) => (a.bounds[axis + 'min'] + a.bounds[axis + 'max']) - (b.bounds[axis + 'min'] + b.bounds[axis + 'max']));
  const mid = entries.length >> 1;
  return { bounds, left: wsiSegmentTree(entries.slice(0, mid)), right: wsiSegmentTree(entries.slice(mid)) };
}

function wsiQuerySegments(tree, bounds, visit) {
  if (!tree || !boundsOverlap(tree.bounds, bounds)) return false;
  if (tree.entries) return tree.entries.some(entry => boundsOverlap(entry.bounds, bounds) && visit(entry));
  return wsiQuerySegments(tree.left, bounds, visit) || wsiQuerySegments(tree.right, bounds, visit);
}

function wsiPointSegmentDistance(p, a, b) {
  const dx = b.x - a.x, dy = b.y - a.y, d = dx * dx + dy * dy;
  const t = d ? clamp(((p.x - a.x) * dx + (p.y - a.y) * dy) / d, 0, 1) : 0;
  return Math.hypot(p.x - a.x - t * dx, p.y - a.y - t * dy);
}

function wsiBrushTouches(roi, a, b, radius) {
  const sweep = { xmin: Math.min(a.x, b.x) - radius, xmax: Math.max(a.x, b.x) + radius,
    ymin: Math.min(a.y, b.y) - radius, ymax: Math.max(a.y, b.y) + radius };
  if (!boundsOverlap(roiBounds(roi), sweep)) return false;
  if (roiContainsPoint(roi, b) || roiContainsPoint(roi, a)) return true;
  for (const part of wsiCachedGeometry(roi).parts) {
    if (!boundsOverlap(part.bounds, sweep)) continue;
    if (!part.segmentTree) {
      const entries = [];
      for (const ring of part.group.rings.concat(part.group.holes)) {
        for (let i = 1; i < ring.length; i++) {
          const p = ring[i - 1], q = ring[i];
          entries.push({ a: p, b: q, bounds: { xmin: Math.min(p.x, q.x), xmax: Math.max(p.x, q.x),
            ymin: Math.min(p.y, q.y), ymax: Math.max(p.y, q.y) } });
        }
      }
      part.segmentTree = wsiSegmentTree(entries);
    }
    if (wsiQuerySegments(part.segmentTree, sweep, e => segmentsIntersect(a, b, e.a, e.b) ||
    Math.min(wsiPointSegmentDistance(e.a, a, b), wsiPointSegmentDistance(e.b, a, b),
      wsiPointSegmentDistance(a, e.a, e.b), wsiPointSegmentDistance(b, e.a, e.b)) <= radius)) return true;
  }
  return false;
}

function wsiUpdateBrushSelection(a, b, event) {
  if (brushSelectionIsAdditive(event)) brushAdditiveSelection = true;
  let changed = false;
  rois.forEach((roi, index) => {
    if (!brushTouchedSelection.has(index) && visibleRoi(roi) && isDrawable(roi) && wsiBrushTouches(roi, a, b, brushRadius)) {
      brushTouchedSelection.add(index); selectRoiForBrush(index); changed = true;
    }
  });
  if (changed) {
    if (brushTargetRoi >= 0) selectedRoi = brushTargetRoi;
    updateRoiList(); scheduleViewerStateSync('brush_selection_updated', { indices: Array.from(brushTouchedSelection), additive: brushAdditiveSelection });
  }
}

function wsiAddBrushPoint(p, event = {}) {
  if (!brushing || !pointInsideSlide(p)) return;
  updateCursorFeedback(event); syncBrushRadiusToZoom();
  const last = brushPoints[brushPoints.length - 1];
  if (last && Math.hypot(p.x - last.x, p.y - last.y) < brushPointSpacing(brushRadius)) { wsiRequestInteraction(); return; }
  const current = { x: p.x, y: p.y }; brushPoints.push(current);
  if (annotationMaskBrushEnabled()) {
    paintAnnotationMaskStroke(last ? [last, current] : [current], brushRadius, brushClass || currentRoiClass(), brushOperation === 'subtract' ? 'subtract' : 'paint');
    wsiRequestDraw();
  } else wsiUpdateBrushSelection(last || current, current, event);
  wsiRequestInteraction();
}

function wsiWorkerRequest(task) {
  if (!wsiGeometryWorker) {
    const url = URL.createObjectURL(new Blob([wsiGeometryWorkerSource], { type: 'text/javascript' }));
    try { wsiGeometryWorker = new Worker(url); } finally { URL.revokeObjectURL(url); }
    wsiGeometryWorker.onmessage = event => {
      const job = wsiGeometryJobs.get(event.data.id);
      if (!job) return;
      wsiGeometryJobs.delete(event.data.id);
      if (event.data.ok) {
        const result = event.data.result;
        if (result && result.resident_ids) {
          const resident = new Set(result.resident_ids), changed = new Set(result.changed_resident_ids);
          for (const key of wsiWorkerSourceVersions.keys()) if (!resident.has(key) || changed.has(key)) wsiWorkerSourceVersions.delete(key);
          wsiRuntimeMetrics.geometry_kernel = result.kernel;
          wsiRuntimeMetrics.geometry_cache_bytes = result.cache_bytes;
          if (result.kernel_warning && !wsiKernelWarningShown) {
            wsiKernelWarningShown = true;
            notify('Fast annotation kernel unavailable; using JavaScript: ' + result.kernel_warning, 'warning', 6000);
          }
        }
        job.resolve(result);
      } else { wsiWorkerSourceVersions.clear(); job.reject(new Error(event.data.error)); }
    };
    wsiGeometryWorker.onerror = event => {
      for (const job of wsiGeometryJobs.values()) job.reject(new Error(event.message || 'Geometry worker failed'));
      wsiGeometryJobs.clear(); wsiGeometryWorker.terminate(); wsiGeometryWorker = null; wsiWorkerSourceVersions.clear();
    };
  }
  return new Promise((resolve, reject) => {
    const id = ++wsiGeometryJobId;
    wsiGeometryJobs.set(id, { resolve, reject });
    try {
      const buffers = [];
      if (task.type !== 'parse_geojson') {
        task.packed = true; task.ring_patches = true; task.project_key = wsiProjectKey();
        for (const source of task.sources || []) {
          source.geometry = wsiPackPolygons(source.geometry); buffers.push(...wsiPolygonBuffers(source.geometry));
        }
        if (task.geometry) { task.geometry = wsiPackPolygons(task.geometry); buffers.push(...wsiPolygonBuffers(task.geometry)); }
        task.protection = (task.protection || []).map(polygons => {
          const packed = wsiPackPolygons(polygons); buffers.push(...wsiPolygonBuffers(packed)); return packed;
        });
      }
      wsiRuntimeMetrics.geometry_transfer_bytes += buffers.reduce((n, b) => n + b.byteLength, 0);
      wsiGeometryWorker.postMessage({ id, task }, buffers);
    }
    catch (error) { wsiGeometryJobs.delete(id); reject(error); }
  });
}

function wsiWorkerAnnotationSource(roi, id = null) {
  return { id: id || wsiRoiId(roi), class_key: roiClassKey(roi), geometry: wsiRoiCoordinates(roi), locked: lockedRoi(roi) };
}

function wsiBrushEditBounds(points, radius, target = null) {
  const bounds = boundsFromRing(points || []);
  if (!bounds) return target ? roiBounds(target) : null;
  const padded = { xmin: bounds.xmin - radius, ymin: bounds.ymin - radius,
    xmax: bounds.xmax + radius, ymax: bounds.ymax + radius };
  return target ? unionBounds(padded, roiBounds(target)) : padded;
}

function wsiLayerWorkerId(layer, item) {
  return 'layer::' + String(layer.id || layer.name || 'annotation') + '::' + wsiRoiId(item);
}

function wsiAnnotationRecords(bounds, target = null, includeLayerItems = false) {
  const records = [], seen = new Set();
  for (const roi of annotationCandidateRois(-1)) {
    if (roi !== target && bounds && !boundsOverlap(roiBounds(roi), bounds)) continue;
    const id = wsiRoiId(roi);
    if (!seen.has(id)) { records.push({ id, item: roi, layer: null }); seen.add(id); }
  }
  if (target && !records.some(record => record.item === target)) {
    const id = wsiRoiId(target); records.push({ id, item: target, layer: null }); seen.add(id);
  }
  if (!includeLayerItems) return records;
  for (const layer of layers || []) {
    const sourceType = String(layer && (layer.source_type || layer.type) || '').toLowerCase();
    if (!layer || !/annotation|tissue/.test(sourceType)) continue;
    for (const item of layer.items || []) {
      if (!item || item === target || !isDrawable(item) ||
          (typeof roiIsCellLike === 'function' && roiIsCellLike(item)) ||
          !layerItemMatchesActiveProject(item, layer) ||
          (bounds && !boundsOverlap(roiBounds(item), bounds))) continue;
      const id = wsiLayerWorkerId(layer, item);
      if (!seen.has(id)) { records.push({ id, item, layer }); seen.add(id); }
    }
  }
  return records;
}

function wsiRecordSignature(record, key) {
  return [key, wsiCachedGeometry(record.item), roiClassKey(record.item), lockedRoi(record.item)];
}

function wsiRecordPresent(record) {
  return record.layer ? Array.isArray(record.layer.items) && record.layer.items.includes(record.item) : rois.includes(record.item);
}

function wsiRecordsUnchanged(records, versions, key) {
  return records.every(record => wsiRecordPresent(record) && wsiSameSignature(
    versions.get(record.id), wsiRecordSignature(record, key)));
}

function wsiMaterializePriorityLayerRecords(result, recordsById) {
  const changed = new Set([...(result.updates || []).map(update => String(update.id)),
    ...(result.removed || []).map(String)]), touchedLayers = new Set();
  for (const id of changed) {
    const record = recordsById.get(id);
    if (!record || !record.layer) continue;
    const items = record.layer.items || [], index = items.indexOf(record.item);
    if (index >= 0) items.splice(index, 1);
    record.layer.count = items.length;
    touchedLayers.add(record.layer);
    if (!rois.includes(record.item)) rois.push(record.item);
    record.layer = null;
  }
  if (touchedLayers.size && typeof buildLayerList === 'function') buildLayerList();
  return touchedLayers.size;
}

function wsiBrushProtection(task) {
  const bounds = boundsFromRing(task.points);
  bounds.xmin -= task.radius; bounds.xmax += task.radius;
  bounds.ymin -= task.radius; bounds.ymax += task.radius;
  const protectedItems = [];
  for (const layer of layers) {
    if (layer.visible === false || !/annotation|tissue/.test(String(layer.source_type || ''))) continue;
    for (const item of layer.items || []) {
      if (item.visible !== false && layerItemMatchesActiveProject(item, layer) && isDrawable(item) && boundsOverlap(roiBounds(item), bounds) &&
          (lockedRoi(item) || (!task.priority_claim && roiClassKey(item) !== task.class_key))) {
        const parts = wsiCachedGeometry(item).parts.filter(part => boundsOverlap(part.bounds, bounds));
        if (parts.length) protectedItems.push(parts.map(part => {
          if (!part.coordinates) part.coordinates = part.group.rings.concat(part.group.holes).map(ring => ring.map(p => [p.x, p.y]));
          return part.coordinates;
        }));
      }
    }
  }
  return protectedItems;
}

function wsiResetBrush() {
  brushing = false; brushPoints = []; brushOperation = 'new'; brushTargetRoi = -1;
  brushClass = ''; brushTouchedSelection = new Set(); brushAdditiveSelection = false;
  wsiBrushPriorityClaim = false;
  updateCursorFeedback(); wsiRequestInteraction();
}

function wsiWorkerGeometryGroups(geometry, roi = null) {
  if (geometry && geometry.format === 'wsi-ring-patch-1') {
    if (!roi) throw new Error('Annotation patch has no matching source.');
    const rings = wsiCachedGeometry(roi).groups.flatMap(group => group.rings.concat(group.holes));
    const changed = wsiWorkerGeometryGroups(geometry.changed);
    return geometry.rings.map(group => group.map(ref => {
      const ring = ref >= 0 ? rings[ref] : changed[-ref - 1]?.[0];
      if (!ring) throw new Error('Annotation patch has an invalid ring reference.');
      return ring;
    }));
  }
  if (geometry && geometry.format === 'wsi-polygons-1') {
    const { xy, ringOffsets, polygonOffsets } = geometry, groups = [];
    for (let i = 1; i < polygonOffsets.length; i++) {
      const group = [];
      for (let r = polygonOffsets[i - 1]; r < polygonOffsets[i]; r++) {
        const ring = [];
        for (let p = ringOffsets[r] * 2; p < ringOffsets[r + 1] * 2; p += 2) ring.push({ x: xy[p], y: xy[p + 1] });
        group.push(ring);
      }
      groups.push(group);
    }
    return groups;
  }
  return (geometry || []).map(polygon => polygon.map(ring => ring.map(p => ({ x: p[0], y: p[1] }))));
}

function wsiWorkerGeometryEmpty(geometry) {
  if (geometry && geometry.format === 'wsi-ring-patch-1') return !geometry.rings.length;
  return !geometry || (geometry.format === 'wsi-polygons-1' ? geometry.polygonOffsets.length <= 1 : !geometry.length);
}

function wsiRememberWorkerRecords(records, result, key) {
  const resident = new Set(result.resident_ids || []);
  for (const record of records) if (resident.has(record.id) && wsiRecordPresent(record)) {
    wsiWorkerSourceVersions.set(record.id, wsiRecordSignature(record, key));
  }
}

function wsiSetWorkerGeometry(roi, geometry) {
  const previous = wsiGeometryCache.get(roi);
  const groups = wsiWorkerGeometryGroups(geometry, roi);
  roi.rings = groups[0] || []; roi.add_groups = groups.slice(1);
  roi.add_rings = []; roi.subtract_rings = [];
  roi.raw_coordinates = null; roi.dense_static_geometry = false; roi.dense_geometry = false;
  roi.drawable = groups.length > 0;
  refreshRoiGeometry(roi);
  if (previous) wsiReusableGeometryParts.set(roi, new Map(previous.parts.map(part => [part.group.rings[0], part])));
  return groups;
}

function wsiCommitPriorityNeighbors(result, target, recordsById = null) {
  const trimmed = [];
  for (const update of result.updates || []) {
    const record = recordsById && recordsById.get(String(update.id));
    const roi = record ? record.item : rois.find(item => item !== target && wsiRoiId(item) === String(update.id));
    if (!roi || lockedRoi(roi)) continue;
    wsiSetWorkerGeometry(roi, update.geometry);
    roi.priority_trimmed = true; trimmed.push(wsiRoiId(roi));
  }
  const removed = [];
  for (const workerId of (result.removed || []).map(String)) {
    const record = recordsById && recordsById.get(workerId);
    const roi = record ? record.item : rois.find(item => item !== target && wsiRoiId(item) === workerId);
    if (!roi || roi === target || lockedRoi(roi)) continue;
    const index = rois.indexOf(roi);
    if (index >= 0) rois.splice(index, 1);
    removed.push(wsiRoiId(roi));
  }
  return { trimmed, removed };
}

async function wsiApplyPriorityClaim(groups, index = selectedRoi, operation = 'wand', options = {}) {
  const target = rois[index], key = wsiProjectKey(), generation = wsiEditGeneration;
  if (!target || lockedRoi(target) || !editableRoi(target)) return null;
  const claimBounds = boundsFromRings((groups || []).flat()), records = wsiAnnotationRecords(claimBounds, target, true);
  const recordsById = new Map(records.map(record => [record.id, record])), sourceVersions = new Map();
  const sources = [];
  for (const record of records) {
    const signature = wsiRecordSignature(record, key); sourceVersions.set(record.id, signature);
    if (!wsiSameSignature(wsiWorkerSourceVersions.get(record.id), signature)) sources.push(wsiWorkerAnnotationSource(record.item, record.id));
  }
  const task = { type: 'claim', target_id: wsiRoiId(target), geometry: (groups || []).map(group =>
    group.map(ring => ring.map(p => [Number(p.x), Number(p.y)]))), sources, active_ids: records.map(record => record.id), protection: [],
    wand_cleanup: operation === 'wand', hole_area_threshold: Number(options.hole_area_threshold) || 0 };
  const result = await wsiWorkerRequest(task);
  const unchanged = wsiRecordsUnchanged(records, sourceVersions, key);
  if (generation !== wsiEditGeneration) throw new Error('The priority edit was cancelled.');
  if (key !== wsiProjectKey() || !unchanged || !rois.includes(target)) throw new Error('The tissue or annotations changed before the priority edit finished.');
  if (wsiWorkerGeometryEmpty(result.geometry)) throw new Error('Locked annotations leave no area for this priority edit.');
  const started = performance.now();
  const historyEvent = operation === 'wand' ? 'roi_wand_refined' : 'roi_brush_extend';
  const transaction = wsiBeginEditTransaction(records, result, target);
  wsiMaterializePriorityLayerRecords(result, recordsById);
  const applied = wsiSetWorkerGeometry(target, result.geometry), changed = wsiCommitPriorityNeighbors(result, target, recordsById);
  target.brush_edited = true; target.brush_mask_contour = true; target.priority_claim = true;
  target.brush_ring_count = applied.reduce((n, group) => n + group.length, 0);
  selectedRoi = rois.indexOf(target); showRois = true;
  wsiRememberWorkerRecords(records, result, key);
  const editDelta = wsiCompleteEditTransaction(transaction, target, historyEvent);
  wsiRefreshEditedAnnotations(editDelta); updateButtons();
  const detail = { id: target.id, operation, project_key: key, priority_claim: true,
    filled_artifact_holes: Number(result.filled_artifact_holes) || 0,
    trimmed_ids: changed.trimmed, removed_ids: changed.removed, geometry_worker: true, edit_delta: editDelta };
  recordAnnotationHistory(historyEvent, detail); scheduleViewerStateSync('roi_brush_edited', detail);
  markAnnotationsDirty('roi_brush_edited'); wsiRequestDraw();
  wsiRuntimeMetrics.geometry_jobs++; wsiRuntimeMetrics.geometry_worker_ms += result.duration_ms;
  wsiRuntimeMetrics.geometry_commit_ms = performance.now() - started;
  notify('Priority edit applied; ' + (changed.trimmed.length + changed.removed.length) + ' overlapping annotation' +
    ((changed.trimmed.length + changed.removed.length) === 1 ? '' : 's') + ' reduced', 'success', 3200);
  return target;
}

async function wsiApplyWandEdit(groups, index = selectedRoi, operation = 'extend', options = {}) {
  const target = rois[index], key = wsiProjectKey(), generation = wsiEditGeneration;
  if (!target || lockedRoi(target) || !editableRoi(target)) return null;
  const editBounds = boundsFromRings((groups || []).flat()), records = wsiAnnotationRecords(editBounds, target, false);
  const recordsById = new Map(records.map(record => [record.id, record])), sourceVersions = new Map(), sources = [];
  for (const record of records) {
    const signature = wsiRecordSignature(record, key); sourceVersions.set(record.id, signature);
    if (!wsiSameSignature(wsiWorkerSourceVersions.get(record.id), signature)) sources.push(wsiWorkerAnnotationSource(record.item, record.id));
  }
  const task = { type: 'wand_edit', operation, target_id: wsiRoiId(target), class_key: classPresetKey(roiClassName(target)),
    geometry: (groups || []).map(group => group.map(ring => ring.map(p => [Number(p.x), Number(p.y)]))),
    sources, active_ids: records.map(record => record.id), protection: [],
    hole_area_threshold: Number(options.hole_area_threshold) || 0 };
  const result = await wsiWorkerRequest(task), unchanged = wsiRecordsUnchanged(records, sourceVersions, key);
  if (generation !== wsiEditGeneration) throw new Error('The Wand edit was cancelled.');
  if (key !== wsiProjectKey() || !unchanged || !rois.includes(target)) throw new Error('The tissue or annotations changed before the Wand edit finished.');
  if (wsiWorkerGeometryEmpty(result.geometry)) throw new Error('The Wand selection would remove the entire annotation.');
  const started = performance.now(), eventName = operation === 'subtract' ? 'roi_wand_subtract' : 'roi_wand_refined';
  const transaction = wsiBeginEditTransaction(records, result, target);
  const applied = wsiSetWorkerGeometry(target, result.geometry), changed = wsiCommitPriorityNeighbors(result, target, recordsById);
  target.brush_edited = true; target.brush_mask_contour = true; target.wand = true;
  target.brush_ring_count = applied.reduce((n, group) => n + group.length, 0);
  selectedRoi = rois.indexOf(target); showRois = true; wsiRememberWorkerRecords(records, result, key);
  const editDelta = wsiCompleteEditTransaction(transaction, target, eventName);
  wsiRefreshEditedAnnotations(editDelta); updateButtons();
  const detail = { id: target.id, operation, project_key: key, removed_ids: changed.removed,
    filled_artifact_holes: Number(result.filled_artifact_holes) || 0,
    geometry_worker: true, edit_delta: editDelta };
  recordAnnotationHistory(eventName, detail); scheduleViewerStateSync('roi_brush_edited', detail);
  markAnnotationsDirty('roi_brush_edited'); wsiRequestDraw();
  wsiRuntimeMetrics.geometry_jobs++; wsiRuntimeMetrics.geometry_worker_ms += result.duration_ms;
  wsiRuntimeMetrics.geometry_commit_ms = performance.now() - started;
  return target;
}

async function wsiFinishBrush() {
  if (!brushing) return wsiEditPromise;
  if (annotationMaskBrushEnabled()) {
    brushing = false; finishAnnotationMaskBrush(brushOperation === 'subtract' ? 'subtract' : 'paint', brushClass || currentRoiClass());
    wsiRequestInteraction(); return;
  }
  const key = wsiProjectKey(), target = rois[brushTargetRoi], targetId = target && wsiRoiId(target);
  const task = { type: 'edit', points: brushPoints.slice(), radius: brushRadius, zoom: brushSlideUnitScale(),
    width: cfg.slide_width, height: cfg.slide_height, operation: brushOperation,
    target_id: targetId, class_key: classPresetKey(brushClass || currentRoiClass()),
    priority_claim: !!(wsiBrushPriorityClaim && brushOperation !== 'subtract' && targetId) };
  const cls = brushClass || currentRoiClass(), pane = typeof multiViewPointerPane !== 'undefined' ? multiViewPointerPane : null;
  const generation = wsiEditGeneration;
  brushing = false;
  markAnnotationsDirty('brush_pending');
  wsiRequestInteraction(); wsiRequestDraw();
  const run = async () => {
    const editBounds = wsiBrushEditBounds(task.points, task.radius), records = wsiAnnotationRecords(editBounds, target, task.priority_claim);
    const recordsById = new Map(records.map(record => [record.id, record])), sourceVersions = new Map();
    task.active_ids = records.map(record => record.id); task.sources = [];
    for (const record of records) {
      const version = wsiRecordSignature(record, key);
      sourceVersions.set(record.id, version);
      if (!wsiSameSignature(wsiWorkerSourceVersions.get(record.id), version)) {
        task.sources.push(wsiWorkerAnnotationSource(record.item, record.id));
      }
    }
    task.protection = task.operation === 'subtract' ? [] : wsiBrushProtection(task);
    const result = await wsiWorkerRequest(task);
    const unchanged = wsiRecordsUnchanged(records, sourceVersions, key);
    if (generation !== wsiEditGeneration) { wsiResetBrush(); return; }
    if (key !== wsiProjectKey() || !unchanged || target && !rois.includes(target)) throw new Error('The tissue or annotations changed before the brush edit finished. The edit was not applied.');
    if (wsiWorkerGeometryEmpty(result.geometry)) throw new Error('The brush would remove the whole annotation or overlaps another class. The annotation was left unchanged.');
    const started = performance.now();
    const transaction = wsiBeginEditTransaction(records, result, target);
    if (task.priority_claim) wsiMaterializePriorityLayerRecords(result, recordsById);
    let roi = target;
    if (!roi) {
      newRoiCount++;
      const colour = classColour(cls);
      roi = { id: 'drawn_roi_' + Date.now().toString(36) + '_' + newRoiCount, name: cls, class: cls,
        colour, fill: hexToRgba(colour, .18), source: 'brush', drawn: true };
      rois.push(roi);
    }
    const groups = wsiSetWorkerGeometry(roi, result.geometry);
    roi.drawable = true; roi.brush_edited = true; roi.non_overlapping = true; roi.brush_ring_count = groups.reduce((n, group) => n + group.length, 0);
    roi.priority_claim = !!task.priority_claim;
    const changed = wsiCommitPriorityNeighbors(result, roi, recordsById);
    wsiRememberWorkerRecords(records, result, key);
    selectedRoi = rois.indexOf(roi); showRois = true;
    const editDelta = wsiCompleteEditTransaction(transaction, roi, 'roi_brush_edited');
    wsiResetBrush(); wsiRefreshEditedAnnotations(editDelta); updateButtons();
    recordAnnotationHistory(task.operation === 'subtract' ? 'roi_brush_subtract' : 'roi_brush_extend',
      { id: roi.id, project_key: key, worker_ms: result.duration_ms, removed_ids: changed.removed,
        trimmed_ids: changed.trimmed, priority_claim: !!task.priority_claim, geometry_worker: true });
    scheduleViewerStateSync('roi_brush_edited', { id: roi.id, operation: task.operation, project_key: key,
      priority_claim: !!task.priority_claim, trimmed_ids: changed.trimmed, removed_ids: changed.removed, edit_delta: editDelta });
    if (pane) {
      saveActiveProjectAnnotations();
      for (const view of multiViewPanes) {
        if (view.entry && projectAnnotationKey(view.entry.itemIndex, view.entry.sectionIndex) === key) wsiRequestPaneDraw(view);
      }
    } else wsiRequestDraw();
    if (typeof closeAllToolMenus === 'function') closeAllToolMenus();
    wsiRuntimeMetrics.geometry_jobs++; wsiRuntimeMetrics.geometry_worker_ms += result.duration_ms;
    wsiRuntimeMetrics.geometry_commit_ms = performance.now() - started;
  };
  wsiEditPromise = run().catch(error => { wsiResetBrush(); notify('Brush edit failed: ' + error.message, 'error', 6200); })
    .finally(() => { wsiEditPromise = null; });
  return wsiEditPromise;
}

function wsiSnapshotGeometry(roi, geometry) {
  const keys = ['rings', 'add_groups', 'add_rings', 'subtract_rings', 'raw_coordinates', 'coordinates', 'geometry', 'feature'];
  const fields = Object.fromEntries(keys.filter(key => roi[key] !== undefined).map(key => [key, roi[key]]));
  const groups = [roi.rings, ...(roi.add_groups || [])];
  const canonical = !roi.dense_static_geometry && !roi.raw_coordinates && !(roi.add_rings || []).length &&
    !(roi.subtract_rings || []).length && groups.length === geometry.parts.length &&
    groups.every((g, i) => wsiSameSignature(g, geometry.parts[i].group.rings));
  if (!canonical) return JSON.parse(JSON.stringify(fields));
  // Snapshots own their points. Only immutable, unchanged component snapshots
  // are shared between history entries, never the live editable coordinates.
  const savedGroups = geometry.parts.map(part => {
    if (!part.snapshotRings) part.snapshotRings = JSON.parse(JSON.stringify(part.group.rings));
    return part.snapshotRings;
  });
  delete fields.rings; delete fields.add_groups;
  const saved = JSON.parse(JSON.stringify(fields));
  if (roi.rings !== undefined) saved.rings = savedGroups[0];
  if (roi.add_groups !== undefined) saved.add_groups = savedGroups.slice(1);
  return saved;
}

function wsiSnapshotRoi(roi) {
  const geometry = wsiCachedGeometry(roi), omit = new Set(['rings', 'add_groups', 'add_rings', 'subtract_rings', 'raw_coordinates', 'coordinates', 'geometry', '_dense_full_groups', '_dense_display_cache', 'feature']);
  const metadata = Object.fromEntries(Object.entries(roi).filter(([key]) => !omit.has(key) && !key.startsWith('_wsi')));
  const text = JSON.stringify(metadata), old = wsiSnapshotCache.get(roi);
  if (old && old.geometry === geometry && old.text === text) return old.snapshot;
  const snapshot = JSON.parse(text);
  const saved = old && old.geometry === geometry ? old.saved : wsiSnapshotGeometry(roi, geometry);
  Object.assign(snapshot, saved);
  wsiSnapshotCache.set(roi, { geometry, text, saved, snapshot });
  return snapshot;
}

function wsiAnnotationSnapshot() {
  return { rois: rois.map(wsiSnapshotRoi), selectedRoi, newRoiCount,
    trajectories: JSON.parse(JSON.stringify(trajectories)), selectedTrajectory, trajectorySeq };
}

const wsiAnnotationHistoryMaxEntries = 10;
const wsiAnnotationHistoryMaxBytes = 64 * 1024 * 1024;

function wsiHistorySnapshotPoints(snapshot) {
  if (!snapshot || typeof snapshot !== 'object') return 0;
  const rings = value => {
    if (!Array.isArray(value)) return 0;
    if (value.length >= 2 && Number.isFinite(Number(value[0])) && Number.isFinite(Number(value[1]))) return 1;
    if (value.length && value[0] && typeof value[0] === 'object' && !Array.isArray(value[0]) &&
        Number.isFinite(Number(value[0].x)) && Number.isFinite(Number(value[0].y))) return value.length;
    return value.reduce((n, item) => n + rings(item), 0);
  };
  if (Array.isArray(snapshot.rois)) return snapshot.rois.reduce((n, roi) => n + wsiHistorySnapshotPoints(roi), 0);
  if (snapshot.wsi_delta) return [...(snapshot.before || []), ...(snapshot.after || [])]
    .reduce((n, item) => n + wsiHistorySnapshotPoints(item && item.snapshot), 0);
  return rings(snapshot.rings) + rings(snapshot.add_groups) + rings(snapshot.add_rings) +
    rings(snapshot.subtract_rings) + rings(snapshot.raw_coordinates) + rings(snapshot.coordinates);
}

function wsiHistoryEntryBytes(entry) {
  return Math.max(1024, wsiHistorySnapshotPoints(entry) * 24);
}

function wsiTrimAnnotationHistory(stack) {
  while (stack.length > wsiAnnotationHistoryMaxEntries) stack.shift();
  let bytes = stack.reduce((n, entry) => n + wsiHistoryEntryBytes(entry), 0);
  while (stack.length > 1 && bytes > wsiAnnotationHistoryMaxBytes) bytes -= wsiHistoryEntryBytes(stack.shift());
  return bytes;
}

function wsiRestoreRois(saved) {
  const current = new Map(rois.map(roi => [String(roi.id), roi]));
  return (saved || []).map(snapshot => {
    const roi = current.get(String(snapshot.id));
    return roi && wsiSnapshotRoi(roi) === snapshot ? roi : JSON.parse(JSON.stringify(snapshot));
  });
}

window.addEventListener('pagehide', () => { if (wsiGeometryWorker) wsiGeometryWorker.terminate(); });
