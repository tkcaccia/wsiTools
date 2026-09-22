function wsiEditEntry(item, layer = null) {
  return { id: wsiRoiId(item), layer_id: layer ? String(layer.id) : null,
    index: (layer ? layer.items : rois).indexOf(item), snapshot: wsiSnapshotRoi(item) };
}

function wsiBeginEditTransaction(records, result, target) {
  const changed = new Set((result.updates || []).map(update => String(update.id)).concat((result.removed || []).map(String)));
  const affected = records.filter(record => record.item === target || changed.has(record.id));
  return { wsi_delta: true, key: wsiProjectKey(), before: affected.map(record => wsiEditEntry(record.item, record.layer)),
    selectedBefore: selectedRoi >= 0 ? wsiRoiId(rois[selectedRoi]) : null, countBefore: newRoiCount };
}

function wsiCompleteEditTransaction(transaction, roi, action) {
  const ids = new Set(transaction.before.map(entry => entry.id));
  if (roi && !ids.has(wsiRoiId(roi))) {
    ids.add(wsiRoiId(roi)); transaction.before.push({ id: wsiRoiId(roi), snapshot: null, layer_id: null, index: -1 });
  }
  transaction.after = transaction.before.map(entry => {
    const item = rois.find(item => wsiRoiId(item) === entry.id);
    return item ? wsiEditEntry(item) : { ...entry, snapshot: null };
  });
  transaction.selectedAfter = selectedRoi >= 0 ? wsiRoiId(rois[selectedRoi]) : null;
  transaction.countAfter = newRoiCount;
  pushHistory(annotationUndo, transaction); annotationRedo = [];
  markAnnotationsDirty(action);
  return wsiEditDelta(transaction.before, transaction.after);
}

function wsiEditDelta(before, after) {
  return { upsert: after.filter(entry => entry.snapshot && !entry.layer_id).map(entry => entry.id),
    remove: after.filter(entry => !entry.snapshot || entry.layer_id).map(entry => entry.id),
    order_changed: before.some((entry, i) => !!entry.snapshot !== !!after[i].snapshot || entry.layer_id !== after[i].layer_id) };
}

function wsiRefreshEditedAnnotations(delta) {
  const sort = el('annotationSort');
  if (delta.order_changed || sort && sort.value && sort.value !== 'original') { buildRoiList(); return; }
  for (const id of delta.upsert) {
    const i = rois.findIndex(roi => wsiRoiId(roi) === id), roi = rois[i];
    if (!roi) continue;
    const row = el('roiList')?.querySelector('.roiItem[data-index="' + i + '"]'), details = row?.querySelector('.roiDetails');
    if (!details) continue;
    details.replaceChildren();
    addDetail(details, 'Geometry', geometryType(roi)); addDetail(details, 'Bounds', formatBounds(roiBounds(roi)), true);
    addDetail(details, 'Points', String(pointCount(roi))); addDetail(details, 'Area', Number.isFinite(roi.area) ? fmt(roi.area, 1) : 'NA');
    addDetail(details, 'Source', roi.source || 'geojson'); addDetail(details, 'ID', id, true);
  }
  updateRoiList();
}

function wsiRestoreEditHistory(redo = false) {
  const from = redo ? annotationRedo : annotationUndo, to = redo ? annotationUndo : annotationRedo, transaction = from[from.length - 1];
  if (!transaction || !transaction.wsi_delta) return false;
  if (transaction.key !== wsiProjectKey()) { notify('Open the tissue containing this edit before undoing it.', 'warning'); return false; }
  wsiEditGeneration++;
  const entries = redo ? transaction.after : transaction.before, previous = redo ? transaction.before : transaction.after;
  const ids = new Set(entries.map(entry => entry.id)), layerIds = new Set([...entries, ...previous].map(entry => entry.layer_id).filter(Boolean));
  for (let i = rois.length - 1; i >= 0; i--) if (ids.has(wsiRoiId(rois[i]))) rois.splice(i, 1);
  for (const layer of layers) if (layerIds.has(String(layer.id))) {
    for (let i = (layer.items || []).length - 1; i >= 0; i--) if (ids.has(wsiRoiId(layer.items[i]))) layer.items.splice(i, 1);
    layer.count = layer.items.length;
  }
  for (const entry of entries.filter(entry => entry.snapshot).sort((a, b) => a.index - b.index)) {
    const layer = entry.layer_id ? layers.find(layer => String(layer.id) === entry.layer_id) : null;
    const list = layer ? layer.items : rois, item = JSON.parse(JSON.stringify(entry.snapshot));
    list.splice(Math.max(0, Math.min(entry.index, list.length)), 0, item);
    if (layer) layer.count = list.length;
  }
  selectedRoi = rois.findIndex(roi => wsiRoiId(roi) === (redo ? transaction.selectedAfter : transaction.selectedBefore));
  newRoiCount = redo ? transaction.countAfter : transaction.countBefore;
  wsiWorkerSourceVersions.clear(); wsiAnnotationEpoch++;
  activeVertex = null; draggingVertex = null; draft = []; wsiResetBrush();
  from.pop(); pushHistory(to, transaction);
  const event = redo ? 'annotation_redo' : 'annotation_undo', delta = wsiEditDelta(previous, entries);
  markAnnotationsDirty(event); buildRoiList(); if (layerIds.size) buildLayerList(); updateButtons();
  recordAnnotationHistory(event, { undo: annotationUndo.length, redo: annotationRedo.length });
  scheduleViewerStateSync(event, { edit_delta: delta });
  wsiRequestDraw(); notify(redo ? 'Redo applied' : 'Undo applied', 'success');
  return true;
}
