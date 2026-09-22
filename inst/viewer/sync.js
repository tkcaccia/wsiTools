const wsiFeatureCache = new WeakMap();
const wsiSyncClient = 'viewer_' + Date.now().toString(36) + '_' + Math.random().toString(36).slice(2);
let wsiSyncDocument = null, wsiSyncAcknowledged = null, wsiSyncRevision = 0;
let wsiSyncQueue = [], wsiSyncSending = false, wsiSyncSocketWait = null;
const wsiSyncMetrics = { messages: 0, bytes: 0, full_snapshots: 0, annotation_upserts: 0 };

function wsiCachedFeature(roi, index) {
  wsiRoiId(roi);
  const snapshot = wsiSnapshotRoi(roi), old = wsiFeatureCache.get(roi);
  if (old && old.snapshot === snapshot) return old.feature;
  const feature = wsiUncachedRoiFeature(roi, index);
  wsiFeatureCache.set(roi, { snapshot, feature });
  return feature;
}

function wsiSyncView() {
  return { mode, scale, offset_x: offsetX, offset_y: offsetY, roi_opacity: roiOpacity,
    show_rois: showRois, show_labels: showLabels, overlay_focus: overlayFocusMode,
    image_transform: imageTransformPayload(), base_layer: baseImagePayload() };
}

function wsiSyncCapture(event, detail) {
  const key = wsiProjectKey();
  const viewOnly = event === 'viewport_changed';
  const selectionOnly = /^(roi_selected|roi_deselected|roi_export_selection_updated|brush_selection_updated)$/.test(event);
  const annotation = /^(roi_|rois_|brush_committed|geojson_imported|annotations_|annotation_(history_|undo|redo))/.test(event);
  const logsOnly = /^viewer_log_/.test(event);
  const forceFull = !wsiSyncDocument || wsiSyncDocument.key !== key || !(viewOnly || selectionOnly || annotation || logsOnly);
  let changes, explicitPatch = null;
  const baseRois = wsiSyncDocument && wsiSyncDocument.document.rois;
  if (forceFull) changes = wsiFullViewerStatePayload(event, detail);
  else {
    changes = { event, detail, time: new Date().toISOString(), sequence: ++stateSyncSeq,
      slide: { title: cfg.title, width: cfg.slide_width, height: cfg.slide_height },
      project: projectStatePayload(), view: wsiSyncView(), performance: viewerPerformancePayload() };
    if (annotation || selectionOnly) {
      changes.selected_index = selectedRoi;
      changes.selected_object = selectedObjectPayload();
    }
    if (annotation) {
      const delta = detail && detail.edit_delta;
      if (delta && baseRois) {
        const upsert = delta.upsert.map(id => {
          const index = rois.findIndex(roi => wsiRoiId(roi) === id);
          return index < 0 ? null : roiFeature(rois[index], index);
        }).filter(Boolean);
        const updates = new Map(upsert.map(feature => [String(feature.id), feature])), remove = new Set(delta.remove);
        const previous = new Map(baseRois.features.map(feature => [String(feature.id), feature]));
        const features = delta.order_changed ? rois.map(roi => updates.get(wsiRoiId(roi)) || previous.get(wsiRoiId(roi))).filter(Boolean) :
          baseRois.features.filter(feature => !remove.has(String(feature.id))).map(feature => updates.get(String(feature.id)) || feature);
        changes.rois = { type: 'FeatureCollection', features };
        explicitPatch = { upsert, remove: delta.remove };
        if (delta.order_changed) explicitPatch.order = features.map(feature => String(feature.id));
      } else changes.rois = roiGeojsonObject();
      changes.history = annotationHistoryPayload();
      changes.annotations = { dirty: !!(annotationsDirty || projectDirty),
        dirty_reason: annotationDirtyReason || projectDirtyReason, annotation_dirty: annotationsDirty, project_dirty: projectDirty };
      // Undo may also restore trajectories and measurements.
      changes.trajectories = trajectoryPayload(); changes.measurements = measures.slice();
    }
    if (logsOnly) changes.logs = viewerLogPayload();
  }
  const selectedIds = roiExportIndices().map(i => wsiRoiId(rois[i]));
  const selectedId = selectedRoi >= 0 && rois[selectedRoi] ? wsiRoiId(rois[selectedRoi]) : null;
  const document = forceFull ? changes : Object.assign({}, wsiSyncDocument.document, changes);
  wsiSyncDocument = { key, document };
  return { key, document, changes, forceFull, selectedIds, selectedId, baseRois, explicitPatch };
}

function wsiSyncMessage(capture, forceFull = false) {
  const previous = wsiSyncAcknowledged;
  const full = forceFull || capture.forceFull || !previous || previous.key !== capture.key;
  const payload = Object.assign({}, full ? capture.document : capture.changes);
  const revision = ++wsiSyncRevision;
  const sync = { version: 1, client: wsiSyncClient, revision,
    base_revision: previous ? previous.revision : 0, project_key: capture.key, full,
    selected_ids: capture.selectedIds, selected_id: capture.selectedId };
  if (!full) {
    delete payload.rois; delete payload.selected_roi; delete payload.selected_rois;
    if (capture.explicitPatch && previous.document.rois === capture.baseRois) {
      sync.rois_patch = capture.explicitPatch;
    } else if (previous.document.rois !== capture.document.rois) {
      const before = new Map(previous.document.rois.features.map(f => [String(f.id), f]));
      const after = new Map(capture.document.rois.features.map(f => [String(f.id), f]));
      const upsert = Array.from(after.values()).filter(f => before.get(String(f.id)) !== f);
      const remove = Array.from(before.keys()).filter(id => !after.has(id));
      if (upsert.length || remove.length) sync.rois_patch = { upsert, remove, order: Array.from(after.keys()) };
    }
    if (sync.rois_patch) wsiSyncMetrics.annotation_upserts += sync.rois_patch.upsert.length;
  }
  payload.sync = sync;
  if (full) wsiSyncMetrics.full_snapshots++;
  return payload;
}

function wsiSyncSocketResponse(body) {
  if (!wsiSyncSocketWait) return;
  const ack = body && body.sync_ack;
  if (body && body.ok === false || ack && ack.client === wsiSyncClient && ack.revision === wsiSyncSocketWait.revision) {
    const pending = wsiSyncSocketWait; wsiSyncSocketWait = null;
    clearTimeout(pending.timer); pending.resolve(body);
  }
}

async function wsiSyncTransport(payload) {
  const text = JSON.stringify(payload);
  wsiSyncMetrics.messages++; wsiSyncMetrics.bytes += text.length;
  if (stateSocketReady && stateSocket && stateSocket.readyState === WebSocket.OPEN) {
    const body = await new Promise(resolve => {
      const timer = setTimeout(() => { wsiSyncSocketWait = null; resolve(null); }, 4000);
      wsiSyncSocketWait = { revision: payload.sync.revision, resolve, timer };
      try { stateSocket.send(text); }
      catch (error) { clearTimeout(timer); wsiSyncSocketWait = null; resolve(null); }
    });
    if (body) return body;
  }
  const response = await fetch(cfg.viewer_state_url, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: text });
  const body = await response.json();
  if (!response.ok && !body.error) throw new Error('R synchronization returned HTTP ' + response.status);
  handleViewerCommands(body);
  return body;
}

async function wsiDrainSyncQueue() {
  if (wsiSyncSending) return;
  wsiSyncSending = true;
  try {
    while (wsiSyncQueue.length) {
      const entry = wsiSyncQueue.shift();
      try {
        let payload = wsiSyncMessage(entry.capture), body = await wsiSyncTransport(payload);
        if (body && body.ok === false && /snapshot|revision mismatch/i.test(body.error || '')) {
          payload = wsiSyncMessage(entry.capture, true); body = await wsiSyncTransport(payload);
        }
        if (!body || body.ok === false) throw new Error(body && body.error || 'No response from R');
        if (!body.sync_ack || body.sync_ack.client !== wsiSyncClient || body.sync_ack.revision !== payload.sync.revision) {
          throw new Error('R did not acknowledge the annotation revision. Restart the viewer with the updated wsiTools package.');
        }
        wsiSyncAcknowledged = { key: entry.capture.key, document: entry.capture.document, revision: payload.sync.revision };
        recordViewerSyncHistory('to_R', payload.event, Object.assign({ transport: stateSocketReady ? 'websocket' : 'polling',
          project_key: entry.capture.key, revision: payload.sync.revision, full_snapshot: payload.sync.full }, payload.detail));
        syncMessage(viewerAutosaveMessage(body) || 'R sync: ' + payload.event);
        entry.resolve(true);
      } catch (error) {
        wsiSyncAcknowledged = null;
        recordViewerSyncHistory('error', entry.capture.changes.event, { message: error.message });
        syncMessage('R sync failed: ' + error.message); entry.resolve(false);
      }
    }
  } finally { wsiSyncSending = false; }
}

function wsiQueueSync(event = 'viewer_state', detail = {}) {
  if (!liveSyncAvailable()) return Promise.resolve(false);
  const capture = wsiSyncCapture(event, detail);
  return new Promise(resolve => {
    const last = wsiSyncQueue[wsiSyncQueue.length - 1];
    // Only obsolete camera positions are coalesced; annotation events retain order.
    if (event === 'viewport_changed' && last && last.capture.changes.event === event && last.capture.key === capture.key) {
      last.resolve(true); wsiSyncQueue.pop();
    }
    wsiSyncQueue.push({ capture, resolve });
    queueMicrotask(wsiDrainSyncQueue);
  });
}
