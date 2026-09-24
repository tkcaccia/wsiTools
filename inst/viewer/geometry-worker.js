/* Geometry is expressed in original slide pixels, independent of display zoom. */
const geometrySources = new Map();
let geometryProjectKey = null;
const geometryCacheBudget = 128 * 1024 * 1024;

function geometryClip(operation, subject, ...clips) {
  if (typeof geometryWasm !== 'undefined' && geometryWasm) return geometryClipWasm(operation, subject, clips);
  const precision = 4096, originals = new Map();
  const paths = polygons => polygons.flatMap(polygon => polygon.map((ring, i) => {
    const path = ring.map(p => {
      const X = Math.round(p[0] * precision), Y = Math.round(p[1] * precision);
      if (!Number.isSafeInteger(X) || !Number.isSafeInteger(Y) || Math.abs(X) > 4e15 || Math.abs(Y) > 4e15) throw new Error('Annotation coordinates exceed clipping range.');
      const key = X + ',' + Y;
      if (!originals.has(key)) originals.set(key, p);
      return { X, Y };
    });
    if (ClipperLib.Clipper.Orientation(path) !== (i === 0)) path.reverse();
    return path;
  }));
  const engine = new ClipperLib.Clipper(ClipperLib.Clipper.ioPreserveCollinear | ClipperLib.Clipper.ioStrictlySimple);
  engine.AddPaths(paths(subject), ClipperLib.PolyType.ptSubject, true);
  for (const clip of clips) engine.AddPaths(paths(clip), ClipperLib.PolyType.ptClip, true);
  const tree = new ClipperLib.PolyTree();
  if (!engine.Execute(ClipperLib.ClipType[operation], tree, ClipperLib.PolyFillType.pftNonZero, ClipperLib.PolyFillType.pftNonZero)) return [];
  return ClipperLib.JS.PolyTreeToExPolygons(tree).map(polygon => [polygon.outer, ...polygon.holes].map(path => {
    const ring = path.map(p => originals.get(p.X + ',' + p.Y) || [p.X / precision, p.Y / precision]);
    if (ring.length) ring.push(ring[0]);
    return ring;
  }));
}

const polygonClipping = {
  union: (...polygons) => geometryClip('ctUnion', polygons[0] || [], ...polygons.slice(1)),
  intersection: (subject, clip) => geometryClip('ctIntersection', subject, clip),
  difference: (subject, ...clips) => geometryClip('ctDifference', subject, ...clips)
};

function geometryBounds(polygons) {
  const b = [Infinity, Infinity, -Infinity, -Infinity];
  for (const polygon of polygons) for (const ring of polygon) for (const p of ring) {
    b[0] = Math.min(b[0], p[0]); b[1] = Math.min(b[1], p[1]);
    b[2] = Math.max(b[2], p[0]); b[3] = Math.max(b[3], p[1]);
  }
  return b;
}

function geometryOverlaps(a, b) {
  return a[0] <= b[2] && a[2] >= b[0] && a[1] <= b[3] && a[3] >= b[1];
}

function geometryUnion(polygons) {
  let batch = polygons.filter(p => p.length);
  while (batch.length > 1) {
    const next = [];
    for (let i = 0; i < batch.length; i += 16) {
      next.push(polygonClipping.union(...batch.slice(i, i + 16)));
    }
    batch = next;
  }
  return batch[0] || [];
}

function geometryRingArea(ring) {
  let area = 0;
  for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
    area += ring[j][0] * ring[i][1] - ring[i][0] * ring[j][1];
  }
  return Math.abs(area) / 2;
}

function geometryHoleExisted(hole, originalHoles) {
  if (!hole.length) return false;
  const sample = hole[0];
  return originalHoles.some(original => original.length &&
    (geometryRingContains(original, sample) || geometryRingContains(hole, original[0])));
}

// Wand unions can close a one-pixel channel and leave a tiny artificial hole.
// Preserve every hole already present in the target, and remove only new holes
// below a display-derived area threshold.
function geometryCleanWandHoles(polygons, original, threshold) {
  threshold = Number(threshold);
  if (!Number.isFinite(threshold) || threshold <= 0 || !polygons.length) {
    return { geometry: polygons, filled: 0 };
  }
  const originalHoles = (original || []).flatMap(polygon => polygon.slice(1));
  let filled = 0;
  const geometry = polygons.map(polygon => {
    if (polygon.length < 2) return polygon;
    const holes = polygon.slice(1).filter(hole => {
      const artifact = geometryRingArea(hole) <= threshold &&
        !geometryHoleExisted(hole, originalHoles);
      if (artifact) filled++;
      return !artifact;
    });
    return [polygon[0], ...holes];
  });
  return { geometry, filled };
}

function geometryStroke(points, radius, zoom) {
  const tolerance = Math.min(radius * .05, .35 / Math.max(.0001, zoom));
  const steps = Math.max(24, Math.min(192, Math.ceil(Math.PI / Math.acos(1 - tolerance / radius))));
  const circle = p => {
    const ring = [];
    for (let i = 0; i < steps; i++) {
      const angle = i * 2 * Math.PI / steps;
      ring.push([p.x + radius * Math.cos(angle), p.y + radius * Math.sin(angle)]);
    }
    ring.push(ring[0]);
    return [[ring]];
  };
  const shapes = points.map(circle);
  for (let i = 1; i < points.length; i++) {
    const a = points[i - 1], b = points[i], dx = b.x - a.x, dy = b.y - a.y;
    const length = Math.hypot(dx, dy);
    if (!length) continue;
    const nx = -dy * radius / length, ny = dx * radius / length;
    const ring = [[a.x + nx, a.y + ny], [b.x + nx, b.y + ny],
      [b.x - nx, b.y - ny], [a.x - nx, a.y - ny]];
    ring.push(ring[0]);
    shapes.push([[ring]]);
  }
  return geometryUnion(shapes);
}

function geometryEdit(task) {
  const started = performance.now();
  const active = new Set(task.active_ids);
  const sources = Array.from(geometrySources.values()).filter(source => active.has(source.id));
  const target = sources.find(source => source.id === task.target_id);
  const width = task.width, height = task.height;
  const extent = [[[[0, 0], [width, 0], [width, height], [0, height], [0, 0]]]];
  let stroke = polygonClipping.intersection(geometryStroke(task.points, task.radius, task.zoom), extent);
  if (!stroke.length) return { geometry: [], removed: [], empty: true };
  const strokeBounds = geometryBounds(stroke), removed = [], updates = [];
  if (task.operation !== 'subtract') {
    const protection = (task.protection || []).slice();
    for (const source of sources) {
      if (source.id !== task.target_id && (source.locked || (!task.priority_claim && source.class_key !== task.class_key)) &&
          geometryOverlaps(source.bounds, strokeBounds)) protection.push(source.geometry);
    }
    if (protection.length) stroke = polygonClipping.difference(stroke, ...protection.map(g => geometryLocalCandidates(g, strokeBounds)));
  }
  let result = target
    ? geometryLocalClip(task.operation === 'subtract' ? 'ctDifference' : 'ctUnion', target.geometry, stroke)
    : stroke;
  if (task.operation !== 'subtract' && result.length && !task.priority_claim) {
    let changed = true;
    while (changed) {
      changed = false;
      const bounds = geometryBounds(result);
      for (const source of sources) {
        if (source.id === task.target_id || removed.includes(source.id) ||
            source.locked || source.class_key !== task.class_key || !geometryOverlaps(source.bounds, bounds)) continue;
        if (polygonClipping.intersection(result, source.geometry).length) {
          result = polygonClipping.union(result, source.geometry);
          removed.push(source.id); changed = true;
        }
      }
    }
  }
  if (task.priority_claim && task.operation !== 'subtract' && result.length && stroke.length) {
    const claimBounds = geometryBounds(stroke);
    for (const source of sources) {
      if (source.id === task.target_id || source.locked || !geometryOverlaps(source.bounds, claimBounds)) continue;
      if (!geometryLocalClip('ctIntersection', source.geometry, stroke).length) continue;
      const reduced = geometryLocalClip('ctDifference', source.geometry, stroke);
      if (reduced.length) updates.push({ id: source.id, geometry: reduced });
      else removed.push(source.id);
    }
  }
  return { geometry: result, removed, updates, empty: !result.length, duration_ms: performance.now() - started };
}

function geometryClaim(task) {
  const started = performance.now(), sources = (task.active_ids || (task.sources || []).map(s => s.id)).map(id => geometrySources.get(id)).filter(Boolean), removed = [], updates = [];
  let claim = task.geometry || [];
  if (!claim.length) return { geometry: [], removed, updates, empty: true, duration_ms: performance.now() - started };
  const target = sources.find(source => source.id === task.target_id);
  const immutable = (task.protection || []).slice();
  for (const source of sources) if (source.id !== task.target_id && source.locked) immutable.push(source.geometry);
  if (immutable.length) claim = polygonClipping.difference(claim, ...immutable.map(g => geometryLocalCandidates(g, geometryBounds(claim))));
  if (!claim.length) return { geometry: [], removed, updates, empty: true, duration_ms: performance.now() - started };
  let result = target ? geometryLocalClip('ctUnion', target.geometry, claim) : claim;
  const claimBounds = geometryBounds(claim);
  for (const source of sources) {
    if (source.id === task.target_id || source.locked || !geometryOverlaps(geometryBounds(source.geometry), claimBounds)) continue;
    if (!geometryLocalClip('ctIntersection', source.geometry, claim).length) continue;
    const reduced = geometryLocalClip('ctDifference', source.geometry, claim);
    if (reduced.length) updates.push({ id: source.id, geometry: reduced });
    else removed.push(source.id);
  }
  const cleaned = task.wand_cleanup && target
    ? geometryCleanWandHoles(result, target.geometry, task.hole_area_threshold)
    : { geometry: result, filled: 0 };
  result = cleaned.geometry;
  return { geometry: result, removed, updates, filled_artifact_holes: cleaned.filled,
    empty: false, duration_ms: performance.now() - started };
}

function geometryWandEdit(task) {
  const started = performance.now(), active = new Set(task.active_ids || []);
  const sources = Array.from(geometrySources.values()).filter(source => active.has(source.id));
  const target = sources.find(source => source.id === task.target_id), removed = [], updates = [];
  let selection = task.geometry || [];
  if (!target || !selection.length) return { geometry: [], removed, updates, empty: true, duration_ms: performance.now() - started };
  if (task.operation === 'subtract') {
    const result = geometryLocalClip('ctDifference', target.geometry, selection);
    return { geometry: result, removed, updates, empty: !result.length, duration_ms: performance.now() - started };
  }
  const selectionBounds = geometryBounds(selection), protection = (task.protection || []).slice();
  for (const source of sources) {
    if (source.id !== task.target_id && (source.locked || source.class_key !== task.class_key) && geometryOverlaps(source.bounds, selectionBounds)) {
      protection.push(geometryLocalCandidates(source.geometry, selectionBounds));
    }
  }
  if (protection.length) selection = polygonClipping.difference(selection, ...protection);
  if (!selection.length) return { geometry: target.geometry, removed, updates, empty: false, duration_ms: performance.now() - started };
  let result = geometryLocalClip('ctUnion', target.geometry, selection), changed = true;
  while (changed) {
    changed = false;
    const bounds = geometryBounds(result);
    for (const source of sources) {
      if (source.id === task.target_id || removed.includes(source.id) || source.locked ||
          source.class_key !== task.class_key || !geometryOverlaps(source.bounds, bounds)) continue;
      if (geometryLocalClip('ctIntersection', result, source.geometry).length) {
        result = geometryLocalClip('ctUnion', result, source.geometry);
        removed.push(source.id); changed = true;
      }
    }
  }
  const cleaned = geometryCleanWandHoles(result, target.geometry, task.hole_area_threshold);
  result = cleaned.geometry;
  return { geometry: result, removed, updates, filled_artifact_holes: cleaned.filled,
    empty: !result.length, duration_ms: performance.now() - started };
}

function geometryStore(source) {
  const old = geometrySources.get(source.id);
  if (old) { if (typeof geometryRelease === 'function') geometryRelease(old.geometry); geometrySources.delete(source.id); }
  const geometry = wsiUnpackPolygons(source.geometry);
  const vertices = geometry.reduce((n, p) => n + p.reduce((s, r) => s + r.length, 0), 0);
  geometrySources.set(source.id, { ...source, geometry, bounds: geometryBounds(geometry), bytes: vertices * 144 });
  if (vertices * 144 < geometryCacheBudget && typeof geometryRetain === 'function') geometryRetain(geometry);
}

function geometryEvict(active) {
  let bytes = Array.from(geometrySources.values()).reduce((n, s) => n + s.bytes, 0);
  // Evict cold sources first; exceptionally large current edits are not retained.
  const candidates = [...geometrySources.keys()].sort((a, b) => Number(active.has(a)) - Number(active.has(b)));
  for (const id of candidates) {
    if (bytes <= geometryCacheBudget) break;
    const source = geometrySources.get(id); bytes -= source.bytes;
    if (typeof geometryRelease === 'function') geometryRelease(source.geometry);
    geometrySources.delete(id);
  }
  return bytes;
}

let geometryQueue = Promise.resolve();
self.onmessage = event => { geometryQueue = geometryQueue.then(async () => {
  const { id, task } = event.data;
  try {
    if (task.type === 'parse_geojson') { self.postMessage({ id, ok: true, result: JSON.parse(task.text) }); return; }
    if (typeof geometryKernelReady !== 'undefined') await geometryKernelReady;
    if (geometryProjectKey !== (task.project_key || 'slide')) {
      for (const source of geometrySources.values()) if (typeof geometryRelease === 'function') geometryRelease(source.geometry);
      geometrySources.clear(); geometryProjectKey = task.project_key || 'slide';
    }
    const active = new Set(task.active_ids || (task.sources || []).map(source => source.id));
    for (const source of task.sources || []) geometryStore(source);
    for (const key of active) {
      const source = geometrySources.get(key);
      if (!source) throw new Error('Annotation worker cache is missing ' + key + '. Retry the edit.');
      geometrySources.delete(key); geometrySources.set(key, source);
    }
    task.geometry = wsiUnpackPolygons(task.geometry);
    task.protection = (task.protection || []).map(wsiUnpackPolygons);
    const baselines = new Map(Array.from(active, id => [id, geometrySources.get(id).geometry]));
    const result = task.type === 'claim' ? geometryClaim(task) : task.type === 'wand_edit' ? geometryWandEdit(task) : geometryEdit(task);
    const changed = [...(result.updates || []), ...(task.target_id && result.geometry.length ? [{ id: task.target_id, geometry: result.geometry }] : [])];
    for (const update of changed) {
      const source = geometrySources.get(update.id);
      if (source) geometryStore({ ...source, geometry: update.geometry });
    }
    for (const key of result.removed || []) {
      const source = geometrySources.get(key);
      if (source && typeof geometryRelease === 'function') geometryRelease(source.geometry);
      geometrySources.delete(key);
    }
    result.cache_bytes = geometryEvict(active);
    result.resident_ids = Array.from(geometrySources.keys());
    result.changed_resident_ids = changed.map(update => update.id).concat(result.removed || []);
    result.kernel = typeof geometryWasm !== 'undefined' && geometryWasm ? 'clipper2-wasm' : 'clipper-js';
    result.kernel_warning = typeof geometryKernelFailure !== 'undefined' ? geometryKernelFailure : null;
    const buffers = [];
    if (task.packed) {
      const pack = (geometry, id) => task.ring_patches ? wsiPackRingPatch(geometry, baselines.get(id)) : wsiPackPolygons(geometry);
      result.geometry = pack(result.geometry, task.target_id); buffers.push(...wsiPolygonBuffers(result.geometry));
      for (const update of result.updates || []) { update.geometry = pack(update.geometry, update.id); buffers.push(...wsiPolygonBuffers(update.geometry)); }
    }
    self.postMessage({ id, ok: true, result }, buffers);
  } catch (error) {
    // The client drops its cache signatures after a failure; input will be resent.
    self.postMessage({ id, ok: false, error: String(error && error.message || error) });
  }
}); };
