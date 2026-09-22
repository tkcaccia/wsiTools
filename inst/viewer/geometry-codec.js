// Packed transport only. Canonical coordinates remain Float64 slide pixels.
function wsiPackPolygons(polygons) {
  let points = 0, rings = 0;
  for (const polygon of polygons) for (const ring of polygon) { points += ring.length; rings++; }
  const xy = new Float64Array(points * 2), ringOffsets = new Uint32Array(rings + 1), polygonOffsets = new Uint32Array(polygons.length + 1);
  let p = 0, r = 0, g = 0;
  for (const polygon of polygons) {
    for (const ring of polygon) {
      for (const point of ring) { xy[p++] = point[0]; xy[p++] = point[1]; }
      ringOffsets[++r] = p / 2;
    }
    polygonOffsets[++g] = r;
  }
  return { format: 'wsi-polygons-1', xy, ringOffsets, polygonOffsets };
}

function wsiUnpackPolygons(value) {
  if (!value || value.format !== 'wsi-polygons-1') return value;
  const { xy, ringOffsets, polygonOffsets } = value, polygons = [];
  if (xy.length % 2 || ringOffsets[0] !== 0 || polygonOffsets[0] !== 0 ||
      ringOffsets[ringOffsets.length - 1] !== xy.length / 2 || polygonOffsets[polygonOffsets.length - 1] !== ringOffsets.length - 1) {
    throw new Error('Invalid packed annotation geometry.');
  }
  for (let i = 1; i < polygonOffsets.length; i++) {
    const polygon = [];
    if (polygonOffsets[i] < polygonOffsets[i - 1]) throw new Error('Invalid polygon offsets.');
    for (let j = polygonOffsets[i - 1]; j < polygonOffsets[i]; j++) {
      const ring = [];
      if (ringOffsets[j + 1] < ringOffsets[j]) throw new Error('Invalid ring offsets.');
      for (let k = ringOffsets[j] * 2; k < ringOffsets[j + 1] * 2; k += 2) ring.push([xy[k], xy[k + 1]]);
      polygon.push(ring);
    }
    polygons.push(polygon);
  }
  return polygons;
}

function wsiPolygonBuffers(value) {
  if (value && value.format === 'wsi-ring-patch-1') return wsiPolygonBuffers(value.changed);
  return value && value.format === 'wsi-polygons-1' ? [value.xy.buffer, value.ringOffsets.buffer, value.polygonOffsets.buffer] : [];
}

function wsiPackRingPatch(polygons, previous) {
  if (!previous) return wsiPackPolygons(polygons);
  const refs = new Map(), changed = [];
  let index = 0;
  for (const polygon of previous) for (const ring of polygon) refs.set(ring, index++);
  const rings = polygons.map(polygon => polygon.map(ring => {
    if (refs.has(ring)) return refs.get(ring);
    changed.push([ring]); return -changed.length;
  }));
  return { format: 'wsi-ring-patch-1', rings, changed: wsiPackPolygons(changed) };
}
