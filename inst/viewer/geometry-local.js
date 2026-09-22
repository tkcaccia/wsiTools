// Unchanged components and holes are immutable and need not enter clipping.
const geometryRingBoxes = new WeakMap();
function geometryRingBox(ring) {
  let b = geometryRingBoxes.get(ring);
  if (b) return b;
  b = [Infinity, Infinity, -Infinity, -Infinity];
  for (const p of ring) { b[0] = Math.min(b[0], p[0]); b[1] = Math.min(b[1], p[1]); b[2] = Math.max(b[2], p[0]); b[3] = Math.max(b[3], p[1]); }
  geometryRingBoxes.set(ring, b); return b;
}

function geometryRingContains(ring, p) {
  let inside = false;
  for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
    const a = ring[i], b = ring[j];
    if ((a[1] > p[1]) !== (b[1] > p[1]) && p[0] < (b[0] - a[0]) * (p[1] - a[1]) / (b[1] - a[1]) + a[0]) inside = !inside;
  }
  return inside;
}

function geometryLocalCandidates(polygons, bounds) {
  return polygons.filter(p => geometryOverlaps(geometryRingBox(p[0]), bounds))
    .map(p => p.filter((ring, i) => i === 0 || geometryOverlaps(geometryRingBox(ring), bounds)));
}

function geometryLocalClip(operation, subject, clip) {
  if (!subject.length || !clip.length) return geometryClip(operation, subject, clip);
  const bounds = geometryBounds(clip), affected = [], untouched = [], holes = [];
  for (const polygon of subject) {
    if (!geometryOverlaps(geometryRingBox(polygon[0]), bounds)) { untouched.push(polygon); continue; }
    const rings = [polygon[0]];
    for (const ring of polygon.slice(1)) {
      if (geometryOverlaps(geometryRingBox(ring), bounds)) rings.push(ring); else holes.push(ring);
    }
    affected.push(rings.length === polygon.length ? polygon : rings);
  }
  if (!affected.length) return operation === 'ctUnion' ? subject.concat(clip) : operation === 'ctDifference' ? subject : [];
  const input = affected.length === subject.length && affected.every((p, i) => p === subject[i]) ? subject : affected;
  const output = geometryClip(operation, input, clip);
  if (operation === 'ctIntersection') return output;
  // Edits outside a hole's bounding box cannot remove that hole. Only a split
  // requires finding its new owning component; the common one-component case
  // retains the hole without walking the outer boundary again.
  for (const hole of holes) {
    let owner = output.length === 1 ? output[0] : null;
    if (!owner) {
      const h = geometryRingBox(hole);
      const matches = output.filter(p => {
        const b = geometryRingBox(p[0]);
        return h[0] >= b[0] && h[1] >= b[1] && h[2] <= b[2] && h[3] <= b[3] && geometryRingContains(p[0], hole[0]);
      });
      if (matches.length !== 1) return geometryClip(operation, subject, clip);
      owner = matches[0];
    }
    owner.push(hole);
  }
  return output.concat(untouched);
}
