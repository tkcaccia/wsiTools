let geometryWasm = null, geometryKernelFailure = null;
const geometryPrepared = new WeakMap();
const geometryResident = new WeakSet();
let geometryVertexTag = 1;
const geometryKernelReady = (async () => {
  if (typeof Clipper2ZFactory !== 'function' || typeof wsiClipper2Binary === 'undefined') return;
  try {
    geometryWasm = await Clipper2ZFactory({ wasmBinary: Uint8Array.from(atob(wsiClipper2Binary), c => c.charCodeAt(0)) });
  } catch (error) { geometryKernelFailure = String(error.message || error); }
})();

function geometryPrepare(polygons) {
  const c = geometryWasm, paths = new c.PathsD(), originals = [], firstTag = geometryVertexTag;
  let vertices = 0;
  try {
    for (const polygon of polygons) for (let i = 0; i < polygon.length; i++) {
      const ring = polygon[i], flat = new Float64Array(ring.length * 3);
      let area = 0;
      for (let j = 0; j < ring.length; j++) {
        const p = ring[j], q = ring[(j + 1) % ring.length];
        const x = Math.round(p[0] * 4096), y = Math.round(p[1] * 4096);
        if (!Number.isSafeInteger(x) || !Number.isSafeInteger(y) || Math.abs(x) > 4e15 || Math.abs(y) > 4e15) throw new Error('Annotation coordinates exceed clipping range.');
        flat[j * 3] = x; flat[j * 3 + 1] = y; flat[j * 3 + 2] = geometryVertexTag++;
        originals.push(p); area += p[0] * q[1] - q[0] * p[1];
      }
      // assign() copies the raw XYZ buffer. Z is opaque provenance, not geometry.
      const path = new c.PathD(); path.assign(flat);
      try { if ((area > 0) !== (i === 0)) c.ReversePathD(path); paths.push_back(path); }
      finally { path.delete(); }
      vertices += ring.length;
    }
    return { paths, originals, vertices, firstTag };
  } catch (error) { paths.delete(); throw error; }
}

function geometryRelease(polygons) {
  const data = geometryPrepared.get(polygons);
  if (data) { data.paths.delete(); geometryPrepared.delete(polygons); }
  geometryResident.delete(polygons);
}

function geometryRetain(polygons) {
  // Compile lazily: an outgoing result may never be edited again.
  geometryResident.add(polygons);
}

function geometryClipWasm(operation, subject, clips) {
  const c = geometryWasm, inputs = [subject, ...clips], temporary = [], prepared = [];
  let engine, tree;
  try {
    for (const polygons of inputs) {
      let data = geometryPrepared.get(polygons);
      if (!data) {
        data = geometryPrepare(polygons);
        if (geometryResident.has(polygons)) geometryPrepared.set(polygons, data);
        else temporary.push(data);
      }
      prepared.push(data);
    }
    engine = new c.ClipperD(0); engine.SetPreserveCollinear(true);
    engine.AddSubject(prepared[0].paths);
    for (const data of prepared.slice(1)) engine.AddClip(data.paths);
    tree = new c.PolyPathD();
    const type = { ctUnion: c.ClipType.Union, ctDifference: c.ClipType.Difference, ctIntersection: c.ClipType.Intersection, ctXor: c.ClipType.Xor }[operation];
    if (!type || !engine.ExecutePoly(type, c.FillRule.NonZero, tree)) throw new Error('Annotation clipping did not complete.');
    const output = [];
    const readRing = node => {
      const path = node.polygon();
      try {
        const flat = path.view(), ring = [];
        // The Z-enabled distribution exposes x/y/z triples, not x/y pairs.
        for (let i = 0; i < flat.length; i += 3) {
          const x = Math.round(flat[i]), y = Math.round(flat[i + 1]), tag = flat[i + 2];
          let original;
          for (const data of prepared) {
            const index = tag - data.firstTag;
            if (Number.isInteger(index) && index >= 0 && index < data.originals.length) {
              const p = data.originals[index];
              if (Math.round(p[0] * 4096) === x && Math.round(p[1] * 4096) === y) original = p;
              break;
            }
          }
          ring.push(original || [x / 4096, y / 4096]);
        }
        if (ring.length) ring.push(ring[0]);
        return ring;
      } finally { path.delete(); }
    };
    const visit = parent => {
      for (let i = 0; i < parent.count(); i++) {
        // child() is borrowed from the tree. Deleting it double-frees native memory.
        const outer = parent.child(i), polygon = [readRing(outer)];
        for (let j = 0; j < outer.count(); j++) {
          const hole = outer.child(j); polygon.push(readRing(hole)); visit(hole);
        }
        if (polygon[0].length >= 4) output.push(polygon);
      }
    };
    visit(tree);
    return output;
  } finally {
    if (tree) tree.delete(); if (engine) engine.delete();
    for (const data of temporary) data.paths.delete();
  }
}
