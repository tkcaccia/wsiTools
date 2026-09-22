import fs from 'node:fs';
import path from 'node:path';
import { createRequire } from 'node:module';
import { pathToFileURL } from 'node:url';
import os from 'node:os';
import crypto from 'node:crypto';

export const repo = path.resolve(import.meta.dirname, '../../..');
export const deps = process.env.ANNOTATION_AUDIT_DEPS || import.meta.dirname;
export const requireDep = createRequire(path.join(deps, 'package.json'));
export const dep = name => import(pathToFileURL(requireDep.resolve(name)).href);
export const now = () => performance.now();
export const median = a => [...a].sort((x,y) => x-y)[Math.floor(a.length / 2)];
export const stats = a => ({ median_ms: median(a), min_ms: Math.min(...a), max_ms: Math.max(...a), n: a.length });
export async function timed(fn, n = 3) {
  const times = []; let value;
  for (let i = 0; i < n; i++) { global.gc?.(); const t = now(); value = await fn(); times.push(now()-t); }
  return { ...stats(times), value };
}
export const timing = result => { const { value, ...metrics } = result; return metrics; };
export const polygons = f => f.geometry?.type === 'Polygon' ? [f.geometry.coordinates] :
  f.geometry?.type === 'MultiPolygon' ? f.geometry.coordinates : [];
export const pointsIn = f => polygons(f).reduce((n,p) => n + p.reduce((s,r) => s+r.length,0),0);
export function boxesFor(features) {
  return features.map((f,id) => {
    let minX=Infinity,minY=Infinity,maxX=-Infinity,maxY=-Infinity;
    for (const p of polygons(f)) for (const ring of p) for (const [x,y] of ring) {
      minX=Math.min(minX,x); minY=Math.min(minY,y); maxX=Math.max(maxX,x); maxY=Math.max(maxY,y);
    }
    return {minX,minY,maxX,maxY,id};
  });
}
export function boundsOf(boxes) {
  return boxes.reduce((b,x)=>[Math.min(b[0],x.minX),Math.min(b[1],x.minY),Math.max(b[2],x.maxX),Math.max(b[3],x.maxY)], [Infinity,Infinity,-Infinity,-Infinity]);
}
export const overlaps = (b,q) => b.minX<=q[2] && b.maxX>=q[0] && b.minY<=q[3] && b.maxY>=q[1];
export function pack(features) {
  const coords = new Float64Array(features.reduce((n,f)=>n+pointsIn(f)*2,0));
  const ringOffsets=[0], polygonOffsets=[0], featureOffsets=[0]; let k=0;
  for (const f of features) {
    for (const p of polygons(f)) {
      for (const ring of p) { for (const xy of ring) { coords[k++]=xy[0];coords[k++]=xy[1]; } ringOffsets.push(k/2); }
      polygonOffsets.push(ringOffsets.length-1);
    }
    featureOffsets.push(polygonOffsets.length-1);
  }
  return {coords,ringOffsets:Uint32Array.from(ringOffsets),polygonOffsets:Uint32Array.from(polygonOffsets),featureOffsets:Uint32Array.from(featureOffsets)};
}
export function load(file) {
  const start=now(), bytes=fs.readFileSync(file), readMs=now()-start;
  global.gc?.(); const baseline=process.memoryUsage(), text=bytes.toString('utf8'), t=now();
  const object=JSON.parse(text), parseMs=now()-t; global.gc?.();
  const features=(Array.isArray(object)?object:object.features||[object]).filter(f=>polygons(f).length);
  const boxes=boxesFor(features), bounds=boundsOf(boxes), counts=features.map(pointsIn);
  return {text,features,boxes,bounds,object, summary:{file:path.basename(file),sha256:crypto.createHash('sha256').update(bytes).digest('hex'),
    bytes:bytes.length,features:features.length,vertices:counts.reduce((a,b)=>a+b,0),largest_feature_vertices:counts.reduce((a,b)=>Math.max(a,b),0),bounds,
    first_read_ms:readMs,first_parse_ms:parseMs,heap_delta_text_and_object:process.memoryUsage().heapUsed-baseline.heapUsed}};
}
export function queries(bounds, width, n=100) {
  let state=123456789;
  const random=()=>{state=(Math.imul(state,1664525)+1013904223)>>>0;return state/4294967296;};
  return Array.from({length:n},()=>{
    const x=bounds[0]+random()*Math.max(0,bounds[2]-bounds[0]-width),y=bounds[1]+random()*Math.max(0,bounds[3]-bounds[1]-width);
    return [x,y,x+width,y+width];
  });
}
export function save(file, result) {
  const packageInfo=JSON.parse(fs.readFileSync(path.join(deps,'package.json'),'utf8'));
  const versions=Object.fromEntries(Object.keys(packageInfo.dependencies||{}).map(name=>{
    const p=path.join(deps,'node_modules',name,'package.json');
    return [name,JSON.parse(fs.readFileSync(p,'utf8')).version];
  }));
  const provenance=Object.fromEntries(['inst/viewer/performance.js','inst/viewer/geometry-worker.js','inst/viewer/clipper.js','inst/viewer/sync.js','inst/wasm/wsi_overlay_core.wasm'].map(name=>
    [name,crypto.createHash('sha256').update(fs.readFileSync(path.join(repo,name))).digest('hex')]));
  fs.mkdirSync(path.dirname(file),{recursive:true});
  fs.writeFileSync(file,JSON.stringify({date:new Date().toISOString(),environment:{node:process.version,os:os.release(),arch:os.arch(),cpu:os.cpus()[0].model,logical_cpus:os.cpus().length,ram_bytes:os.totalmem(),versions},provenance,...result},null,2));
  console.log(file);
}
