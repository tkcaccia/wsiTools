import assert from 'node:assert/strict';
import {Worker} from 'node:worker_threads';
import {repo,requireDep,load,polygons,pointsIn,timed,timing,save,now,stats} from './common.mjs';

const data=load(process.argv[2]), result={dataset:data.summary,operations:[]};
const wasmStart=now(),factory=requireDep('clipper2-wasm'),clip=await factory();result.wasm_initialization_ms=now()-wasmStart;
const signedArea=ring=>ring.slice(1).reduce((a,p,i)=>a+ring[i][0]*p[1]-p[0]*ring[i][1],0)/2;
const geometryArea=geometry=>geometry.reduce((sum,p)=>sum+Math.abs(signedArea(p[0]))-p.slice(1).reduce((n,r)=>n+Math.abs(signedArea(r)),0),0);
async function boundedJs(operation,g,claim) {
  const worker=new Worker(new URL('./clipping-worker.cjs',import.meta.url),{workerData:{repo}});
  const result=await new Promise((resolve,reject)=>{
    let timer;
    worker.on('error',reject);
    worker.on('message',message=>{
      if(message.ready)worker.postMessage({operation,g,claim});
      else if(message.started)timer=setTimeout(()=>resolve({timeout_ms:15000}),15000);
      else {clearTimeout(timer);resolve({...stats(message.times),area:message.area});}
    });
  });
  await worker.terminate();return result;
}
function paths(geometry) {
  const out=new clip.PathsD();
  for(const polygon of geometry)for(let i=0;i<polygon.length;i++) {
    let ring=polygon[i];if((signedArea(ring)>0)!==(i===0))ring=[...ring].reverse();
    const p=clip.MakePathD(ring.flat());out.push_back(p);p.delete();
  }
  return out;
}
function extract(solution) {
  let vertices=0;const packed=[];
  for(let i=0;i<solution.size();i++){const p=solution.get(i);packed.push(Float64Array.from(p.view()));vertices+=p.size();p.delete();}
  return {vertices,packed};
}
const ranked=data.features.map((f,i)=>({f,i,n:pointsIn(f)})).sort((a,b)=>b.n-a.n);
for(const record of [ranked[0],ranked[Math.floor(ranked.length*.1)],ranked[Math.floor(ranked.length*.5)]]) {
  const g=polygons(record.f),anchor=g[0][0][0],radius=80;
  const claim=[[[[anchor[0]-radius,anchor[1]-radius],[anchor[0]+radius,anchor[1]-radius],
    [anchor[0]+radius,anchor[1]+radius],[anchor[0]-radius,anchor[1]+radius],[anchor[0]-radius,anchor[1]-radius]]]];
  const operation={feature_index:record.i,vertices:record.n,claim_width_px:160,original_area:geometryArea(g)};
  for(const [name,jsType,wasmName] of [['union','ctUnion','UnionD'],['subtract','ctDifference','DifferenceD']]) {
    console.log('Benchmark operation',record.n,name);
    const js=await boundedJs(jsType,g,claim);
    const imported=await timed(()=>{const a=paths(g),b=paths(claim);a.delete();b.delete();},3);
    const a=paths(g),b=paths(claim);
    const kernel=await timed(()=>{const s=clip[wasmName](a,b,clip.FillRule.NonZero,4),area=Math.abs(clip.AreaPathsD(s));s.delete();return area;},3);
    const endToEnd=await timed(()=>{const a=paths(g),b=paths(claim),s=clip[wasmName](a,b,clip.FillRule.NonZero,4);
      const area=Math.abs(clip.AreaPathsD(s)),output=extract(s);a.delete();b.delete();s.delete();return {area,...output};},3);
    const jsArea=js.area??null,areaError=jsArea===null?null:Math.abs(jsArea-endToEnd.value.area);
    if(jsArea!==null)assert.ok(areaError/Math.max(1,jsArea)<1e-5,'Areas must agree within the stated precision tolerance');
    operation[name]={current_js_clipper:js,wasm_import:timing(imported),wasm_resident_kernel:timing(kernel),wasm_with_import_output:timing(endToEnd),
      js_area:jsArea,wasm_area:endToEnd.value.area,absolute_area_difference:areaError,wasm_output_vertices:endToEnd.value.vertices};
    a.delete();b.delete();
  }
  const recordBytes=Buffer.byteLength(JSON.stringify({type:'claim',target:record.i,claim,revision:2}));
  operation.lazy_claim_record_bytes=recordBytes;
  result.operations.push(operation);
  console.log('Clipped feature',record.i,record.n);
}
result.note='Single-feature union/subtraction component benchmarks. Current JS uses 1/4096 px; Clipper2-WASM uses 1e-4 px. WASM output extracted to packed arrays; current JS reconstructs nested polygon arrays. Area agreement is not a topology proof. No end-to-end speedup inferred.';
result.max_rss_kib=process.resourceUsage().maxRSS;
save(process.argv[3],result);
