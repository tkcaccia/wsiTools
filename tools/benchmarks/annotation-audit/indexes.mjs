import fs from 'node:fs';
import path from 'node:path';
import assert from 'node:assert/strict';
import {Worker} from 'node:worker_threads';
import {dep,repo,load,pack,queries,overlaps,timed,timing,now,save} from './common.mjs';

const {default:RBush}=await dep('rbush'),{default:Flatbush}=await dep('flatbush');
const data=load(process.argv[2]), {features,boxes,bounds}=data, result={dataset:data.summary};
console.log('Index benchmark',data.summary);
result.json_parse=timing(await timed(()=>JSON.parse(data.text)));
result.json_stringify=timing(await timed(()=>JSON.stringify(data.object)));
const packed=await timed(()=>pack(features)); result.pack=timing(packed);
result.packed_geometry_bytes=Object.values(packed.value).reduce((n,a)=>n+a.byteLength,0);
result.structured_clone=timing(await timed(()=>structuredClone(data.object)));
const rb=await timed(()=>new RBush().load(boxes)); result.rbush_build=timing(rb);
const flat=await timed(()=>{const idx=new Flatbush(boxes.length,16,Float64Array);for(const b of boxes)idx.add(b.minX,b.minY,b.maxX,b.maxY);idx.finish();return idx;});
result.flatbush_build=timing(flat);result.flatbush_bytes=flat.value.data.byteLength;
const array=Float64Array.from(boxes.flatMap(b=>[b.minX,b.minY,b.maxX,b.maxY]));
const wasmStart=now(),wasm=await WebAssembly.instantiate(fs.readFileSync(path.join(repo,'inst/wasm/wsi_overlay_core.wasm'))),w=wasm.instance.exports;
result.wasm_init_ms=now()-wasmStart;
const bp=w.wsi_overlay_alloc(array.byteLength),op=w.wsi_overlay_alloc(boxes.length*4);
new Float64Array(w.memory.buffer,bp,array.length).set(array);
const linear=q=>boxes.filter(b=>overlaps(b,q)).map(b=>b.id);
const flatScan=q=>{const ids=[];for(let i=0;i<array.length;i+=4)if(array[i]<=q[2]&&array[i+2]>=q[0]&&array[i+1]<=q[3]&&array[i+3]>=q[1])ids.push(i/4);return ids;};
const wasmQuery=q=>{const n=w.wsi_filter_bboxes(bp,boxes.length,...q,op);return new Uint32Array(w.memory.buffer,op,n);};
const methods={linear,typed_linear:flatScan,wasm_linear:wasmQuery,rbush:q=>rb.value.search({minX:q[0],minY:q[1],maxX:q[2],maxY:q[3]}).map(b=>b.id),flatbush:q=>flat.value.search(...q)};
result.queries=[];
for(const width of [512,2048,8192,Math.ceil(Math.max(bounds[2]-bounds[0],bounds[3]-bounds[1]))]) {
  const qs=queries(bounds,width,100),ref=qs.map(linear),record={width,queries:qs.length,average_hits:ref.reduce((n,ids)=>n+ids.length,0)/qs.length,methods:{}};
  for(const [name,fn] of Object.entries(methods)) {
    for(let i=0;i<qs.length;i++)assert.deepEqual(Array.from(fn(qs[i])).sort((a,b)=>a-b),ref[i]);
    const run=await timed(()=>{let count=0;for(const q of qs)count+=fn(q).length;return count;},5);
    record.methods[name]={...timing(run),median_ms_per_query:run.median_ms/qs.length};
  }
  result.queries.push(record);
}
const edit=boxes[Math.floor(boxes.length/2)];
result.rbush_single_replace=timing(await timed(()=>{rb.value.remove(edit);rb.value.insert(edit);},10));
result.full_snapshot_bytes=Buffer.byteLength(JSON.stringify(data.object));
result.one_feature_patch=timing(await timed(()=>JSON.stringify({upsert:[features[Math.floor(features.length/2)]],remove:[],revision:2}),10));
result.one_feature_patch_bytes=Buffer.byteLength(JSON.stringify({upsert:[features[Math.floor(features.length/2)]],remove:[],revision:2}));
const worker=new Worker(`const {parentPort}=require('node:worker_threads');parentPort.on('message',v=>parentPort.postMessage({count:v?.features?.length||v?.byteLength||0}));`,{eval:true});
await new Promise(resolve=>worker.once('online',resolve));
const send=(value,transfer=[])=>new Promise(resolve=>{worker.once('message',resolve);worker.postMessage(value,transfer);});
result.worker_object_send_ack=timing(await timed(()=>send({features}),3));
let buffer=packed.value.coords.slice().buffer;
result.worker_buffer_copy_send_ack=timing(await timed(()=>send(buffer),3));
result.worker_transfer_send_ack=timing(await timed(async()=>{const b=packed.value.coords.slice().buffer;const t=now();await send(b,[b]);return now()-t;},3));
// Separate ownership-transfer time from the copy needed if the sender retains its source.
const transferTimes=[];
for(let i=0;i<5;i++){buffer=packed.value.coords.slice().buffer;const t=now();await send(buffer,[buffer]);transferTimes.push(now()-t);}
result.worker_transfer_only_ms=transferTimes;
await worker.terminate(); w.wsi_overlay_dealloc(bp,array.byteLength);w.wsi_overlay_dealloc(op,boxes.length*4);
result.max_rss_kib=process.resourceUsage().maxRSS;
save(process.argv[3],result);
