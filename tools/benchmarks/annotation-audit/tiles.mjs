import fs from 'node:fs';
import assert from 'node:assert/strict';
import {dep,requireDep,load,polygons,timed,timing,now,save,median} from './common.mjs';

const {default:GeoJSONVT}=await dep('geojson-vt'), vtPbf=requireDep('vt-pbf');
const {VectorTile}=await dep('@mapbox/vector-tile'),pbfModule=await dep('pbf');
const Pbf=pbfModule.PbfReader||pbfModule.default;
const fgb=await dep('flatgeobuf');
const data=load(process.argv[2]),result={dataset:data.summary};
console.log('Tile benchmark',data.summary.file);
// Adapter only for benchmarking unmodified geographic geojson-vt. Persisted
// annotations remain in original WSI pixels. Pad the square to avoid wrap edges.
const b=data.bounds,S=Math.max(b[2]-b[0],b[3]-b[1])/.8;
const project=([x,y])=>{const u=.1+(x-b[0])/S,v=.1+(y-b[1])/S;return[(u-.5)*360,Math.atan(Math.sinh(Math.PI*(1-2*v)))*180/Math.PI];};
const adapter=await timed(()=>({type:'FeatureCollection',features:data.features.map((f,i)=>({type:'Feature',id:i,
  properties:{id:i},geometry:{type:'MultiPolygon',coordinates:polygons(f).map(p=>p.map(r=>r.map(project)))}}))}));
result.wsi_inverse_mercator_adapter=timing(adapter);
result.note='Unmodified geojson-vt 5.0.2, WSI pixels mapped through inverse Mercator solely for benchmarking; full-resolution source is not changed.';
result.modes=[];
for(const tolerance of [0,3]) {
  global.gc?.();const baseline=process.memoryUsage();
  const build=await timed(()=>new GeoJSONVT(adapter.value,{tolerance,maxZoom:12,indexMaxZoom:5,indexMaxPoints:100000,extent:4096,buffer:64}));
  const index=build.value;global.gc?.();
  const rec={tolerance,build:timing(build),initial_tiles:index.tileCoords.length,heap_delta:process.memoryUsage().heapUsed-baseline.heapUsed,
    arraybuffer_delta:process.memoryUsage().arrayBuffers-baseline.arrayBuffers,levels:[]};
  for(const z of [0,3,6]) {
    const scale=2**z,mid=Math.min(scale-1,Math.floor(scale*.45));
    const t=now(),raw=index.getTileRaw(z,mid,mid),coldMs=now()-t;
    const tile=index.getTile(z,mid,mid),v=tile?.features.reduce((n,f)=>n+(f.geometry||[]).reduce((s,r)=>s+r.length,0),0)||0;
    const rawBenchmark=await timed(()=>{let n=0;for(let j=0;j<100;j++)n+=index.getTileRaw(z,mid,mid)?.features.length||0;return n;},5);
    const legacyBenchmark=await timed(()=>{let n=0;for(let j=0;j<100;j++)n+=index.getTile(z,mid,mid)?.features.length||0;return n;},5);
    assert.equal(rawBenchmark.value,legacyBenchmark.value);
    const lv={z,first_lookup_ms:coldMs,features:raw?.features.length||0,vertices:v,
      raw_lookup_ms:rawBenchmark.median_ms/100,legacy_lookup_ms:legacyBenchmark.median_ms/100};
    if(tile?.features.length) {
      const enc=await timed(()=>vtPbf.fromGeojsonVt({annotations:tile}));
      const dec=await timed(()=>{const layer=new VectorTile(new Pbf(enc.value)).layers.annotations;let n=0;for(let i=0;i<layer.length;i++)n+=layer.feature(i).loadGeometry().length;return n;});
      lv.mvt={encode:timing(enc),decode_all_rings:timing(dec),bytes:enc.value.length,json_bytes:Buffer.byteLength(JSON.stringify(tile))};
    }
    rec.levels.push(lv);
  }
  const visitTimes=[];
  for(let y=20;y<30;y++)for(let x=20;x<30;x++){const t=now();index.getTileRaw(6,x,y);visitTimes.push(now()-t);}
  rec.pan_100_new_tiles_median_ms=median(visitTimes);rec.retained_tiles_after_pan=index.tileCoords.length;
  result.modes.push(rec);
}
// The JS writer emits indexNodeSize=0: this tests binary encoding/decoding,
// not an indexed HTTP Range query. Indexed FlatGeobuf is tested separately in R/GDAL.
const canonical={type:'FeatureCollection',features:data.features.map((f,i)=>({type:'Feature',properties:{feature_id:String(f.id??i)},geometry:f.geometry}))};
const encode=await timed(()=>fgb.geojson.serialize(canonical));
result.flatgeobuf_unindexed={encode:timing(encode),bytes:encode.value.byteLength};
result.flatgeobuf_unindexed.decode=timing(await timed(async()=>{let n=0;for await(const f of fgb.geojson.deserialize(encode.value)){n++;}assert.equal(n,canonical.features.length);return n;}));
result.max_rss_kib=process.resourceUsage().maxRSS;
save(process.argv[3],result);
