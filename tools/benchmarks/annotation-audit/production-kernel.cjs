// Same production kernels, input, precision and output path; not a UI benchmark.
const {Worker,isMainThread,parentPort,workerData}=require('node:worker_threads');
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm');
const root=path.resolve(__dirname,'../../../inst/viewer');
if(isMainThread) {
  const [input,output]=process.argv.slice(2);
  if(!input||!output)throw new Error('Usage: node production-kernel.cjs input.geojson output.json');
  const run=(engine)=>new Promise((resolve,reject)=>{
    const worker=new Worker(__filename,{workerData:{input,engine}});let timer;
    worker.on('error',reject);worker.on('message',value=>{
      if(value.ready)timer=setTimeout(()=>{worker.terminate().then(()=>resolve({engine,timeout_ms:20000}));},20000);
      else {clearTimeout(timer);worker.terminate().then(()=>resolve(value));}
    });
  });
  (async()=>{
    const results=[]; for(const engine of ['clipper-js','clipper2-wasm'])results.push(await run(engine));
    fs.writeFileSync(output,JSON.stringify({input,date:new Date().toISOString(),results},null,2));console.log(JSON.stringify(results));
  })().catch(e=>{console.error(e);process.exitCode=1;});
} else {
  (async()=>{
    const features=JSON.parse(fs.readFileSync(workerData.input,'utf8')).features.filter(f=>/^(Multi)?Polygon$/.test(f.geometry?.type));
    const polygons=f=>f.geometry.type==='MultiPolygon'?f.geometry.coordinates:[f.geometry.coordinates];
    const count=g=>g.reduce((n,p)=>n+p.reduce((s,r)=>s+r.length,0),0);
    const g=polygons(features.reduce((a,b)=>count(polygons(a))>count(polygons(b))?a:b));
    const p=g[0][0][0],radius=80,claim=[[[[p[0]-radius,p[1]-radius],[p[0]+radius,p[1]-radius],[p[0]+radius,p[1]+radius],[p[0]-radius,p[1]+radius],[p[0]-radius,p[1]-radius]]]];
    const c=vm.createContext({performance,atob,console,navigator:{userAgent:'chrome',appName:'Netscape'},self:{},g,claim});
    for(const file of ['clipper.js','geometry-codec.js'])vm.runInContext(fs.readFileSync(path.join(root,file),'utf8'),c);
    c.ClipperLib=c.self.ClipperLib;
    if(workerData.engine==='clipper2-wasm'){
      c.wsiClipper2Binary=fs.readFileSync(path.join(root,'clipper2.wasm')).toString('base64');
      vm.runInContext(fs.readFileSync(path.join(root,'clipper2.js'),'utf8'),c);
    }
    for(const file of ['geometry-kernel.js','geometry-local.js','geometry-worker.js'])vm.runInContext(fs.readFileSync(path.join(root,file),'utf8'),c);
    await vm.runInContext('geometryKernelReady',c);
    parentPort.postMessage({ready:true});
    const results=[];
    for(const operation of ['ctUnion','ctDifference']){
      const times=[];let output;
      for(let i=0;i<3;i++){
        const start=performance.now();output=vm.runInContext('geometryClip("'+operation+'",g,claim)',c);times.push(performance.now()-start);
      }
      const area=ring=>Math.abs(ring.slice(1).reduce((s,p,i)=>s+ring[i][0]*p[1]-p[0]*ring[i][1],0))/2;
      results.push({operation,times_ms:times,vertices:count(output),area:output.reduce((s,p)=>s+area(p[0])-p.slice(1).reduce((n,r)=>n+area(r),0),0)});
    }
    parentPort.postMessage({engine:workerData.engine,input_vertices:count(g),results});
  })().catch(e=>{throw e;});
}
