const {parentPort,workerData}=require('node:worker_threads');
const fs=require('node:fs'),path=require('node:path');
const ClipperLib=require(path.join(workerData.repo,'inst/viewer/clipper.js'));
const geometryClip=new Function('ClipperLib','self',
  fs.readFileSync(path.join(workerData.repo,'inst/viewer/geometry-worker.js'),'utf8')+'\nreturn geometryClip;'
)(ClipperLib,{postMessage(){}});
const area=geometry=>{
  const ring=r=>Math.abs(r.slice(1).reduce((sum,p,i)=>sum+r[i][0]*p[1]-p[0]*r[i][1],0))/2;
  return geometry.reduce((sum,p)=>sum+ring(p[0])-p.slice(1).reduce((a,h)=>a+ring(h),0),0);
};
parentPort.on('message',({operation,g,claim})=>{
  const times=[];let result;
  parentPort.postMessage({started:true});
  for(let i=0;i<3;i++) {
    const t=performance.now();result=geometryClip(operation,g,claim);times.push(performance.now()-t);
    if(times[0]>4000)break;
  }
  parentPort.postMessage({times,area:area(result)});
});
parentPort.postMessage({ready:true});
