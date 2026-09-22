// Compare full-feature and component-local clipping using the same WASM kernel.
const fs = require('node:fs'), path = require('node:path'), vm = require('node:vm');
const assert = require('node:assert/strict');
const [input, output] = process.argv.slice(2);
if (!input || !output) throw new Error('Usage: node local-components.cjs annotations.geojson results.json');
const root = path.resolve(__dirname, '../../../inst/viewer');
const features = JSON.parse(fs.readFileSync(input, 'utf8')).features;
const polygons = f => f.geometry.type === 'MultiPolygon' ? f.geometry.coordinates : [f.geometry.coordinates];
const count = g => g.reduce((n,p) => n + p.reduce((s,r) => s+r.length,0),0);
const subjects = features.filter(f => /^(Multi)?Polygon$/.test(f.geometry?.type)).map(polygons);
const context = vm.createContext({performance, atob, console, navigator:{userAgent:'chrome',appName:'Netscape'},self:{}});
for (const file of ['clipper.js','geometry-codec.js']) vm.runInContext(fs.readFileSync(path.join(root,file),'utf8'),context);
context.ClipperLib = context.self.ClipperLib;
context.wsiClipper2Binary = fs.readFileSync(path.join(root,'clipper2.wasm')).toString('base64');
for (const file of ['clipper2.js','geometry-kernel.js','geometry-local.js','geometry-worker.js']) vm.runInContext(fs.readFileSync(path.join(root,file),'utf8'),context);
const area = g => g.reduce((s,p) => s + p.reduce((n,r,j) => n + (j ? -1 : 1) * Math.abs(r.slice(1).reduce((a,b,i) => a+r[i][0]*b[1]-b[0]*r[i][1],0))/2,0),0);
(async () => {
  await vm.runInContext('geometryKernelReady',context);
  const results = [];
  for (let feature = 0; feature < subjects.length; feature++) {
    const subject = subjects[feature], largest = subject.reduce((a,b) => a[0].length>b[0].length ? a : b);
    const p = largest[0][0], r = 180;
    context.subject = subject;
    context.clip = [[[[p[0]-r,p[1]-r],[p[0]+r,p[1]-r],[p[0]+r,p[1]+r],[p[0]-r,p[1]+r],[p[0]-r,p[1]-r]]]];
    for (const operation of ['ctUnion','ctDifference','ctIntersection']) {
      context.operation = operation;
      const times = {};
      for (const [name, fn] of [['full','geometryClip'],['local','geometryLocalClip']]) {
        times[name] = [];
        for (let i=0;i<3;i++) {
          const start = performance.now();
          context[name] = vm.runInContext(`${fn}(operation,subject,clip)`,context);
          times[name].push(performance.now()-start);
        }
      }
      const delta = area(vm.runInContext('geometryClip("ctXor",full,local)',context));
      assert.ok(delta < .01, `${feature} ${operation}: symmetric difference ${delta}`);
      results.push({feature,vertices:count(subject),parts:subject.length,operation,times_ms:times,symmetric_difference_px2:delta});
    }
  }
  const report = {date:new Date().toISOString(),file:path.basename(input),total_vertices:subjects.reduce((n,g)=>n+count(g),0),results};
  fs.writeFileSync(output,JSON.stringify(report,null,2)); console.log(JSON.stringify(report,null,2));
})().catch(e=>{console.error(e);process.exitCode=1;});
