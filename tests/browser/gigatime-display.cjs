const assert=require('node:assert/strict'),fs=require('node:fs'),vm=require('node:vm'),path=require('node:path');
const context=vm.createContext({});vm.runInContext(fs.readFileSync(path.join(__dirname,'../../inst/viewer/experiment-workbench.js'),'utf8'),context);
const entry={image:{metadata:{gigatime:{display_max:65535}}},cell:{items:[{value:4000}]}};
const before=JSON.stringify(entry);
assert.deepEqual(JSON.parse(JSON.stringify(context.wsiGigatimeImageSettings(entry,0,4000,.65))),{contrast_min:0,contrast_max:4000/65535,opacity:.65});
assert.equal(JSON.stringify(entry),before,'Display settings do not mutate scientific scores or bounds');
assert.equal(context.wsiGigatimeImageSettings({image:{metadata:{gigatime:{display_max:1}}}},.1,.9,1).contrast_max,.9);
for(const values of [[0,0,.5],[-1,4000,.5],[0,65536,.5],[0,4000,2],[NaN,4000,.5]])assert.throws(()=>context.wsiGigatimeImageSettings(entry,...values),/display range/);
console.log('8 GigaTIME image display range/value-preservation contracts passed.');
