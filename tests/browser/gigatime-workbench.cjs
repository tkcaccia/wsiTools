// Data ownership and display scope contracts; no model or browser dependency.
const assert=require('node:assert/strict'),fs=require('node:fs'),vm=require('node:vm'),path=require('node:path');
const source=fs.readFileSync(path.join(__dirname,'../../inst/viewer/experiment-workbench.js'),'utf8');
const meta=(tissue,artifact,marker,extra={})=>({tissue_id:tissue,artifact_id:artifact,marker,...extra});
const image=(tissue,artifact,marker)=>({id:[tissue,artifact,marker].join('/'),metadata:{gigatime:meta(tissue,artifact,marker)},visible:false});
const cell=(tissue,artifact,marker,index,extra={})=>({id:[tissue,artifact,marker].join('/'),visible:false,
 metadata:{gigatime:meta(tissue,artifact,marker,{source_artifact_id:'image',mask_name:'cyto',segmentation_unit:'cell',score_semantics:'uncalibrated_virtual_marker_score'})},
 items:[{id:tissue+'::'+artifact+'::1',original_id:'1',tissue_id:tissue,project_image_index:index,x:12.5,y:8.5,value:42,...extra}]});
const channels=[image('a','image','CD3'),image('a','image','CD8'),image('b','image','CD3')];
const data=[cell('a','cyto','CD3',0),cell('a','cyto','CD8',0),cell('a','nuclei','CD3',0),cell('b','cyto','CD3',1)];
const context=vm.createContext({console,cfg:{experiment:{}},activeProjectIndex:0,projectItems:[{tissue_id:'a',layers:data.slice(0,3)},{tissue_id:'b',layers:data.slice(3)}],layers:data,channelSources:channels,markProjectDirty(){}});
vm.runInContext(source,context);
const plain=value=>JSON.parse(JSON.stringify(value));
let entries=context.wsiGigatimeEntries();
assert.equal(entries.length,3);assert.equal(new Set(entries.map(e=>e.key)).size,3,'Distinct masks keep their own marker entries');
assert.equal(entries[0].image.id,'a/image/CD3');assert.equal(entries[2].image.id,'a/image/CD3','Both masks link the correct same-tissue image');
assert.deepEqual(plain(context.wsiGigatimeRows(entries[0])).map(row=>[row.original_id,row.value,row.x,row.y]),[['1',42,12.5,8.5]],'Native raw scores are not normalized to probabilities');
context.wsiGigatimeSetCellVisible(entries[0],true);assert.equal(data[0].visible,true);
context.wsiGigatimeSetCellVisible(entries[1],true);assert.equal(data[0].visible,false);assert.equal(data[1].visible,true);assert.equal(data[3].visible,false,'Other tissue display untouched');
context.activeProjectIndex=1;entries=context.wsiGigatimeEntries();assert.equal(entries.length,1);assert.equal(entries[0].cell.id,'b/cyto/CD3');
const csv=context.wsiGigatimeCsv(entries[0]);assert.match(csv,/b::cyto::1/);assert.doesNotMatch(csv,/a::cyto/);assert.match(csv,/uncalibrated_virtual_marker_score/);
context.layers[3].items.push({id:'wrong',original_id:'wrong',tissue_id:'a',project_image_index:1,value:999});
assert.equal(context.wsiGigatimeRows(entries[0]).length,1,'A wrong tissue cannot be admitted by a matching image index');
context.layers[3].items[0].original_id='=SUM(A1)';assert.match(context.wsiGigatimeCsv(entries[0]),/"'=SUM\(A1\)"/,'Text IDs cannot become spreadsheet formulas');
context.wsiGigatimeCaptureSources();
const snapshot={project:{active_index:1,items:[{tissue_id:'a',layers:[data[0]]},{tissue_id:'b',layers:[data[3]]}]},layers:[data[3]],channel_sources:channels};
const modified=plain(snapshot);modified.layers[0].items[0].value=99999;modified.layers[0].items[0].x=99999;
modified.project.items[1].layers[0].items[0].value=99999;modified.project.items[1].path='wrong-image';modified.project.items[1].image_data_uri='wrong pixels';modified.layers[0].visible=true;
modified.channel_sources[2].metadata.gigatime.marker='wrong';modified.channel_sources[2].path='https://wrong.invalid/';
modified.layers.push({id:'gigatime_injected',metadata:{gigatime:{marker:'invented'}}});
const checked=context.wsiGigatimePrepareSnapshot(modified);
assert.equal(checked.project.items[1].path,undefined);assert.equal(checked.project.items[1].image_data_uri,undefined,'Fresh scientific image sources override saved pixels/routes');
assert.equal(checked.layers.length,1);assert.equal(checked.layers[0].items[0].value,42);assert.equal(checked.layers[0].items[0].x,12.5);
assert.equal(checked.layers[0].visible,true,'Display preference survives while scientific values do not come from saved state');
assert.equal(checked.channel_sources[2].metadata.gigatime.marker,'CD3');assert.equal(checked.channel_sources[2].path,undefined);
modified.project.items.reverse();assert.throws(()=>context.wsiGigatimePrepareSnapshot(modified),/identities/);
context.layers=[];assert.equal(context.wsiGigatimeEntries()[0].cell,null,'Image-only outputs remain usable without invented cells');
console.log('8 GigaTIME workbench scope/join/value/export/restore contracts passed.');
