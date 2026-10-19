// Use only a disposable, locally served multi-tissue experiment fixture.
// The integration fixture has three 448px crops, four pooled tiles per image,
// and a 32x32 spatial-token grid; the two held-out tissues contain eight tiles.
// WSITOOLS_TEST_URL=http://127.0.0.1:... PLAYWRIGHT_MODULE=/path/to/playwright node tests/browser/experiment-workbench-live.cjs
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const crypto = require('node:crypto');
const {chromium} = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const url = process.env.WSITOOLS_TEST_URL;
assert.match(url || '',/^http:\/\/(127\.0\.0\.1|localhost)(:|\/)/);
const output = process.env.WSITOOLS_TEST_OUTPUT || fs.mkdtempSync(path.join(os.tmpdir(),'wsitools-experiment-ui-'));
fs.mkdirSync(output,{recursive:true});
let browser,page;
(async()=>{
  browser=await chromium.launch({channel:'chrome',headless:true});
  page=await browser.newPage({viewport:{width:1600,height:1100}});
  const errors=[];
  page.on('pageerror',error=>errors.push(error.message));
  await page.goto(url,{waitUntil:'domcontentloaded'});
  await page.waitForFunction(()=>typeof osdReady!=='undefined'&&osdReady&&typeof wsiExperimentSnapshot==='function'&&document.getElementById('experimentWorkbench'),{}, {timeout:60000});
  const synced=()=>page.waitForFunction(()=>!wsiSyncSending&&!wsiSyncQueue.length,{}, {timeout:30000});
  await synced();
  const setup=await page.evaluate(()=>{
    if(!cfg.experiment||projectItems.length<2)throw new Error('A disposable multi-tissue experiment is required.');
    const width=Number(cfg.slide_width),height=Number(cfg.slide_height);
    const feature=(id,name,x0,x1,colour)=>({type:'Feature',id,properties:{name,classification:{name,color:colour}},geometry:{type:'Polygon',coordinates:[[[x0,0],[x1,0],[x1,height],[x0,height],[x0,0]]]}});
    addImportedGeojson({type:'FeatureCollection',features:[feature('training-left','Training left',0,width*0.5,[50,170,90]),feature('training-right','Training right',width*0.5,width,[230,100,40])]},'Experiment test training');
    openPredictionWindow();fillPredictionAnnotations();
    const sources=predictionSources().filter(source=>String(source.id).startsWith('experiment:'));
    if(!sources.length)throw new Error('The fixture has no typed experiment feature source.');
    return {sources,tissues:projectItems.length,width,height};
  });
  const contrastSource=setup.sources.find(source=>source.id==='experiment:virchow2-spatial') || setup.sources[0];
  await page.selectOption('#predictionFeatureSource',contrastSource.id);
  const labels=await page.locator('#predictionTrainAnnotations option').evaluateAll(options=>options.filter(o=>/training-left|training-right/.test(o.value)).map(o=>o.value));
  assert.equal(labels.length,2,'Two training ROIs are available');
  await page.selectOption('#predictionTrainAnnotations',labels);
  const pooled=setup.sources.find(source=>source.id==='experiment:virchow2-pooled');
  assert.ok(pooled,'The fixture includes pooled tile features for PLS');
  await page.selectOption('#predictionFeatureSource',pooled.id);
  await page.fill('#predictionNcomp','2');
  await page.evaluate(()=>{const refine=el('predictionRefineSvm');if(refine)refine.checked=false;});
  await page.locator('#predictionRun').click();
  await page.waitForFunction(()=>wsiExperimentSnapshot().classifiers.length>0||/Prediction failed/.test(el('predictionSummary').textContent),{}, {timeout:60000});
  const classifier=await page.evaluate(()=>({status:el('predictionSummary').textContent,models:wsiExperimentSnapshot().classifiers,
    result:viewerLogPayload().findLast(entry=>/PLS-LDA finished/.test(entry.message))?.message}));
  assert.equal(classifier.models.length,1,classifier.status);assert.match(classifier.result||'',/PLS-LDA finished/);
  console.log('Pooled PLS classifier fitted and saved.');
  assert.equal(classifier.models[0].feature_source,pooled.id);
  const modelPath=classifier.models[0].path,modelDigest=()=>crypto.createHash('sha256').update(fs.readFileSync(modelPath)).digest('hex');
  const fittedDigest=modelDigest();
  const targets=await page.evaluate(()=>projectItems.slice(1).map(item=>String(item.tissue_id||item.id)));
  await page.selectOption('#experimentTissues',targets);
  await page.locator('#experimentApplyClassifier').click();
  await page.waitForFunction(()=>!wsiExperimentBusy&&/Applied|failed|could not/i.test(el('experimentStatus').textContent),{}, {timeout:60000});
  classifier.application=await page.evaluate(()=>({status:el('experimentStatus').textContent,layer:predictionResultLayer()}));
  assert.match(classifier.application.status,/Applied the saved classifier to 8 observations/);
  assert.equal(modelDigest(),fittedDigest,'Applying to other tissues does not alter the fitted classifier');
  classifier.sha256=fittedDigest;
  console.log('Saved classifier applied to the other two tissues without changing its digest.');
  await page.selectOption('#predictionFeatureSource',contrastSource.id);
  await page.selectOption('#experimentContrastScope','shared');
  await page.locator('#experimentTissues').evaluate(node=>{for(const option of node.options)option.selected=true;});
  await page.locator('#experimentContrastCreate').click();
  await page.waitForFunction(()=>!wsiExperimentBusy,{}, {timeout:60000});
  const result=await page.evaluate(()=>({status:el('experimentStatus').textContent,appearance:wsiExperimentActiveAppearance(),
    tissueLayers:projectItems.map(item=>(item.layers||[]).filter(layer=>layer.id==='wsi_experiment_appearance').length)}));
  assert.ok(result.appearance?.points?.length,result.status);
  assert.ok(result.tissueLayers.every(n=>n===1),'Shared contrast is available for every selected tissue');
  assert.ok(result.appearance.points.every(p=>Array.isArray(p.probabilities)&&p.probabilities.length>=2));
  console.log('Shared spatial appearance created on all selected tissues.');
  await page.evaluate(()=>{el('experimentWandTolerance').value='0.05';el('experimentWandReach').value='250';});
  await page.locator('#experimentUseWand').click();
  assert.equal(await page.locator('#experimentWandSource').inputValue(),'feature');
  await synced();
  // Preserve training in the fitted recipe, then remove only this disposable
  // fixture's annotations so the click creates an independently inspectable ROI.
  const click=await page.evaluate(()=>{
    while(rois.length)deleteRoi(rois.length-1);
    selectAnnotation(-1,true);buildRoiList();saveActiveProjectAnnotations();
    if(typeof closeProjectPanel==='function')closeProjectPanel();
    if(typeof setRoiPanelOpen==='function')setRoiPanelOpen(false);
    const active=wsiExperimentActiveAppearance(),cx=Number(cfg.slide_width)/2,cy=Number(cfg.slide_height)/2,
      p=active.points.reduce((best,p)=>(p.x-cx)**2+(p.y-cy)**2<(best.x-cx)**2+(best.y-cy)**2?p:best);
    const native={x:p.x,y:p.y};return {native,screen:slideToCanvas(native)};
  });
  await synced();
  await page.mouse.click(click.screen.x,click.screen.y);
  await page.waitForFunction(()=>!wandBusy&&rois.some(roi=>roi.wand_source==='virchow2-probability'),{}, {timeout:30000});
  await synced();
  const first=await page.evaluate(()=>{
    const roi=rois.find(roi=>roi.wand_source==='virchow2-probability');
    return {geometry:wsiRoiCoordinates(roi),count:roi.wand_observation_count,tolerance:roi.wand_tolerance,recipe:roi.appearance_recipe};
  });
  assert.equal(first.tolerance,0.05);assert.ok(first.count>0);assert.ok(first.recipe);
  const roundtrip=await page.evaluate(async()=>{
    saveActiveProjectAnnotations();const saved=projectBrowserSnapshot(false);
    el('experimentWandReach').value='111';el('experimentWandTolerance').value='0.9';el('experimentWandSource').value='he';
    await restoreBrowserProjectInLiveSession(saved);
    return {saved,current:wsiExperimentSnapshot(),points:wsiExperimentActiveAppearance().points.length,
      tissues:projectItems.map(item=>(item.layers||[]).some(layer=>layer.id==='wsi_experiment_appearance'))};
  });
  await synced();
  assert.equal(roundtrip.current.wand_source,'feature');assert.equal(roundtrip.current.reach_um,250);
  assert.equal(roundtrip.current.feature_tolerance,0.05);assert.ok(roundtrip.points>0);assert.ok(roundtrip.tissues.every(Boolean));
  assert.equal(roundtrip.current.classifiers.length,1);assert.equal(roundtrip.current.classifiers[0].path,modelPath);
  const invariance=await page.evaluate(async native=>{
    const active=wsiExperimentActiveAppearance(),mpp=projectItems[active.index].mpp,
      mx=Number(typeof mpp==='number'?mpp:mpp.x),my=Number(typeof mpp==='number'?mpp:mpp.y);
    const physicalGrid={...active.grid};
    for(const name of ['spacing_x_px','tile_width_px','origin_x_px'])physicalGrid[name]*=mx;
    for(const name of ['spacing_y_px','tile_height_px','origin_y_px'])physicalGrid[name]*=my;
    const select=points=>wsiExperimentConnectedSelection(points.map(p=>({...p,x:p.x*mx,y:p.y*my})),
      physicalGrid,{x:native.x*mx,y:native.y*my},0.05,250).map(r=>r.point.id).sort();
    const expected=select(active.points),recoloured=active.points.map(p=>({...p,r:255-p.r,g:255-p.g,b:255-p.b}));
    const original=JSON.stringify(wsiRoiCoordinates(rois.find(roi=>roi.wand_source==='virchow2-probability')));
    const layer=layers.find(layer=>layer.id==='wsi_experiment_appearance');
    const palette=layer.items.map(item=>({item,colour:item.colour,fill:item.fill}));
    for(const item of layer.items) { item.colour='#ffffff';item.fill='#ffffff'; }
    osdViewer.viewport.zoomBy(1.6);osdViewer.viewport.applyConstraints();
    while(rois.length)deleteRoi(rois.length-1);
    selectAnnotation(-1,true);buildRoiList();saveActiveProjectAnnotations();
    await wsiQueueSync('viewer_state',{reason:'test_pre_repeat'});
    await runWandAt(slideToCanvas(native),native);
    const repeated=rois.find(roi=>roi.wand_source==='virchow2-probability');
    for(const saved of palette){saved.item.colour=saved.colour;saved.item.fill=saved.fill;}
    wsiRequestDraw();
    return {expected,recoloured:select(recoloured),zoomed:select(active.points),
      original_geometry:original,repeated_geometry:JSON.stringify(wsiRoiCoordinates(repeated)),
      repeated_count:repeated.wand_observation_count};
  },click.native);
  assert.deepEqual(invariance.recoloured,invariance.expected);assert.deepEqual(invariance.zoomed,invariance.expected);
  assert.equal(invariance.expected.length,first.count,'The selector uses the same physical reach as the real wand');
  assert.equal(invariance.repeated_count,first.count);assert.equal(invariance.repeated_geometry,invariance.original_geometry,
    'A second production wand selection preserves native geometry after zoom and palette change');
  await synced();
  const sync=await page.evaluate(async()=>{
    const snapshotUrl=new URL(cfg.viewer_state_url);snapshotUrl.pathname='/native-renderer-state';
    return {revision:wsiSyncAcknowledged?.revision,errors:viewerSyncHistory.filter(entry=>entry.direction==='error'),
      logs:viewerLogPayload().filter(entry=>entry.level==='error'),
      state:await(await fetch(cfg.viewer_state_url)).json(),
      snapshot:await(await fetch(snapshotUrl.href)).json(),geometry:roiGeojsonObject()};
  });
  assert.ok(sync.revision);assert.deepEqual(sync.errors,[]);assert.deepEqual(sync.logs,[]);
  assert.equal(sync.snapshot.annotations.features.length,sync.geometry.features.length);
  assert.deepEqual(sync.snapshot.annotations.features.map(feature=>feature.geometry),sync.geometry.features.map(feature=>feature.geometry));
  const exportedEvidence=sync.snapshot.annotations.features[0].properties.wsiTools.experiment_wand;
  assert.ok(exportedEvidence,'R GeoJSON retains feature-wand provenance');
  assert.equal(exportedEvidence.recipe,first.recipe);
  assert.equal(exportedEvidence.observation_count,first.count);
  assert.deepEqual(errors,[]);
  await page.screenshot({path:path.join(output,'feature-wand.png')});
  fs.writeFileSync(path.join(output,'report.json'),JSON.stringify({setup,classifier,status:result.status,first,
    workbench:roundtrip.current,tissueLayers:roundtrip.tissues,invariance,sync,errors},null,2));
  fs.writeFileSync(path.join(output,'saved.wsiproject.json'),JSON.stringify(roundtrip.saved,null,2));
  console.log('Live experiment UI, shared appearance, feature-wand click, settings/evidence round trip and palette/zoom invariance passed:',output);
})().catch(async error=>{
  if(page) {
    await page.screenshot({path:path.join(output,'failure.png')}).catch(()=>{});
    const state=await page.evaluate(()=>({status:document.getElementById('experimentStatus')?.textContent,
      logs:typeof viewerLogPayload==='function'?viewerLogPayload().slice(-10):[]})).catch(()=>({}));
    fs.writeFileSync(path.join(output,'failure.json'),JSON.stringify({error:String(error),...state},null,2));
  }
  console.error(error);process.exitCode=1;
}).finally(async()=>{if(browser)await browser.close();});
