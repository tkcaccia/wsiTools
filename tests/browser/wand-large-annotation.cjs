// Run against the exact large-annotation fixture with WSITOOLS_TEST_URL.
const {chromium} = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const assert = require('node:assert/strict');
const url = process.env.WSITOOLS_TEST_URL;
assert.match(url || '', /^http:\/\/(127\.0\.0\.1|localhost)(:|\/)/);

let browser;
(async () => {
  browser = await chromium.launch({channel:'chrome',headless:true});
  const page = await browser.newPage({viewport:{width:1600,height:1100}}), errors=[];
  page.on('pageerror',error=>errors.push(error.message));
  await page.goto(url,{waitUntil:'domcontentloaded',timeout:180000});
  await page.waitForFunction(()=>osdReady && rois.length >= 3,{}, {timeout:180000});
  const prepared = await page.evaluate(async()=>{
    const counts=rois.map(roi=>pointCount(roi)),index=counts.indexOf(Math.max(...counts));
    selectedRoi=index;showRois=true;buildRoiList();updateButtons();wsiRequestDraw();
    const geometry=wsiCachedGeometry(rois[index]);
    let chosen=null;
    for(const part of geometry.parts){
      const b=part.bounds,w=Math.max(1,b.xmax-b.xmin),h=Math.max(1,b.ymax-b.ymin);
      if(w<32||h<32)continue;
      for(let gy=1;gy<12&&!chosen;gy++)for(let gx=1;gx<12&&!chosen;gx++){
        const point={x:b.xmin+w*gx/12,y:b.ymin+h*gy/12};
        if(pointInRingsEvenOdd(point,part.group.rings))chosen=point;
      }
      if(chosen)break;
    }
    if(!chosen)throw new Error('No interior point found in the large annotation');
    const span=1600;
    zoomToSlideBounds({xmin:chosen.x-span/2,ymin:chosen.y-span/2,xmax:chosen.x+span/2,ymax:chosen.y+span/2},1);
    await new Promise(resolve=>setTimeout(resolve,1800));
    return {index,counts,point:chosen,before:pointCount(rois[index])};
  });
  const result = await page.evaluate(async prepared=>{
    const canvasPoint=slideToCanvas(prepared.point),started=performance.now();
    await runWandAt(canvasPoint,prepared.point,null,{altKey:true});
    if(wsiEditPromise)await wsiEditPromise;
    const elapsed=performance.now()-started,entry=annotationUndo[annotationUndo.length-1];
    return {elapsed,after:pointCount(rois[prepared.index]),contains:roiContainsPoint(rois[prepared.index],prepared.point),
      history_delta:!!(entry&&entry.wsi_delta),history_bytes:wsiHistoryEntryBytes(entry),history_entries:annotationUndo.length,
      worker_ms:wsiRuntimeMetrics.geometry_worker_ms,commit_ms:wsiRuntimeMetrics.geometry_commit_ms,
      wand_reach:rois[prepared.index].wand_reach,wand_points:rois[prepared.index].wand_pixel_count};
  },prepared);
  assert.equal(result.contains,false,'The local Wand subtraction removes the clicked region');
  assert.equal(result.history_delta,true,'Wand undo stores a changed-feature delta');
  assert.ok(result.history_entries<=10,'Undo history is bounded');
  assert.ok(result.history_bytes<=64*1024*1024 || result.history_entries===1,'Undo memory is bounded except for one retained restorable edit');
  assert.equal(result.wand_reach,256);
  await page.keyboard.press('Control+z');
  assert.equal(await page.evaluate(point=>roiContainsPoint(rois[selectedRoi],point),prepared.point),true,'Undo restores the clicked region');
  assert.deepEqual(errors,[]);
  console.log(JSON.stringify({fixture_vertices:prepared.counts.reduce((a,b)=>a+b,0),selected_vertices:prepared.before,...result},null,2));
})().catch(error=>{console.error(error);process.exitCode=1;}).finally(async()=>{if(browser)await browser.close();});
