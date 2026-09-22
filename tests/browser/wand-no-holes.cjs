// Generate with: Rscript tests/browser/wand-fixture.R /tmp/wand.png
// Open that image in a disposable live viewer and set WSITOOLS_TEST_URL.
const {chromium} = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const assert = require('node:assert/strict'), fs = require('node:fs'), path = require('node:path'), os = require('node:os');
const url = process.env.WSITOOLS_TEST_URL;
assert.match(url || '', /^http:\/\/(127\.0\.0\.1|localhost)(:|\/)/);
const output = fs.mkdtempSync(path.join(os.tmpdir(), 'wsitools-wand-'));
let browser;
(async () => {
  browser = await chromium.launch({channel:'chrome', headless:true});
  const page = await browser.newPage({viewport:{width:1600,height:1100}}), errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  await page.goto(url,{waitUntil:'domcontentloaded'});
  await page.waitForFunction(()=>osdReady && osdViewer.world.getItemAt(0)?.getFullyLoaded(), {}, {timeout:60000});
  const unit = await page.evaluate(() => {
    const mask = document.createElement('canvas'); mask.width=120; mask.height=90;
    const cx=mask.getContext('2d'); cx.fillRect(10,10,80,70); cx.clearRect(20,20,20,20);
    cx.clearRect(60,10,10,20); cx.fillRect(100,50,10,10);
    const map=wandMappedRing;
    try {
      wandMappedRing=ring=>ring;
      const groups=wandGroupsFromMask(mask);
      const contains=p=>groups.some(g=>pointInRingsEvenOdd(p,g));
      const result={parts:groups.length,rings:groups.map(g=>g.length),area:groups.reduce((n,g)=>n+polygonArea(g),0),
        hole_filled:contains({x:25,y:25}),notch_open:!contains({x:65,y:15}),island_present:contains({x:105,y:55})};
      const source=document.createElement('canvas');source.width=120;source.height=90;
      const sourceContext=source.getContext('2d');sourceContext.fillStyle='#ba5a90';sourceContext.fillRect(0,0,120,90);
      const local=wandConnectedMask(source,60,45,5,20),localPixels=local.canvas.getContext('2d').getImageData(0,0,120,90).data;
      result.local_seed=localPixels[(45*120+60)*4+3]===255;
      result.local_far=localPixels[(45*120+10)*4+3]===0;
      result.local_count=local.count;
      cx.clearRect(0,0,120,90); result.empty=wandGroupsFromMask(mask).length; return result;
    } finally { wandMappedRing=map; }
  });
  assert.deepEqual(unit,{parts:2,rings:[1,1],area:5500,hole_filled:true,notch_open:true,island_present:true,
    local_seed:true,local_far:true,local_count:1257,empty:0});
  const seed=await page.evaluate(()=>slideToCanvas({x:550,y:300}));
  await page.locator('#wandReach').evaluate(input=>{input.value='512';input.dispatchEvent(new Event('input',{bubbles:true}));});
  await page.locator('#toolWand').click();
  await page.mouse.click(seed.x,seed.y);
  await page.waitForFunction(()=>!wandBusy && rois.length===1,{}, {timeout:30000});
  const selected=await page.evaluate(()=>({rings:roiDrawGroups(rois[0]).map(g=>g.rings.length+g.holes.length),
    point_count:pointCount(rois[0]),seed_included:roiContainsPoint(rois[0],{x:550,y:300}),
    distant_connected_area_excluded:!roiContainsPoint(rois[0],{x:170,y:300}),
    notch_open:!roiContainsPoint(rois[0],{x:505,y:150}),wand:rois[0].wand,reach:wandReach()}));
  assert.deepEqual(selected.rings,[1]); assert.equal(selected.seed_included,true);
  assert.equal(selected.distant_connected_area_excluded,true); assert.equal(selected.notch_open,true);
  assert.equal(selected.wand,true); assert.equal(selected.reach,512); assert.ok(selected.point_count<=768);
  await page.screenshot({path:path.join(output,'filled-selection.png')});
  await page.keyboard.press('Control+z');
  assert.equal(await page.evaluate(()=>rois.length),0,'Creation undo removes only the new Wand ROI');
  await page.evaluate(()=>restoreAnnotationRedo());
  assert.equal(await page.evaluate(()=>roiContainsPoint(rois[0],{x:550,y:300})),true,'Redo restores the local Wand region');
  const before=await page.evaluate(()=>{
    setRoiPositiveGroups(rois[0],[[[{x:50,y:50},{x:750,y:50},{x:750,y:550},{x:50,y:550},{x:50,y:50}]]]);
    selectedRoi=0; draw(); return JSON.stringify(roiGeojsonObject());
  });
  await page.keyboard.down('Alt'); await page.mouse.click(seed.x,seed.y); await page.keyboard.up('Alt');
  await page.waitForFunction(()=>!wandBusy && rois[0]?.wand_operation==='subtract',{}, {timeout:30000});
  assert.equal(await page.evaluate(()=>roiContainsPoint(rois[0],{x:550,y:300})),false,'Subtraction removes the local connected region');
  assert.equal(await page.evaluate(()=>roiContainsPoint(rois[0],{x:700,y:500})),true,'Subtraction preserves the surrounding annotation');
  await page.screenshot({path:path.join(output,'subtraction.png')});
  await page.keyboard.press('Control+z');
  assert.equal(await page.evaluate(()=>JSON.stringify(roiGeojsonObject())),before,'Subtraction undo is exact');
  await page.evaluate(()=>syncViewerState('roi_updated',{test:true}));
  await page.waitForFunction(()=>!wsiSyncSending && !wsiSyncQueue.length,{}, {timeout:30000});
  const sync=await page.evaluate(()=>({revision:wsiSyncAcknowledged?.revision,errors:viewerSyncHistory.filter(e=>e.direction==='error'),logs:viewerLogPayload().filter(e=>e.level==='error')}));
  assert.ok(sync.revision); assert.deepEqual(sync.errors,[]); assert.deepEqual(sync.logs,[]); assert.deepEqual(errors,[]);
  fs.writeFileSync(path.join(output,'report.json'),JSON.stringify({unit,selected,sync,errors},null,2));
  console.log('Wand filled selection, open notch, mask topology, subtraction, undo/redo and live R sync passed:',output);
})().catch(e=>{console.error(e);process.exitCode=1;}).finally(async()=>{if(browser)await browser.close();});
