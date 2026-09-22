// Use a disposable live project with two 800x600 images from wand-fixture.R.
const {chromium} = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const assert = require('node:assert/strict'), fs = require('node:fs'), path = require('node:path'), os = require('node:os');
const url = process.env.WSITOOLS_TEST_URL;
assert.match(url || '', /^http:\/\/(127\.0\.0\.1|localhost)(:|\/)/);
const output = fs.mkdtempSync(path.join(os.tmpdir(), 'wsitools-ctrl-selection-'));
let browser;
(async () => {
  browser = await chromium.launch({channel:'chrome', headless:true});
  const page = await browser.newPage({viewport:{width:1600,height:1100}}), errors=[];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(url, {waitUntil:'domcontentloaded'});
  await page.waitForFunction(()=>osdReady && osdViewer.world.getItemAt(0)?.getFullyLoaded(), {}, {timeout:60000});
  await page.evaluate(()=>{
    window.testImport = prefix => addImportedGeojson({type:'FeatureCollection',features:[
      {type:'Feature',id:prefix+'-stroma',properties:{name:'Stroma',classification:{name:'Stroma',color:[50,170,90]}},geometry:{type:'Polygon',coordinates:[[[170,130],[380,130],[380,470],[170,470],[170,130]]]}},
      {type:'Feature',id:prefix+'-tumour',properties:{name:'Tumour',classification:{name:'Tumour',color:[230,100,40]}},geometry:{type:'Polygon',coordinates:[[[420,130],[630,130],[630,470],[420,470],[420,130]]]}}
    ]}, 'Tissue annotation');
    testImport('first'); selectAnnotation(-1,true); draw();
    window.testWandCalls=0; window.testBrushCalls=0;
    const wand=runWandAt, brush=startBrush;
    runWandAt=(...args)=>{testWandCalls++;return wand(...args);};
    startBrush=(...args)=>{testBrushCalls++;return brush(...args);};
    window.testGeometry=()=>JSON.stringify(rois.map(r=>({id:r.id,geometry:wsiRoiCoordinates(r)})));
  });
  const point=await page.evaluate(()=>slideToCanvas({x:550,y:300}));
  for (const tool of ['Brush','Wand']) {
    await page.locator('#tool'+tool).click();
    await page.evaluate(()=>{selectAnnotation(-1,true);draw();});
    const before=await page.evaluate(()=>({geometry:testGeometry(),undo:annotationUndo.length,zoom:osdViewer.viewport.getZoom()}));
    await page.keyboard.down('Control');
    await page.mouse.click(point.x,point.y);
    assert.equal(await page.evaluate(()=>selectedRoi),-1,'Ctrl single-click does not select or edit');
    await page.keyboard.up('Control');
    await page.keyboard.down('Control');
    await page.mouse.dblclick(point.x,point.y,{delay:90});
    assert.equal(await page.evaluate(()=>rois[selectedRoi]?.id),'first-tumour');
    assert.equal(await page.evaluate(()=>mode),tool.toLowerCase());
    assert.equal(await page.locator('#roiList .roiItem.active').count(),1);
    assert.equal(await page.evaluate(()=>getComputedStyle(canvas).cursor),'default');
    const empty=await page.evaluate(()=>slideToCanvas({x:700,y:520}));
    await page.mouse.dblclick(empty.x,empty.y,{delay:80});
    assert.equal(await page.evaluate(()=>rois[selectedRoi]?.id),'first-tumour','Empty Ctrl double-click preserves selection');
    await page.screenshot({path:path.join(output,tool.toLowerCase()+'-selected.png')});
    await page.keyboard.up('Control');
    assert.equal(await page.evaluate(()=>wsiAnnotationPickMode()),false);
    assert.deepEqual(await page.evaluate(()=>({geometry:testGeometry(),undo:annotationUndo.length,zoom:osdViewer.viewport.getZoom()})),before);
    assert.deepEqual(await page.evaluate(()=>[testWandCalls,testBrushCalls]),[0,0]);
  }
  // Real brush editing and exact undo still work after releasing Control.
  await page.locator('#toolBrush').click();
  const edge=await page.evaluate(()=>slideToCanvas({x:630,y:300}));
  const beforeEdit=await page.evaluate(()=>testGeometry());
  await page.mouse.move(edge.x-4,edge.y); await page.mouse.down();
  await page.mouse.move(edge.x+12,edge.y+12,{steps:5}); await page.mouse.up();
  await page.waitForFunction(()=>!brushing&&!wsiEditPromise,{}, {timeout:30000});
  assert.equal(await page.evaluate(()=>testBrushCalls),1);
  assert.notEqual(await page.evaluate(()=>testGeometry()),beforeEdit);
  await page.keyboard.press('Control+z');
  assert.equal(await page.evaluate(()=>testGeometry()),beforeEdit);

  await page.evaluate(()=>{saveActiveProjectAnnotations();setMultiViewLayout(2);});
  await page.waitForFunction(()=>multiViewPanes.length===2&&multiViewPanes.every(p=>!p.blank&&p.viewer?.world.getItemAt(0)?.getFullyLoaded()),{}, {timeout:60000});
  await page.evaluate(()=>{multiViewActivatePane(1);testImport('second');selectAnnotation(-1,true);saveActiveProjectAnnotations();multiViewActivatePane(0);draw();});
  for (const [index,tool,prefix] of [[1,'Wand','second'],[0,'Brush','first']]) {
    await page.locator('#tool'+tool).click();
    const screen=await page.evaluate(index=>{const pane=multiViewPanes[index],p=multiViewSlideToCanvas({x:550,y:300},pane),r=pane.overlay.getBoundingClientRect();return {x:r.left+p.x,y:r.top+p.y};},index);
    const zooms=await page.evaluate(()=>multiViewPanes.map(p=>p.viewer.viewport.getZoom()));
    await page.keyboard.down('Control');
    await page.mouse.dblclick(screen.x,screen.y,{delay:90});
    await page.keyboard.up('Control');
    assert.equal(await page.evaluate(()=>activeProjectIndex),index,'Selects the correct tissue, not the previous pane');
    assert.equal(await page.evaluate(()=>rois[selectedRoi]?.id),prefix+'-tumour');
    assert.equal(await page.evaluate(()=>mode),tool.toLowerCase());
    assert.deepEqual(await page.evaluate(()=>multiViewPanes.map(p=>p.viewer.viewport.getZoom())),zooms);
  }
  assert.deepEqual(await page.evaluate(()=>[testWandCalls,testBrushCalls]),[0,1]);
  await page.screenshot({path:path.join(output,'multiview-selection.png')});
  await page.waitForFunction(()=>!wsiSyncSending&&!wsiSyncQueue.length,{}, {timeout:30000});
  const sync=await page.evaluate(async()=>({revision:wsiSyncAcknowledged?.revision,errors:viewerSyncHistory.filter(e=>e.direction==='error'),
    logs:viewerLogPayload().filter(e=>e.level==='error'),state:await(await fetch(cfg.viewer_state_url)).json()}));
  assert.ok(sync.revision);assert.deepEqual(sync.errors,[]);assert.deepEqual(sync.logs,[]);assert.deepEqual(errors,[]);
  fs.writeFileSync(path.join(output,'report.json'),JSON.stringify({sync,errors},null,2));
  console.log('Ctrl selection, unchanged tools/geometry/zoom, subsequent editing/undo, multi-view tissue scoping and live R sync passed:',output);
})().catch(error=>{console.error(error);process.exitCode=1;}).finally(async()=>{if(browser)await browser.close();});
