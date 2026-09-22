// Use a disposable live viewer containing the large breast tissue GeoJSON.
const {chromium}=require(process.env.PLAYWRIGHT_MODULE||'playwright');
const assert=require('node:assert/strict'),fs=require('node:fs'),path=require('node:path'),os=require('node:os');
const url=process.env.WSITOOLS_TEST_URL;
assert.match(url||'',/^http:\/\/(127\.0\.0\.1|localhost)(:|\/)/);
const output=fs.mkdtempSync(path.join(os.tmpdir(),'wsitools-ctrl-dense-'));
let browser;
(async()=>{
  browser=await chromium.launch({channel:'chrome',headless:true});
  const page=await browser.newPage({viewport:{width:1600,height:1100}}),errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  await page.goto(url,{waitUntil:'domcontentloaded'});
  await page.waitForFunction(()=>osdReady&&layers.some(l=>l.items?.some(i=>i.point_count>200000)),{},{timeout:60000});
  const input=await page.evaluate(()=>{
    const layer=layers.find(l=>l.items?.some(i=>i.point_count>200000));
    const item=layer.items.reduce((a,b)=>a.point_count>b.point_count?a:b);
    const b=roiBounds(item);let p;
    for(let iy=1;iy<30&&!p;iy++)for(let ix=1;ix<30&&!p;ix++){
      const candidate={x:b.xmin+(b.xmax-b.xmin)*ix/30,y:b.ymin+(b.ymax-b.ymin)*iy/30};
      if(layerObjectAt(candidate)?.item===item)p=candidate;
    }
    if(!p)throw new Error('Could not locate an interior point');
    layers.push({id:'test-cells',type:'vector',source_type:'segmentation',visible:true,items:[
      {id:'test-cell',drawable:true,rings:[[{x:p.x-30,y:p.y-30},{x:p.x+30,y:p.y-30},{x:p.x+30,y:p.y+30},{x:p.x-30,y:p.y+30},{x:p.x-30,y:p.y-30}]]},
      {id:'test-spot',type:'point',x:p.x,y:p.y}
    ]});
    if(layerObjectAt(p)?.item.id!=='test-spot')throw new Error('Test point must cover the tissue target');
    const width=6000,height=width*innerHeight/innerWidth;
    osdViewer.viewport.fitBounds(osdViewer.world.getItemAt(0).imageToViewportRectangle(p.x-width/2,p.y-height/2,width,height),true);
    window.testDenseGeometry=()=>JSON.stringify(layers.map(l=>({id:l.id,items:(l.items||[]).map(i=>({id:i.id,geometry:wsiRoiCoordinates(i)}))})));
    window.testOriginalDense=testDenseGeometry();window.testWandCount=0;window.testBrushCount=0;
    const wand=runWandAt,brush=startBrush;
    runWandAt=(...args)=>{testWandCount++;return wand(...args);};startBrush=(...args)=>{testBrushCount++;return brush(...args);};
    return {point:p,id:item.id,vertices:item.point_count};
  });
  await page.waitForFunction(()=>osdViewer.world.getItemAt(0).getFullyLoaded(),{},{timeout:60000});
  const screen=await page.evaluate(p=>slideToCanvas(p),input.point),timings=[];
  for(const tool of ['Brush','Wand']){
    await page.locator('#tool'+tool).click();
    await page.evaluate(()=>clearSelectedLayerObject(true));
    await page.keyboard.down('Control');
    const start=Date.now();await page.mouse.dblclick(screen.x,screen.y,{delay:70});
    assert.equal(await page.evaluate(()=>selectedLayerObject()?.item.id),input.id);
    timings.push({tool,elapsed_ms:Date.now()-start});
    assert.equal(await page.evaluate(()=>mode),tool.toLowerCase());
    assert.equal(await page.evaluate(()=>testDenseGeometry()===testOriginalDense),true);
    assert.deepEqual(await page.evaluate(()=>[testWandCount,testBrushCount]),[0,0]);
    await page.screenshot({path:path.join(output,tool.toLowerCase()+'.png')});
    await page.keyboard.up('Control');
  }
  await page.waitForFunction(()=>!wsiSyncSending&&!wsiSyncQueue.length,{},{timeout:30000});
  const sync=await page.evaluate(()=>({revision:wsiSyncAcknowledged?.revision,errors:viewerSyncHistory.filter(e=>e.direction==='error'),logs:viewerLogPayload().filter(e=>e.level==='error')}));
  assert.ok(sync.revision);assert.deepEqual(sync.errors,[]);assert.deepEqual(sync.logs,[]);assert.deepEqual(errors,[]);
  fs.writeFileSync(path.join(output,'report.json'),JSON.stringify({input,timings,sync,errors},null,2));
  console.log('Dense imported-tissue selection passed:',output,JSON.stringify({vertices:input.vertices,timings}));
})().catch(e=>{console.error(e);process.exitCode=1;}).finally(async()=>{if(browser)await browser.close();});
