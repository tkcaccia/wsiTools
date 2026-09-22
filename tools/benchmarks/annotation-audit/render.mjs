import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';
import assert from 'node:assert/strict';
import {dep,requireDep,load,save} from './common.mjs';
const {chromium}=requireDep('playwright-core');
const input=path.resolve(process.argv[2]),summary=load(input).summary;
const pageSource=fs.readFileSync(new URL('./render-page.js',import.meta.url));
const earcut=fs.readFileSync(requireDep.resolve('earcut'));
const server=http.createServer((req,res)=>{
  if(req.url==='/annotations'){res.setHeader('Content-Type','application/json');fs.createReadStream(input).pipe(res);}
  else if(req.url==='/earcut.js'){res.setHeader('Content-Type','text/javascript');res.end(earcut);}
  else if(req.url==='/benchmark.js'){res.setHeader('Content-Type','text/javascript');res.end(pageSource);}
  else {res.setHeader('Content-Type','text/html');res.end('<!doctype html><html><head><style>html,body{margin:0;background:white}canvas,svg{position:absolute;left:0;top:0;width:1024px;height:768px}</style></head><body><canvas width="1024" height="768"></canvas><svg width="1024" height="768"></svg><script type="module" src="/benchmark.js"></script></body></html>');}
});
await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
let browser;
try {
  browser=await chromium.launch({channel:'chrome',headless:true});
  const result={dataset:summary,browser:browser.version(),modes:[],note:'Actual tissue polygon fill-only component benchmark at 1024x768 DPR1. No tissue imagery, labels, border tessellation, hit testing or R synchronization. Canvas flush uses 1px readback, WebGL uses finish, SVG uses layout then two RAF; these are not identical fences. Screenshot coverage and nonblank checks validate rendering only, not exact topology.'};
  for(const mode of ['canvas_rebuild','canvas_cached','svg','webgl']) {
    const page=await browser.newPage({viewport:{width:1024,height:768},deviceScaleFactor:1});const errors=[];page.on('pageerror',err=>errors.push(err.message));
    await page.goto(`http://127.0.0.1:${server.address().port}`);
    await page.waitForFunction(()=>window.auditReady,{},{timeout:60000});
    const record=await page.evaluate(mode=>window.runRender(mode),mode);
    assert.equal(errors.length,0);if(!record.unavailable)assert.ok(record.nonblank>0);
    const png='/tmp/wsitools-annotation-audit-'+mode+'.png';await page.screenshot({path:png});record.screenshot=png;
    result.modes.push(record);console.log(mode,JSON.stringify(record));await page.close();
  }
  save(process.argv[3],result);
} finally {if(browser)await browser.close();await new Promise(resolve=>server.close(resolve));}
