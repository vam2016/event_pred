// Offline guide verification; every network attempt is blocked and recorded.
const fs=require('fs'),path=require('path'),crypto=require('crypto');
const {chromium}=require(process.env.PLAYWRIGHT_MODULE||'playwright');
const html=process.env.LEARNING_OUTPUT||'reports/learning-v2/SurvCast_STATISTICIAN_GUIDE.html';
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const checks=[],errors=[],requests=[],check=(ok,label)=>{if(!ok)throw Error(label);checks.push(label);console.log('PASS: '+label)};
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.CHROME_PATH});
 try {
  const page=await browser.newPage({viewport:{width:1440,height:1000}});
  page.on('pageerror',e=>errors.push(e.message));
  await page.route(/^https?:/,route=>{requests.push(route.request().url());return route.abort()});
  const expected=(fs.readFileSync(html,'utf8').match(/<script type="math\/tex/g)||[]).length;
  await page.goto('file://'+path.resolve(html));
  await page.waitForFunction(n=>document.querySelectorAll('.hb-equation').length===n&&document.querySelectorAll('.core-handbook script[type^="math/tex"]').length===0,expected,{timeout:30000});
  check(expected>362,'all historical and added formulas finish rendering ('+expected+')');
  check(await page.locator('[data-mml-node="merror"]').count()===0,'no MathJax formula errors');
  check(await page.locator('.math-fallback').count()===0,'no fallback math strings');
  const text=await page.locator('article').innerText();
  check(!text.includes('\uFFFD')&&!/\\(?:frac|widehat|operatorname|Phi|sigma|begin)\b/.test(text),'no broken encoding or visible TeX leakage');
  const stats=await page.evaluate(()=>({tables:document.querySelectorAll('article table').length,tableFormulas:document.querySelectorAll('article table .hb-equation').length,images:[...document.querySelectorAll('article img')].map(x=>({loaded:x.complete&&x.naturalWidth>0,embedded:x.src.startsWith('data:image/svg+xml;base64,')})),details:document.querySelectorAll('article details').length}));
  check(stats.tables>=26&&stats.tableFormulas>0,'tables preserve rendered inline formulas ('+stats.tables+' tables)');
  check(stats.images.length===7&&stats.images.every(x=>x.loaded&&x.embedded),'all seven diagrams decode from embedded SVG');
  check(stats.details===12,'all twelve original self-tests available');
  for(let n=1;n<=34;n++) {
   const id='s'+String(n).padStart(2,'0');
   check(await page.locator('#'+id).count()===1&&await page.locator('nav a[href="#'+id+'"]').count()===1,'chapter '+n+' has one anchor and navigation entry');
  }
  for(const id of ['log_time','bootstrap','validation','event_simulation'])check(text.includes(id),'current shared case '+id+' is readable');
  check(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+2),'desktop reading fits viewport');
  await page.locator('nav a[href="#s30"]').click();await page.waitForFunction(()=>location.hash==='#s30');await page.waitForFunction(()=>{const r=document.getElementById('s30').getBoundingClientRect();return r.top>=0&&r.top<200});
  check(await page.locator('#s30').evaluate(e=>{const r=e.getBoundingClientRect();return r.top>=0&&r.top<innerHeight}),'added chapter navigation lands below fixed header');
  const details=page.locator('article details').first();await details.locator('summary').click();check(await details.evaluate(e=>e.open),'self-test disclosure opens');
  await page.setViewportSize({width:390,height:844});await page.reload();await page.waitForFunction(n=>document.querySelectorAll('.hb-equation').length===n,expected);
  check(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+2),'390px reading has no document overflow');
  await page.locator('#contents summary').click();await page.locator('nav a[href="#s34"]').click();await page.waitForFunction(()=>location.hash==='#s34');await page.waitForFunction(()=>{const r=document.getElementById('s34').getBoundingClientRect();return r.top>=0&&r.top<200});
  check(await page.locator('#s34').evaluate(e=>{const r=e.getBoundingClientRect();return r.top>=0&&r.top<innerHeight}),'mobile navigation reaches last new chapter');
  await page.screenshot({path:'reports/learning-v2/learning-mobile.png'});
  await page.emulateMedia({media:'print'});
  check(await page.locator('.top').evaluate(e=>getComputedStyle(e).display==='none')&&await page.locator('aside').evaluate(e=>getComputedStyle(e).display==='none'),'print stylesheet hides controls and navigation');
  check(requests.length===0,'offline reading makes zero network requests');check(errors.length===0,'zero uncaught browser errors');
  fs.writeFileSync('validation/v2_learning_browser_results.json',JSON.stringify({status:'passed',passed:checks.length,checks,formulas:expected,...stats,errors,requests,html_sha256:sha(html),manifest_sha256:sha('docs/learning-assets/guide-manifest.json'),scope:'Actual standalone HTML; desktop1440 and mobile390; blocked network; print CSS, not PDF pagination'},null,2)+'\n');
  console.log('LEARNING_BROWSER_CHECKS_PASSED '+checks.length);
 }finally {await browser.close()}
})().catch(e=>{console.error(e);process.exit(1)});
