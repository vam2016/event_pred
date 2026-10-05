/* Build from the source root. Requires Node + Playwright + installed Chromium.
   Environment: PLAYWRIGHT_MODULE (optional module path), CHROME_PATH (optional).
   SVG font paths are embedded in each equation; runtime/HTML need no network. */
const fs=require('fs'),path=require('path'),crypto=require('crypto'),{spawnSync}=require('child_process');
const {chromium}=require(process.env.PLAYWRIGHT_MODULE||'playwright');
(async()=>{
  const deferValidation=process.argv.includes("--defer-validation");
  const root=process.cwd(),scratch=fs.mkdtempSync(require('os').tmpdir()+'/event-pred-handbook-');
  let browser;
  try {
    const raw=spawnSync('Rscript',['-e','source("R/handbook.R"); cat(handbook_html())'],{encoding:'utf8'});
    if(raw.status!==0)throw Error(raw.stderr);
    const engine=path.join(root,'scripts/vendor/mathjax-tex-svg-3.2.2.js');
    const input=path.join(scratch,'source.html');
    fs.writeFileSync(input,'<!doctype html><meta charset="utf-8"><script>MathJax={loader:{load:[]},tex:{packages:["base","ams"],autoload:false},svg:{fontCache:"local"},options:{enableMenu:false},startup:{typeset:false}};</script><script src="file://'+engine+'"></script><main>'+raw.stdout+'</main>');
    browser=await chromium.launch({headless:true,...(process.env.CHROME_PATH?{executablePath:process.env.CHROME_PATH}:{})});
    const page=await browser.newPage({viewport:{width:1100,height:900}});
    const requests=[];await page.route('http://**',r=>{requests.push(r.request().url());r.abort()});await page.route('https://**',r=>{requests.push(r.request().url());r.abort()});
    await page.goto('file://'+input);await page.waitForFunction(()=>window.MathJax&&MathJax.tex2svgPromise);
    await page.evaluate(()=>MathJax.startup.promise);
    const result=await page.evaluate(async(deferValidation)=>{
      const scripts=[...document.querySelectorAll('script[type^="math/tex"]')];
      let errors=[];
      for(let i=0;i<scripts.length;i++){
        const s=scripts[i],tex=s.textContent.trim(),display=s.type.includes('mode=display');
        const svg=await MathJax.tex2svgPromise(tex,{display,em:16,ex:8,containerWidth:800});
        if(!deferValidation&&svg.querySelector('[data-mml-node="merror"]'))errors.push({i,tex,error:svg.textContent});
        const wrapper=document.createElement('span');wrapper.className=display?'hb-equation hb-display':'hb-equation hb-inline';
        wrapper.setAttribute('data-equation',i+1);wrapper.setAttribute('data-tex',tex);
        svg.querySelectorAll('mjx-assistive-mml').forEach(x=>x.remove());
        const image=svg.querySelector('svg');image.removeAttribute('aria-hidden');image.setAttribute('aria-label',tex);
        svg.setAttribute('aria-label',tex);wrapper.appendChild(svg);s.replaceWith(wrapper);
      }
      const styles=[...document.head.querySelectorAll('style')].map(x=>x.outerHTML).join('');
      return {html:styles+document.querySelector('main').innerHTML,count:scripts.length,errors};
    },deferValidation);
    if(!deferValidation&&result.errors.length)throw Error(JSON.stringify(result.errors));
    if(!deferValidation&&requests.length)throw Error('Rendering requested external assets: '+JSON.stringify(requests));
    const metadata={markdown_md5:crypto.createHash('md5').update(fs.readFileSync('docs/METHODS.md')).digest('hex'),engine:'MathJax 3.2.2 SVG',equations:result.count,parse_errors:deferValidation?null:0,external_requests:deferValidation?null:0,validation_status:deferValidation?"deferred":"render_checked"};
    fs.writeFileSync('docs/handbook-rendered.html',result.html);
    fs.writeFileSync('docs/handbook-rendered.json',JSON.stringify(metadata,null,2)+'\n');
    console.log(JSON.stringify(metadata,null,2));
  } finally {if(browser)await browser.close();fs.rmSync(scratch,{recursive:true,force:true})}
})().catch(e=>{console.error(e);process.exit(1)});
