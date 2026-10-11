// Display compatibility after shared group summaries gained request/failure counts.
const fs=require('fs');const {chromium}=require(process.env.PLAYWRIGHT_MODULE||'playwright');
const checks=[],errors=[];const check=(ok,label)=>{if(!ok)throw Error(label);checks.push(label);console.log('PASS: '+label)};
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.CHROME_PATH});
 try{
  const page=await browser.newPage({viewport:{width:1450,height:1050}});page.setDefaultTimeout(30000);page.on('pageerror',e=>errors.push(e.message));
  const tab=async name=>{await page.getByRole('tab',{name,exact:true}).click()};
  const fill=async(id,value)=>{await page.locator('#'+id).fill(String(value));await page.locator('#'+id).dispatchEvent('change');await page.waitForFunction(({id,value})=>{const v=Shiny.shinyapp.$inputValues;const key=Object.keys(v).find(k=>k===id||k.startsWith(id+':'));return String(v[key])===String(value)},{id,value})};
  await page.goto(process.env.BOOTSTRAP_FULL_URL||'http://127.0.0.1:3854');await page.waitForFunction(()=>window.Shiny?.shinyapp?.isConnected()&&Shiny.shinyapp.$inputValues.task==='home');await page.locator('#task_count').click();await page.locator('#run').waitFor({state:'visible'});
  await tab('队列');await page.locator('#analysis_mode input[value="grouped"]').check();await tab('分组参数');await page.locator('#g1_future_n').waitFor({state:'visible'});await fill('g1_future_n',20);await page.locator('#group_inputs a[data-value="组 2 · B"]').click();await fill('g2_future_n',20);await tab('运行设置');await fill('sims',50);await page.locator('#run').click();await page.waitForSelector('#count_end_table table');await tab('分组事件');await page.waitForSelector('#group_event_table table');
  const text=await page.locator('#group_event_table').innerText();check(text.includes('有效联合次数')&&text.includes('请求次数')&&text.includes('联合失败次数'),'full entry renders all new shared group count columns');
  check(text.includes('A')&&text.includes('B'),'full entry retains both original treatment group rows');
  check((await page.locator('.shiny-output-error:visible:not(:empty)').allTextContents()).length===0,'full entry group result table and plots have no visible errors');check(errors.length===0,'full entry compatibility run has no uncaught JavaScript errors');
  fs.writeFileSync('validation/v2_bootstrap_full_entry_browser_results.json',JSON.stringify({status:'passed',passed:checks.length,checks,errors,scope:'shared group summary display compatibility; parameter-mode two-group count forecast; no advanced method validation'},null,2)+'\n');
 }finally{await browser.close()}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
