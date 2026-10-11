// V2-102 real UI, synthetic upload, frozen downloads, and offline formula rendering.
const fs=require('fs'),path=require('path');
const {chromium}=require(process.env.PLAYWRIGHT_MODULE||'playwright');
const base=process.env.LOG_MODELS_URL||'http://127.0.0.1:3852';
const out=process.env.LOG_MODELS_DOWNLOADS;if(!out)throw Error('Set LOG_MODELS_DOWNLOADS');fs.mkdirSync(out,{recursive:true});
const checks=[],errors=[];const check=(ok,label)=>{if(!ok)throw Error(label);checks.push(label);console.log('PASS: '+label)};
const near=(a,b)=>Math.abs(Number(a)-b)<=1e-7*Math.max(1,Math.abs(b));
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.CHROME_PATH});
 const page=await browser.newPage({viewport:{width:1450,height:1050},acceptDownloads:true});page.setDefaultTimeout(30000);page.on('pageerror',e=>errors.push(e.message));
 const tab=async name=>{await page.getByRole('tab',{name,exact:true}).click()};
 const settled=async()=>{await page.waitForFunction(()=>!document.querySelector('.shiny-busy'));await page.waitForTimeout(150)};
 const choose=async(id,value)=>{await page.locator('#'+id).waitFor({state:'attached'});await page.evaluate(({id,value})=>document.getElementById(id).selectize.setValue(value),{id,value});await page.waitForFunction(({id,value})=>Shiny.shinyapp.$inputValues[id]===value,{id,value});await settled()};
 const fill=async(id,value)=>{await page.locator('#'+id).fill(String(value));await page.locator('#'+id).dispatchEvent('change');await page.waitForFunction(({id,value})=>{const v=Shiny.shinyapp.$inputValues;const key=Object.keys(v).find(k=>k===id||k.startsWith(id+':'));return String(v[key])===String(value)},{id,value});await settled()};
 const fresh=async task=>{await page.goto(base);await page.waitForFunction(()=>window.Shiny?.shinyapp?.isConnected()&&Shiny.shinyapp.$inputValues.task==='home');await page.locator('#task_'+task).click();await page.locator(task==='design'?'#sim_run':'#run').waitFor({state:'visible'});await settled()};
 const download=async(id,name)=>{await page.locator('#'+id).waitFor({state:'visible'});const wait=page.waitForEvent('download');await page.locator('#'+id).click();const d=await wait;if(await d.failure())throw Error(await d.failure());await d.saveAs(path.join(out,name));return fs.readFileSync(path.join(out,name),'utf8')};
 const noErrors=async label=>check((await page.locator('.shiny-output-error:visible:not(:empty)').allTextContents()).length===0,label+' has no visible result errors');
 const run=async()=>{await page.locator('#run').click();await page.waitForSelector('#count_end_table table');await settled()};
 const save=async(name,target=false)=>{await tab('导出');const cfg=JSON.parse(await download('config_download',name+'-config.json'));await download(target?'summary_download':'curves_download',name+(target?'-milestones.csv':'-curves.csv'));await download('report_download',name+'-report.md');return cfg};
 try {
  for(const id of ['lognormal','loglogistic']) {
   await fresh('count');await tab('队列与资料');await fill('active_n',60);await fill('known_n',5);await fill('cut',4);await fill('duration',3);
   await tab('事件模型');await choose('design_model',id);await fill('median',9);await fill('scale',id==='lognormal'?.65:1.2);await choose('log_input','mu');
   check(near(await page.locator('#log_mu').inputValue(),Math.log(9))&&!await page.locator('#median').isVisible(),id+' median-to-mu toggle preserves distribution and changes visible form');
   for(const [unit,f] of [['days',1],['weeks',7],['months',30.4375]]) {
    await tab('队列与资料');await choose('time_unit',unit);await tab('事件模型');
    check(near(await page.locator('#log_mu').inputValue(),Math.log(9*30.4375/f))&&near(await page.locator('#scale').inputValue(),id==='lognormal'?.65:1.2),id+' '+unit+' converts mu and preserves sigma');
   }
   await choose('log_input','median');check(near(await page.locator('#median').inputValue(),9),id+' mu-to-median round trip');await choose('log_input','mu');
   await tab('未来入组与退出');await fill('future_n',0);await fill('dropout_rate',.02);
   await tab('运行设置');await fill('sims',50);await fill('horizon',12);await fill('seed',1741);await run();
   const cfg=await save(id);check(cfg.config.parameter_model.method===id&&cfg.config.parameter_model.input_basis==='mu'&&cfg.config.uncertainty==='plugin',id+' successful run freezes distribution, input form and interval scope');
   await tab('参数与计算记录');await download('parameters_download',id+'-parameters.csv');await noErrors(id);
   await tab('任务参数');await tab('事件模型');await fill('scale',0);await page.locator('#run').click();await tab('预测结果');await page.waitForFunction(()=>document.getElementById('run_status')?.textContent.includes('log-time scale'));
   await tab('导出');const retained=JSON.parse(await download('config_download',id+'-retained.json'));
   check(retained.config.parameter_model.params.scale===cfg.config.parameter_model.params.scale,id+' invalid sigma reports failure and retains last successful download');
  }
  // Parameter groups use distinct log-time models, with a separate current risk-age cohort.
  await fresh('count');await tab('队列与资料');await choose('time_unit','days');await fill('cut',100);await page.locator('#analysis_mode input[value="grouped"]').check();await tab('分组参数');
  for(const [j,id] of [[1,'lognormal'],[2,'loglogistic']]) {
   if(j===2)await page.locator('#group_inputs a[data-value="组 2 · B"]').click();
   await choose('g'+j+'_design_model',id);await fill('g'+j+'_active_n',30);await fill('g'+j+'_known_n',2);await fill('g'+j+'_duration',80);await fill('g'+j+'_median',180);await fill('g'+j+'_scale',.7);await fill('g'+j+'_future_n',0);await choose('g'+j+'_log_input','mu');
   check(near(await page.locator('#g'+j+'_log_mu').inputValue(),Math.log(180)),'group '+j+' has working log-time parameter conversion');
  }
  await tab('运行设置');await fill('sims',50);await fill('horizon',120);await run();const grouped=await save('grouped');
  check(grouped.config.groups.g1.parameter_model.method==='lognormal'&&grouped.config.groups.g2.parameter_model.method==='loglogistic','known parameter groups freeze distinct distributions');await tab('分组事件');await download('group_curves_download','grouped-group-curves.csv');await noErrors('grouped parameters');
  // Data-fitting candidates, using the same independently generated V2-101 fixture.
  for(const grouped of [false,true]) {
   await fresh('count');await tab('队列与资料');await choose('time_unit','days');await page.locator('#input_mode input[value="adtte"]').check();await page.locator('#file').setInputFiles(path.join(process.env.SAS_CHECK_FIXTURES,'synthetic-v8.xpt'));
   await page.waitForFunction(()=>document.getElementById('paramcd')?.selectize?.getValue()==='OS');await choose('analysis_flag','ANL01FL');await fill('cut',100);
   if(grouped){await page.locator('#analysis_mode input[value="grouped"]').check();await page.waitForFunction(()=>document.getElementById('group_column')?.selectize?.getValue()==='TRTP')}
   await tab('事件模型');for(const box of await page.locator('#methods input').all())await box.uncheck();for(const id of ['lognormal','loglogistic'])await page.locator('#methods input[value="'+id+'"]').check();
   if(grouped){await tab('分组参数');await choose('g1_fit_method','lognormal');await fill('g1_future_n',0);await page.locator('#group_inputs a[data-value="组 2 · B"]').click();await choose('g2_fit_method','loglogistic');await fill('g2_future_n',0)}else{await tab('未来入组与退出');await fill('future_n',0)}
   await tab('运行设置');await fill('sims',50);await fill('horizon',80);await run();const cfg=await save(grouped?'fitted-grouped':'fitted');
   check(cfg.data_summary.n===60&&cfg.data_summary.events===36&&cfg.config.methods.includes('lognormal')&&cfg.config.methods.includes('loglogistic'),(grouped?'grouped':'pooled')+' XPT upload fits log-time models on 60 filtered rows');await noErrors(grouped?'grouped fit':'pooled fit');
  }
  await fresh('target');await tab('事件模型');await choose('design_model','loglogistic');await fill('scale',1.2);await tab('未来入组与退出');await fill('future_n',10);await tab('运行设置');await fill('target',10000);await fill('sims',50);await page.locator('#run').click();await page.waitForSelector('#milestones table');await settled();const target=await save('target',true);
  check(target.config.task==='target'&&(await page.locator('#milestones').innerText()).includes('超过预测窗口'),'log-logistic unreachable target keeps open dates and all simulated paths');
  // Fixed-DCO two-group data generation, followed by actual script replay in R.
  await fresh('design');await tab('研究设置');await fill('sim_n',120);await fill('sim_reps',2);await fill('sim_cuts','12,24');await tab('组别与分布');
  for(const [j,id] of [[1,'lognormal'],[2,'loglogistic']]) {
   if(j===2)await page.locator('#sim_group_inputs a[data-value="组 2"]').click();await choose('s'+j+'_design_model',id);await fill('s'+j+'_median',9);await fill('s'+j+'_scale',j===1?.65:1.2);await choose('s'+j+'_log_input','mu');
   check(near(await page.locator('#s'+j+'_log_mu').inputValue(),Math.log(9)),'simulation group '+j+' toggles log-time parameter form');
  }
  await tab('研究设置');await choose('sim_unit','days');await tab('组别与分布');
  check(near(await page.locator('#s1_log_mu').inputValue(),Math.log(9*30.4375))&&near(await page.locator('#s2_log_mu').inputValue(),Math.log(9*30.4375)),'simulation converts both log locations to days');
  await page.locator('#sim_run').click();await page.waitForSelector('#sim_summary table');await settled();await download('sim_observed_download','sim-observed.csv');await download('sim_adtte_download','sim-adtte.csv');await tab('导出');
  const scfg=JSON.parse(await download('sim_config_download','sim-config.json'));for(const [id,name] of [['sim_truth_download','sim-truth.csv'],['sim_summary_download','sim-summary.csv'],['sim_script_download','sim-reproduce.R'],['sim_report_download','sim-report.md']])await download(id,name);
  check(scfg.config.groups[0].model.id==='lognormal'&&scfg.config.groups[1].model.id==='loglogistic'&&scfg.config.cut_mode==='fixed','simulation exports distinct log-time distributions and fixed-DCO configuration');await noErrors('log-time simulation');
  await tab('工作手册');await page.waitForFunction(()=>document.querySelectorAll('.hb-equation').length>=20&&document.querySelectorAll('.core-handbook script[type^="math/tex"]').length===0);
  check((await page.locator('.core-handbook').innerText()).includes('5.2 Log-normal')&&await page.locator('[data-mml-node="merror"]').count()===0,'new log-time manual formulas render online');await download('handbook_html_download','log-models-handbook.html');
  const offline=await browser.newPage();const requests=[];await offline.route(/^https?:/,r=>{requests.push(r.request().url());return r.abort()});await offline.goto('file://'+path.join(out,'log-models-handbook.html'));await offline.waitForFunction(()=>document.querySelectorAll('.hb-equation').length>=20&&document.querySelectorAll('script[type^="math/tex"]').length===0);
  check(await offline.locator('[data-mml-node="merror"]').count()===0&&requests.length===0,'downloaded manual renders new formulas offline without external requests');await offline.close();
  await page.setViewportSize({width:390,height:844});check(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+2),'updated manual fits a narrow screen');await page.screenshot({path:path.join(out,'manual-mobile.png')});
  check(errors.length===0,'no uncaught browser JavaScript errors');fs.writeFileSync('validation/v2_log_models_browser_results.json',JSON.stringify({status:'passed',passed:checks.length,checks,errors,scope:'actual parameter, grouped, XPT-fit, target and fixed-DCO simulation downloads; online/offline manual'},null,2)+'\n');
 }catch(e){await page.screenshot({path:path.join(out,'failure.png')});console.error(await page.locator('.shiny-output-error:visible:not(:empty)').allTextContents());throw e}finally{await browser.close()}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
