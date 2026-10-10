// Actual core file upload, prediction and download checks using synthetic fixtures.
const fs = require('fs'), path = require('path');
const {chromium} = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const base = process.env.SAS_CHECK_URL || 'http://127.0.0.1:3851';
const fixtures = process.env.SAS_CHECK_FIXTURES;
if (!fixtures) throw Error('Set SAS_CHECK_FIXTURES to the generated synthetic fixture directory');
const out = process.env.SAS_BROWSER_DOWNLOADS || path.resolve('sas-check-downloads');
const output = process.env.SAS_BROWSER_OUTPUT || 'validation/v2_sas_browser_results.json';
fs.mkdirSync(out, {recursive:true});
const checks = [], jsErrors = [];
const check = (ok,label) => {if(!ok) throw Error(label); checks.push(label); console.log('PASS: '+label)};
(async()=>{
  const options = {headless:true}; if(process.env.CHROME_PATH) options.executablePath = process.env.CHROME_PATH;
  const browser = await chromium.launch(options);
  const page = await browser.newPage({viewport:{width:1450,height:1050},acceptDownloads:true});
  page.setDefaultTimeout(30000); page.on('pageerror',e=>jsErrors.push(e.message));
  const tab = async name => {await page.getByRole('tab',{name,exact:true}).click()};
  const settled = async () => {await page.waitForFunction(()=>!document.querySelector('.shiny-busy'))};
  const choose = async (id,value) => {
    await page.evaluate(({id,value})=>document.getElementById(id).selectize.setValue(value),{id,value});
    await page.waitForFunction(({id,value})=>Shiny.shinyapp.$inputValues[id]===value,{id,value}); await settled();
  };
  const fill = async (id,value) => {
    await page.locator('#'+id).fill(String(value)); await page.locator('#'+id).dispatchEvent('change');
    await page.waitForFunction(({id,value})=>{
      const inputs=Shiny.shinyapp.$inputValues;
      const key=Object.keys(inputs).find(k=>k===id || k.startsWith(id+':'));
      return String(inputs[key])===String(value);
    },{id,value}); await settled();
  };
  const download = async (id,name) => {
    const pending=page.waitForEvent('download');await page.locator('#'+id).click();const d=await pending;
    if(await d.failure()) throw Error(await d.failure()); await d.saveAs(path.join(out,name));
    return fs.readFileSync(path.join(out,name),'utf8');
  };
  async function setup(filename,unit='days',grouped=false,task='count') {
    await page.goto(base);
    await page.waitForFunction(()=>window.Shiny?.shinyapp?.isConnected() && Shiny.shinyapp.$inputValues.task==='home');
    await page.locator('#task_'+task).click(); await page.locator('#run').waitFor({state:'visible'});
    await tab('队列与资料'); await choose('time_unit',unit);
    await page.locator('#input_mode input[value="adtte"]').check();
    await page.locator('#file').setInputFiles(path.join(fixtures,filename));
    await page.waitForFunction(()=>document.getElementById('paramcd')?.selectize?.getValue()==='OS');
    await choose('analysis_flag','ANL01FL'); await fill('cut',unit==='months'?100/30.4375:100);
    if(filename==='synthetic-weeks.xpt') await choose('aval_unit','weeks');
    if(grouped) {await page.locator('#analysis_mode input[value="grouped"]').check();
      await page.waitForFunction(()=>document.getElementById('group_column')?.selectize?.getValue()==='TRTP');}
    await tab('事件模型');
    for(const box of await page.locator('#methods input').all()) await box.uncheck();
    await page.locator('#methods input[value="exponential"]').check();
    if(grouped) {
      await tab('分组参数');await page.locator('#g1_future_n').waitFor({state:'visible'});await fill('g1_future_n',0);
      await page.locator('#group_inputs a[data-value="组 2 · B"]').click();await fill('g2_future_n',0);
    } else {await tab('未来入组与退出');await fill('future_n',0);}
    await tab('运行设置');await fill('sims',50);await fill('seed',8173);await fill('horizon',unit==='months'?80/30.4375:80);
    if(task==='target') await fill('target',45);
    await settled();
  }
  const cases=[['csv','synthetic.csv','days',false],['v5','synthetic-v5.xpt','days',false],
    ['v8','synthetic-v8.xpt','days',false],['sas','synthetic.sas7bdat','days',false],
    ['weeks','synthetic-weeks.xpt','months',false],['grouped','synthetic.sas7bdat','days',true],
    ['target','synthetic.sas7bdat','days',false,'target'],['numeric','synthetic-numeric.xpt','days',false]];
  try {
    for(const [name,filename,unit,grouped,task='count'] of cases) {
      await setup(filename,unit,grouped,task);
      if(name==='numeric') {
        await page.locator('#run').click();await tab('预测结果');await page.waitForFunction(()=>document.getElementById('run_status')?.textContent.includes('数值 SAS 日期请选择 SAS 编码'));
        check(true,'numeric SAS dates fail visibly until encoding is selected');
        await tab('任务参数');await tab('队列与资料');await choose('date_encoding','sas');
      }
      await page.locator('#run').click();await page.waitForSelector(task==='target'?'#milestones table':'#count_end_table table');await settled();
      await tab('导出');const saved=JSON.parse(await download('config_download',name+'-config.json'));
      await download(task==='target'?'summary_download':'curves_download',name+(task==='target'?'-milestones.csv':'-curves.csv'));
      const report=await download('report_download',name+'-report.md');
      check(saved.version==='2.0.0-dev' && saved.data_summary.n===60 && saved.data_summary.events===36,name+' upload runs on filtered synthetic analysis rows');
      check(saved.config.adtte_source.format===path.extname(filename).slice(1) && saved.config.adtte_mapping.flag==='ANL01FL',name+' downloaded config freezes format and analysis mapping');
      check(report.includes('adtte_source') && report.includes('未包含参数估计不确定性'),name+' report includes source metadata and interval scope');
      if(grouped) check(saved.config.analysis_mode==='grouped' && saved.config.adtte_mapping.group_column==='TRTP', 'SAS known-group upload freezes chosen treatment column');
      if(task==='target') check(saved.config.task==='target' && saved.config.target===45,'SAS target-date prediction preserves cumulative target and task');
      if(name==='weeks') check(saved.config.display_unit==='months' && saved.config.adtte_mapping.aval_unit==='weeks','display months stay independent of file week units');
      if(name==='v8') {
        await tab('任务参数');await tab('运行设置');await fill('seed',19);
        await tab('预测结果');await tab('导出');const frozen=JSON.parse(await download('config_download','frozen-config.json'));
        check(frozen.config.seed===8173,'editing controls retains last successful SAS configuration');
      }
      check((await page.locator('.shiny-output-error:visible:not(:empty)').allTextContents()).length===0,name+' results have no visible output errors');
    }
    await tab('任务参数');await tab('队列与资料');
    await page.locator('#file').setInputFiles(path.join(fixtures,'corrupt.xpt'));
    await page.waitForFunction(()=>document.getElementById('file_status')?.textContent.includes('读取 ADTTE（XPT）失败'));
    check(true,'damaged file reports a visible format-specific import error');
    await page.locator('#run').click();await tab('预测结果');await page.waitForFunction(()=>document.getElementById('run_status')?.textContent.includes('读取 ADTTE（XPT）失败'));
    await tab('导出');const retained=JSON.parse(await download('config_download','retained-config.json'));
    check(retained.config.adtte_mapping.date_encoding==='sas' && retained.data_summary.n===60,'failed import preserves last successful downloadable result');
    check(jsErrors.length===0,'no uncaught browser JavaScript errors');
    fs.mkdirSync(path.dirname(output),{recursive:true});
    fs.writeFileSync(output,JSON.stringify({status:'passed',scope:'real core synthetic uploads, forecasts, downloads, frozen configuration and visible failures',passed:checks.length,checks,jsErrors},null,2)+'\n');
  } catch(e) {await page.screenshot({path:path.join(out,'failure.png')});console.error(await page.locator('.shiny-output-error:visible:not(:empty)').allTextContents());throw e;}
  finally {await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
