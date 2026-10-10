// Loading/navigation smoke checks; no advanced statistical run is certified.
const fs = require('fs'), path = require('path');
const { chromium } = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const checks = [], jsErrors = [];
const check = (ok, label) => {
  if (!ok) throw new Error(label);
  checks.push(label); console.log('PASS: ' + label);
};
(async () => {
  const options = { headless: true };
  if (process.env.CHROME_PATH) options.executablePath = process.env.CHROME_PATH;
  const browser = await chromium.launch(options);
  try {
    const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });
    page.on('pageerror', e => jsErrors.push(e.message));
    await page.goto(process.env.EXTENSION_CHECK_URL || 'http://127.0.0.1:3850');
    await page.waitForFunction(() => window.Shiny?.shinyapp?.isConnected() && Shiny.shinyapp.$inputValues.task === 'home');
    check(true, 'full entry connects with initialized browser inputs');
    for (const [task, panel] of [
      ['study', 'st_tabs'], ['conditional_batch', 'cb_tabs'], ['heterogeneity', 'hg_tabs'],
      ['joint_sequential', 'je_tabs'], ['multiarm', 'ma_tabs'], ['design_search', 'ds_tabs'],
      ['reverse_calibration', 'rc_solve']
    ]) {
      await page.getByRole('tab', { name: '需求入口', exact: true }).click();
      await page.locator('#task_' + task).click();
      await page.waitForFunction(task => Shiny.shinyapp.$inputValues.task === task, task);
      await page.locator('#' + panel).waitFor({ state: task === 'reverse_calibration' ? 'attached' : 'visible' });
      await page.waitForFunction(() => !document.querySelector('.shiny-busy'));
      check((await page.locator('.shiny-output-error:visible:not(:empty)').allTextContents()).length === 0,
        task + ' opens without visible output errors');
      if (task === 'study') {
        check((await page.locator('#st_tabs a[data-value="st_design"]:visible').count()) === 1 &&
          (await page.locator('#st_tabs a[data-value="st_exports"]:visible').count()) === 1,
          'study design and export panels remain in their tab container');
      }
    }
    await page.evaluate(() => document.getElementById('rc_solve').selectize.setValue('dco'));
    await page.waitForFunction(() => document.querySelector('label[for="rc_lower"]')?.textContent.includes('DCO下界'));
    check((await page.locator('label[for="rc_upper"]').innerText()).includes('DCO上界'),
      'reverse calibration DCO bounds update labels');
    await page.evaluate(() => document.getElementById('rc_unit').selectize.setValue('days'));
    await page.waitForFunction(() => document.querySelector('label[for="rc_lower"]')?.textContent.includes('DCO下界（日）') && document.querySelector('label[for="rc_upper"]')?.textContent.includes('DCO上界（日）'));
    check((await page.locator('label[for="rc_upper"]').innerText()).includes('DCO上界（日）'),
      'reverse calibration bounds follow changed time units');
    check(jsErrors.length === 0, 'no uncaught browser JavaScript errors');
    const output = process.env.EXTENSION_BROWSER_OUTPUT;
    if (output) {
      fs.mkdirSync(path.dirname(output), { recursive: true });
      fs.writeFileSync(output, JSON.stringify({ status: 'passed', scope: 'full-entry initialization, seven task pages and reverse-planning labels; no advanced method validation',
        passed: checks.length, checks, jsErrors }, null, 2) + '\n');
    }
  } finally { await browser.close(); }
})().catch(e => { console.error(e.message); process.exitCode = 1; });
