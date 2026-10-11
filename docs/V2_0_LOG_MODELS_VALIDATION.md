# V2-102 Log-normal / Log-logistic 验收

日期：2026-10-11（北京时间）。分支 `development/v2.0`，核心 `2.0.0-dev`；基线提交 `a97eb7ea27a2f8ad432dae873b7052e32c5afd8f`。本机验收通过，本机源码提交`3fe3cfc0b416153fa340bbbcf6cab27fc209afee`；本批20文件已同步至GitHub，远程Linux/Windows检查通过；未生成发布候选包或部署。

## 已完成与模型契约

核心候选模型从指数/Weibull/PWE扩展至五种。参数输入可使用中位数 m 或对数位置 mu=log(m/所选单位)，以及无量纲 sigma>0；日/周/月变换只平移 mu，保持 sigma。单组和已知两组共用同一控件、转换器和引擎；ADTTE右删失拟合及固定DCO数据生成也可选择两种分布。原三种拟合候选仍为默认选择。

Log-normal 的 log(T) 为正态分布，Log-logistic 的 log(T) 为 Logistic 分布，拟合使用 survival::survreg AFT。事件贡献 log密度，删失贡献 log生存率，两参数 AIC=4−2logLik。原数值实现无需改写：独立参照已核对分布、反演、似然与积分。拟合警告或非有限估计被拒绝；至少两个事件为最低输入限制，不保证外推稳定。

条件抽样依据已有无事件随访年龄 a，而非从零重新抽样；尾部计算保留log生存率。Log-logistic 的 sigma≥1 时完整时间分布均值为无穷，仍有有限中位数和有限窗口事件概率。等价参数表显示∞，CSV保留Inf；不得将无穷均值读作永不发生事件的概率质量。

预测采用plugin区间，含未来随机性，尚不包含拟合参数估计不确定性。不自动混合模型。已知两组按同轮轨迹相加；未达标路径保留在分母及无条件日期分布。模拟观察记录、真值与ADTTE分开导出，多个固定截点共享同一轨迹。

参数定义参照[R survival分布说明](https://stat.ethz.ch/R-manual/R-devel/library/survival/html/survreg.distributions.html)和[survreg说明](https://stat.ethz.ch/R-manual/R-devel/library/survival/html/survreg.html)。工作手册第5章新增公式、单位及均值限制，保留八章导航。

## 本机证据

|检查|结果|记录|
|---|---|---|
|独立分布/似然/条件抽样/积分与生成|43项通过|validation/v2_log_models_results.json|
|真实浏览器参数/组别/XPT/目标/模拟及手册|34项通过|validation/v2_log_models_browser_results.json|
|实际下载配置/CSV/R脚本重放|21项通过|validation/v2_log_models_replay_results.json|
|原核心方法/边界与展示数据|51+13项通过|validation/v2_core_check_results.json、v2_core_display_results.json|
|SAS输入回归|37项通过|validation/v2_sas_input_results.json|
|语法及两入口构造/注册|139个R文件、9项通过|validation/v2_log_models_extension_load_results.json|

独立似然由高斯/Logistic定义计算，BFGS优化mu/log(sigma)，与AFT估计对照；参数容差2e-5，似然/AIC容差1e-8，其余确定性默认1e-9。模拟均值对照独立条件密度与退出概率积分；容差在计算前固定为4个经验MCSE+1e-6。Log-normal期望56.998288、实测57.0715、MCSE0.119893；Log-logistic期望46.859222、实测46.9045、MCSE0.118174。每种2000轮、120名已随访80日的当前患者、5个已知事件、120日窗口、每天退出率0.003。

潜在时间生成采用独立生存概率变换，3000名两组患者按10分位箱核对，容差为4个二项MCSE。无穷均值、少于两个事件、退化并列事件、无效sigma/mu及尾部生存率下溢另作边界检查。

浏览器实测单组两种分布的m/mu互换和日/周/月换算、无效sigma后保留成功下载、两组不同分布、XPT主分析筛选后两候选拟合与各组不同拟合、不可达目标，以及两组固定DCO模拟。采用V2-101合成XPT资料（筛选后60人、36事件），不含真实患者数据。

重放读取Chrome实际下载的JSON与原XPT或参数队列，逐模型比对完整曲线/分位数、人数和事件数；组内曲线与逐轮组间相加另核对。实际下载R脚本重新生成所有固定截点摘要、潜在事件/退出时钟、观察与ADTTE日期编码。在线与实际离线HTML公式无错误、无外部请求；390px窄屏无横向溢出。浏览器下载及截图保存在本机临时目录，未纳入仓库。

两次首次浏览器尝试因测试定位/断言错误停止：等价参数下载误定位至输入数据页，错误提示断言误用“正数”而实际为“log-time scale需大于0”。修正测试后全流程通过，没有据此更改或掩盖产品错误。

## 复现和边界

在源码根目录运行 `Rscript scripts/check_log_models.R`。基础回归为 `scripts/check_core.R`、`scripts/check_core_results.R`、`scripts/check_sas_inputs.R`、`scripts/check_extensions.R`。

浏览器准备：使用 `scripts/create_sas_fixtures.R::create_sas_fixtures()`生成合成文件，在127.0.0.1:3852启动`app_core.R`；配置 `SAS_CHECK_FIXTURES`、`LOG_MODELS_DOWNLOADS`（绝对路径）后执行`node scripts/check_log_models_browser.cjs`。可用 `LOG_MODELS_URL`、`PLAYWRIGHT_MODULE`、`CHROME_PATH`指定环境。然后在相同环境变量下运行`Rscript scripts/replay_log_models_downloads.R`。

环境：macOS、R version 4.6.0 (2026-04-24)、survival 3.8.6、本机Chrome/Playwright。服务器仅在本机监听，验收后关闭。远程Linux工作流已执行并通过独立模型检查；浏览器及下载重放为本机证据，未在远程重复。未开展Bootstrap、覆盖率研究、事件驱动截点或高级研究方法验收；这些仍按后续工作项处理。下一单元V2-201：先完成原三模型的患者/组别Bootstrap、失败/有效次数、区间解释与下载重放，再逐项核验扩展模型。

## 同步及远程证据

首次自动审批以此前M0/V2-101授权未覆盖本批20文件及GitHub目的地为由拒绝上传。用户随后明确授权V2-102本批同步及自动检查，原上传流程成功执行，未采用绕过方式。

源码提交 `28bc8e8e960256155a88936e6602a06beb5b968b` 已同步至 `vam2016/event_pred` 的 `development/v2.0`，本机与远程文件树核对一致。[远程运行38107692014](https://github.com/vam2016/event_pred/actions/runs/38107692014)最终为success，两项任务均完成：

|远程任务|实测结果|
|---|---|
|Linux core|139个R文件解析、9项两入口构造/注册、51项核心、13项展示、43项独立模型检查及37项SAS输入均通过|
|Windows windows-launch|默认核心依赖安装和Start-Windows.cmd启动准备通过；没有启动浏览器服务|

检查日志已逐项核对；本机浏览器34项和实际下载重放21项未在远程重复。随后的文档/JSON状态提交只记录验收结果，不改动已验证源码。下一项V2-201。
