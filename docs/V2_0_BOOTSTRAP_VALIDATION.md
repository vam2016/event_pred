# V2-201 患者 Bootstrap 验收

日期：2026-10-11（北京时间）；分支`development/v2.0`，核心`2.0.0-dev`。基线提交`0f05b305b2dc318a8eb969c75e7fd7de287427db`。本机验收完成，源码同步及远程CI待执行。

## 已完成与结果契约

核心ADTTE预测增加固定拟合参数/患者Bootstrap选择，本阶段对指数、Weibull、PWE开放，合并与已知两组的事件数及达标日期均适用。Log-normal/Log-logistic使用固定拟合参数；参数输入没有经验患者样本，始终使用给定参数。检查最终所选组别方法，避免隐藏控制或各组单独选模绕开限制。

每轮从完整患者观察记录有放回抽取原人数，重新拟合，再预测原始当前队列。时间、事件/退出指示及记录成对保留；不按事件状态分层。两组在组内抽样，保持组别和组人数。已记录事件、永久退出者及仍随访者的风险年龄固定；未来入组和退出计划固定。每个有效拟合生成一条未来轨迹，区间近似同时包含拟合参数估计及未来随机性。

每模型/联合情景记录请求、有效、拟合失败、未来模拟失败及失败轮次/组别/阶段/原因，不补抽失败轮次。任一组失败会排除整轮联合轨迹；所有组及总体使用同一有效轮次集合。失败超过请求次数10%时整次提交停止，错误给出有效/请求及原因，旧结果保留；恰好10%允许返回但明确为有效轮次条件结果。

有效但未达标轮次保留无穷日期，进入分母和日期分位数。拟合/模拟失败是未知结果，不能视作未达标或零事件。达标概率/MCSE、事件分布及分位数使用全部有效轮次；MCSE不表示置信区间或方法误差。失败可能改变重抽样分布，10%为运行规则，不是可靠性证明；不声明名义覆盖率保证。

配置JSON增加冻结的重抽样方案、原始预测队列定义、诊断及失败表，新增次数/失败CSV，报告同源。时点读数CSV与目标CSV带请求/有效/失败数，分组汇总区分请求与有效联合次数。原数据点估计用于拟合曲线/等价参数表。编辑输入或失败新提交均不改写最近成功下载。严格DCO、独立非信息性右删失、未来计划与模型假设沿用核心契约。

方法类型参照[R boot的censboot说明](https://search.r-project.org/CRAN/refmans/boot/html/censboot.html)中ordinary case resampling及观察记录的组内抽样；该来源不是本平台预测覆盖率的验证。工作手册第3.1节解释操作、方程、失败和分母，第8节按区间方式说明范围。

## 本机证据

|检查|结果|记录|
|---|---|---|
|患者抽样/独立似然/穷举预测参照/组内与失败边界|33项通过|validation/v2_bootstrap_results.json|
|核心真实浏览器上传/三模型/分组/目标/冻结/手册|34项通过|validation/v2_bootstrap_browser_results.json|
|完整入口共享分组表格兼容|4项通过|validation/v2_bootstrap_full_entry_browser_results.json|
|实际下载JSON/曲线/次数/失败/读数重放|25项通过|validation/v2_bootstrap_replay_results.json|
|语法与两入口构造/注册|142个R文件、9项通过|validation/v2_bootstrap_extension_load_results.json|
|原核心与展示回归|51+13项通过|validation/v2_core_check_results.json、v2_core_display_results.json|
|SAS输入、对数时间模型回归|37+43项通过|validation/v2_sas_input_results.json、v2_log_models_results.json|

独立参照包括指数事件/暴露MLE、PWE逐段事件/暴露、Weibull右删失似然BFGS优化。完整指数Bootstrap预测与独立重拟合及条件未来事件路径逐格相同。确定性默认容差1e-9，独立Weibull参数容差2e-5。

另穷举五患者的全部5^5=3125个样本，3104个达到至少两事件的拟合限制，失败概率0.00672。固定2000轮、种子24019：有效1988，拟合失败12；条件期望窗口末事件数4.784035，实测4.779175，MCSE0.009306，预定容差4MCSE+1e-6=0.037223。实际失败数也在预定4个二项MCSE内。该案例检验实现的混合预测与失败分母，不是覆盖率研究。

控制性故障检查包括5/50失败允许返回、6/50失败停止、拟合与未来模拟分别记录、B组失败导致A/B/总体同轮排除，以及未达标仍保留。种子复现对原三模型分别核验。

浏览器使用合成XPT（筛选后60人、36事件）检查三模型的合并/组内Bootstrap和不可达目标。另用60人仅2事件的CSV验证失败超过10%后保留旧结果；60人6事件、种子2、100轮、无退出的CSV产生一个真实拟合失败，实际下载包含99有效/1失败及非空失败原因。该种子用于构造失败展示案例，不用于MCSE容差评估。

重放以原合成文件及Chrome实际下载配置运行相同源码，核对三模型完整曲线/区间、达标概率/无穷日期、请求/有效/失败数、失败阶段/原因、时点读数及组内逐轮相加。在线与实际下载离线HTML公式无错误或外部请求，390px窄屏无溢出。下载和截图仅保存在临时目录，不纳入仓库。

## 修复和检查过程

共享分组汇总增加两列后，两个入口的固定列名映射未同步，导致表格格式化出现NA列名。已在app.R和app_core.R修复，新增核心分组表格可见输出断言，并实际运行完整入口两组参数预测检查列名和A/B行，无可见错误。完整入口只声明这一展示兼容性检查，不据此宣布高级方法通过。

首次组合脚本尝试因SAS脚本全局加载函数覆盖，令故障注入未作用于预期环境而停止；Bootstrap测试现显式调用隔离应用环境，其他回归与CI相同，分别运行独立R进程。首次浏览器单模型断言误将JSON单值字符串当数组，修正断言；更新手册后重启测试应用，以免核验旧UI缓存。保留这些检查过程，不把停止的尝试计为通过。

## 复现、环境与下一项

源码根目录执行`Rscript scripts/check_bootstrap.R`。基础回归执行`check_core.R`、`check_core_results.R`、`check_sas_inputs.R`、`check_log_models.R`、`check_extensions.R`；各脚本独立R进程运行。

浏览器准备：在临时目录生成资料，执行`source("scripts/create_bootstrap_fixtures.R");create_bootstrap_fixtures(directory)`（需要可选haven），在127.0.0.1:3853启动`app_core.R`，配置`SAS_CHECK_FIXTURES`和`BOOTSTRAP_DOWNLOADS`绝对路径，执行`node scripts/check_bootstrap_browser.cjs`，之后同环境执行`Rscript scripts/replay_bootstrap_downloads.R`。用`BOOTSTRAP_URL`指定地址；需要时指定`PLAYWRIGHT_MODULE`、`CHROME_PATH`。可复用的资料助手已核对与实际两种CSV输入字节一致。

完整入口展示检查：在3854启动app.R，执行`scripts/check_bootstrap_full_entry.cjs`，可用`BOOTSTRAP_FULL_URL`指定地址。这一脚本仅覆盖参数模式两组事件数的共享表格兼容。

环境为macOS、R4.6.0、本机Chrome/Playwright，所有合成数据不含真实患者记录。两测试服务均已关闭。Linux自动检查已加入33项Bootstrap专项；远程同步及CI尚未执行，浏览器/下载重放目前为本机证据。未生成发布包或部署，未开放对数时间Bootstrap。下一工作项V2-301：事件驱动模拟。

## 同步状态

本机源码提交bdfb366已保存。向vam2016/event_pred的development/v2.0同步这批25个文件时，自动审批认定此前批次授权未覆盖V2-201，拒绝上传；随后用户于2026-10-11明确授权本批同步及远程自动检查，现按授权执行。文件仅含项目源码、文档与合成验证记录。
