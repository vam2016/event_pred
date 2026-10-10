# V2-101：核心 SAS 输入接入与验收

日期：2026-10-10（北京时间）。分支：`development/v2.0`。开发基准：`df95ec43396326daf1e43f22b660876fe634388d`。核心入口版本：`2.0.0-dev`。

## 结果与契约

核心入口的事件数和达标日期预测均可上传CSV、XPT v5/v8或SAS7BDAT，继续共用ADTTE规范化、终点/分析标志/组别筛选和三种核心模型。默认核心安装仍可使用CSV与参数输入；`Rscript scripts/install_core.R --with-sas`补充可选haven依赖，缺包时显示明确安装提示。Linux自动检查安装该依赖并运行跨格式检查，Windows保留默认核心启动准备。

导入状态显示格式、原始记录数及已解码日期字段。字符日期按ISO，未标记的数值日期需明确选择SAS整数日；已解码日期直接使用，非零时间部分不静默取整。SAS值标签不替代CNSR或组别的存储编码；不导入独立SAS7BCAT。重复变量名、缺失字段、重复分析记录、日期/AVAL不符或错误单位会被拒绝。

配置冻结`adtte_source`（格式、读取器及版本）与`adtte_mapping`（终点、筛选、日期编码/加项、文件单位、永久退出编码、所选组别）；原文件路径和患者记录不嵌入配置。实际下载重放同时修复了合并模式空组别被编码为`{}`的问题，改为JSON `null`。编辑输入或导入失败不改变上次成功结果及下载。

核心开发版名称与导出版本同步更新；旧打包脚本在源码版本与1.0候选包名称不符时停止，避免把2.0改动打成旧版包。没有生成2.0发布候选包或改动main。

## 本机证据

|检查|结果|记录|
|---|---|---|
|输入与跨格式等价|37项通过|`validation/v2_sas_input_results.json`|
|真实Chrome上传、预测、下载、失败/冻结|40项通过|`validation/v2_sas_browser_results.json`|
|实际下载JSON/CSV重放|16项通过|`validation/v2_sas_replay_results.json`|
|核心方法/边界回归|51项通过|`validation/v2_core_check_results.json`|
|展示数据回归|13项通过|`validation/v2_core_display_results.json`|
|语法及两入口构造/注册|137个R文件、9项通过|`validation/v2_sas_extension_load_results.json`|
|可选安装准备|本机`--with-sas`执行成功，依赖已存在|本轮实际运行|
|历史包名保护|旧打包器拒绝将2.0-dev打成1.0-rc.1|本轮实际运行；未写出新包|

合成资料含180行：60名患者的OS主分析、PFS和不保留的重复分析记录。选择OS且ANL01FL=Y后为60人、36事件、6永久退出、18截点删失，两组各30人。跨格式比较使用同一患者及种子，对指数、Weibull、PWE的合并预测和指数分组预测检查结果/轨迹一致。独立声明的日数、状态、组别作为规范化参照；标签/特殊缺失、类型日期、数值SAS日期、周单位及损坏文件另验收。

浏览器实测四种基本格式、文件周/显示月、已知两组、SAS达标日期，以及未标记数值日期先失败再明确选择编码。重放使用Chrome实际下载的冻结JSON、曲线或达标日期CSV与原合成文件，检查日数、均值/分位数、目标达标概率和人数/事件数；未达标质量保留。下载字节只存本机临时目录，不纳入仓库。历史1.0验证记录保留，新增回归写入2.0记录。

环境：macOS、R 4.6.0、haven 2.5.5、隔离无头Chrome/Playwright；服务仅监听127.0.0.1，测试结束后关闭。本机验收及远程CI均完成。[远程运行38057951632](https://github.com/vam2016/event_pred/actions/runs/38057951632)对应源码提交`8b832c2b9a2b513c117c9863ace4617cbd1633a7`，结论success：Linux可选依赖准备、137文件解析/9项两入口检查、核心51项/展示13项回归及37项SAS输入验收全部通过，Windows默认核心启动准备通过。浏览器40项及实际下载重放16项为本机证据，未在远程重复。

## 复现与边界

在源码根目录运行`Rscript scripts/check_sas_inputs.R`。用`SAS_CHECK_FIXTURES`指定合成文件目录；默认生成在临时目录。浏览器检查启动`app_core.R`后运行`scripts/check_sas_browser.cjs`，配置`SAS_CHECK_URL`、`SAS_CHECK_FIXTURES`、`SAS_BROWSER_DOWNLOADS`，环境需要时另指定`PLAYWRIGHT_MODULE`和`CHROME_PATH`。随后在相同文件/下载目录配置下运行`Rscript scripts/replay_sas_downloads.R`。读取接口参照[haven SAS文档](https://haven.tidyverse.org/reference/read_sas.html)和[XPT文档](https://haven.tidyverse.org/reference/read_xpt.html)，变量和值标签始终保留其存储语义。

测试文件由haven的XPT写入器和仅用于合成测试的弃用SAS写入器生成；没有独立SAS系统生成文件、SAS7BCAT或非英文编码文件的实测证据。这里验收平台输入与复现链路，不代表CDISC合规、所有SAS编码变体、正式环境部署或高级统计方法通过。当前核心保持三模型、plugin区间、合并/已知两组及严格确认至DCO规则。下一单元：V2-102（Log-normal/Log-logistic）。
