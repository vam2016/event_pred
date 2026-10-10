# SurvCast 2.0 M0 开发基线恢复

日期：2026-10-10。基准提交：`5c1893065f663785a1a61d5d9261be2ada7a4b4f`。分支：`development/v2.0`。

## 已修复

- `study_ui.R`：beta设置提前关闭容器，导致后续面板游离且文件不能解析；恢复八个子页在同一导航容器内。
- `research_workbench_ui.R`：条件批量与异质患者入口各有一处多余闭括号。
- `round_five_server.R`：求解上下界标签调用缺失闭括号；另补DCO上下界单位标签随日/月切换，数值仍按经过时间等价换算。
- `round_three_ui.R`：补齐`je_compare`的预定同轮比较说明；主规则保持预先指定。
- `help.R`：未登记或空说明明确报告参数ID，不再触发无法定位的向量下标异常，也不静默省略帮助。

## 核对main与扩展分支

main头为`83555c112eca6231c4e8e254bf2a726704e546fb`。基准共有65个同路径文件，其中54个相同；11个差异为入口、工作流、容器、安装/历史检查及文档用途差异。main的`app.R`与扩展的`app_core.R`内容相同；核心模块、结果交互、启动器及核心检查均一致，因此没有整分支合并。

扩展安装器改为复用核心依赖准备，保留扩展包，尊重环境镜像并限制并行安装数。扩展Dockerfile补入主分支已有的libuv/curl头文件和pkg-config，并复制共享安装器。容器仍使用原R镜像，本轮没有构建，不能据此宣布容器启动通过。

## 本机验收

|范围|结果|证据|
|---|---|---|
|R语法|134文件全部解析（含新增检查脚本）|`validation/v2_extension_load_results.json`|
|入口/注册/导航结构|9项通过；核心与完整入口分别构造，server注册；不刷新无浏览器输入的观察器|同上；`scripts/check_extensions.R`|
|真实浏览器|12项通过；初始化连接、7个任务页面、设计/导出层级、DCO标签及单位切换、无未捕获JS错误|`validation/v2_extension_browser_results.json`；`scripts/check_extensions_browser.cjs`|
|核心方法与边界回归|51项通过|`scripts/check_core.R`；本轮汇总`validation/v2_m0_results.json`|
|展示数据回归|13项通过|`scripts/check_core_results.R`；本轮汇总同上|

环境：macOS，R 4.6.0及原本机R包库。浏览器使用隔离的无头Chrome与Playwright，服务仅监听127.0.0.1。本轮浏览器范围覆盖设计评价、实际IA条件批量、异质患者、联合序贯、多臂、设计搜索、期望反向校准页面；没有执行这些高级研究任务或校准其统计性质。

复现加载检查：在源码根目录执行`Rscript scripts/check_extensions.R`。需要JSON时指定`EXTENSION_CHECK_OUTPUT`。浏览器检查需启动完整`app.R`在本机3850端口，然后运行`node scripts/check_extensions_browser.cjs`；可通过`EXTENSION_CHECK_URL`、`PLAYWRIGHT_MODULE`、`CHROME_PATH`及`EXTENSION_BROWSER_OUTPUT`指定地址/环境/记录路径。

## 自动检查与范围

工作流已加入2.0分支源码push及PR自动触发，在Linux核心依赖准备后执行解析/两个入口加载检查，再运行原核心及展示回归；Windows保留启动器准备检查。首次远程运行（提交`4f18998`、运行`38052643770`）Windows准备通过，Linux在完整入口构造时因缺少`markdown`失败。本机原包库已有该包；此次干净环境检查暴露了安装清单遗漏，已在完整安装器和加载检查工作流补齐文档渲染依赖，并更新缓存键。修复后的远程运行尚待回读。

此次完成M0本机恢复。高级方法专项、实际下载重放、Docker构建、目标环境及公网发布未实施；初次失败基线记录保留。下一工作单元为V2-101：SAS输入接入核心流程。
