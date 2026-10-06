# 1.0 核心候选版验证报告

检查日期：2026-10-05 至 2026-10-06。版本：1.0.0-rc.1。覆盖三个核心入口：事件数、达标日期、固定截点单终点生存数据生成。v0.20–v0.35 高级新增分支不在本报告通过范围内。没有公网部署。

## 实际检查结果

|范围|结果与记录|
|---|---|
|R 加载、参数、独立数值和边界|50 项断言通过；`validation/core_check_results.json`|
|真实浏览器任务操作、上传、分组、输出及窄屏|36 项断言通过；`validation/core_browser_results.json`|
|真实下载文件和脚本复现|7 项断言通过；`validation/core_replay_results.json`|
|独立源码包|解压至新目录，默认 app.R 启动；50 项 R 检查、7 项真实下载重放通过，首页三个入口及 17 个公式通过；`validation/core_portable_results.json`|
|工作手册|全部 17 个公式显示、0 个 MathJax 错误、0 个残留未渲染公式；8 个章节锚点；独立 HTML 禁止网络仍正确渲染|
|浏览器资源与错误|0 个未捕获 JavaScript 错误、0 个控制台错误、0 个外部网络请求|

R 检查以独立参照核对指数事件/暴露 MLE、PWE 分段暴露及切点事件归属、Weibull 右删失似然独立优化与 survreg 转换。独立累计风险反演覆盖条件于既有随访年龄、零风险段与有限尾部累计风险。

指数/Weibull/PWE 的在随访者未来事件期望与独立积分比较；独立永久退出时钟纳入积分。恒定/分段未来入组采用有限人数截断的 Poisson 到达过程，独立参照对各次到达的 Gamma 累计强度密度积分，不用未截断平均人数替代有限入组计划。模拟期望比较容差预设为 4 倍经验 Monte Carlo 标准误加 1e-6；确定性默认容差 1e-8，独立 Weibull 优化参数容差 2e-5。每项期望、实际值、MCSE 和容差随 JSON 记录。

分组计数按同一模拟轮次相加；已知事件固定、永久退出不贡献未来事件。目标为 D* 次事件日期，包含并列事件；窗口外事件不算窗口内达标，未达标轮次保留无穷时间，达标概率分母为全部成功模拟轮次。

ADTTE 检查覆盖日期加项仅校验 AVAL 而不增加连续暴露、日/周/月文件单位、不一致时间、未确认随访缺口和重复分析记录拒绝。浏览器用合成 60 人、36 事件文件实际上传，分别进行合并及 TRTP 已知组别拟合预测。没有真实受试者数据进入仓库或公共示例。

下载复现直接使用浏览器保存的配置、曲线、目标汇总、ADTTE 与模拟 R 脚本；核对均值/分位数、目标概率/日期、组别映射及模拟汇总。未仅对内部配置副本做重放。输入编辑不改写最近成功结果，错误提交保留最近成功配置。模拟观察、真值、ADTTE 分开，观察结果不含潜在未来时钟。

## 修复的问题

1. 核心 UI 构造引用任务参数帮助但未定义，补入帮助定义。
2. Shiny 默认自动加载 R 目录中的所有模块，导致入口显式依赖顺序失效；加入 `_disable_autoload.R`，各入口仍按自身所需显式加载。
3. 任务切换时先选中随后隐藏的面板，导致空白页；调整面板显示与选中顺序。
4. 本地 MathJax 未支持 PWE 似然中的 boldsymbol 写法，改为明确参数列表，数学含义不变。页面与离线下载均通过复核。

## 环境与复现

本机：macOS，R 4.6.0；shiny 1.14.0、bslib 0.11.0、commonmark 2.0.0、survival 3.8-6、ggplot2 4.0.3、plotly 4.12.0、DT 0.34.0、jsonlite 2.0.0、scales 1.4.0。浏览器采用 Chrome/Playwright；版本按记录 JSON 核对。

在源码根目录执行 `Rscript scripts/check_core.R`。核心应用在 3839 启动后执行 `node scripts/check_core_browser.cjs`，随后执行 `Rscript scripts/replay_core_downloads.R`。Node 模块与 Chrome 路径可分别通过 PLAYWRIGHT_MODULE、CHROME_PATH 指定；CORE_CHECK_OUTPUT 为可写下载目录。R_LIBS_USER 可指定已有核心依赖库。

## 发布状态与边界

本机通过记录支持核心发布候选版，不表示所有扩展代码已验证，也不构成所有临床数据情形的穷举检查。预测区间条件于给定/拟合参数，未计入参数估计不确定性，不应解释为同时置信带。

目标部署环境尚未指定；当前无 Docker 运行环境，Dockerfile 未构建，依赖尚未锁定。正式 v1.0.0 标记需在目标环境启动、资源限制与核心合成示例检查通过后创建。原 v0.19 的 3838 预览未替换。

GitHub 核心源码提交：`e18a814b7af94341ddc65f47f77371431c2cfda5`；完整扩展提交：`49dc94d408bbf1fddbcdf0585b519b3384e7eaeb`。分别回读核对 51 和 185 个源码/文档/记录文件的 Git blob 摘要，全部一致。同步详情见 `validation/core_github_sync.json`。

## Linux CI 安装修复

首次 GitHub Actions 安装因缺失 libuv 和 libcurl 头文件失败，未进入 R 检查；运行记录 `37339695766`。补入 Ubuntu 的 libuv1-dev、libcurl4-openssl-dev 与 pkg-config；安装脚本尊重运行环境配置的 CRAN 镜像，并并行安装缺失包。Docker 核心准备文件同步补入系统库，镜像构建仍未运行。该失败属于依赖准备，不计为方法/数值检查通过。

修复后 GitHub Actions 运行 [37341056792](https://github.com/vam2016/event_pred/actions/runs/37341056792) 成功：系统依赖、R 包安装及核心 50 项断言全部通过。检查提交为 `d2fcc441536ddf21afb2596dd21df111c7724f80`；后续同步仅补充文档/状态记录，不改变已检查计算源码。Docker 构建和指定部署环境检查仍待完成。

## 2026-10-06 命名与本机启动

显示名称改为 SurvCast（生存事件预测与模拟工作台），仓库仍为 event_pred。新增 Mac / Windows / Linux 启动入口及 LOCAL_START.md。Mac 原目录与含空格新解压目录均通过启动、首页、17 个公式、离线手册标题及 390px 窄屏检查；ZIP 保留执行权限，安装包按 R 与系统架构独立缓存。实际记录见 validation/core_launch_results.json。Windows 准备流程由新的 CI 运行记录；未将本机 Mac 检查称为另一台 Windows 的浏览器实测。
