# event_pred 1.0 核心发布候选版

版本 1.0.0-rc.1，2026-10-05。核心本机检查已通过；尚未部署公网，目标部署环境检查待完成。

三个任务：未来事件数、达标日期、单终点生存数据生成。指数、Weibull、PWE；合并或已知两组；预测采用参数或 ADTTE CSV。日/周/月可选，默认月。

在包或 GitHub main 分支的根目录启动：

```r
source("scripts/install_core.R")
shiny::runApp(".", host="127.0.0.1", port=3839)
```

必要检查：`Rscript scripts/check_core.R`。浏览器检查另需 Node.js、Playwright 和 Chrome，先在 3839 启动核心应用，再执行 `node scripts/check_core_browser.cjs`。环境变量 `PLAYWRIGHT_MODULE`、`CHROME_PATH` 可指定运行环境；`CORE_CHECK_OUTPUT` 指定下载目录。同一目录执行 `Rscript scripts/replay_core_downloads.R` 复现真实下载结果。

main 分支及核心包的 `app.R` 对应完整源码中的 `app_core.R`。v0.35 扩展开发稿保存在 `development/v0.35`，新增高级分支仍待专项验证。通用计算文件中未开放的方法定义不表示核心入口支持这些方法。

[核心工作手册](docs/CORE_HANDBOOK.md)详述模型、推导、数据与复现。公式运行时随包提供，页面可下载离线 HTML。实际检查范围、修复和环境见[核心验证报告](docs/CORE_VALIDATION.md)；[1.0 发布计划](docs/V1_0_RELEASE_PLAN.md)与[2.0 计划](docs/V2_0_DEVELOPMENT_PLAN.md)说明后续工作。

Dockerfile 为部署准备文件，未构建；镜像 R 4.5.1 与本机检查 R 4.6.0 不同，目标环境需重新确认。安装脚本安装缺失依赖，尚未锁定版本。正式 v1.0.0 标记在目标环境启动、资源限制和核心示例检查完成后创建。

Linux 从源码安装 R 依赖时需要 libuv、libcurl 开发头文件与 pkg-config；GitHub CI 和核心 Dockerfile 已列出相应系统包。安装脚本使用当前 R 的 repos 配置，CORE_CRAN_REPO 可覆盖镜像，缺失包采用最多 4 个并行安装进程。
