# SurvCast 本机启动与电脑间转移

SurvCast（生存事件预测与模拟工作台），版本 1.0.0-rc.1；GitHub 仓库仍为 event_pred。本文针对三个核心入口。

## 当前 Mac

在工作台目录双击 `Start-Mac.command`。当前源码位置 `/Users/lerist/Documents/Codex/2026-10-03/cha/outputs/event_pred`。

浏览器自动打开；通常地址 `http://127.0.0.1:3839/`。端口占用时依次使用 3840–3859，实际地址显示在启动窗口。保持窗口打开，关闭窗口或按 Ctrl+C 停止。

双击提示文件不可执行时，在终端进入解压目录执行 `bash Start-Mac.command`，或 `chmod +x Start-Mac.command` 后双击。系统限制下载脚本时，也可用下文 R 启动方式，无须修改全局系统设置。

## 发送至另一台电脑

1. 发送 `SurvCast-v1.0.0-rc.1.zip`，不要只发 app.R，也不需要发当前电脑安装的 R 包。
2. 对方安装 R：[Windows 官方下载](https://cran.r-project.org/bin/windows/base/)、[Mac 官方下载](https://cran.r-project.org/bin/macosx/)。Mac 按 Apple Silicon / Intel 选择安装包。实测 R 4.6.0，其他版本未逐一检查；不需要 RStudio。
3. 完整解压至可写的个人目录，不能在 ZIP 内启动。
4. Windows 双击 `Start-Windows.cmd`；Mac 双击 `Start-Mac.command`；Linux 在解压目录执行 `sh Start-Linux.sh`。

首次缺少依赖时需联网，启动程序自动安装；依赖就绪后，核心任务与工作手册均可在本机运行。每台电脑分别打开自己的 localhost 地址，不需要共享网络服务。

这是源码运行包，未内置 R，不能在未安装 R 的电脑上直接作为 EXE 使用。不同系统/芯片的 R 包不能直接互拷。新安装依赖按 R 版本和平台保存在 `.local-library`，不要将缓存、真实上传文件或私人报告混入发送包。

## R / RStudio / 终端

设置工作目录为含 app_core.R、R、scripts、www 的解压文件夹，在 R 中执行：

```r
source("scripts/start_core.R")
```

终端方式为 `Rscript --vanilla scripts/start_core.R`。Rscript 不在 PATH 时使用其完整路径；Windows 双击入口还会查询安装注册表和常见安装目录。

## 可选参数

- `--port=3841`：指定端口，占用时报错。
- `--no-browser`：启动服务但不自动打开浏览器。
- `--prepare-only`：仅准备依赖，不启动服务。

例如 `Rscript --vanilla scripts/start_core.R --port=3841 --no-browser`。CORE_CRAN_REPO 可指定 CRAN 镜像；CORE_R_LIBRARY 可复用已有依赖库。自动生成的 local-runtime.json 记录实际 R/系统/核心包版本。

Ubuntu 从源码安装 R 包所需系统库可用 `sudo apt-get install libuv1-dev libcurl4-openssl-dev pkg-config` 安装。Docker 不是本机双击启动的前提。

## 检查记录

本机入口及独立包启动检查见 `validation/core_launch_results.json`；Windows 准备流程另外由 GitHub Actions 检查。自动化服务器检查不替代另一台电脑的真实浏览器操作。正式部署和扩展模块状态见核心验证报告。
