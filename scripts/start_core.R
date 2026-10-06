# Common local launcher for the checkout and the standalone core bundle.
args <- commandArgs(trailingOnly=TRUE)
allowed <- grepl("^--port=[0-9]+$",args) | args %in% c("--no-browser","--prepare-only")
if(any(!allowed)) stop("未知启动参数：",paste(args[!allowed],collapse=", "))
port_arg <- grep("^--port=",args,value=TRUE)
if(length(port_arg)>1L) stop("端口只填写一次。")
requested_port <- if(length(port_arg)) suppressWarnings(as.integer(sub("^--port=","",port_arg))) else NULL
if(length(port_arg) && (is.na(requested_port)||requested_port<1024L||requested_port>65535L)) stop("端口须为 1024–65535 的整数。")
file_arg <- grep("^--file=",commandArgs(trailingOnly=FALSE),value=TRUE)
app_root <- if(length(file_arg)) dirname(dirname(normalizePath(sub("^--file=","",file_arg[1]),mustWork=TRUE))) else normalizePath(getwd(),mustWork=TRUE)
setwd(app_root)
if(!file.exists("scripts/install_core.R")||!file.exists("app_core.R")) stop("请先完整解压工作台，在含 app_core.R 的目录启动。")
# Packages remain local and separate for different R/OS architectures.
lib <- file.path(app_root,".local-library",paste0(R.version$major,".",strsplit(R.version$minor,".",fixed=TRUE)[[1]][1],"-",R.version$platform))
dir.create(lib,recursive=TRUE,showWarnings=FALSE)
if(!dir.exists(lib)||file.access(lib,2)!=0) stop("工作台目录不可写，请移动到个人文档目录后启动。")
extra <- Sys.getenv("CORE_R_LIBRARY",unset="")
# Reuse the original workspace library when available; it is never bundled.
dev_lib <- file.path(app_root,"..","..","work","R-library")
.libPaths(c(lib,if(nzchar(extra))extra,if(dir.exists(dev_lib))dev_lib,.libPaths()))
cat("SurvCast · 生存事件预测与模拟工作台\n检查核心依赖；首次缺少依赖时需要联网安装。\n")
source("scripts/install_core.R",local=TRUE)
packages <- c("shiny","bslib","commonmark","survival","ggplot2","plotly","DT","jsonlite","scales")
runtime <- list(platform="SurvCast",R=R.version.string,os=R.version$platform,packages=setNames(as.list(vapply(packages,function(p)as.character(packageVersion(p)),character(1))),packages),entry="app_core.R")
jsonlite::write_json(runtime,"local-runtime.json",auto_unbox=TRUE,pretty=TRUE)
if("--prepare-only" %in% args) {
  cat("启动准备完成；没有启动服务。实际环境已记录在 local-runtime.json。\n")
} else {
  free <- function(port) tryCatch({
    s <- httpuv::startServer("127.0.0.1",port,list(call=function(req)list(status=200L,headers=list(),body="")))
    httpuv::stopServer(s);TRUE
  },error=function(e)FALSE)
  candidates <- if(is.null(requested_port))3839:3859 else requested_port
  port <- NULL
  for(candidate in candidates) if(free(candidate)){port<-candidate;break}
  if(is.null(port)) stop("启动端口被占用，请关闭已有工作台，或使用 --port=3841 指定其他端口。")
  cat("本机地址：http://127.0.0.1:",port,"/\n保持启动窗口打开；关闭窗口或按 Ctrl+C 可停止。\n",sep="")
  shiny::runApp("app_core.R",host="127.0.0.1",port=port,launch.browser=!("--no-browser" %in% args))
}
