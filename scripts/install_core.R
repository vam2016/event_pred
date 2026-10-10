# Dependencies for app_core.R only; no installation is performed during development.
packages <- c("shiny","bslib","commonmark","survival","ggplot2","plotly","DT","jsonlite","scales")
if ("--with-sas" %in% commandArgs(trailingOnly=TRUE)) packages <- c(packages,"haven")
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) {
  target <- .libPaths()[1]
  if(file.access(target,2)!=0) {
    target <- path.expand(Sys.getenv("R_LIBS_USER"))
    if(!nzchar(target))stop("请设置可写的 R_LIBS_USER 目录。")
    dir.create(target,recursive=TRUE,showWarnings=FALSE);.libPaths(c(target,.libPaths()))
  }
  repos <- getOption("repos")
  if(!length(repos) || any(repos == "@CRAN@")) repos <- c(CRAN="https://cloud.r-project.org")
  mirror <- Sys.getenv("CORE_CRAN_REPO", unset="")
  if(nzchar(mirror)) repos <- c(CRAN=mirror)
  cores <- parallel::detectCores(logical=FALSE)
  jobs <- if(is.na(cores)) 1L else max(1L,min(4L,cores))
  install.packages(missing,repos=repos,lib=target,Ncpus=jobs)
}
remaining <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(remaining))stop(paste("未安装核心依赖：",paste(remaining,collapse=", ")))
cat("核心依赖可用；在源码根目录启动 app_core.R。\n")
