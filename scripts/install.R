packages <- c("shiny", "commonmark", "bslib", "survival", "ggplot2", "plotly", "DT", "jsonlite", "scales", "rmarkdown", "knitr", "haven", "posterior", "processx", "ps", "survRM2", "rpact", "mvtnorm")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  target <- .libPaths()[1]
  if (file.access(target, 2) != 0) {
    target <- path.expand(Sys.getenv("R_LIBS_USER"))
    if (!nzchar(target)) stop("请设置可写的 R_LIBS_USER 包目录。")
    dir.create(target, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(target, .libPaths()))
  }
  install.packages(missing, repos = "https://cloud.r-project.org", lib = target)
}
remaining <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(remaining)) stop(paste("未安装的依赖：", paste(remaining, collapse = ", ")))
cat("Dependencies available. Run shiny::runApp('.') from the repository root.\n")
