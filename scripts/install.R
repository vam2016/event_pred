packages <- c("shiny", "bslib", "survival", "ggplot2", "plotly", "DT", "jsonlite", "scales", "rmarkdown", "knitr")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("Dependencies available. Run shiny::runApp('.') from the repository root.\n")
