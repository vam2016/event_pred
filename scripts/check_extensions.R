# Loading checks only: these do not certify advanced statistical methods.
checks <- list()
check <- function(ok, label) {
  if (!isTRUE(ok)) stop(paste("FAILED:", label))
  checks[[length(checks) + 1L]] <<- list(label = label, status = "passed")
  cat("PASS:", label, "\n")
}
files <- c("app.R", "app_core.R", list.files("R", "[.]R$", full.names = TRUE),
  list.files("scripts", "[.]R$", full.names = TRUE))
for (file in files) {
  parse(file)
}
check(TRUE, paste(length(files), "R source files parse"))

for (entry in c("app.R", "app_core.R")) {
  e <- new.env(parent = globalenv())
  sys.source(entry, envir = e)
  check(inherits(e$ui, "shiny.tag.list") && is.function(e$server), paste(entry, "constructs UI and server"))
  # Register server outputs/observers without flushing uninitialized browser inputs.
  # Browser initialization and interaction are separate checks.
  shiny::testServer(e$server, {
    stopifnot(is.environment(environment()))
  })
  check(TRUE, paste(entry, "registers server without running a research task"))
  if (entry == "app.R") {
    check("je_compare" %in% names(e$parameter_help) && nzchar(e$parameter_help[["je_compare"]]),
      "joint rule comparison includes parameter help")
    missing <- tryCatch({e$parameter_label("missing_help_test", "test"); NULL}, error = identity)
    check(inherits(missing, "error") && grepl("missing_help_test", conditionMessage(missing), fixed = TRUE),
      "missing help reports the parameter identifier")
    # Guard the repaired nesting, not merely balanced parentheses.
    body_call <- body(e$study_ui)
    tabs <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("navset_card_tab")), as.list(body_call)[-1])
    check(length(tabs) == 1L, "study has one nested tab container")
    panels <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("nav_panel")), as.list(tabs[[1]])[-1])
    values <- vapply(panels, function(x) as.list(x)$value, character(1))
    check(identical(unname(values), c("st_design", "st_generation", "st_analysis", "st_results", "st_sample", "st_cp", "st_inference", "st_exports")),
      "all eight study panels remain inside the tab container")
  }
}
output <- Sys.getenv("EXTENSION_CHECK_OUTPUT", unset = "")
if (nzchar(output)) {
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(status = "passed", scope = "R parsing, UI construction, unflushed server registration and study navigation; no advanced method validation",
    parsed_files = length(files), checks = checks, R = R.version.string), output, auto_unbox = TRUE, pretty = TRUE)
}
cat("EXTENSION_LOAD_CHECKS_PASSED", length(checks), "\n")
