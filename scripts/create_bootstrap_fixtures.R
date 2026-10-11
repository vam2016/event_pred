# Synthetic-only browser fixtures; run from the source root into a temporary directory.
create_bootstrap_fixtures <- function(directory) {
  source("scripts/create_sas_fixtures.R",local=TRUE)
  create_sas_fixtures(directory)
  raw <- read.csv(file.path(directory,"synthetic.csv"))
  analysis <- raw[raw$PARAMCD=="OS" & raw$ANL01FL=="Y",]
  origin <- as.Date("2025-01-01"); cut <- origin+100
  for(events in c(2L,6L)) {
    data <- analysis; data$CNSR <- 1L; data$ADT <- as.character(cut); data$AVAL <- 101
    times <- seq_len(events)*10
    data$CNSR[seq_len(events)] <- 0L
    data$ADT[seq_len(events)] <- as.character(origin+times)
    data$AVAL[seq_len(events)] <- times+1
    write.csv(data,file.path(directory,if(events==2L)"sparse.csv" else "partial.csv"),row.names=FALSE)
  }
  invisible(directory)
}
