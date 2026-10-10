# Synthetic fixtures only; no patient source files are copied into the repository.
create_sas_fixtures <- function(directory) {
  if (!requireNamespace("haven", quietly=TRUE)) stop("Fixture generation requires haven.")
  dir.create(directory, recursive=TRUE, showWarnings=FALSE)
  i <- 0:59; day <- ifelse(i < 36, 10 + i * 2, ifelse(i < 42, 90, 100))
  origin <- as.Date("2025-01-01")
  primary <- data.frame(USUBJID=sprintf("SYN%03d",i),PARAMCD="OS",STARTDT=origin,
    ADT=origin+day,AVAL=day+1,AVALU="DAYS",CNSR=ifelse(i<36,0,ifelse(i<42,2,1)),
    TRTP=ifelse(i%%2,"B","A"),ANL01FL="Y",stringsAsFactors=FALSE)
  other <- primary; other$PARAMCD <- "PFS"
  ignored <- primary; ignored$ANL01FL <- "N"; ignored$ADT <- origin+20; ignored$AVAL <- 21; ignored$CNSR <- 0
  raw <- rbind(primary, other, ignored)
  attr(raw$STARTDT,"label") <- "Risk start date"
  attr(raw$CNSR,"label") <- "Stored censor code"
  csv <- raw; csv$STARTDT <- as.character(csv$STARTDT); csv$ADT <- as.character(csv$ADT)
  write.csv(csv,file.path(directory,"synthetic.csv"),row.names=FALSE,na="")
  haven::write_xpt(raw,file.path(directory,"synthetic-v5.xpt"),version=5,name="ADTTE")
  haven::write_xpt(raw,file.path(directory,"synthetic-v8.xpt"),version=8,name="ADTTE")
  # haven's deprecated writer is used only to exercise the SAS reader with synthetic bytes.
  suppressWarnings(haven::write_sas(raw,file.path(directory,"synthetic.sas7bdat")))
  numeric_dates <- raw
  numeric_dates$STARTDT <- as.numeric(raw$STARTDT-as.Date("1960-01-01"))
  numeric_dates$ADT <- as.numeric(raw$ADT-as.Date("1960-01-01"))
  haven::write_xpt(numeric_dates,file.path(directory,"synthetic-numeric.xpt"),version=8,name="ADTTE")
  weeks <- raw; weeks$AVAL <- raw$AVAL/7; weeks$AVALU <- "WEEKS"
  haven::write_xpt(weeks,file.path(directory,"synthetic-weeks.xpt"),version=8,name="ADTTE")
  invisible(raw)
}
