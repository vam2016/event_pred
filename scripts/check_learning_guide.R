# V2-402: source preservation, independent legacy checks and real packaged replays.
e<-new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e))
checks<-character();check<-function(ok,label){if(!isTRUE(ok))stop(paste("FAILED:",label));checks<<-c(checks,label);cat("PASS:",label,"\n")}
near<-function(x,y)isTRUE(all.equal(x,y,tolerance=1e-9,check.attributes=FALSE))
sha<-function(path)digest::digest(file=path,algo="sha256")
readtext<-function(path)paste(readLines(path,warn=FALSE,encoding="UTF-8"),collapse="\n")
manifestpath<-"docs/learning-assets/guide-manifest.json"
m<-jsonlite::read_json(manifestpath,simplifyVector=FALSE)
p<-jsonlite::read_json("docs/learning-history/source-inventory.json",simplifyVector=TRUE)
check(identical(sha(p$archive_markdown),p$markdown_sha256),"archived v1 Markdown is byte-identical to verified historical source")
check(length(unique(unlist(p$markdown_copies)))==1&&length(unique(unlist(p$html_copies)))==1&&isTRUE(p$zip_integrity_ok)&&all(p$bundle_files$matches),"recorded three-copy source verification and original ZIP integrity remain distinguishable from this run")
old<-readtext(p$archive_markdown);md<-readtext("docs/SurvCast_STATISTICIAN_GUIDE.md")
anchors<-function(txt)regmatches(txt,gregexpr('<a id="s[0-9]{2}"></a>',txt,perl=TRUE))[[1]]
check(identical(anchors(old),sprintf('<a id="s%02d"></a>',1:28)),"original source has exactly 28 ordered chapter anchors")
check(identical(anchors(md),sprintf('<a id="s%02d"></a>',1:34)),"current source preserves 28 chapters and adds six ordered unique chapters")
check(endsWith(md,readtext("docs/LEARNING_V2_ADDENDUM.md")),"current appended six chapters equal editable addendum")
check(lengths(regmatches(md,gregexpr('<details>',md,fixed=TRUE)))==12,"all 12 original self-tests are retained")
check(all(vapply(6:11,function(i)grepl(paste0("23.",i),old,fixed=TRUE),logical(1))),"six historical workflow scenarios retain their source index")
check(!grepl("ADTTE 模式可以同时选三个",md,fixed=TRUE)&&grepl("Bootstrap限原三个",md,fixed=TRUE)&&grepl("sim_target",md,fixed=TRUE),"current capability dictionary reflects five plugin models, restricted Bootstrap and event DCO")
for(path in names(m$files_sha256))check(identical(sha(path),m$files_sha256[[path]]),paste("versioned file checksum",path))
check(identical(readLines("docs/CORE_CASES.md",warn=FALSE),e$core_case_markdown()),"embedded new case guide agrees with shared engine definitions")
output<-Sys.getenv("LEARNING_OUTPUT","reports/learning-v2/SurvCast_STATISTICIAN_GUIDE.html")
html<-readtext(output)
check(!grepl("CORE_CASE_CATALOG",html,fixed=TRUE)&&all(vapply(names(e$core_case_catalog()),function(id)grepl(id,html,fixed=TRUE),logical(1))),"built HTML embeds all four shared current cases")
check(lengths(regmatches(html,gregexpr('src="data:image/svg+xml;base64,',html,fixed=TRUE)))==7&&!grepl('src="https?://',html),"seven diagrams are embedded for offline reading")
# Original four cases execute against current code in a fresh output directory.
assets<-tempfile("legacy-learning-");dir.create(assets);previous<-Sys.getenv("LEARNING_ASSETS",unset=NA_character_)
Sys.setenv(LEARNING_ASSETS=assets);legacy<-new.env(parent=globalenv())
invisible(tryCatch(capture.output(sys.source("scripts/learning_examples.R",envir=legacy)),finally={if(is.na(previous))Sys.unsetenv("LEARNING_ASSETS") else Sys.setenv(LEARNING_ASSETS=previous)}))
check(near(legacy$bench$analytic_quantiles,c(80,90,100))&&legacy$bench$analytic_mean==90&&legacy$bench$analytic_sd==5,"case A independent Binomial mean, SD and exact quantiles")
check(abs(legacy$bench$simulated_mean-90)<4*5/sqrt(1000),"case A simulated mean falls within four independent analytical MCSE")
check(abs(legacy$expected_future-23.52552)<1e-5&&abs(legacy$process$analytic_total_mean-112.22204)<1e-5&&legacy$process$uncapped_future_expected>legacy$expected_future,"case B capped Poisson entry integral and dropout agree with archived analytical references")
check(abs(legacy$process$simulated_total_mean-legacy$process$analytic_total_mean)<4*sd(legacy$progress_res$counts$exponential[,121])/sqrt(1000),"case B Monte Carlo mean agrees with independent competing-risk and capped-entry integral")
check(near(legacy$d$time,c(45,64,59,23))&&sum(legacy$d$event)==2&&abs(legacy$fit$params[1]-2/191)<1e-12,"case C inclusive-date conversion, 191 days exposure and independent event/exposure MLE")
check(all(legacy$sim$summary$n==legacy$sim$summary$events+legacy$sim$summary$dropouts+legacy$sim$summary$administrative)&&nrow(legacy$sim$cuts)==60,"case D all 20 trials and three DCOs conserve observed status counts")
for(file in list.files(assets,pattern="\\.csv$")) {
  a<-read.csv(file.path(assets,file),check.names=FALSE);b<-read.csv(file.path("docs/learning-assets",file),check.names=FALSE)
  check(near(a,b),paste("rerun legacy figure/input data matches retained CSV",file))
}
new<-jsonlite::read_json(file.path(assets,"example-results.json"),simplifyVector=TRUE)
ref<-jsonlite::read_json("docs/learning-assets/example-results.json",simplifyVector=TRUE)
for(k in setdiff(names(ref),"R"))check(near(new[[k]],ref[[k]]),paste("legacy numerical JSON matches historical reference",k))
unlink(assets,recursive=TRUE)
# Run the actual bundled JSON + R scripts in isolated folders, not merely a function round trip.
for(id in names(e$core_case_catalog())) {
  folder<-file.path(dirname(output),"cases",id)
  x<-jsonlite::read_json(file.path(folder,"survcast_case.json"),simplifyVector=TRUE)
  ref<-e$core_case_tables(e$core_run_case(e$core_case_spec(id)))
  check(near(e$core_case_tables(e$core_run_case(x)),ref),paste(id,"built case JSON reproduces source-defined tables"))
  check(identical(readLines(file.path(folder,"survcast_case_replay.R"),warn=FALSE),e$core_case_replay_script()),paste(id,"built replay script matches online download source"))
  tmp<-tempfile("learning-case-");dir.create(tmp)
  for(path in c("app_core.R","R","docs","www"))stopifnot(file.symlink(normalizePath(path),file.path(tmp,path)))
  stopifnot(all(file.copy(list.files(folder,full.names=TRUE),tmp)))
  wd<-getwd();setwd(tmp);log<-tryCatch(system2(file.path(R.home("bin"),"Rscript"),"survcast_case_replay.R",stdout=TRUE,stderr=TRUE),finally=setwd(wd))
  check(is.null(attr(log,"status")),paste(id,"packaged R script executes successfully"))
  for(k in names(ref)) {
    cols<-vapply(ref[[k]],function(z)if(is.character(z))"character" else if(is.logical(z))"logical" else "numeric",character(1))
    csv<-read.csv(file.path(tmp,paste0("survcast_case_",k,".csv")),check.names=FALSE,colClasses=cols)
    rownames(ref[[k]])<-NULL;rownames(csv)<-NULL
    check(near(csv,ref[[k]]),paste(id,k,"packaged script CSV reproduces every saved table value"))
  }
  if(isTRUE(x$config$input_mode %in% "adtte")) {
    d<-e$normalize_adtte(e$read_adtte(file.path(folder,"survcast_case_adtte.csv")),"OS",x$config$origin,x$config$cut,offset=1,dropout_codes=2)
    check(identical(e$core_run_case(x)$forecast$counts,e$run_forecast(d,x$config)$counts),paste(id,"bundled ADTTE preserves every prediction path"))
  }
  unlink(tmp,recursive=TRUE)
}
jsonlite::write_json(list(status="passed",passed=length(checks),checks=checks,manifest_sha256=sha(manifestpath),html_sha256=sha(output),R=R.version.string,legacy_results=new,scope="archived provenance; current synthetic numerical and packaged replay checks; six scenarios are narrative workflows, not six numerical validations"),"validation/v2_learning_results.json",pretty=TRUE,auto_unbox=TRUE,digits=NA)
cat("LEARNING_CHECKS_PASSED",length(checks),"\n")
