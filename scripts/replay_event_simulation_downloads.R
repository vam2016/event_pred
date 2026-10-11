# V2-301 replay of actual Chrome downloads with the original saved configuration.
e<-new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e));attach(e)
downloads<-Sys.getenv("EVENT_SIMULATION_DOWNLOADS");if(!nzchar(downloads))stop("Set EVENT_SIMULATION_DOWNLOADS")
checks<-character();check<-function(ok,label){if(!isTRUE(ok))stop(paste("FAILED:",label));checks<<-c(checks,label);cat("PASS:",label,"\n")}
near<-function(x,y)isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-10))
source_script<-function(file,envir){
 original<-getwd();sandbox<-tempfile("reproduce-",tmpdir=downloads);dir.create(sandbox)
 if(!file.symlink(file.path(original,"R"),file.path(sandbox,"R")))stop("Cannot link read-only source for isolated replay")
 setwd(sandbox);on.exit(setwd(original));invisible(sys.source(file,envir))
}
read_csv<-function(name,suffix)read.csv(file.path(downloads,paste0(name,"-",suffix,".csv")),check.names=FALSE)
same<-function(got,expected){if(nrow(got)!=nrow(expected)||!all(names(expected)%in%names(got)))return(FALSE)
 all(vapply(names(expected),function(id){a<-got[[id]];b<-expected[[id]];if(is.numeric(b))near(a,b) else identical(as.character(a),as.character(b))},logical(1)))}
for(name in c("mixed","unreachable","weibull","pwe","lognormal","all-attained","zero-events","no-entry","fixed")){
 file<-file.path(downloads,paste0(name,"-config.json"));if(!file.exists(file))stop(paste("Missing actual browser download:",file));saved<-jsonlite::fromJSON(file);cfg<-saved$config
 cfg$groups<-jsonlite::fromJSON(file,simplifyVector=FALSE)$config$groups
 cfg$groups<-lapply(cfg$groups,function(g){g$model$params<-unlist(g$model$params);g$model$cuts<-unlist(g$model$cuts);g})
 if(cfg$cut_mode=="fixed")cfg$target<-1
 r<-e$run_survival_simulation(cfg)
 check(same(read_csv(name,"truth"),e$unit_export(r$truth,cfg$display_unit,names(r$truth)[grepl("_day$",names(r$truth))])),paste(name,"actual latent truth CSV exactly replays from frozen config"))
 cuts<-r$cuts;if(cfg$cut_mode=="fixed")cuts<-cuts[,setdiff(names(cuts),c("target_reached","target_day")),drop=FALSE]
 check(same(read_csv(name,"cuts"),e$unit_export(cuts,cfg$display_unit,names(cuts)[grepl("_day$|^DCO_DAY$",names(cuts))])),paste(name,"actual DCO, attained flag, event count and cut reason CSV replay"))
 check(same(read_csv(name,"summary"),e$unit_export(r$summary,cfg$display_unit,names(r$summary)[grepl("_day$|^DCO_DAY$",names(r$summary))]))&&same(read_csv(name,"fixed"),e$unit_export(r$fixed,cfg$display_unit,names(r$fixed)[grepl("_day$|^DCO_DAY$",names(r$fixed))])),paste(name,"KM summaries and fixed follow-up values replay without latent observations"))
 b<-if(name=="mixed")5 else 1;k<-if(name=="fixed")2 else 1;obs<-r$observed[r$observed$SIMID==b&r$observed$CUTID==k,,drop=FALSE]
 check(same(read_csv(name,"observed"),e$unit_export(obs,cfg$display_unit,c("DCO_DAY","entry_day","time_day","obs_day")))&&same(read_csv(name,"adtte"),e$simulation_adtte(obs,cfg)),paste(name,"selected continuous and ADTTE exports replay dates, censor status and all groups"))
 if(name=="mixed")check(identical(sort(unique(read_csv(name,"observed")$group)),c("A","B"))&&all(read_csv(name,"observed")$SIMID==5),"display-only group filter does not alter actual selected-trial exports")
 if(cfg$cut_mode=="target"){
  o<-e$simulation_target_overview(r);stored<-saved$target_summary
  check(same(read_csv(name,"target-summary"),e$unit_export(o,cfg$display_unit,names(o)[grepl("_day$",names(o))]))&&all(vapply(names(o),function(id)if(is.na(o[[id]]))is.null(stored[[id]])||is.na(stored[[id]]) else near(o[[id]],stored[[id]]),logical(1))),paste(name,"actual target CSV and JSON preserve all trials and conditional hit-time definition"))
  check(o$trials==cfg$reps&&o$reached+o$unreached==cfg$reps&&near(o$reached_mcse,sqrt(o$reached_probability*(1-o$reached_probability)/cfg$reps)),paste(name,"downloaded attainment denominator and MCSE include window misses"))
 }
 # Execute the actual downloaded reproduction script, not a regenerated replacement.
 replay<-new.env(parent=globalenv());source_script(file.path(downloads,paste0(name,"-reproduce.R")),replay)
 check(same(replay$res$truth,r$truth)&&same(replay$res$observed,r$observed)&&same(replay$res$cuts,r$cuts)&&same(replay$res$summary,r$summary),paste(name,"actual downloaded R script reproduces all latent, observed, cut and KM records"))
}
base<-jsonlite::fromJSON(file.path(downloads,"mixed-config.json"))
for(name in c("frozen","invalid-retained")){
 x<-jsonlite::fromJSON(file.path(downloads,paste0(name,if(name=="frozen")"-config.json" else ".json")))
 check(identical(base$config,x$config)&&identical(base$target_summary,x$target_summary),paste(name,"actual download remains the last successful run"))
}
# Reproduction files above are synthetic temporary outputs, kept outside source tree.
jsonlite::write_json(list(status="passed",passed=length(checks),checks=checks,R=R.version.string,scope="actual Chrome downloads, nine synthetic cases, saved configuration and original downloaded R scripts"),"validation/v2_event_simulation_replay_results.json",auto_unbox=TRUE,pretty=TRUE)
cat("EVENT_SIMULATION_REPLAY_CHECKS_PASSED",length(checks),"\n")
