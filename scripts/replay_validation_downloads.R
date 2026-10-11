# Reproduce actual browser exports, including downloaded user-facing scripts.
e<-new.env(parent=globalenv());invisible(sys.source('app_core.R',envir=e))
out<-Sys.getenv('VALIDATION_DOWNLOADS');if(!nzchar(out)||!dir.exists(out))stop('Set VALIDATION_DOWNLOADS to browser download directory')
checks<-character();check<-function(ok,label){if(!isTRUE(ok))stop(paste('FAILED:',label));checks<<-c(checks,label);cat('PASS:',label,'\n')}
equal_table<-function(x,y){rownames(x)<-NULL;rownames(y)<-NULL;if(nrow(x)==0&&nrow(y)==0)return(identical(names(x),names(y)));isTRUE(all.equal(x,y,tolerance=1e-9,check.attributes=FALSE))}
# Empty quoted text and all-NA numeric columns need the defined export schema;
# automatic CSV type inference would turn an empty diagnostic note into logical NA.
read_export<-function(file,reference)read.csv(file,check.names=FALSE,colClasses=vapply(reference,function(x)if(is.character(x))'character' else if(is.logical(x))'logical' else 'numeric',character(1)))
for(name in c('pooled-count-backtest','pooled-count-scenario','group-bootstrap-backtest','group-bootstrap-scenario','target-weeks-backtest','target-weeks-scenario','parameter-lognormal-scenario')){
 x<-readRDS(file.path(out,paste0(name,'-input.rds')));kind<-x$kind
 cfg<-jsonlite::read_json(file.path(out,paste0(name,'-config.json')),simplifyVector=TRUE)
 r<-if(kind=='backtest')e$backtest_forecast(x$data,x$config,x$cuts,x$truth_cut,plan=x$plan) else e$core_scenarios(x$data,x$config,x$values,x$models_override)
 csv<-read_export(file.path(out,paste0(name,'-summary.csv')),e$core_validation_export(r,kind))
 check(equal_table(e$core_validation_export(r,kind),csv),paste(name,'actual summary CSV replays frozen input and configuration'))
 check(equal_table(r$diagnostics,read_export(file.path(out,paste0(name,'-diagnostics.csv')),r$diagnostics)),paste(name,'actual per-analysis requested/effective counts replay'))
 check(equal_table(r$simulation_failures,read_export(file.path(out,paste0(name,'-failures.csv')),r$simulation_failures)),paste(name,'actual replicate failure CSV replays with analysis key'))
 check(equal_table(r$failures,read_export(file.path(out,paste0(name,'-candidate_failures.csv')),r$failures)),paste(name,'actual candidate failures replay without fabricated rows'))
 # Compare JSON with its defined null encoding rather than converting null to attained dates.
 r$created_at<-cfg$created_at;r$base_created_at<-cfg$base_created_at
 json<-jsonlite::fromJSON(jsonlite::toJSON(e$core_validation_bundle(r,kind),auto_unbox=TRUE,digits=NA,na='null',null='null'))
 check(equal_table(json$summary,cfg$summary)&&equal_table(json$diagnostics,cfg$diagnostics),paste(name,'JSON summaries and diagnostics match canonical replay including null dates'))
 if(kind=='backtest'){
  p<-read_export(file.path(out,paste0(name,'-plan.csv')),e$core_validation_plan_export(r))
  check(equal_table(e$core_validation_plan_export(r),p),paste(name,'actual historical plan preserves groups and canonical/selected units'))
  check(all(vapply(names(r$snapshots),function(key){d<-r$snapshots[[key]];all(d$obs_day<=as.numeric(key))&&all(d$entry<as.numeric(key))},logical(1))),paste(name,'replayed historical snapshots contain no post-cut observations'))
 } else check(identical(r$pairing,'independent_seeds')&&length(unique(vapply(r$configurations,`[[`,integer(1),'seed')))==length(r$values),paste(name,'replay preserves independent seeds without a paired MCSE'))
 # Run the exact downloaded script in an isolated folder; source directories read-only linked.
 tmp<-tempfile('validation-replay-');dir.create(tmp)
 for(p in c('app_core.R','R','docs','www'))stopifnot(file.symlink(normalizePath(p),file.path(tmp,p)))
 file.copy(file.path(out,paste0(name,'-input.rds')),file.path(tmp,paste0(kind,'_input.rds')))
 file.copy(file.path(out,paste0(name,'-replay.R')),file.path(tmp,'replay.R'))
 previous<-getwd();setwd(tmp);log<-tryCatch(system2(file.path(R.home('bin'),'Rscript'),'replay.R',stdout=TRUE,stderr=TRUE),finally=setwd(previous))
 check(is.null(attr(log,'status'))&&file.exists(file.path(tmp,paste0(kind,'_replayed_summary.csv'))),paste(name,'actual downloaded R script executes in an isolated source-linked folder'))
 check(equal_table(csv,read_export(file.path(tmp,paste0(kind,'_replayed_summary.csv')),csv)),paste(name,'downloaded script reproduces its actual summary CSV'))
 unlink(tmp,recursive=TRUE)
}
jsonlite::write_json(list(status='passed',passed=length(checks),checks=checks,tolerance='1e-9; downloaded RDS exact input; CSV continuous days retained; JSON Inf/null semantics compared; source-linked temp runs do not write to repository'), 'validation/v2_validation_replay_results.json',auto_unbox=TRUE,pretty=TRUE)
cat('VALIDATION_REPLAY_CHECKS_PASSED',length(checks),'\n')
