# Only internally created configuration and job metadata are read as RDS.
args<-commandArgs(TRUE);if(length(args)!=2)stop("Expected configuration and private job directory")
source("R/research_sources.R")
cfg<-readRDS(args[1]);job_dir<-args[2];meta<-readRDS(file.path(job_dir,"job.rds"));last_durable<-Sys.time()
atomic_save<-function(x,name)batch_atomic_rds(x,file.path(job_dir,name))
main<-function(){
  if(!is.null(meta$store)){
    on.exit(unlink(file.path(meta$store$path,"lease"),recursive=TRUE),add=TRUE)
    batch_atomic_rds(list(pid=Sys.getpid(),process_start=ps::ps_create_time(ps::ps_handle())),file.path(meta$store$path,"lease","owner.rds"))
  }
  latest<-batch_initial_snapshot(cfg,meta$initial)
  publish<-function(s,force=FALSE){
    if(!ma_is(cfg)&&!ds_is(cfg)&&!jr_is(cfg)&&!abr_is(cfg)&&!cb_is(cfg)&&!hg_is(cfg)&&!sp_is(cfg)&&!ms_is(cfg)&&!je_is(cfg)&&!fp_is(cfg)&&!ob_is(cfg)){s$comparisons<-batch_comparisons(s$overview,cfg$reference_scenario);s$candidates<-batch_candidates(s$overview,cfg)}
    if(!is.null(meta$store)){
      m<-meta$store$meta;s$run_id<-m$id;s$run_title<-m$title;s$parent_id<-m$parent_id;s$operation<-m$operation;s$configuration_hash<-m$configuration_hash
      if(force||as.numeric(difftime(Sys.time(),last_durable,units="secs"))>=30){s<-batch_save_checkpoint(s,meta$store$path);last_durable<<-Sys.time()}
    }
    latest<<-s;atomic_save(s,"progress.rds");s
  }
  tryCatch({
    publish(latest,TRUE)
    r<-run_batch_research(cfg,progress=publish,should_cancel=function()file.exists(file.path(job_dir,"cancel.flag")),resume_state=meta$initial,continuation_operation=meta$operation)
    r<-publish(r,TRUE);atomic_save(r,"result.rds");0L
  },error=function(e){latest$status<-"failed";latest$worker_error<-conditionMessage(e)
    try(publish(latest,TRUE),silent=TRUE);atomic_save(list(message=conditionMessage(e)),"error.rds");1L})
}
quit(status=main())
