# Private local worker. Arguments are configuration RDS and a per-session job directory.
args<-commandArgs(TRUE)
if(length(args)!=2)stop("Expected configuration path and private job directory")
for(f in c("units","models","forecast","inputs","simulation","nph","sequential","study"))source(paste0("R/",f,".R"))
cfg<-readRDS(args[1]);job_dir<-args[2]
atomic_save<-function(x,name){tmp<-tempfile("write-",tmpdir=job_dir);saveRDS(x,tmp);if(!file.rename(tmp,file.path(job_dir,name)))stop("Cannot publish worker state")}
tryCatch({
  atomic_save(list(completed=0L,total=nrow(study_scenarios(cfg))*cfg$reps,status="running"),"progress.rds")
  res<-run_design_study(cfg,progress=function(s)atomic_save(s,"progress.rds"),should_cancel=function()file.exists(file.path(job_dir,"cancel.flag")))
  res$dependencies<-study_dependencies(cfg)
  atomic_save(res,"result.rds")
},error=function(e){atomic_save(list(message=conditionMessage(e)),"error.rds");quit(status=1)})
