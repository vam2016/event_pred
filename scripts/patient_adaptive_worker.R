args<-commandArgs(TRUE)
if(length(args)!=2)stop("Expected configuration and private job directory")
for(f in c("units","models","forecast","inputs","bayes","simulation","study","adaptation","patient_adaptation","patient_adaptive_prediction","patient_prediction_process"))source(paste0("R/",f,".R"))
payload<-readRDS(args[1]);prediction<-!is.null(payload$config);cfg<-if(prediction)payload$config else payload;dir<-args[2]
publish<-function(x,name){tmp<-tempfile("write-",tmpdir=dir);saveRDS(x,tmp);if(!file.rename(tmp,file.path(dir,name)))stop("Cannot save worker state")}
tryCatch({
  publish(list(completed=0,total=if(prediction)cfg$reps*if(patient_prediction_is_extended(cfg))nrow(patient_prediction_scenarios(cfg)) else 1L else cfg$reps*length(unique(c(1,cfg$hr_true)))),"progress.rds")
  cb<-function(s)publish(s,"progress.rds");cancel<-function()file.exists(file.path(dir,"cancel.flag"))
  r<-if(prediction)run_patient_prediction(payload$data,cfg,progress=cb,should_cancel=cancel) else run_patient_adaptive(cfg,progress=cb,should_cancel=cancel)
  publish(r,"result.rds")
},error=function(e){publish(list(message=conditionMessage(e)),"error.rds");quit(status=1)})
