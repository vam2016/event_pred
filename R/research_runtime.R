# Shared durable family runner. Patient trajectories are retained only on demand.
research_family_run <- function(cfg,sc,trial,aggregate,progress=function(state)NULL,should_cancel=function()FALSE,replay_keys=NULL,resume_state=NULL,continuation_operation="resume",prepare=function(state)state) {
  if(!is.null(resume_state)){if(!is.null(replay_keys))stop("重放与续跑不能同时使用。");batch_validate_continuation(resume_state,cfg,continuation_operation)}
  seed_rows<-function(k){d<-resume_state[[k]];if(is.data.frame(d)&&nrow(d))list(d) else list()}
  rows<-seed_rows("rows");mr<-seed_rows("method_rows");lk<-seed_rows("looks");dr<-seed_rows("draw_rows");handled<-matrix(FALSE,nrow(sc),cfg$reps)
  if(length(rows))handled[cbind(rows[[1]]$SCENARIO,rows[[1]]$SIMID)]<-TRUE
  initial_completed<-sum(handled);done<-initial_completed;cancelled<-FALSE;next_publish<-Sys.time()
  state<-list(config=cfg,scenarios=sc,prepared=resume_state$prepared,completed=done,total=nrow(sc)*cfg$reps,version=batch_version,review_status="pending_v0.35_review",schema="event_pred.batch_research.v4",created_at=format(Sys.time(),tz="UTC",usetz=TRUE),dependencies=batch_dependencies(cfg),source_hash=batch_source_hash(),initial_completed=initial_completed,resumed_from_run_id=resume_state$run_id %||% "",continuation_operation=if(is.null(resume_state))"new" else continuation_operation)
  snapshot<-function(status){s<-state;s$rows<-batch_bind_columns(rows);s$method_rows<-batch_bind_columns(mr);s$looks<-batch_bind_columns(lk);s$draw_rows<-batch_bind_columns(dr);s$completed<-done;s$status<-status;aggregate(s)}
  if(should_cancel())return(snapshot("cancelled"))
  state<-prepare(state);progress(snapshot("running"))
  for(j in seq_len(nrow(sc))){for(b in seq_len(cfg$reps)){
    if(handled[j,b])next
    if(!is.null(replay_keys)&&!any(replay_keys$SCENARIO==j&replay_keys$SIMID==b))next
    if(should_cancel()){cancelled<-TRUE;break}
    a<-trial(cfg,sc[j,,drop=FALSE],b,state$prepared);rows[[length(rows)+1L]]<-a$row;mr[[length(mr)+1L]]<-a$method_rows;lk[[length(lk)+1L]]<-a$looks %||% data.frame();dr[[length(dr)+1L]]<-a$draw_rows %||% data.frame();done<-done+1L;handled[j,b]<-TRUE
    if(done==initial_completed+1L||b==cfg$reps||Sys.time()>=next_publish){progress(snapshot("running"));next_publish<-Sys.time()+2}
  };if(cancelled)break}
  snapshot(if(cancelled)"cancelled" else if(!is.null(replay_keys)&&done<nrow(sc)*cfg$reps)"partial_replay" else "complete")
}
research_initial <- function(cfg,sc,aggregate,initial=NULL)aggregate(list(config=cfg,scenarios=sc,rows=initial$rows %||% data.frame(),method_rows=initial$method_rows %||% data.frame(),looks=initial$looks %||% data.frame(),draw_rows=initial$draw_rows %||% data.frame(),prepared=initial$prepared,completed=initial$completed %||% 0L,total=nrow(sc)*cfg$reps,status="running",version=batch_version,review_status="pending_v0.35_review",schema="event_pred.batch_research.v4",created_at=format(Sys.time(),tz="UTC",usetz=TRUE),dependencies=batch_dependencies(cfg),source_hash=batch_source_hash()))
research_aggregate <- function(state,methods,labels,metric_fields=c("decision_reject","target_reached")) {
  cfg<-state$config;sc<-state$scenarios;mr<-state$method_rows;rows<-state$rows;ov<-metrics<-resources<-paths<-pairs<-list()
  for(id in sc$SCENARIO){s<-sc[sc$SCENARIO==id,,drop=FALSE];r<-if(nrow(rows))rows[rows$SCENARIO==id,,drop=FALSE] else rows
    ov[[length(ov)+1L]]<-cbind(s,jr_probability(if(nrow(r))ifelse(r$decision_valid,r$decision_reject,NA) else logical(),cfg$reps))
    for(m in methods){d<-if(nrow(mr))mr[mr$SCENARIO==id&mr$method==m,,drop=FALSE] else mr
      for(k in metric_fields){v<-if(k %in% names(d))d[[k]] else rep(NA,nrow(d));if(k=="decision_reject"&&nrow(d))v<-ifelse(d$decision_valid,v,NA)
        metrics[[length(metrics)+1L]]<-cbind(data.frame(SCENARIO=id,label=s$label,method=m,method_label=labels[[m]],is_primary=m==cfg$primary,metric=k),jr_probability(v,cfg$reps))}
      generated<-if(nrow(d))d$generated %in% TRUE else logical();g<-d[generated,,drop=FALSE]
      avg<-function(k)if(k %in% names(g)&&any(is.finite(g[[k]])))mean(g[[k]][is.finite(g[[k]])]) else NA_real_
      q<-function(k,p)if(k %in% names(g)&&any(is.finite(g[[k]])))as.numeric(quantile(g[[k]][is.finite(g[[k]])],p)) else NA_real_
      resources[[length(resources)+1L]]<-data.frame(SCENARIO=id,method=m,requested=cfg$reps,processed=nrow(d),generated=sum(generated),mean_n=avg("n_observed"),mean_events=avg("events"),mean_final_day=avg("final_day"),final_p05_day=q("final_day",.05),final_median_day=q("final_day",.5),final_p95_day=q("final_day",.95),mean_wait_day=avg("wait_day"),mean_future_analyses=avg("future_analysis_count"))
      if("action" %in% names(d))for(v in unique(d$action[!is.na(d$action)]))paths[[length(paths)+1L]]<-cbind(data.frame(SCENARIO=id,method=m,action=v),jr_probability(d$action==v,cfg$reps))
    }
    if(nrow(mr))for(m in setdiff(methods,cfg$primary)){
      d<-mr[mr$SCENARIO==id,,drop=FALSE];cols<-c("SIMID","decision_valid","decision_reject","generated","final_day");for(k in setdiff(cols,names(d)))d[[k]]<-NA
      z<-merge(d[d$method==cfg$primary,cols],d[d$method==m,cols],by="SIMID",suffixes=c("_primary","_comparison"));x<-ifelse(z$decision_valid_primary,z$decision_reject_primary,NA);y<-ifelse(z$decision_valid_comparison,z$decision_reject_comparison,NA)
      good<-!is.na(x)&!is.na(y);delta<-as.integer(y[good])-as.integer(x[good]);lo<-function(v)ifelse(is.na(v),0,as.integer(v));hi<-function(v)ifelse(is.na(v),1,as.integer(v));g<-z$generated_primary %in% TRUE&z$generated_comparison %in% TRUE&is.finite(z$final_day_primary)&is.finite(z$final_day_comparison);dt<-z$final_day_comparison[g]-z$final_day_primary[g]
      pairs[[length(pairs)+1L]]<-data.frame(SCENARIO=id,primary_method=cfg$primary,comparison_method=m,paired_valid=sum(good),paired_difference=if(length(delta))mean(delta) else NA_real_,paired_mcse=if(length(delta)>1)sd(delta)/sqrt(length(delta)) else NA_real_,difference_lower_all=(sum(lo(y)-hi(x))-(cfg$reps-nrow(z)))/cfg$reps,difference_upper_all=(sum(hi(y)-lo(x))+(cfg$reps-nrow(z)))/cfg$reps,resource_pairs=sum(g),mean_final_difference_day=if(length(dt))mean(dt) else NA_real_)
    }
  }
  state$overview<-do.call(rbind,ov);state$method_overview<-do.call(rbind,metrics);state$resources<-do.call(rbind,resources);state$stops<-batch_bind_columns(paths);state$method_pairs<-batch_bind_columns(pairs);state$comparisons<-state$candidates<-state$policy_overview<-state$gs_plans<-data.frame();state
}
research_adtte <- function(o,cfg,extra=character()) {
  dates<-function(x)as.character(as.Date(cfg$origin)+floor(x))
  d<-data.frame(SCENARIO=o$SCENARIO,SIMID=o$SIMID,USUBJID=o$USUBJID,PARAMCD=cfg$paramcd,TRTP=o$group,STARTDT=dates(o$entry_day),ADT=dates(o$obs_day),AVAL=floor(o$obs_day)-floor(o$entry_day),AVALU="DAYS",CNSR=ifelse(o$event==1,0L,ifelse(o$status=="dropout",2L,1L)),ANL01FL="Y",ENGINE_ENTRY_DAY=o$entry_day,ENGINE_OBS_DAY=o$obs_day,ENGINE_TIME_DAY=o$time_day)
  for(k in intersect(extra,names(o)))d[[k]]<-o[[k]];d
}
research_replay_script <- function(r,runner) {
  dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  keys<-if(nrow(r$rows))r$rows[,c("SCENARIO","SIMID"),drop=FALSE] else data.frame(SCENARIO=integer(),SIMID=integer())
  c("# Development draft. May contain IA patient records; replay has not been checked.",'source("R/research_sources.R")',paste0("cfg <- ",dump(r$config)),paste0("keys <- ",dump(keys)),paste0('if(batch_version!="',r$version,'")stop("Version differs")'),paste0('if(batch_source_hash()!="',r$source_hash,'")stop("Source differs")'),paste0("expected_dependencies <- ",dump(r$dependencies)),'if(!identical(batch_dependencies(cfg),expected_dependencies))stop("Dependencies differ")',paste0('result <- ',runner,'(cfg,replay_keys=keys)'),vapply(c("scenarios","rows","method_rows","looks","draw_rows","overview","method_overview","method_pairs","resources","stops","scenario_pairs","ia_overview","model_overview","recovery","count_overview","target_overview","model_reference","fold_records","process_overview","backlog_overview","arm_overview","candidate_overview","selection_record","confirmation_overview","confirmation_decision","intersection_overview","covariance_overview","interval_overview"),function(k)paste0('write.csv(result$',k,' %||% data.frame(),"research_',k,'_replayed.csv",row.names=FALSE)'),character(1)))
}
