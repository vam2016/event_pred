# Immutable, complete ADTTE snapshots up to the selected original analysis.
# Snapshot metadata are observations, never latent patient event times.
conditional_has_history <- function(cfg) !is.null(cfg$prediction_history)
ia_history_choices <- function(raw,paramcd,flag="",flag_value="Y") {
  if(!all(c("IASEQ","DCO","PARAMCD") %in% names(raw)))stop("历史文件需在ADTTE字段外增加IASEQ（分析次序）和DCO（该次截点日期）。")
  keep<-!is.na(raw$PARAMCD)&raw$PARAMCD==paramcd
  if(nzchar(flag)) {if(!flag %in% names(raw))stop("分析标志变量不存在。");keep<-keep&!is.na(raw[[flag]])&raw[[flag]]==flag_value}
  x<-raw[keep,,drop=FALSE]
  if(!nrow(x))stop("所选终点及分析标志下没有历史记录。")
  if(!is.numeric(x$IASEQ)||any(!is.finite(x$IASEQ)|x$IASEQ!=floor(x$IASEQ)|x$IASEQ<1|x$IASEQ>5))stop("IASEQ需为1–5的原计划分析次序；不能重编号省略早期分析。")
  sort(unique(as.integer(x$IASEQ)))
}
normalize_ia_history <- function(raw,paramcd,origin,look,offset=1,dropout_codes=numeric(),flag="",flag_value="Y",date_encoding="iso",aval_unit="days",group_column="TRTP") {
  choices<-ia_history_choices(raw,paramcd,flag,flag_value)
  if(length(look)!=1||!is.numeric(look)||!is.finite(look)||!look %in% choices)stop("请选择文件中已存在的历史IA次序。")
  if(!all(seq_len(look) %in% choices))stop("截至所选IA的每个原计划分析都需提供快照，不能跳过缺失次序。")
  origin<-as.Date(origin);if(length(origin)!=1||is.na(origin))stop("研究起点日期无效。")
  keep<-!is.na(raw$PARAMCD)&raw$PARAMCD==paramcd&raw$IASEQ<=look
  if(nzchar(flag))keep<-keep&!is.na(raw[[flag]])&raw[[flag]]==flag_value
  # Exclude all later clinical records before reading dates, groups or outcomes.
  prefix<-raw[keep,,drop=FALSE];snapshots<-lapply(seq_len(look),function(k) {
    x<-prefix[prefix$IASEQ==k,,drop=FALSE];dc<-unique(adtte_date(x$DCO,date_encoding))
    if(length(dc)!=1)stop(paste0("分析",k,"的DCO必须唯一。"))
    cut<-as.numeric(dc-origin)
    d<-normalize_adtte(x,paramcd,as.character(origin),cut,offset,dropout_codes,flag,flag_value,date_encoding,"strict",aval_unit,group_column)
    d<-d[order(d$id),c("id","entry","time","obs_day","status","event","group"),drop=FALSE];rownames(d)<-NULL
    list(look=as.integer(k),cut=cut,data=d)
  })
  validate_ia_history(snapshots)
  list(snapshots=snapshots,data=snapshots[[look]]$data,cut=snapshots[[look]]$cut,origin=as.character(origin),paramcd=paramcd,source="history")
}
validate_ia_history <- function(snapshots) {
  if(!is.list(snapshots)||length(snapshots)<1||length(snapshots)>5)stop("历史路径需1–5个连续分析快照。")
  last<-NULL;last_cut<-NULL;groups<-NULL
  for(k in seq_along(snapshots)) {
    s<-snapshots[[k]]
    if(!is.list(s)||length(s$look)!=1||!is.numeric(s$look)||!is.finite(s$look)||s$look!=k||!is.numeric(s$cut)||length(s$cut)!=1||!is.finite(s$cut)||s$cut<=0)stop("历史分析次序须从1连续递增，且DCO需为正研究时间。")
    d<-validate_data(s$data,s$cut,require_events=FALSE,gap_mode="strict")
    if(!all(c("group","event","obs_day") %in% names(s$data))||anyNA(d$group)||any(!nzchar(trimws(d$group)))||length(unique(d$group))!=2)stop("每次历史IA需已知且非空的两组患者记录。")
    if(any(abs(d$obs_day-d$entry-d$time)>1e-6)||!is.numeric(s$data$event)||any(!is.finite(s$data$event)|!s$data$event %in% c(0,1))||!identical(as.integer(s$data$event),d$event))stop("历史快照的事件指标或经过时间不一致。")
    if(k==1)groups<-sort(unique(d$group)) else {
      if(s$cut<=last_cut)stop("历史DCO需严格递增。")
      if(!identical(groups,sort(unique(d$group))))stop("历史路径的两组标签发生变更。")
      idx<-match(last$id,d$id)
      if(anyNA(idx))stop("后一次快照缺少之前的患者；历史事件和退出者也需保留。")
      now<-d[idx,,drop=FALSE]
      if(any(now$group!=last$group)||any(abs(now$entry-last$entry)>1e-7))stop("同一患者的组别或入组日期在历史快照间发生变更。")
      terminal<-last$status %in% c("event","dropout")
      if(any(now$status[terminal]!=last$status[terminal])||any(abs(now$time[terminal]-last$time[terminal])>1e-7)||any(abs(now$obs_day[terminal]-last$obs_day[terminal])>1e-7))stop("已发生事件或永久退出记录被回改；当前入口不处理历史裁定修订或恢复随访。")
      if(any(now$time[!terminal]<last$time[!terminal]-1e-7))stop("仍随访患者的后续观察年龄早于之前确认的年龄。")
      newly_terminal<-!terminal & now$status %in% c("event","dropout")
      if(any(now$time[newly_terminal]<=last$time[newly_terminal]+1e-7))stop("新记录的事件或永久退出须晚于之前确认的随访年龄；不能补记到已观察历史。")
      fresh<-!d$id %in% last$id
      if(any(d$entry[fresh]<=last_cut+1e-7))stop("新出现患者在前一次DCO前已经入组，历史快照不完整或存在迟录入。")
    }
    last<-d;last_cut<-s$cut
  }
  invisible(TRUE)
}
conditional_history_path <- function(snapshots,cfg) {
  validate_ia_history(snapshots)
  if(cfg$prediction_design!="sequential"||cfg$cut_mode!="target")stop("历史多次IA当前只用于原PH/log-rank事件驱动组序贯路径。")
  plan<-conditional_prediction_plan(cfg);m<-length(snapshots)
  if(m>nrow(plan))stop("历史分析次数超过原计划。")
  rows<-lapply(seq_len(m),function(k) {
    s<-snapshots[[k]];d<-s$data
    if(sum(d$event)!=plan$target_events[k])stop(paste0("分析",k,"事件数",sum(d$event),"与原计划目标",plan$target_events[k],"不一致；并列超目标或信息时点变更需另行处理。"))
    o<-data.frame(USUBJID=d$id,group=d$group,entry_day=d$entry,time_day=d$time,obs_day=d$obs_day,event=d$event,status=d$status,DCO_DAY=s$cut)
    a<-conditional_prediction_logrank(o,cfg)
    if(!a$valid)stop(paste0("历史分析",k,"的log-rank无效。"))
    data.frame(look=k,information_fraction=plan$information_fraction[k],alpha_spent=plan$alpha_spent[k],DCO_DAY=s$cut,target_events=plan$target_events[k],events=sum(d$event),n=nrow(d),target_reached=TRUE,performed=TRUE,valid=TRUE,benefit_z=a$w,score_variance=a$variance,nominal_p=a$p,upper_z=plan$upper_z[k],lower_z=plan$lower_efficacy_z[k],futility_z=plan$futility_z[k],action=gs_action(a$w,TRUE,k,plan,cfg$prediction_sided),note=a$note)
  })
  path<-do.call(rbind,rows);rownames(path)<-NULL
  if(any(head(path$action,-1)!="continue"))stop("所选IA以前已经按原方案停止；不能从停止后的快照继续检验。")
  path
}
ia_history_template <- function() {
  origin<-as.Date("2025-01-01");ids<-sprintf("H%03d",1:120);entry<-c(rep(0,100),rep(25,20));group<-rep(c("Control","Treatment"),60)
  event_day<-c(1:60,rep(Inf,60));drop_day<-rep(Inf,120);drop_day[90]<-12
  out<-lapply(1:3,function(k) {
    cut<-20*k;use<-entry<cut;obs<-pmin(event_day,drop_day,cut);status<-ifelse(event_day<=drop_day&event_day<=cut,0L,ifelse(drop_day<event_day&drop_day<=cut,2L,1L))
    data.frame(IASEQ=k,DCO=as.character(origin+cut),USUBJID=ids[use],PARAMCD="OS",STARTDT=as.character(origin+entry[use]),ADT=as.character(origin+obs[use]),AVAL=obs[use]-entry[use]+1,AVALU="DAYS",CNSR=status[use],TRTP=group[use],ANL01FL="Y")
  })
  do.call(rbind,out)
}
