# Clinical snapshot adapter. Original design is fixed; no future process is generated.
history_inference_config <- function(input) {
  side<-input$hi_sided %||% "benefit"
  list(target=input$hi_target, timing=parse_unit_numbers(input$hi_timing),
    sided=side, alpha=input$hi_alpha, spending=input$hi_spending,
    futility=if(side=="benefit")input$hi_futility %||% "none" else "none",
    futility_z=if(side=="benefit"&&identical(input$hi_futility,"z"))parse_unit_numbers(input$hi_futility_z) else numeric(),
    treatment_fraction=input$hi_allocation, control=input$hi_control,
    display_unit=input$hi_unit %||% "months",beta=input$hi_beta,beta_spending=input$hi_beta_spending,
    beta_gamma=input$hi_beta_gamma,design_hr=input$hi_design_hr)
}
history_inference_prediction_config <- function(cfg) {
  scalar<-function(x,lo,hi)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi
  if(!scalar(cfg$target,1,1000000)||cfg$target!=floor(cfg$target))stop("原最终事件目标D*需为1–1000000的整数。")
  if(!scalar(cfg$alpha,.0001,.2)||!cfg$sided %in% c("benefit","two"))stop("总alpha需0.0001–0.2，并指定单侧获益或双侧差异。")
  if(!scalar(cfg$treatment_fraction,.01,.99))stop("原Treatment分配概率需为0.01–0.99；不能用观察到的组别人数比例替代。")
  if(!cfg$futility %in% c("none","z","beta")||(cfg$sided=="two"&&cfg$futility!="none"))stop("单侧可沿用原Z或约束性beta无效界，双侧仅无无效界。")
  if(!cfg$display_unit %in% c("days","weeks","months"))stop("显示单位请选择日、周或月。")
  list(prediction_design="sequential",cut_mode="target",target=cfg$target,
    prediction_timing=cfg$timing,prediction_spending=cfg$spending,
    prediction_futility=cfg$futility,prediction_futility_z=cfg$futility_z,
    prediction_sided=cfg$sided,prediction_alpha=cfg$alpha,prediction_control=cfg$control,
    prediction_beta=cfg$beta,prediction_beta_spending=cfg$beta_spending,prediction_beta_gamma=cfg$beta_gamma,
    prediction_design_hr=cfg$design_hr,prediction_allocation=cfg$treatment_fraction)
}
run_history_inference <- function(snapshots,cfg,metadata=list()) {
  # A normalized prefix is the only clinical input. No fit, resampling or future data.
  c<-history_inference_prediction_config(cfg)
  validate_ia_history(snapshots)
  labs<-sort(unique(snapshots[[length(snapshots)]]$data$group))
  if(length(cfg$control)!=1||is.na(cfg$control)||!cfg$control %in% labs)stop("请从所选历史前缀的两组中指定Control。")
  plan<-conditional_prediction_plan(c)
  path<-conditional_history_path(snapshots,c)
  names(path)[names(path)=="lower_z"]<-"lower_efficacy_z"
  names(path)[names(path)=="n"]<-"n_observed"
  # Internal identifiers satisfy the common engine contract; never label as simulated trials.
  path$SCENARIO<-1L;path$SIMID<-1L
  infer_cfg<-list(design_mode="sequential",alpha=cfg$alpha,sided=cfg$sided,
    gs_spending=cfg$spending,gs_futility=cfg$futility,gs_futility_z=cfg$futility_z,
    treatment_fraction=cfg$treatment_fraction,gs_beta=cfg$beta,gs_beta_spending=cfg$beta_spending,gs_beta_gamma=cfg$beta_gamma,gs_design_hr=cfg$design_hr)
  inference<-gs_inference(infer_cfg,plan,path,length(snapshots))
  path$event_information<-path$events*cfg$treatment_fraction*(1-cfg$treatment_fraction)
  path$variance_to_event_information<-path$score_variance/path$event_information
  path$source<-"observed_history"
  diagnostics<-do.call(rbind,lapply(snapshots,function(s) {
    d<-s$data
    data.frame(look=s$look,DCO_DAY=s$cut,n_control=sum(d$group==cfg$control),
      n_treatment=sum(d$group!=cfg$control),events_control=sum(d$event[d$group==cfg$control]),
      events_treatment=sum(d$event[d$group!=cfg$control]),active=sum(d$status=="active"),
      permanent_dropout=sum(d$status=="dropout"))
  }))
  inference$review_status<-"pending_v0.30_clinical_adapter_review"
  list(inference=inference,path=path,diagnostics=diagnostics,plan=plan,config=cfg,
    snapshots=snapshots,metadata=metadata,control=cfg$control,treatment=setdiff(labs,cfg$control),
    created_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z"),version="0.30.0",
    status="pending_review",source="observed_history",
    information_assumption="PH/log-rank canonical approximation; I_k=D_k*p*(1-p); actual score variance diagnostic only")
}
history_inference_export_table <- function(d) d[,setdiff(names(d),c("SCENARIO","SIMID")),drop=FALSE]
history_inference_records <- function(r) {
  do.call(rbind,lapply(r$snapshots,function(s)cbind(IASEQ=s$look,DCO_DAY=s$cut,s$data)))
}
history_inference_bundle <- function(r) {
  list(schema="event_pred.history_inference.v1",version=r$version,status=r$status,
    source=r$source,created_at=r$created_at,metadata=r$metadata,config=r$config,
    selected_look=length(r$snapshots),snapshots=r$snapshots,plan=r$plan,path=history_inference_export_table(r$path),
    diagnostics=r$diagnostics,repeated=history_inference_export_table(r$inference$repeated),
    final=history_inference_export_table(r$inference$final),
    rpact_version=r$inference$rpact_version,information_assumption=r$information_assumption)
}
history_inference_report <- function(r) {
  tab<-function(d)capture.output(print(history_inference_export_table(d),row.names=FALSE))
  c("# 历史 IA 调整推断（开发稿，待复核）","",paste0("版本：",r$version,"；保存时间：",r$created_at),
    paste0("所选原分析：",length(r$snapshots),"；Control：",r$control,"；Treatment：",r$treatment),
    paste0("显示单位：",time_label(r$config$display_unit),"；导出DCO_DAY固定为研究经过日数。"),
    "","## 原设计","","```json",jsonlite::toJSON(r$config,auto_unbox=TRUE,pretty=TRUE,digits=NA),"```",
    "","## 已观察历史路径","","```text",tab(r$path),"```",
    "","## 重复推断","","```text",tab(r$inference$repeated),"```",
    "","## 所选分析的停止推断","","```text",tab(r$inference$final),"```",
    "","## 方法与范围","",
    "基于每次完整两组ADTTE快照重新计算普通log-rank，正Z表示Treatment获益。使用原分配概率和I=D*p*(1-p)做PH/canonical近似；实际log-rank方差另列诊断，不用于重定信息时点。近似HR不等于Cox拟合HR。",
    "重复推断逐次报告；继续试验的前缀不提供阶段排序停止推断。原停止规则已触发后不允许加入后续检验。单侧区间水平为1−2alpha，双侧为1−alpha。",
    "不支持计划变更、重估设计、事件超目标、迟报/裁定修订、缺失快照及非比例风险专用分析。未计算未来拒绝概率、功效或Ⅰ类错误。",
    "本版临床数据适配、导出重放及公式显示尚未验证；历史v0.19验证记录不作为本入口的通过记录。")
}
history_inference_script <- function(r) {
  dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  c("# event_pred v0.30.0 clinical-history inference draft; replay not yet verified.",
    "# Run in the matching source root. Only normalized observed snapshots are embedded.",
    'if (!exists("%||%", mode="function")) `%||%` <- function(x,y) if (is.null(x)) y else x',
    'for (f in c("units","models","forecast","inputs","nph","sequential","sequential_inference","study","conditional_prediction","ia_history","history_inference")) source(paste0("R/",f,".R"))',
    paste0('required_rpact <- "',r$inference$rpact_version,'"'),
    'if (as.character(packageVersion("rpact")) != required_rpact) stop("rpact version differs from the saved calculation")',
    paste0("original_config <- ",dump(r$config)),paste0("observed_snapshots <- ",dump(r$snapshots)),
    paste0("source_metadata <- ",dump(r$metadata)),paste0("saved_plan <- ",dump(r$plan)),
    'result <- run_history_inference(observed_snapshots, original_config, source_metadata)',
    'if (!isTRUE(all.equal(result$plan,saved_plan,tolerance=1e-8))) stop("Original design differs from the saved plan")',
    'write.csv(history_inference_export_table(result$path),"history_path_replayed.csv",row.names=FALSE)',
    'write.csv(history_inference_export_table(result$inference$repeated),"history_repeated_replayed.csv",row.names=FALSE)',
    'write.csv(history_inference_export_table(result$inference$final),"history_final_replayed.csv",row.names=FALSE)')
}
