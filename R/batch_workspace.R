# Local research workspace, lossless configuration transport and continuation.
batch_version <- "0.35.0"
batch_node_names <- function(x)if(is.null(names(x)))NULL else as.list(names(x))
batch_encode_tree <- function(x) {
  if(is.null(x))return(list(type="null"))
  if(is.data.frame(x))return(list(type="data.frame",names=as.list(names(x)),row_names=as.list(rownames(x)),data=unname(lapply(x,batch_encode_tree))))
  if(is.list(x))return(list(type="list",names=batch_node_names(x),data=unname(lapply(x,batch_encode_tree))))
  if(!is.atomic(x)||is.object(x)||!is.null(dim(x)))stop("配置仅允许基本向量/list/data.frame，不接受对象或表达式。")
  t<-typeof(x)
  if(!t %in% c("double","integer","logical","character"))stop("不支持的配置字段类型。")
  if(t=="double")return(list(type=t,names=batch_node_names(x),length=length(x),data=paste(sprintf("%02x",as.integer(writeBin(x,raw(),size=8,endian="little"))),collapse="")))
  list(type=t,names=batch_node_names(x),data=unname(as.list(x)))
}
batch_decode_tree <- function(node,depth=0L,budget=new.env(parent=emptyenv())) {
  if(is.null(budget$n))budget$n<-0L;budget$n<-budget$n+1L
  if(depth>80||budget$n>50000L||!is.list(node)||length(node$type)!=1)stop("配置结构或规模不支持。")
  t<-node$type
  if(!is.character(t)||is.na(t)||!t %in% c("null","list","data.frame","double","integer","logical","character"))stop("配置节点类型无效。")
  if(t=="null")return(NULL)
  next_node<-function(x)batch_decode_tree(x,depth+1L,budget)
  nms<-if(is.null(node$names))NULL else vapply(node$names,function(x){if(!is.character(x)||length(x)!=1||is.na(x))stop("字段名称无效。");x},character(1))
  if(t %in% c("list","data.frame")) {
    if(!is.list(node$data))stop("配置列表格式无效。")
    x<-lapply(node$data,next_node)
    if(!is.null(nms)){if(length(nms)!=length(x))stop("配置名称数量不符。");names(x)<-nms}
    if(t=="data.frame") {
      if(is.null(nms)||anyDuplicated(nms))stop("表列名无效。")
      rn<-vapply(node$row_names,function(y){if(!is.character(y)||length(y)!=1||is.na(y))stop("表行名无效。");y},character(1))
      if(anyDuplicated(rn)||any(vapply(x,length,integer(1))!=length(rn)))stop("表行数不符。")
      x<-as.data.frame(x,stringsAsFactors=FALSE,check.names=FALSE,row.names=rn)
    }
    return(x)
  }
  if(t=="double") {
    n<-node$length;h<-node$data
    if(!is.numeric(n)||length(n)!=1||!is.finite(n)||n<0||n>100000||n!=floor(n)||!is.character(h)||length(h)!=1||nchar(h)!=16*n||grepl("[^0-9a-f]",h))stop("双精度字段格式无效。")
    bytes<-if(n)as.raw(strtoi(substring(h,seq.int(1,nchar(h),2),seq.int(2,nchar(h),2)),16L)) else raw()
    x<-readBin(bytes,what="double",n=n,size=8,endian="little")
  } else {
    if(!t %in% c("integer","logical","character")||!is.list(node$data)||length(node$data)>100000)stop("向量格式无效。")
    x<-switch(t,
      integer=vapply(node$data,function(y){if(is.null(y))return(NA_integer_);if(!is.numeric(y)||length(y)!=1||!is.finite(y)||y!=floor(y)||abs(y)>.Machine$integer.max)stop("整数格式无效。");as.integer(y)},integer(1)),
      logical=vapply(node$data,function(y){if(is.null(y))return(NA);if(!is.logical(y)||length(y)!=1||is.na(y))stop("逻辑格式无效。");y},logical(1)),
      character=vapply(node$data,function(y){if(is.null(y))return(NA_character_);if(!is.character(y)||length(y)!=1||is.na(y))stop("字符格式无效。");y},character(1)))
  }
  if(!is.null(nms)){if(length(nms)!=length(x))stop("向量名称数量不符。");names(x)<-nms};x
}
batch_tree_text <- function(cfg)as.character(jsonlite::toJSON(batch_encode_tree(cfg),auto_unbox=TRUE,null="null",na="null",digits=NA))
batch_text_hash <- function(text) {
  path<-tempfile("batch-hash-");on.exit(unlink(path),add=TRUE);writeBin(charToRaw(enc2utf8(text)),path);unname(tools::md5sum(path))
}
batch_config_hash <- function(cfg,exclude_reps=FALSE) {if(exclude_reps)cfg$reps<-NULL;batch_text_hash(batch_tree_text(cfg))}
batch_source_hash <- function(root=getwd()) {
  files<-paste0("R/",c("units","models","forecast","inputs","bayes","simulation","conditional_simulation","nph","sequential","study","parameter_calibration","batch_research","batch_analysis","batch_sequential","batch_workspace","joint_survival","joint_research","joint_marginal","adaptation","patient_adaptation","patient_adaptive_prediction","patient_prediction_process","conditional_prediction","research_runtime","conditional_batch","heterogeneity","ia_history","sequential_beta","research_sources","shared_adaptation","shared_riskset","sequential_repeated_binding","multistate_prediction","joint_sequential","flexible_models","flexible_splines","flexible_prediction","observation_process","multiarm_design","multiarm_multiplicity","design_search","reverse_calibration","ia_reverse_calibration","panel_multistate"),".R")
  batch_text_hash(paste(files,unname(tools::md5sum(file.path(root,files))),collapse="\n"))
}
batch_config_document <- function(cfg)list(schema="event_pred.batch_config.v1",version=batch_version,configuration_hash=batch_config_hash(cfg),summary=list(research_family=cfg$research_family %||% "single_endpoint",mode=cfg$mode,design=cfg$design_mode %||% "fixed",effect=cfg$effect_mode %||% "ph",primary=cfg$primary,display_unit=cfg$display_unit,reps=cfg$reps,seed=cfg$seed),exact_config=batch_encode_tree(cfg),note="exact_config is authoritative; double values are little-endian IEEE-754 hex, not R expressions")
batch_read_config <- function(path) {
  if(file.info(path)$size>5*1024^2)stop("配置JSON最多5MB。")
  d<-jsonlite::read_json(path,simplifyVector=FALSE)
  if(!identical(d$schema,"event_pred.batch_config.v1"))stop("请选择新格式无损配置JSON；旧版仅供阅读的JSON暂不导入。")
  cfg<-batch_decode_tree(d$exact_config)
  if(!identical(batch_config_hash(cfg),d$configuration_hash))stop("配置内容与配置摘要值不符。")
  validate_batch_config(cfg);batch_prepare_sequential(cfg)
}
batch_validate_continuation <- function(state,cfg,operation="resume") {
  if(!identical(state$version,batch_version)||!identical(state$source_hash,batch_source_hash()))stop("续跑需同一代码版本与引擎内容；可载入旧结果查看，但不能混合不同引擎的轮次。")
  if(!identical(state$dependencies,batch_dependencies(cfg)))stop("续跑运行依赖或RNG类型不一致。")
  same<-identical(batch_config_hash(state$config,operation=="extend"),batch_config_hash(cfg,operation=="extend"))
  if(!same)stop("续跑不能改变原配置；追加仅允许增加每情景重复数。")
  if(ds_is(cfg)&&operation=="extend")stop("搜索与独立确认不追加B；请新建研究，以免改变冻结选择。")
  if(operation=="extend"&&cfg$reps<=state$config$reps)stop("追加重复数需大于原请求数。")
  if(operation=="resume"&&cfg$reps!=state$config$reps)stop("续跑需保留原请求数。")
  sc<-batch_scenarios(cfg);d<-state$rows
  if(!is.data.frame(d)||!all(c("SCENARIO","SIMID","replicate_seed") %in% names(d))&&nrow(d))stop("已处理轮次结构不符。")
  if(length(state$completed)!=1||!is.finite(state$completed)||state$completed!=nrow(d))stop("快照处理数与记录数不符。")
  if(!nrow(d)&&is.data.frame(state$method_rows)&&nrow(state$method_rows))stop("空轮次不能包含方法记录。")
  if(nrow(d)) {
    if(anyNA(d[,c("SCENARIO","SIMID")])||any(!d$SCENARIO %in% sc$SCENARIO)||any(d$SIMID<1|d$SIMID>state$config$reps|d$SIMID!=floor(d$SIMID))||anyDuplicated(d[,c("SCENARIO","SIMID")]))stop("已处理键无效或重复。")
    if(anyNA(d$replicate_seed)||any(!is.finite(d$replicate_seed))||any(d$replicate_seed!=mapply(batch_replicate_seed,MoreArgs=list(seed=cfg$seed),scenario=d$SCENARIO,replicate=d$SIMID)))stop("已处理轮次种子与原配置不符。")
    mr<-state$method_rows
    if(!is.data.frame(mr)||!all(c("SCENARIO","SIMID","method") %in% names(mr))||anyNA(mr[,c("SCENARIO","SIMID","method")])||any(!mr$method %in% batch_methods(cfg))||anyDuplicated(mr[,c("SCENARIO","SIMID","method")]))stop("方法轮次无效。")
    for(m in batch_methods(cfg))if(!setequal(paste(d$SCENARIO,d$SIMID),paste(mr$SCENARIO[mr$method==m],mr$SIMID[mr$method==m])))stop("已处理方法记录不完整。")
  }
  if(jr_is(cfg)&&!nrow(d)&&is.data.frame(state$policy_rows)&&nrow(state$policy_rows))stop("空轮次不能有联合规则记录。")
  if(jr_is(cfg)&&nrow(d)){
    pr<-state$policy_rows
    if(!is.data.frame(pr)||!all(c("SCENARIO","SIMID","policy") %in% names(pr))||anyNA(pr[,c("SCENARIO","SIMID","policy")])||anyDuplicated(pr[,c("SCENARIO","SIMID","policy")])||any(!pr$policy %in% jr_policies(cfg)))stop("联合规则轮次结构不符。")
    for(m in jr_policies(cfg))if(!setequal(paste(d$SCENARIO,d$SIMID),paste(pr$SCENARIO[pr$policy==m],pr$SIMID[pr$policy==m])))stop("联合规则已处理记录不完整。")
  }
  if(ds_is(cfg))ds_validate_saved(state,cfg)
  if(fp_is(cfg))fp_validate_saved(state,cfg)
  if(ms_is(cfg))ms_validate_saved(state,cfg)
  if(je_is(cfg))je_validate_saved(state,cfg)
  if(sp_is(cfg))sp_validate_saved(state,cfg)
  if(cb_is(cfg))cb_validate_saved(state,cfg)
  if(abr_is(cfg))abr_validate_paths(state,cfg)
  lk<-state$looks
  if(!is.null(lk)&&nrow(lk)&&(!all(c("SCENARIO","SIMID") %in% names(lk))||!all(paste(lk$SCENARIO,lk$SIMID) %in% paste(d$SCENARIO,d$SIMID))))stop("分析路径包含未处理轮次。")
  invisible(TRUE)
}
batch_workspace_root <- function(token,root=getwd()) {
  if(length(token)!=1||!is.character(token)||!grepl("^[a-f0-9]{32}$",token))stop("等待本机浏览器工作区建立。")
  file.path(root,"data","private","batch-runs",token)
}
batch_run_path <- function(token,id,root=getwd()) {
  if(length(id)!=1||!is.character(id)||!grepl("^[A-Za-z0-9_-]{1,100}$",id))stop("研究ID无效。")
  file.path(batch_workspace_root(token,root),id)
}
batch_atomic_rds <- function(x,path) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE);temp<-tempfile("write-",tmpdir=dirname(path));on.exit(unlink(temp),add=TRUE);saveRDS(x,temp,version=3);if(!file.rename(temp,path))stop("本机记录保存失败。")
}
batch_read_local <- function(path)if(file.exists(path))readRDS(path) else NULL
batch_local_active <- function(run_path) {
  l<-batch_read_local(file.path(run_path,"lease","owner.rds"));if(is.null(l)){p<-file.path(run_path,"lease");return(dir.exists(p)&&as.numeric(difftime(Sys.time(),file.info(p)$mtime,units="secs"))<30)}
  tryCatch({p<-ps::ps_handle(l$pid);ps::ps_is_running(p)&&abs(ps::ps_create_time(p)-l$process_start)<.001},error=function(e)FALSE)
}
batch_acquire_run <- function(run_path) {
  lease<-file.path(run_path,"lease");dir.create(run_path,recursive=TRUE,showWarnings=FALSE)
  if(dir.exists(lease)){if(batch_local_active(run_path))stop("该研究仍在其他会话运行，请等待或载入其他研究。");unlink(lease,recursive=TRUE)}
  if(!dir.create(lease,showWarnings=FALSE))stop("研究已被另一会话占用。")
  batch_atomic_rds(list(pid=Sys.getpid(),process_start=ps::ps_create_time(ps::ps_handle()),created_at=format(Sys.time(),tz="UTC",usetz=TRUE)),file.path(lease,"owner.rds"));invisible(TRUE)
}
batch_create_run <- function(cfg,token,title,parent=NULL,operation="new",root=getwd()) {
  if(length(title)!=1||is.na(title)||!nzchar(trimws(title))||nchar(title)>100)stop("研究名称需1–100字符。")
  id<-paste0("run_",format(Sys.time(),"%Y%m%dT%H%M%S",tz="UTC"),"_",gsub("[^A-Za-z0-9]","",basename(tempfile())))
  path<-batch_run_path(token,id,root);dir.create(path,recursive=TRUE,showWarnings=FALSE)
  meta<-list(id=id,title=trimws(title),created_at=format(Sys.time(),tz="UTC",usetz=TRUE),updated_at=format(Sys.time(),tz="UTC",usetz=TRUE),status="created",version=batch_version,source_hash=batch_source_hash(root),configuration_hash=batch_config_hash(cfg),dependencies=batch_dependencies(cfg),research_family=cfg$research_family %||% "single_endpoint",parent_id=parent %||% "",operation=operation,archived=FALSE)
  batch_atomic_rds(cfg,file.path(path,"config.rds"));batch_atomic_rds(meta,file.path(path,"meta.rds"));list(path=path,meta=meta)
}
batch_save_checkpoint <- function(state,run_path) {
  m<-batch_read_local(file.path(run_path,"meta.rds"));if(is.null(m))stop("研究元信息缺失。")
  state$run_id<-m$id;state$run_title<-m$title;state$parent_id<-m$parent_id;state$operation<-m$operation;state$source_hash<-m$source_hash
  state$schema<-"event_pred.batch_research.v4";state$configuration_hash<-m$configuration_hash
  revision<-paste0(format(Sys.time(),"%Y%m%dT%H%M%OS6",tz="UTC"),"_",gsub("[^A-Za-z0-9]","",basename(tempfile())))
  checkpoint<-paste0("checkpoint_",gsub("[^A-Za-z0-9_-]","",revision),".rds")
  batch_atomic_rds(state,file.path(run_path,"checkpoints",checkpoint))
  m$latest_checkpoint<-checkpoint;m$updated_at<-format(Sys.time(),tz="UTC",usetz=TRUE);m$status<-state$status;m$completed<-state$completed;m$total<-state$total
  batch_atomic_rds(m,file.path(run_path,"meta.rds"))
  # Retain the latest three revision files. Configuration and parent studies remain.
  revisions<-list.files(file.path(run_path,"checkpoints"),pattern="^checkpoint_.*\\.rds$",full.names=TRUE)
  old<-setdiff(revisions,head(revisions[order(file.info(revisions)$mtime,decreasing=TRUE)],3))
  if(length(old))unlink(old)
  state
}
batch_load_run <- function(token,id,root=getwd()) {
  path<-batch_run_path(token,id,root);m<-batch_read_local(file.path(path,"meta.rds"));if(is.null(m)||is.null(m$latest_checkpoint)||!grepl("^checkpoint_[A-Za-z0-9_-]+\\.rds$",m$latest_checkpoint))stop("研究尚无保存快照。")
  state<-batch_read_local(file.path(path,"checkpoints",m$latest_checkpoint));if(is.null(state))stop("快照缺失。")
  if(!identical(state$configuration_hash,batch_config_hash(state$config)))stop("快照配置不符。")
  state$run_title<-m$title;state$store_active<-batch_local_active(path);if(state$status=="running"&&!state$store_active)state$status<-"interrupted";state
}
batch_list_runs <- function(token,include_archived=FALSE,root=getwd()) {
  base<-batch_workspace_root(token,root);if(!dir.exists(base))return(data.frame())
  out<-lapply(list.dirs(base,recursive=FALSE,full.names=TRUE),function(p){m<-tryCatch(batch_read_local(file.path(p,"meta.rds")),error=function(e)NULL);if(is.null(m)||(!include_archived&&isTRUE(m$archived)))return(NULL)
    data.frame(run_id=m$id,title=m$title,research_family=m$research_family %||% "single_endpoint",version=m$version,status=if(batch_local_active(p))"running" else if(m$status=="running")"interrupted" else m$status,completed=m$completed %||% 0,total=m$total %||% NA_integer_,updated_at=m$updated_at,parent_id=m$parent_id,operation=m$operation,archived=m$archived,stringsAsFactors=FALSE)})
  d<-do.call(rbind,out);if(is.null(d))data.frame() else d[order(d$updated_at,decreasing=TRUE),,drop=FALSE]
}
batch_update_run <- function(token,id,title=NULL,archive=NULL,root=getwd()) {
  path<-batch_run_path(token,id,root);if(batch_local_active(path))stop("运行期间不能修改研究名称或归档状态。")
  m<-batch_read_local(file.path(path,"meta.rds"));if(is.null(m))stop("未找到研究。")
  if(!is.null(title)){if(length(title)!=1||is.na(title)||!nzchar(trimws(title))||nchar(title)>100)stop("名称需1–100字符。");m$title<-trimws(title)}
  if(!is.null(archive))m$archived<-isTRUE(archive);m$updated_at<-format(Sys.time(),tz="UTC",usetz=TRUE);batch_atomic_rds(m,file.path(path,"meta.rds"));invisible(m)
}
batch_bind_columns <- function(tables) {
  tables<-Filter(function(x)is.data.frame(x)&&nrow(x)>0,tables);if(!length(tables))return(data.frame())
  cols<-unique(unlist(lapply(tables,names),use.names=FALSE))
  do.call(rbind,lapply(tables,function(d){for(k in setdiff(cols,names(d)))d[[k]]<-NA;d[,cols,drop=FALSE]}))
}
batch_collect_runs <- function(token,ids,root=getwd()) {
  if(!length(ids)||length(ids)>20||anyDuplicated(ids))stop("汇总选择1–20个不同研究。")
  states<-lapply(ids,function(id)batch_load_run(token,id,root))
  annotate<-function(r,key){d<-r[[key]];if(is.null(d)||!nrow(d))return(data.frame());cbind(run_id=r$run_id,run_title=r$run_title,parent_id=r$parent_id,source_version=r$version,configuration_hash=r$configuration_hash,mode=r$config$mode,design=r$config$design_mode %||% "fixed",primary=r$config$primary,endpoint=r$config$paramcd,sided=r$config$sided,alpha=r$config$alpha,effect=r$config$effect_mode %||% "ph",cut_rule=r$config$cut_rule,seed=r$config$seed,research_family=r$config$research_family %||% "single_endpoint",source_display_unit=r$config$display_unit,d[,setdiff(names(d),c("run_id","run_title","parent_id","source_version","configuration_hash","mode","design","primary","endpoint","sided","alpha","effect","cut_rule","seed","source_display_unit","research_family")),drop=FALSE])}
  overlaps<-list()
  if(length(states)>1)for(pair in combn(seq_along(states),2,simplify=FALSE)){a<-states[[pair[1]]];b<-states[[pair[2]]];common<-if(nrow(a$rows)&&nrow(b$rows))length(intersect(a$rows$replicate_seed,b$rows$replicate_seed)) else 0L;overlaps[[length(overlaps)+1L]]<-data.frame(run_a=a$run_id,run_b=b$run_id,common_replicate_seeds=common,note=if(common)"存在共享种子；不按独立研究计算差值MCSE" else "未发现已处理种子重合；本表未计算跨研究检验")}
  list(ids=ids,configurations=setNames(lapply(states,function(r)batch_config_document(r$config)),ids),overview=batch_bind_columns(lapply(states,annotate,key="overview")),methods=batch_bind_columns(lapply(states,annotate,key="method_overview")),policies=batch_bind_columns(lapply(states,annotate,key="policy_overview")),resources=batch_bind_columns(lapply(states,annotate,key="resources")),overlaps=batch_bind_columns(overlaps),created_at=format(Sys.time(),tz="UTC",usetz=TRUE),version=batch_version,review_status="pending_v0.35_review",note="Descriptive collection only; no pooling, independence assumption or automatic design selection")
}
batch_collection_report <- function(r) {
  tab<-function(d)capture.output(print(d,row.names=FALSE))
  c("# 本机研究汇总（开发稿，待复核）","",paste0("生成时间：",r$created_at),"所有原始时间量以日计，保留原显示单位标签。仅并列记录，不合并轮次，不进行跨研究显著性检验或自动择优。","","## 主分析","","```text",tab(r$overview),"```","","## 方法","","```text",tab(r$methods),"```","","## 联合终点规则","","```text",tab(r$policies),"```","","## 资源","","```text",tab(r$resources),"```","","## 共享种子记录","","```text",tab(r$overlaps),"```")
}

batch_initial_snapshot <- function(cfg,initial=NULL) {
  if(ma_is(cfg))return(ma_initial(cfg,initial))
  if(ds_is(cfg))return(ds_initial(cfg,initial))
  if(fp_is(cfg))return(fp_initial(cfg,initial))
  if(ob_is(cfg))return(ob_initial(cfg,initial))
  if(ms_is(cfg))return(ms_initial(cfg,initial))
  if(je_is(cfg))return(je_initial(cfg,initial))
  if(sp_is(cfg))return(sp_initial(cfg,initial))
  if(cb_is(cfg))return(cb_initial(cfg,initial))
  if(hg_is(cfg))return(hg_initial(cfg,initial))
  if(abr_is(cfg))return(abr_initial(cfg,initial))
  if(jr_is(cfg))return(jr_initial(cfg,initial))
  sc<-batch_scenarios(cfg);d<-if(is.null(initial))data.frame() else initial$rows
  ov<-batch_overview(d,cfg,sc)
  batch_attach_method_outputs(list(config=cfg,scenarios=sc,rows=d,method_rows=initial$method_rows %||% data.frame(),looks=initial$looks %||% data.frame(),overview=ov,
    comparisons=batch_comparisons(ov,cfg$reference_scenario),candidates=batch_candidates(ov,cfg),completed=nrow(d),total=nrow(sc)*cfg$reps,status="running",version=batch_version,
    review_status="pending_v0.35_review",schema="event_pred.batch_research.v4",created_at=format(Sys.time(),tz="UTC",usetz=TRUE),dependencies=batch_dependencies(cfg),source_hash=batch_source_hash()))
}
