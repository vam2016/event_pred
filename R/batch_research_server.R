batch_config_input <- function(input,unit) {
  f<-time_factor(unit);source<-input$ba_source %||% "parameters"
  v<-function(pre){d<-simulation_defaults(unit);setNames(lapply(names(d),function(k)input[[paste0(pre,k)]] %||% d[[k]]),names(d))}
  calibration<-if(source=="two_survival")calibrate_weibull_survival(c(input$ba_cal_t1,input$ba_cal_t2),c(input$ba_cal_s1,input$ba_cal_s2),unit) else NULL
  mode<-input$ba_mode;design<-input$ba_design_mode %||% "fixed";sequential<-design=="sequential";rule<-if(sequential)"target" else input$ba_cut_rule;enroll<-input$ba_enroll_mode;from<-input$ba_scenario_source
  cfg<-list(mode=mode,display_unit=unit,engine_unit="days",origin=as.character(input$ba_origin),paramcd=input$ba_endpoint,
    control_model=if(is.null(calibration))simulation_parameter_model(v("b_")$design_model,v("b_"),unit) else calibration$model,
    control_source=source,calibration=calibration,treatment_fraction=input$ba_allocation,
    dropout_rates=c(dropout_input_rate(v("bc_")),dropout_input_rate(v("bt_")))/f,
    enroll_mode=enroll,enroll_rate=if(enroll=="constant")input$ba_enroll_rate/f else NULL,
    enroll_cuts=if(enroll=="piecewise")parse_unit_numbers(input$ba_enroll_cuts)*f else numeric(),enroll_rates=if(enroll=="piecewise")parse_unit_numbers(input$ba_enroll_rates)/f else numeric(),
    cut_rule=rule,max_day=if(rule!="fixed")input$ba_max*f else NULL,
    minimum_day=if(rule=="event_minimum")input$ba_minimum*f else NULL,
    miss_policy=if(rule!="fixed")input$ba_miss else "analyze",
    primary=input$ba_primary,sided=if(mode=="assurance")"benefit" else input$ba_sided,alpha=input$ba_alpha,estimate_hr=isTRUE(input$ba_estimate),
    reps=input$ba_reps,seed=input$ba_seed,scenario_source=from,reference_scenario=input$ba_reference,filter_enabled=isTRUE(input$ba_filter))
  cfg$effect_mode<-if(sequential||mode=="assurance")"ph" else input$ba_effect_mode %||% "ph"
  if(!batch_is_ph(cfg))cfg$profiles<-batch_parse_profiles(input$ba_profiles,unit)
  cfg$design_mode<-design
  if(sequential){cfg$primary<-"logrank";cfg$estimate_hr<-FALSE;cfg$miss_policy<-"no_reject";cfg$gs_timing<-parse_unit_numbers(input$ba_gs_timing);cfg$gs_spending<-input$ba_gs_spending;cfg$gs_futility<-if(cfg$sided=="benefit")input$ba_gs_futility else "none";cfg$gs_futility_z<-if(cfg$gs_futility=="z")parse_unit_numbers(input$ba_gs_z) else numeric();cfg$fixed_benchmark<-isTRUE(input$ba_fixed_benchmark)
    if(cfg$gs_futility=="beta"){cfg$gs_beta_basis<-input$ba_beta_basis;cfg$gs_beta<-if(input$ba_beta_basis=="beta")input$ba_gs_beta else 1-input$ba_gs_power;cfg$gs_beta_spending<-input$ba_gs_beta_spending;cfg$gs_design_hr<-input$ba_gs_design_hr;if(cfg$gs_beta_spending=="bsHSD")cfg$gs_beta_gamma<-input$ba_gs_gamma}
  }
  cfg$compare_methods<-!sequential&&isTRUE(input$ba_compare_methods)
  if(cfg$compare_methods)cfg$extra_methods<-unique(as.character(input$ba_extra_methods %||% character()))
  methods<-batch_methods(cfg)
  if("fh" %in% methods){cfg$fh_rho<-input$ba_fh_rho;cfg$fh_gamma<-input$ba_fh_gamma}
  if(any(methods %in% c("rmst","survival")))cfg$analysis_tau<-input$ba_tau*f
  if(mode=="assurance"){cfg$prior_loghr_sd<-input$ba_prior_hr_sd;cfg$prior_logtime_sd<-input$ba_prior_time_sd}
  if(isTRUE(cfg$filter_enabled)){cfg$filter_basis<-input$ba_filter_basis;cfg$success_threshold<-input$ba_threshold}
  if(from=="csv") {
    if(is.null(input$ba_file))stop("请选择情景CSV或改用列表组合。")
    cfg$scenario_table<-read.csv(input$ba_file$datapath,stringsAsFactors=FALSE,check.names=FALSE)
    cfg$scenario_filename<-input$ba_file$name
    cfg$table_unit<-if(rule %in% c("fixed","last_entry_followup"))input$ba_csv_unit %||% "months" else "days"
  } else cfg$axes<-list(n=parse_unit_numbers(input$ba_n_values),hr=parse_unit_numbers(if(batch_is_ph(cfg))input$ba_hr_values else input$ba_effect_scales),time_scale=parse_unit_numbers(input$ba_time_scales),enroll_scale=parse_unit_numbers(input$ba_enroll_scales),dropout_scale=parse_unit_numbers(input$ba_dropout_scales),cut_value=parse_unit_numbers(input$ba_cut_values))
  if(from=="grid"&&!batch_is_ph(cfg))cfg$axes$profile<-names(cfg$profiles)
  validate_batch_config(cfg);sc<-batch_scenarios(cfg)
  if(length(cfg$reference_scenario)!=1||!is.finite(cfg$reference_scenario)||!cfg$reference_scenario %in% sc$SCENARIO)stop("参照情景ID不在本次情景表中。")
  cfg
}
batch_start_job <- function(cfg,root=getwd(),initial_state=NULL,run_store=NULL,operation="new") {
  validate_batch_config(cfg)
  if(!is.null(initial_state))batch_validate_continuation(initial_state,cfg,operation)
  if(!is.null(run_store))batch_acquire_run(run_store$path)
  started<-FALSE;dir<-NULL
  on.exit({if(!started){if(!is.null(run_store))unlink(file.path(run_store$path,"lease"),recursive=TRUE);if(!is.null(dir))unlink(dir,recursive=TRUE)}},add=TRUE)
  dir<-tempfile("event-pred-batch-");dir.create(dir);saveRDS(cfg,file.path(dir,"config.rds"))
  saveRDS(list(store=run_store,initial=initial_state,operation=operation),file.path(dir,"job.rds"))
  initial<-batch_initial_snapshot(cfg,initial_state)
  if(!is.null(run_store))initial<-batch_save_checkpoint(initial,run_store$path)
  saveRDS(initial,file.path(dir,"progress.rds"))
  p<-tryCatch(processx::process$new(file.path(R.home("bin"),"Rscript"),c(file.path(root,"scripts/batch_worker.R"),file.path(dir,"config.rds"),dir),wd=root,
    stdout=file.path(dir,"stdout.log"),stderr=file.path(dir,"stderr.log"),supervise=TRUE,cleanup=TRUE,cleanup_tree=TRUE),error=function(e){
      if(!is.null(run_store)){initial$status<-"failed";initial$worker_error<-conditionMessage(e);batch_save_checkpoint(initial,run_store$path);unlink(file.path(run_store$path,"lease"),recursive=TRUE)}
      unlink(dir,recursive=TRUE);stop(e)})
  started<-TRUE
  list(process=p,dir=dir,config=cfg,store=run_store,initial=initial)
}

batch_read_job <- function(job) {
  read<-function(name){p<-file.path(job$dir,name);if(file.exists(p))tryCatch(readRDS(p),error=function(e)NULL) else NULL}
  r<-read("result.rds");if(!is.null(r))return(list(result=r,finished=TRUE))
  e<-read("error.rds");s<-read("progress.rds")
  if(!is.null(e))return(list(error=e$message,state=s,finished=TRUE))
  if(!job$process$is_alive())return(list(error="后台结束，未保存完整结果；保留最近部分快照。",state=s,finished=TRUE))
  list(state=s,finished=FALSE)
}
batch_calibration_table <- function(r) {
  f<-time_factor(r$display_unit)
  data.frame(参数=c("Weibull形状k","尺度eta","中位时间m"),数值=c(r$shape,r$eta_day/f,r$median_day/f),单位=c("无量纲",rep(time_label(r$display_unit),2)))
}
register_batch_server <- function(input,output,session) {
  result<-reactiveVal(NULL);calibrated<-reactiveVal(NULL);kind<-reactiveVal("");job<-reactiveVal(NULL);error<-reactiveVal(NULL);cancelled<-reactiveVal(FALSE)
  dirs<-character();unit<-reactive(input$ba_unit %||% "months");previous_unit<-reactiveVal("months")
  observeEvent(input$ba_primary,{
    choices<-batch_method_names[names(batch_method_names)!=input$ba_primary]
    old<-isolate(input$ba_extra_methods) %||% character()
    updateCheckboxGroupInput(session,"ba_extra_methods",choices=setNames(names(choices),unname(choices)),selected=intersect(old,names(choices)))
  })
  observeEvent(input$ba_gs_beta,{if(is.numeric(input$ba_gs_beta)&&length(input$ba_gs_beta)==1&&is.finite(input$ba_gs_beta)){freezeReactiveValue(input,"ba_gs_power");updateNumericInput(session,"ba_gs_power",value=1-input$ba_gs_beta)}},ignoreInit=TRUE)
  observeEvent(input$ba_gs_power,{if(is.numeric(input$ba_gs_power)&&length(input$ba_gs_power)==1&&is.finite(input$ba_gs_power)){freezeReactiveValue(input,"ba_gs_beta");updateNumericInput(session,"ba_gs_beta",value=1-input$ba_gs_power)}},ignoreInit=TRUE)
  register_parameter_controls(input,output,session,unit,c("b_","bc_","bt_"),simulation_defaults,simulation_parameter_model)
  observeEvent(input$ba_unit,{
    to<-unit();from<-previous_unit();if(identical(to,from))return();ratio<-time_factor(from)/time_factor(to);u<-time_label(to)
    specs<-list(duration=c("ba_cal_t1","ba_cal_t2","ba_max","ba_minimum","ba_tau"),duration_text=c("ba_enroll_cuts"),rate=c("ba_enroll_rate"),rate_text=c("ba_enroll_rates"))
    if(input$ba_design_mode!="sequential"&&input$ba_cut_rule %in% c("fixed","last_entry_followup"))specs$duration_text<-c(specs$duration_text,"ba_cut_values")
    labels<-c(ba_tau=paste0("预定随访时点 tau（",u,"）"),ba_cal_t1=paste0("第一随访时点（",u,"）"),ba_cal_t2=paste0("第二随访时点（",u,"）"),ba_max=paste0("最大研究窗口（",u,"）"),ba_minimum=paste0("最短研究持续时间（",u,"）"),ba_enroll_cuts=paste0("入组切点（研究",u,"数）"),ba_enroll_rate=paste0("基准入组率（人/",u,"）"),ba_enroll_rates=paste0("基准各段入组率（人/",u,"）"),ba_cut_values=paste0("DCO（",u,"）/ D* / 追加随访（",u,"）"))
    profile_text<-isolate(input$ba_profiles)
    converted<-tryCatch(batch_profile_text(batch_parse_profiles(profile_text,from),to),error=function(e)NULL)
    if(!is.null(converted)){freezeReactiveValue(input,"ba_profiles");updateTextAreaInput(session,"ba_profiles",label=parameter_label("ba_profiles",paste0("分段HR定义：名称 | 切点（",u,"） | HR列表")),value=converted)}
    else if(input$ba_mode=="fixed_truth"&&input$ba_effect_mode=="nph")showNotification("HR定义格式无效，无法换算，请按当前单位重新填写。",type="error")
    update<-function(id,kind,label){val<-isolate(input[[id]]);if(is.null(val))return();value<-tryCatch(switch(kind,duration=val*ratio,rate=val/ratio,log_time=val+log(ratio),duration_text=paste(format(parse_unit_numbers(val)*ratio,digits=16,trim=TRUE),collapse=","),rate_text=paste(format(parse_unit_numbers(val)/ratio,digits=16,trim=TRUE),collapse=",")),error=function(e)val);freezeReactiveValue(input,id);if(kind %in% c("duration_text","rate_text"))updateTextInput(session,id,label=parameter_label(id,label),value=value) else updateNumericInput(session,id,label=parameter_label(id,label),value=value)}
    for(k in names(specs))for(id in specs[[k]])update(id,k,labels[[id]])
    for(pre in c("b_","bc_","bt_"))for(k in names(base_unit_fields))for(field in intersect(base_unit_fields[[k]],names(simulation_defaults(from)))) {
      label<-switch(field,median=paste0("中位时间（",u,"）"),median2=paste0("第二成分中位时间（",u,"）"),eta=paste0("Weibull尺度eta（",u,"）"),log_mu=paste0("对数时间位置（",u,"）"),exp_rate=paste0("指数率（每",u,"）"),g_rate=paste0("Gompertz初始率（每",u,"）"),g_shape=paste0("Gompertz形状（每",u,"）"),parameter_cuts=paste0("风险年龄切点（",u,"）"),parameter_rates=paste0("各段风险率（每",u,"）"),dropout_rate=paste0("独立退出率（每",u,"）"),drop_period=paste0("退出概率窗口（",u,"）"),NULL)
      update(paste0(pre,field),k,label)
    }
    for(pre in c("b_","bc_","bt_"))for(field in c("survival_time","pwe_last_rate"))update(paste0(pre,field),if(field=="survival_time")"duration" else "rate",paste0(if(field=="survival_time")"随访时点（" else "末段风险率（每",u,"）"))
    previous_unit(to)
  },ignoreInit=TRUE)
  activate<-function(r){
    result(r);kind(if(ma_is(r$config))"multiarm" else if(ds_is(r$config))"design_search" else if(fp_is(r$config))"flexible_prediction" else if(ob_is(r$config))"observation" else if(ms_is(r$config))"multistate" else if(je_is(r$config))"joint_sequential" else if(sp_is(r$config))"shared_adaptation" else if(cb_is(r$config))"conditional_batch" else if(hg_is(r$config))"heterogeneity" else if(abr_is(r$config))"adaptive_batch" else if(jr_is(r$config))"joint_research" else "batch");error(NULL)
    if(ma_is(r$config)){updateSelectInput(session,"task",selected="multiarm");nav_select("ma_tabs","ma_results",session=session)} else if(ds_is(r$config)){updateSelectInput(session,"task",selected="design_search");nav_select("ds_tabs","ds_results",session=session)} else if(fp_is(r$config)){updateSelectInput(session,"task",selected="flexible_prediction");nav_select("fp_tabs","fp_results",session=session)} else if(ob_is(r$config)){updateSelectInput(session,"task",selected="observation");nav_select("ob_tabs","ob_results",session=session)} else if(ms_is(r$config)){updateSelectInput(session,"task",selected="multistate");nav_select("ms_tabs","ms_results",session=session)} else if(je_is(r$config)){updateSelectInput(session,"task",selected="joint_sequential");nav_select("je_tabs","je_results",session=session)} else if(sp_is(r$config)){updateSelectInput(session,"task",selected="shared_adaptation");nav_select("sp_tabs","sp_results",session=session)} else if(cb_is(r$config)){updateSelectInput(session,"task",selected="conditional_batch");nav_select("cb_tabs","cb_results",session=session)} else if(hg_is(r$config)){updateSelectInput(session,"task",selected="heterogeneity");nav_select("hg_tabs","hg_results",session=session)} else if(abr_is(r$config)){updateSelectInput(session,"task",selected="adaptive_batch");nav_select("ab_tabs","ab_results",session=session)} else if(jr_is(r$config)){updateSelectInput(session,"task",selected="joint_research");nav_select("jr_tabs","jr_results",session=session)} else {updateSelectInput(session,"task",selected="batch");nav_select("ba_tabs","ba_results",session=session)}
  }
  start_saved<-function(cfg,initial=NULL,store=NULL,operation="new") {
    if(!is.null(job()))stop("后台任务尚未结束，请先取消或等待完成。")
    j<-batch_start_job(cfg,initial_state=initial,run_store=store,operation=operation);dirs<<-c(dirs,j$dir)
    job(j);cancelled(FALSE);activate(j$initial)
  }
  workspace<-register_batch_workspace_server(input,output,session,result,job,error,kind,start_saved,activate)
  observeEvent(input$ba_run,{
    if(!is.null(job())){error("后台任务尚未结束，请先取消或等待完成。");return()}
    error(NULL)
    tryCatch({
      if((input$ba_run_source %||% "ui")!="config_json"&&input$ba_mode=="calibration") {
        r<-calibrate_weibull_survival(c(input$ba_cal_t1,input$ba_cal_t2),c(input$ba_cal_s1,input$ba_cal_s2),unit());r$version<-batch_version;r$created_at<-format(Sys.time(),tz="UTC",usetz=TRUE)
        calibrated(r);kind("calibration");nav_select("ba_tabs","ba_results",session=session)
      } else {
        cfg<-if((input$ba_run_source %||% "ui")=="config_json")workspace$imported() else batch_prepare_sequential(batch_config_input(input,unit()))
        if(is.null(cfg))stop("请先选择有效的无损配置JSON。")
        store<-batch_create_run(cfg,input$ba_workspace_token,input$ba_run_name)
        start_saved(cfg,store=store);workspace$refresh()
      }
    },error=function(e){error(conditionMessage(e));nav_select("ba_tabs","ba_results",session=session)})
  },ignoreInit=TRUE)
  observeEvent(input$ba_cancel,{j<-job();if(is.null(j))return();file.create(file.path(j$dir,"cancel.flag"));cancelled(TRUE)},ignoreInit=TRUE)
  observe({j<-job();if(is.null(j))return();invalidateLater(500,session);s<-batch_read_job(j)
    if(!is.null(s$state))result(s$state)
    if(isTRUE(s$finished)) {
      if(!is.null(s$result))result(s$result)
      if(!is.null(s$error)){error(s$error);r<-isolate(result());if(!is.null(r)){r$status<-"failed";r$worker_error<-s$error;result(r)}}
      if(!is.null(j$store)&&!j$process$is_alive()){
        r<-isolate(result());if(!is.null(r))try(batch_save_checkpoint(r,j$store$path),silent=TRUE)
        unlink(file.path(j$store$path,"lease"),recursive=TRUE)
      }
      job(NULL);cancelled(FALSE);workspace$refresh()
    }
  })
  session$onSessionEnded(function(){j<-isolate(job());if(!is.null(j)){
    file.create(file.path(j$dir,"cancel.flag"));if(j$process$is_alive())j$process$kill()
    if(!is.null(j$store))tryCatch({s<-batch_read_job(j);r<-s$result %||% s$state %||% j$initial
      if(r$status=="running")r$status<-"interrupted"
      batch_save_checkpoint(r,j$store$path);unlink(file.path(j$store$path,"lease"),recursive=TRUE)},error=function(e)NULL)
  };for(d in dirs)unlink(d,recursive=TRUE)})

  output$ba_result_kind<-renderText(kind());outputOptions(output,"ba_result_kind",suspendWhenHidden=FALSE)
  output$ba_methods_saved<-renderText({r<-result();if(!is.null(r)&&(batch_is_sequential(r$config)||isTRUE(r$config$compare_methods)||r$config$primary %in% c("rmst","survival")))"yes" else "no"});outputOptions(output,"ba_methods_saved",suspendWhenHidden=FALSE)
  output$ba_sequential_saved<-renderText({r<-result();if(!is.null(r)&&batch_is_sequential(r$config))"yes" else "no"});outputOptions(output,"ba_sequential_saved",suspendWhenHidden=FALSE)
  output$ba_nonbinding_z_saved<-renderText({r<-result();if(!is.null(r)&&batch_is_sequential(r$config)&&r$config$gs_futility=="z")"yes" else "no"});outputOptions(output,"ba_nonbinding_z_saved",suspendWhenHidden=FALSE)
  output$ba_status<-renderUI({e<-error();r<-result();c<-calibrated();tagList(if(!is.null(e))p(class="field-note",paste0("本次未生成完整新结果：",e,"；下载对应下方保存快照。")),if(kind()=="calibration"&&!is.null(c))p(class="field-note",paste0("已保存两时点校准 · v",c$version,"待复核；单位：",time_label(c$display_unit),"。指定概率校准，不提供参数估计区间。")) else if(!is.null(r))p(class="field-note",paste0(r$run_title %||% "研究", " · ",r$run_id %||% "", " · v",r$version,"待复核；模式",r$config$mode,"；主分析",batch_method_label(r$config$primary,r$config),"；效应",r$config$effect_mode %||% "ph","；处理",r$completed,"/",r$total,"轮；状态",r$status,if(cancelled())"，已请求取消" else "","。有效轮次概率与全部请求上下界分别报告；情景独立生成。")) else p(class="field-note","运行后保存该需求的配置和结果。"))})
  table<-function(d){if(is.null(d)||!ncol(d))d<-data.frame(记录=character());DT::datatable(d,rownames=FALSE,options=list(scrollX=TRUE,pageLength=8,dom="tip",language=list(emptyTable="尚无记录",info="_START_–_END_ / _TOTAL_",infoEmpty="无记录")))}
  output$ba_calibrated<-renderDT({r<-calibrated();req(r,kind()=="calibration");table(batch_calibration_table(r))})
  output$ba_calibration_targets<-renderDT({r<-calibrated();req(r,kind()=="calibration");d<-r$targets;d$time_day<-d$time_day/time_factor(r$display_unit);names(d)<-c(paste0("目标时点（",time_label(r$display_unit),"）"),"目标生存率");table(d)})
  display<-function(d,r) {
    if(is.null(d)||!ncol(d))return(table(data.frame()))
    labels<-c(stage_nominal_p="停止阶段名义p（非调整p）",gs_stop_reason="停止原因",gs_stop_look="停止分析次序",gs_analysis_count="已执行分析数",gs_stop_planned_fraction="停止原信息比例",resource_n="资源有效轮次",mean_looks="平均分析次数",mean_observed="平均停止时入组数",known_decisions="已知停止决策",stopped="停止轮次",probability_lower_all="全部请求路径概率下界",probability_upper_all="全部请求路径概率上界",probability_valid="已知路径比例",unknown_decisions="未知决策数",p_value="名义p",z="原分析Z",estimate="效应估计",se="标准误",lower="普通95%下限",upper="普通95%上限",true_effect="本轮效应真值",covered="区间覆盖本轮真值",method="方法",method_label="分析方法",is_primary="主分析",primary_method="主方法",comparison_method="比较方法",effect_mode="效应模式",profile="HR定义",drawn_effect_scale="本轮HR/效应幅度",analysis_valid="分析有效",analysis_performed="按预定规则执行分析",paired_valid="配对有效决策",primary_success_paired="配对集主方法成功概率",comparison_success_paired="配对集比较方法成功概率",paired_difference="配对成功概率差",paired_mcse="配对MCSE",paired_difference_lower_all="全部请求配对差下界",paired_difference_upper_all="全部请求配对差上界",truth_n="估计/真值有效数",estimate_n="估计有效数",estimate_unit="估计量单位",mean_estimate="平均估计",mean_true_effect="估计/真值有效集平均真值",bias="偏倚",rmse="RMSE",mean_se="平均标准误",null_rows="已处理满足所用零假设轮次",null_status="零假设状态",benefit_z="正向获益Z",truth_note="真值说明",analysis_note="分析说明",baseline_time_scale="基准时间倍数",effect_scale="HR/效应幅度",SCENARIO="情景ID",label="情景名称",mode="研究模式",n="计划N",configured_hr="PH HR/先验中位HR或NPH幅度",time_scale="时间缩放",enroll_scale="入组倍数",dropout_scale="退出倍数",configured_cut_value="原截点参数",cut_value_engine="引擎截点参数（日或D*）",requested="请求轮次",processed="处理轮次",not_run="未运行",valid="有效决策",invalid_processed="已处理无效",generation_failures="生成/分析失败",successes="成功轮次",success_lower_all="全部请求成功概率下界",success_upper_all="全部请求成功概率上界",success_conditional="有效轮次成功概率",mcse="MCSE",wilson_lower="Wilson95%下限",wilson_upper="Wilson95%上限",rule_reached="完整规则满足比例",mean_drawn_hr="生成成功轮次平均真实HR",reference="参照情景",difference_valid="有效轮次概率差",independent_mcse="独立差值MCSE",difference_lower_all="全部请求差值下界",difference_upper_all="全部请求差值上界",criterion_value="筛选依据值",threshold="预定阈值",meets_threshold="满足阈值",basis="依据",failure_stage="失败阶段",formal_test_performed="执行正式Final",prior_time_scale="先验抽样时间倍数",configured_cut_value="原截点参数",cut_rule="原截点规则")
    if(batch_is_sequential(r$config)){labels["logrank_p"]<-"阶段名义p（非GS调整p）";labels["logrank_reject"]<-"阶段名义拒绝（非主决策）"}
    labels["hr"]<-if(batch_is_ph(r$config))"PH HR/先验中位HR" else "NPH效应幅度"
    day_labels<-c(mean_stop_day="平均停止时间",stop_p05_day="停止时间P05",stop_median_day="停止时间中位数",stop_p95_day="停止时间P95",analysis_tau_day="预定风险年龄tau",DCO_DAY="实际Final DCO",mean_dco_day="平均Final DCO",target_day="事件达标时间",last_entry_day="最后计划患者入组时间",planned_rule_day="完整规则计划时间")
    if("estimate_unit" %in% names(d)) {
      rmst<-d$estimate_unit=="days";f<-time_factor(r$config$display_unit)
      for(k in intersect(names(d),c("estimate","se","lower","upper","true_effect","mean_estimate","mean_true_effect","bias","rmse","mean_se")))d[[k]][rmst]<-d[[k]][rmst]/f
      d$estimate_unit[rmst]<-time_label(r$config$display_unit)
    }
    for(k in names(d)[grepl("_day$|^DCO_DAY$",names(d))]){d[[k]]<-d[[k]]/time_factor(r$config$display_unit);names(d)[names(d)==k]<-paste0(if(k %in% names(day_labels))day_labels[[k]] else k,"（",time_label(r$config$display_unit),"）")}
    for(k in intersect(names(d),names(labels)))names(d)[names(d)==k]<-labels[[k]]
    study_table(d)
  }
  for(pair in list(c("ba_plan","scenarios"),c("ba_overview","overview"),c("ba_comparisons","comparisons"),c("ba_candidates","candidates"),c("ba_method_overview","method_overview"),c("ba_method_pairs","method_pairs"),c("ba_gs_plans","gs_plans"),c("ba_stops","stops"),c("ba_resources","resources")))local({id<-pair[1];key<-pair[2];output[[id]]<-renderDT({r<-result();req(r,kind()=="batch");d<-r[[key]];if(is.null(d)||!ncol(d))return(table(data.frame()));display(d,r)})})
  observe({r<-result();if(is.null(r))return();old<-isolate(input$ba_view_scenario);labs<-setNames(r$scenarios$SCENARIO,paste(r$scenarios$SCENARIO,r$scenarios$label));updateSelectInput(session,"ba_view_scenario",choices=labs,selected=if(length(old)==1&&old %in% as.character(r$scenarios$SCENARIO))old else 1)})
  observe({r<-result();req(r,input$ba_view_scenario);choices<-if(nrow(r$rows))r$rows$SIMID[r$rows$SCENARIO==as.integer(input$ba_view_scenario)&r$rows$generated] else integer();old<-isolate(input$ba_view_trial);updateSelectInput(session,"ba_view_trial",choices=choices,selected=if(length(old)==1&&old %in% as.character(choices))old else head(choices,1))})
  output$ba_rows<-renderDT({r<-result();req(r,input$ba_view_scenario);d<-if(nrow(r$rows))r$rows[r$rows$SCENARIO==as.integer(input$ba_view_scenario),,drop=FALSE] else r$rows;if(!ncol(d))return(table(data.frame()));display(d,r)})
  output$ba_method_rows<-renderDT({r<-result();req(r,input$ba_view_scenario);d<-r$method_rows %||% data.frame();if(nrow(d))d<-d[d$SCENARIO==as.integer(input$ba_view_scenario),,drop=FALSE];display(d,r)})
  selected<-reactive({r<-result();req(r,input$ba_view_scenario,input$ba_view_trial);batch_sample(r,as.integer(input$ba_view_scenario),as.integer(input$ba_view_trial))})
  output$ba_template<-downloadHandler(filename="batch_scenarios_template.csv",content=function(file){
    value<-if(input$ba_design_mode=="sequential"||input$ba_cut_rule %in% c("target","event_minimum"))180 else 24*time_factor("months")/time_factor(input$ba_csv_unit %||% "months")
    nph<-input$ba_design_mode!="sequential"&&input$ba_mode=="fixed_truth"&&input$ba_effect_mode=="nph"
    d<-data.frame(label=c("base","larger_N"),n=c(200,300),hr=if(nph)1 else .7,time_scale=1,enroll_scale=1,dropout_scale=1,cut_value=value)
    if(nph){profiles<-batch_parse_profiles(input$ba_profiles,unit());d$profile<-names(profiles)[1]}
    write.csv(d,file,row.names=FALSE)
  })
  output$ba_calibration_csv<-downloadHandler(filename="weibull_two_point_calibration.csv",content=function(file){r<-calibrated();req(r);write.csv(batch_calibration_table(r),file,row.names=FALSE)})
  output$ba_calibration_json<-downloadHandler(filename="weibull_calibration_draft.json",content=function(file){r<-calibrated();req(r);jsonlite::write_json(r,file,auto_unbox=TRUE,pretty=TRUE,digits=NA)})
  for(key0 in c("scenarios","rows","overview","comparisons","candidates","method_rows","method_overview","method_pairs","gs_plans","looks","stops","resources"))local({key<-key0;id<-if(key=="scenarios")"ba_plan_download" else paste0("ba_",key,"_download");output[[id]]<-downloadHandler(filename=paste0("batch_",key,".csv"),content=function(file){r<-result();req(r);write.csv(r[[key]],file,row.names=FALSE)})})
  output$ba_config_download<-downloadHandler(filename="batch_research_config_draft.json",content=function(file){r<-result();req(r);jsonlite::write_json(batch_config_document(r$config),file,auto_unbox=TRUE,pretty=TRUE,digits=NA,na="null")})
  output$ba_result_download<-downloadHandler(filename="batch_research_snapshot_draft.rds",content=function(file){r<-result();req(r);saveRDS(r,file,version=3)})
  output$ba_script_download<-downloadHandler(filename="replay_batch_research_draft.R",content=function(file){r<-result();req(r);writeLines(batch_reproduction_script(r),file,useBytes=TRUE)})
  output$ba_report_download<-downloadHandler(filename="batch_research_draft.md",content=function(file){r<-result();req(r);writeLines(batch_report(r),file,useBytes=TRUE)},contentType="text/markdown; charset=utf-8")
  for(key0 in c("observed","truth"))local({key<-key0;output[[paste0("ba_",key,"_download")]]<-downloadHandler(filename=paste0("batch_selected_",key,"_days.csv"),content=function(file){s<-selected();write.csv(s[[key]],file,row.names=FALSE)})})
  output$ba_adtte_download<-downloadHandler(filename="batch_selected_adtte.csv",content=function(file){s<-selected();write.csv(simulation_adtte(s$observed,s$study_config),file,row.names=FALSE)})
  cp<-reactiveVal(NULL);cp_error<-reactiveVal(NULL)
  observeEvent(input$ba_run,{cp(NULL);cp_error(NULL)},ignoreInit=TRUE)
  observeEvent(list(input$ba_view_scenario,input$ba_view_trial,result()$run_id),{cp(NULL);cp_error(NULL)},ignoreInit=TRUE)
  output$ba_looks<-renderDT({r<-result();req(r,input$ba_view_scenario,input$ba_view_trial);d<-r$looks %||% data.frame();if(nrow(d))d<-d[d$SCENARIO==as.integer(input$ba_view_scenario)&d$SIMID==as.integer(input$ba_view_trial),,drop=FALSE];display(d,r)})
  observe({r<-result();if(is.null(r)||!batch_is_sequential(r$config))return();d<-r$looks %||% data.frame();choices<-integer();if(nrow(d)&&length(input$ba_view_scenario)==1&&length(input$ba_view_trial)==1)choices<-d$look[d$SCENARIO==as.integer(input$ba_view_scenario)&d$SIMID==as.integer(input$ba_view_trial)&d$valid&d$action=="continue"];old<-isolate(input$ba_cp_look);updateSelectInput(session,"ba_cp_look",choices=choices,selected=if(length(old)==1&&old %in% as.character(choices))old else head(choices,1))})
  observeEvent(input$ba_cp_run,{
    cp_error(NULL)
    tryCatch({r<-result();if(is.null(r)||!batch_is_sequential(r$config)||length(input$ba_cp_look)!=1||!nzchar(input$ba_cp_look))stop("所选轮次暂无有效且继续的IA。")
      respect<-if(r$config$gs_futility=="z")isTRUE(input$ba_cp_respect) else TRUE
      d<-batch_saved_cp(r,as.integer(input$ba_view_scenario),as.integer(input$ba_view_trial),as.integer(input$ba_cp_look),input$ba_cp_hr,respect)
      cp(list(data=d,config=r$config,dependencies=r$dependencies,version=r$version,created_at=format(Sys.time(),tz="UTC",usetz=TRUE),review_status="pending_v0.35_review"))
    },error=function(e)cp_error(conditionMessage(e)))
  },ignoreInit=TRUE)
  output$ba_cp_note<-renderUI({e<-cp_error();a<-cp();tagList(if(!is.null(e))p(class="field-note",e),if(!is.null(a))p(class="field-note",paste0("已保存诊断CP；假定未来HR=",a$data$future_hr,"。canonical PH近似，不改变原模拟路径。下载保持本次诊断的假设与原配置。")))})
  output$ba_cp<-renderDT({a<-cp();req(a);d<-a$data;d$observed_day<-d$observed_day/time_factor(a$config$display_unit);names(d)[names(d)=="observed_day"]<-paste0("已观察IA时间（",time_label(a$config$display_unit),"）");table(d)})
  output$ba_cp_download<-downloadHandler(filename="batch_ia_cp_diagnostic.csv",content=function(file){a<-cp();req(a);write.csv(a$data,file,row.names=FALSE)})
  output$ba_cp_json<-downloadHandler(filename="batch_ia_cp_diagnostic_draft.json",content=function(file){a<-cp();req(a);jsonlite::write_json(a,file,auto_unbox=TRUE,pretty=TRUE,digits=NA,na="null")})
  invisible(list(result=result,job=job,error=error,start_saved=start_saved,workspace=workspace,activate=activate))
}
