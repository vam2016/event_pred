study_input_config <- function(input,unit="months") {
  f<-time_factor(unit);goal<-input$st_goal %||% "power";grid<-goal=="grid"
  values<-function(pre){d<-simulation_defaults(unit);setNames(lapply(names(d),function(id)input[[paste0(pre,id)]] %||% d[[id]]),names(d))}
  design<-input$st_design_mode %||% "fixed";seq_design<-identical(design,"sequential");primary<-if(seq_design)"logrank" else input$st_primary
  v<-values("p_");effect<-if(goal=="type1"||seq_design)"ph" else input$st_effect %||% "ph"
  hr<-if(effect!="ph")numeric() else if(goal=="type1")1 else if(grid)parse_unit_numbers(input$st_hr_grid) else input$st_hr
  if(effect=="ph"&&goal!="type1"&&isTRUE(input$st_h0))hr<-unique(c(hr,1))
  profile<-switch(effect,ph=list(cuts=numeric(),hr=numeric()),delayed=list(cuts=input$st_delay*f,hr=c(1,input$st_late_hr)),crossing=list(cuts=input$st_change*f,hr=c(input$st_early_hr,input$st_cross_hr)),piecewise=list(cuts=parse_unit_numbers(input$st_hr_cuts)*f,hr=parse_unit_numbers(input$st_hr_profile)))
  if(is.null(profile))stop("未知效应形式。")
  if(effect=="crossing"&&(length(profile$hr)!=2||prod(profile$hr-1)>=0))stop("交叉风险情景需前后HR分别在1的两侧。")
  if(effect=="delayed"&&(length(profile$hr)!=2||!is.finite(profile$hr[2])||profile$hr[2]>=1))stop("延迟获益情景的后段HR需小于1。")
  mode<-input$st_enroll_mode;cut<-if(seq_design)"target" else input$st_cut_mode;precision<-input$st_precision
  if(identical(precision,"mcse")&&(length(input$st_epsilon)!=1||!is.finite(input$st_epsilon)||input$st_epsilon<.005||input$st_epsilon>.1))stop("epsilon需在0.005–0.1。")
  weights<-if(primary=="fh")switch(input$st_weight_preset %||% "custom",`00`=c(0,0),`10`=c(1,0),`01`=c(0,1),`11`=c(1,1),custom=c(input$st_fh_rho,input$st_fh_gamma)) else NULL
  cfg<-list(goal=goal,effect_mode=effect,hr_cuts=profile$cuts,hr_profile=profile$hr,include_h0=goal!="type1"&&isTRUE(input$st_h0),n_values=if(grid)parse_unit_numbers(input$st_n_grid) else input$st_n,hr_values=hr,
    treatment_fraction=input$st_allocation,control_model=simulation_parameter_model(v$design_model,v,unit),control_entered=v,
    dropout_rates=c(dropout_input_rate(values("pc_")),dropout_input_rate(values("pt_")))/f,
    dropout_entered=list(control=values("pc_")[c("drop_input","dropout_rate","drop_prob","drop_period")],treatment=values("pt_")[c("drop_input","dropout_rate","drop_prob","drop_period")]),
    display_unit=unit,engine_unit="days",origin=as.character(input$st_origin),paramcd=input$st_endpoint,
    enroll_mode=mode,enroll_rate=if(mode=="constant")input$st_enroll_rate/f else NULL,
    enroll_cuts=if(mode=="piecewise")parse_unit_numbers(input$st_enroll_cuts)*f else numeric(),enroll_rates=if(mode=="piecewise")parse_unit_numbers(input$st_enroll_rates)/f else numeric(),entry_days=if(mode=="schedule")parse_unit_numbers(input$st_schedule)*f else numeric(),
    cut_mode=cut,dco_values=if(cut=="fixed")if(grid)parse_unit_numbers(input$st_dco_grid)*f else input$st_dco*f else numeric(),
    target_values=if(cut=="target")if(grid)parse_unit_numbers(input$st_target_grid) else input$st_target else numeric(),
    max_day=if(cut=="target")input$st_max*f else NULL,miss_policy=if(seq_design)"no_reject" else if(cut=="target")input$st_miss else "analyze",
    primary=primary,fh_rho=if(length(weights))weights[1] else NULL,fh_gamma=if(length(weights))weights[2] else NULL,analysis_tau=if(primary %in% c("rmst","survival"))input$st_tau*f else NULL,sided=input$st_sided,alpha=input$st_alpha,estimate_hr=!seq_design&&(primary=="cox"||isTRUE(input$st_estimate)),
    precision_mode=precision,planned_epsilon=if(precision=="mcse")input$st_epsilon else NULL,
    reps=if(precision=="mcse")ceiling(.25/input$st_epsilon^2) else input$st_reps,seed=input$st_seed,reference_power=if(goal!="type1"&&effect=="ph"&&!seq_design&&primary %in% c("logrank","cox"))input$st_power else .8)
  cfg$design_mode<-design
  if(!design %in% c("fixed","sequential"))stop("请选择固定分析或组序贯。")
  if(seq_design) {
    cfg$gs_timing<-parse_unit_numbers(input$st_gs_timing);cfg$gs_spending<-input$st_gs_spending
    cfg$gs_futility<-if(cfg$sided=="benefit")input$st_gs_futility %||% "none" else "none"
    cfg$gs_futility_z<-if(cfg$gs_futility=="z")parse_unit_numbers(input$st_gs_futility_z) else numeric()
    if(gs_is_binding(cfg)) {
      basis<-input$st_gs_beta_input %||% "power"
      if(!basis %in% c("beta","power"))stop("请选择输入设计beta或目标功效。")
      cfg$gs_beta_input<-basis;cfg$gs_beta<-if(basis=="beta")input$st_gs_beta else 1-input$st_gs_power
      cfg$gs_beta_spending<-input$st_gs_beta_spending;cfg$gs_design_hr<-input$st_gs_design_hr
      if(cfg$gs_beta_spending=="bsHSD")cfg$gs_beta_gamma<-input$st_gs_beta_gamma
    }
  }
  if(!goal %in% c("power","type1","grid"))stop("请选择评价需求。")
  if(length(cfg$reference_power)!=1||!is.finite(cfg$reference_power)||cfg$reference_power<.5||cfg$reference_power>=1)stop("解析参照功效需在[0.5,1)。")
  validate_study_config(cfg);gs_prepare(cfg)
}
study_start_job <- function(cfg,root=getwd()) {
  validate_study_config(cfg);cfg<-gs_prepare(cfg);dir<-tempfile("event-pred-study-");dir.create(dir);saveRDS(cfg,file.path(dir,"config.rds"))
  p<-tryCatch(processx::process$new(file.path(R.home("bin"),"Rscript"),c(file.path(root,"scripts/study_worker.R"),file.path(dir,"config.rds"),dir),wd=root,
    stdout=file.path(dir,"stdout.log"),stderr=file.path(dir,"stderr.log"),supervise=TRUE,cleanup=TRUE,cleanup_tree=TRUE),error=function(e){unlink(dir,recursive=TRUE);stop(e)})
  list(process=p,dir=dir,config=cfg,created_at=format(Sys.time(),tz="UTC",usetz=TRUE))
}
study_read_job <- function(job) {
  read<-function(name){path<-file.path(job$dir,name);if(file.exists(path))tryCatch(readRDS(path),error=function(e)NULL) else NULL}
  final<-read("result.rds");if(!is.null(final))return(list(result=final,finished=TRUE))
  err<-read("error.rds");if(!is.null(err))return(list(error=err$message,finished=TRUE))
  state<-read("progress.rds")
  if(!job$process$is_alive())return(list(error="后台进程结束，未保存完整结果。已保存的轮次可导出。",state=state,finished=TRUE))
  list(state=state,finished=FALSE)
}
study_cancel_job <- function(job) {file.create(file.path(job$dir,"cancel.flag"));invisible(NULL)}
study_job_partial <- function(job,state,status="running") {
  if(is.null(state$rows))return(NULL)
  list(config=job$config,scenarios=state$scenario,rows=state$rows,overview=state$overview,completed=state$completed,total=state$total,status=status,looks=state$looks %||% gs_empty_looks(),version="0.30.0",created_at=job$created_at,
    dependencies=study_dependencies(job$config))
}
study_table <- function(d) {
  labels<-c(SCENARIO="情景",n="计划N",hr="真实HR",true_hr="HR / 效应",hypothesis="假设",cut_value="DCO / D*",requested="计划轮次",completed="完成轮次",decision_valid="有效决策",invalid="无效决策",generation_failures="生成失败",rejection_lower="拒绝概率下界",rejection_upper="拒绝概率上界",rejection_conditional="有效决策拒绝率",mcse="MCSE",wilson_lower="Wilson95%下限",wilson_upper="Wilson95%上限",mean_events="平均事件数",mean_dco_day="平均DCO(日)",target_reached="窗口内达标比例",cox_valid="有效Cox估计",beta_bias="logHR偏倚",beta_rmse="logHR RMSE",beta_empirical_sd="logHR经验SD",mean_se_beta="平均SE(logHR)",hr_bias="HR偏倚",coverage="95%区间覆盖率",coverage_mcse="覆盖率MCSE",coverage_lower="覆盖率Wilson下限",coverage_upper="覆盖率Wilson上限",scenario_a="情景A",scenario_b="情景B",paired_completed="完成配对",paired_valid="有效配对",conditional_difference="配对拒绝率差(B−A)",paired_mcse="配对MCSE",approximate_rejection="近似拒绝率",approximate_required_events="近似所需D",events="事件数",effect_label="HR随时间",mean_beta="平均拟合logHR",median_cox_hr="拟合HR中位数",true_effect="真实差值",valid="有效估计",mean_control="Control估计均值",mean_treatment="Treatment估计均值",mean_difference="差值估计均值",bias="差值偏倚",rmse="差值RMSE",empirical_sd="差值经验SD",mean_se="平均差值SE",mean_risk_control="平均Control在险数(tau)",mean_risk_treatment="平均Treatment在险数(tau)")
  for(id in intersect(names(d),names(labels)))names(d)[names(d)==id]<-labels[[id]]
  counts<-intersect(names(d),c("情景","计划N","计划轮次","完成轮次","有效决策","无效决策","生成失败","有效Cox估计","完成配对","有效配对","情景A","情景B","D*","SCENARIO","SIMID","BASEID","replicate_seed","n_observed","n_control","n_treatment","events"))
  for(id in counts)d[[id]]<-as.integer(d[[id]])
  tab<-DT::datatable(d,rownames=FALSE,options=list(scrollX=TRUE,columnDefs=list(list(className="dt-nowrap",targets="_all")),pageLength=6,dom="tip",language=list(emptyTable="尚无记录",info="_START_–_END_ / _TOTAL_",infoEmpty="无记录",paginate=list(previous="上一页",`next`="下一页"))))
  cols<-names(d)[vapply(d,is.double,logical(1))];if(length(cols))DT::formatRound(tab,cols,4) else tab
}
register_study_server <- function(input,output,session) {
  job<-reactiveVal(NULL);result<-reactiveVal(NULL);partial<-reactiveVal(NULL);error<-reactiveVal(NULL);cancelling<-reactiveVal(FALSE);progress<-reactiveVal(NULL)
  dirs<-character();unit<-reactive(input$st_unit %||% "months");previous_unit<-reactiveVal("months")
  register_parameter_controls(input,output,session,unit,c("p_","pc_","pt_"),simulation_defaults,simulation_parameter_model)
  # Conversion operates on mounted controls, preserving text precision and basis values.
  observeEvent(input$st_unit,{
    to<-unit();from<-previous_unit();if(identical(from,to))return();ratio<-time_factor(from)/time_factor(to);u<-time_label(to)
    convert<-function(id,kind,label=NULL) {
      val<-isolate(input[[id]]);if(is.null(val))return()
      value<-tryCatch(switch(kind,duration=val*ratio,rate=val/ratio,log_time=val+log(ratio),duration_text=paste(format(parse_unit_numbers(val)*ratio,digits=16),collapse=","),rate_text=paste(format(parse_unit_numbers(val)/ratio,digits=16),collapse=",")),error=function(e)val)
      freezeReactiveValue(input,id)
      if(kind %in% c("duration_text","rate_text"))updateTextInput(session,id,label=parameter_label(id,label),value=value) else updateNumericInput(session,id,label=parameter_label(id,label),value=value)
    }
    for(id in c("st_dco","st_max","st_delay","st_change","st_tau"))convert(id,"duration",paste0(c(st_dco="研究DCO（",st_max="最大研究窗口（",st_delay="获益开始随访时间（",st_change="HR改变随访时间（",st_tau="预设个体随访tau（")[[id]],u,"）"))
    for(id in c("st_dco_grid","st_enroll_cuts","st_schedule","st_hr_cuts"))convert(id,"duration_text",paste0(c(st_dco_grid="DCO候选值",st_enroll_cuts="入组切点",st_schedule="最大N个入组时间",st_hr_cuts="HR改变随访切点")[[id]],"（",u,"）"))
    convert("st_enroll_rate","rate",paste0("总体入组率（人/",u,"）"));convert("st_enroll_rates","rate_text",paste0("分段入组率（人/",u,"）"))
    for(pre in c("p_","pc_","pt_")) {
      for(kind in names(base_unit_fields))for(field in intersect(base_unit_fields[[kind]],names(simulation_defaults(from)))) {
        label<-switch(field,median=paste0("中位时间 mPFS / mOS（",u,"；治愈模型为未治愈成分）"),median2=paste0("第二成分中位时间（",u,"）"),eta=paste0("Weibull尺度eta（",u,"）"),log_mu=paste0("对数位置mu=log(m/",u,")"),exp_rate=paste0("指数风险率（每",u,"）"),g_rate=paste0("初始风险率b（每",u,"）"),g_shape=paste0("Gompertz形状g（每",u,"）"),parameter_cuts=paste0("风险切点（",u,"）"),parameter_rates=paste0("各段事件风险率（每",u,"）"),dropout_rate=paste0("独立永久脱落风险率（每",u,"）"),drop_period=paste0("脱落概率对应窗口（",u,"）"),NULL)
        convert(paste0(pre,field),kind,label)
      }
      convert(paste0(pre,"survival_time"),"duration",paste0("随访时点（",u,"）"));convert(paste0(pre,"pwe_last_rate"),"rate",paste0("末段风险率（每",u,"）"))
    }
    previous_unit(to)
  },ignoreInit=TRUE)
  cfg<-reactive(study_input_config(input,unit()))
  output$st_truth_plot<-renderPlotly({z<-tryCatch(cfg(),error=function(e)NULL);req(z);sc<-study_scenarios(z);horizon<-if(z$cut_mode=="fixed")max(z$dco_values) else z$max_day;t<-sort(unique(c(seq(0,horizon,length.out=200),z$hr_cuts[z$hr_cuts<=horizon])));keep<-!duplicated(if(study_is_ph(z))sc$hr else sc$profile_id);cells<-sc[keep,,drop=FALSE]
    data<-data.frame(time=t/time_factor(z$display_unit),survival=model_survival(z$control_model,t),curve="Control")
    for(j in seq_len(nrow(cells))){pr<-study_profile(z,cells[j,]);if(!study_is_ph(z)&&all(pr$hr==1))next;data<-rbind(data,data.frame(time=t/time_factor(z$display_unit),survival=exp(-nph_cumhaz(z$control_model,t,pr$cuts,pr$hr)),curve=paste0("Treatment · ",study_effect_label(z,cells[j,]))))}
    plotly::layout(plotly::ggplotly(ggplot(data,aes(time,survival,colour=curve))+geom_line(linewidth=.8)+scale_colour_manual(values=setNames(c("#607685",grDevices::colorRampPalette(c("#126b72","#2f718f","#6c4d94"))(max(0,length(unique(data$curve))-1))),unique(data$curve)))+coord_cartesian(ylim=c(0,1))+labs(x=paste0("真值 · 个体随访时间（",time_label(z$display_unit),"）"),y="S(t)",colour=NULL)+theme_minimal()+theme(legend.position="bottom")),legend=list(orientation="h",x=0,y=-.38,xanchor="left",yanchor="top"),margin=list(b=100))})
  output$st_hr_plot<-renderPlotly({z<-tryCatch(cfg(),error=function(e)NULL);req(z,!study_is_ph(z));horizon<-if(z$cut_mode=="fixed")max(z$dco_values) else z$max_day;pr<-list(cuts=z$hr_cuts,hr=z$hr_profile);last<-max(horizon,max(c(0,pr$cuts))*1.1);tt<-c(0,pr$cuts,last);dd<-data.frame(time=tt/time_factor(z$display_unit),hr=c(pr$hr,tail(pr$hr,1)))
    plotly::ggplotly(ggplot(dd,aes(time,hr))+geom_step(direction="hv",colour="#126b72",linewidth=.9)+geom_hline(yintercept=1,linetype="dashed",colour="#607685")+labs(x=paste0("HR(t) · 个体随访时间（",time_label(z$display_unit),"）"),y="Treatment / Control HR")+theme_minimal())})
  output$st_matrix_note<-renderText({tryCatch({z<-cfg();sprintf("%d情景 × 每情景%d轮 = %d%s。各N/效应情景独立，多个设计目标共享轨迹。",nrow(study_scenarios(z)),z$reps,nrow(study_scenarios(z))*z$reps,if(study_is_sequential(z))"次完整试验" else "次截点分析")},error=function(e)conditionMessage(e))});outputOptions(output,"st_matrix_note",suspendWhenHidden=FALSE)
  output$st_matrix<-renderDT({z<-tryCatch(cfg(),error=function(e)NULL);req(z);d<-study_scenarios(z);if(!study_is_ph(z)){d$profile_id<-NULL;d$hr<-NULL};if(z$primary=="rmst")d$true_effect<-d$true_effect/time_factor(z$display_unit);if("true_effect" %in% names(d))names(d)[names(d)=="true_effect"]<-if(z$primary=="rmst")paste0("真实RMST差（",time_label(z$display_unit),"）") else "真实S(tau)差";d$cut_value<-d$cut_value/if(z$cut_mode=="fixed")time_factor(z$display_unit) else 1;names(d)[names(d)=="cut_value"]<-if(z$cut_mode=="fixed")paste0("DCO（",time_label(z$display_unit),"）") else "D*";study_table(d[,setdiff(names(d),"BASEID"),drop=FALSE])})
  output$st_precision_note<-renderUI({b<-if(identical(input$st_precision,"mcse"))ceiling(.25/(input$st_epsilon %||% .01)^2) else input$st_reps %||% 500;p(class="field-note",paste0("预定每情景B=",b,"；全部决策有效时最坏MCSE≤",signif(sqrt(.25/b),4),"。无效决策另报概率上下界，0次或全次拒绝仍给Wilson区间。"))})
  set_busy<-function(busy)session$sendCustomMessage("studyBusy",list(busy=busy))
  observeEvent(input$st_run,{
    if(!is.null(job()))return()
    error(NULL)
    tryCatch({z<-isolate(cfg());j<-study_start_job(z);dirs<<-c(dirs,j$dir);result(NULL);partial(NULL);progress(list(completed=0,total=nrow(study_scenarios(z))*z$reps));cancelling(FALSE);job(j);set_busy(TRUE)},error=function(e){error(conditionMessage(e));showNotification(conditionMessage(e),type="error")})
  },ignoreInit=TRUE)
  observeEvent(input$st_cancel,{j<-job();if(!is.null(j)){study_cancel_job(j);cancelling(TRUE)}},ignoreInit=TRUE)
  observe({j<-job();if(is.null(j))return();invalidateLater(500,session);s<-study_read_job(j)
    if(!is.null(s$state)){progress(s$state[c("completed","total")]);p<-study_job_partial(j,s$state);if(!is.null(p))partial(p)}
    if(isTRUE(s$finished)) {
      if(!is.null(s$result)){result(s$result);partial(NULL);progress(s$result[c("completed","total")]);nav_select("st_tabs","st_results",session=session)}
      else {error(s$error);p<-partial();if(!is.null(p)){p$status<-"interrupted";result(p);partial(NULL)}}
      # A worker can publish its final RDS just before process exit; give it cleanup on session end.
      job(NULL);cancelling(FALSE);set_busy(FALSE)
    }
  })
  session$onSessionEnded(function(){j<-isolate(job());if(!is.null(j)&&j$process$is_alive())j$process$kill_tree();for(d in dirs)unlink(d,recursive=TRUE)})
  current<-reactive(partial() %||% result())
  output$st_status<-renderUI({j<-job();r<-current();g<-progress();err<-error();tagList(
    if(!is.null(err))p(class="field-note",paste("运行问题：",err)),
    if(!is.null(j))tagList(div(class="study-progress",tags$progress(value=g$completed,max=g$total),span(sprintf("%s · 已保存%d / %d%s",if(cancelling())"正在取消" else "后台运行",g$completed,g$total,if(study_is_sequential(j$config))"次试验" else "次截点分析"))),p(class="field-note","当前运行使用提交时的配置；修改表单不会改变这一批试验。"))
    else if(!is.null(r))p(class="field-note",sprintf("%s · 完成%d / %d%s · 版本%s · %s",if(r$status=="complete")"运行完成" else "未完成运行",r$completed,r$total,if(study_is_sequential(r$config))"次试验" else "次截点分析",r$version,r$created_at)) else p(class="field-note","先核对设计矩阵，再运行。"))})
  output$st_result_note<-renderUI({r<-current();req(r);tagList(p(class="field-note",paste0("主检验：",study_primary_label(r$config),"；",if(r$config$sided=="two")"双侧" else "单侧Treatment获益","；alpha=",r$config$alpha,"。",if(study_is_sequential(r$config))"组序贯拒绝按首次跨越效力边界决定，阶段名义p只作记录。" else "",if(r$status!="complete")"这是已完成轮次的部分结果，不能视为全计划功效评价。" else "")),
    p(class="field-note",paste0(if(r$config$primary %in% c("rmst","survival"))"H0表示预设差值为0，H1表示指定非零差值；不同曲线也可能在tau有相同目标量。" else "H0表示两组生存分布相同，H1为指定效应情景。","有效决策拒绝率及Wilson区间仅以有效轮次为分母；下/上界将无效轮次分别视为不拒绝/拒绝。未达事件目标按预定规则保留；组序贯窗口关闭时不追加检验。")))})
  output$st_has_cox<-renderText({r<-current();if(!is.null(r)&&r$config$estimate_hr)"yes" else "no"});outputOptions(output,"st_has_cox",suspendWhenHidden=FALSE)
  output$st_has_pairs<-renderText({r<-current();if(!is.null(r)&&anyDuplicated(r$scenarios$BASEID)>0)"yes" else "no"});outputOptions(output,"st_has_pairs",suspendWhenHidden=FALSE)
  output$st_show_reference<-renderText({r<-current();if(!is.null(r)&&r$config$goal!="type1"&&!study_is_sequential(r$config)&&study_is_ph(r$config)&&r$config$primary %in% c("logrank","cox"))"yes" else "no"});outputOptions(output,"st_show_reference",suspendWhenHidden=FALSE)
  display_overview<-function(r){d<-r$overview;if(!study_is_ph(r$config))d$true_hr<-d$effect_label;d$cut_value<-d$cut_value/if(r$config$cut_mode=="fixed")time_factor(r$config$display_unit) else 1;names(d)[names(d)=="cut_value"]<-if(r$config$cut_mode=="fixed")paste0("DCO（",time_label(r$config$display_unit),"）") else "D*";d}
  output$st_probabilities<-renderDT({r<-current();req(r,nrow(r$overview)>0);d<-display_overview(r);study_table(d[,c(1:5,7:9,11:16)])})
  output$st_diagnostics<-renderDT({r<-current();req(r,nrow(r$overview)>0);d<-display_overview(r);d$mean_dco_day<-d$mean_dco_day/time_factor(r$config$display_unit);names(d)[names(d)=="mean_dco_day"]<-paste0("平均DCO（",time_label(r$config$display_unit),"）");cols<-c(1:5,6:10,17,18);if(r$config$cut_mode=="target"&&!study_is_sequential(r$config))cols<-c(cols,19);study_table(d[,cols])})
  output$st_failure_reasons<-renderDT({r<-current();req(r);study_table(study_failure_reasons(r))})
  output$st_recovery<-renderDT({r<-current();req(r,nrow(r$overview)>0,r$config$estimate_hr);d<-display_overview(r)
    study_table(if(study_is_ph(r$config))d[,c(1:5,20:29)] else d[,c("SCENARIO","n","effect_label","cox_valid","mean_beta","median_cox_hr","beta_empirical_sd","mean_se_beta")])})
  output$st_contrast_on<-renderText({r<-current();if(!is.null(r)&&r$config$primary %in% c("rmst","survival"))"yes" else "no"});outputOptions(output,"st_contrast_on",suspendWhenHidden=FALSE)
  output$st_contrast_note<-renderUI({r<-current();req(r);p(class="field-note",paste0(study_primary_label(r$config),"；tau=",signif(r$config$analysis_tau/time_factor(r$config$display_unit),5),time_label(r$config$display_unit),"。",if(r$config$primary=="rmst")paste0("时间量单位为",time_label(r$config$display_unit),"；") else "生存率量为概率；","差值为Treatment−Control；超出观察支持且KM未降至0时不计算。偏倚/覆盖仅以有效估计为分母。"))})
  output$st_contrasts<-renderDT({r<-current();req(r);d<-study_contrast_overview(r)
    if(r$config$primary=="rmst")for(id in intersect(names(d),c("true_effect","mean_control","mean_treatment","mean_difference","bias","rmse","empirical_sd","mean_se")))d[[id]]<-d[[id]]/time_factor(r$config$display_unit)
    study_table(d)})
  output$st_cox_note<-renderUI({r<-current();req(r);p(class="field-note",if(study_is_ph(r$config))"PH真值用于log HR偏倚及双侧95%区间覆盖。" else "NPH下Cox HR是该截点风险集的拟合摘要；只显示有效估计分布，不报告单个恒定HR真值的偏倚或覆盖率。")})
  output$st_pairs<-renderDT({r<-current();req(r);study_table(study_paired_comparisons(r))})
  output$st_reference<-renderDT({r<-current();req(r,nrow(r$overview)>0);d<-r$overview;z<-study_ph_reference(d$true_hr,d$mean_events,r$config$treatment_fraction,r$config$alpha,r$config$sided,r$config$reference_power);study_table(cbind(SCENARIO=d$SCENARIO,z))})
  output$st_probability_plot<-renderPlotly({r<-current();req(r,nrow(r$overview)>0);d<-r$overview;d$label<-paste0("情景",d$SCENARIO," · N",d$n,if(study_is_ph(r$config))" · HR" else " · ",if(study_is_ph(r$config))d$true_hr else d$effect_label," · ",if(r$config$cut_mode=="target")"D*" else "DCO",if(r$config$cut_mode=="target")d$cut_value else signif(d$cut_value/time_factor(r$config$display_unit),4))
    p<-ggplot(d,aes(x=reorder(label,SCENARIO),y=rejection_conditional,colour=hypothesis))+geom_point(size=2)+geom_errorbar(aes(ymin=wilson_lower,ymax=wilson_upper),width=.2)+geom_hline(yintercept=r$config$alpha,linetype=2,colour="#607685")+coord_flip(ylim=c(0,1))+labs(x=NULL,y="有效决策拒绝率 / Wilson 95%区间",colour="情景")+theme_minimal(base_size=12)+scale_colour_manual(values=c(H0="#607685",H1="#126b72"));plotly::ggplotly(p)})
  register_sequential_server(input,output,session,current,cfg)
  observe({r<-current();if(is.null(r)||!nrow(r$rows))return();ids<-unique(r$rows$SCENARIO[r$rows$generated]);old<-isolate(input$st_view_scenario);choices<-setNames(ids,paste0("情景",ids));updateSelectInput(session,"st_view_scenario",choices=choices,selected=if(old %in% as.character(ids))old else head(ids,1))})
  observe({r<-current();req(r,input$st_view_scenario);ids<-r$rows$SIMID[r$rows$SCENARIO==as.integer(input$st_view_scenario)&r$rows$generated];old<-isolate(input$st_view_rep);updateSelectInput(session,"st_view_rep",choices=ids,selected=if(old %in% as.character(ids))old else head(ids,1))})
  sample<-reactive({r<-current();req(r,input$st_view_scenario,input$st_view_rep);study_sample(r,as.integer(input$st_view_scenario),as.integer(input$st_view_rep))})
  output$st_km<-renderPlotly({r<-current();s<-sample();d<-s$analysis$curves;d<-d[d$scope=="group",];req(nrow(d));d$time<-d$time_day/time_factor(r$config$display_unit);plotly::ggplotly(ggplot(d,aes(time,survival,colour=group))+geom_step()+coord_cartesian(ylim=c(0,1))+labs(x=paste0("随访时间（",time_label(r$config$display_unit),"）"),y="KM生存率",colour="组别")+theme_minimal()+scale_colour_manual(values=c("#173d50","#126b72")))})
  output$st_sample_data<-renderDT({r<-current();study_table(unit_table(sample()$observed,r$config$display_unit,c("entry_day","time_day","obs_day","DCO_DAY")))})
  output$st_sample_note<-renderUI({r<-current();s<-sample();n<-nrow(simulation_export_issues(simulation_adtte(s$observed,r$config)));p(class="field-note",paste0("检验使用连续时间；ADTTE日期向下取整，AVAL含首日。CNSR=0事件、1行政删失、2永久退出。",n,"条同日起止记录保留；日期精度可能改变并列时间。"))})
  download<-function(id,name,fun)output[[id]]<-downloadHandler(filename=name,content=fun)
  for(key in c("summary","trials","failures","pairs","contrasts","looks","stopping"))local({k<-key;download(paste0("st_",k,"_download"),paste0("study_",k,".csv"),function(file){r<-current();req(r);d<-switch(k,summary=r$overview,contrasts=study_contrast_overview(r),looks=r$looks,stopping=gs_overview(r),trials=r$rows,failures=r$rows[!r$rows$generated|!r$rows$decision_valid|(r$config$estimate_hr&!r$rows$cox_valid),,drop=FALSE],pairs=study_paired_comparisons(r));write.csv(d,file,row.names=FALSE)})})
  download("st_config_download","design_study_config.json",function(file){r<-current();req(r);jsonlite::write_json(list(version=r$version,created_at=r$created_at,status=r$status,completed=r$completed,total=r$total,config=r$config,dependencies=r$dependencies),file,auto_unbox=TRUE,pretty=TRUE,digits=NA)})
  download("st_observed_download","study_observed_continuous.csv",function(file){r<-current();write.csv(unit_export(sample()$observed,r$config$display_unit,c("entry_day","time_day","obs_day","DCO_DAY")),file,row.names=FALSE)})
  download("st_adtte_download","study_adtte.csv",function(file){r<-current();s<-sample();d<-simulation_adtte(s$observed,r$config);d$SCENARIO<-s$observed$SCENARIO;write.csv(d,file,row.names=FALSE)})
  download("st_truth_download","study_sample_truth.csv",function(file){r<-current();d<-sample()$truth;write.csv(unit_export(d,r$config$display_unit,names(d)[grepl("_day$",names(d))]),file,row.names=FALSE)})
  download("st_script_download","reproduce_design_study.R",function(file){r<-current();req(r);writeLines(study_reproduction_script(r),file,useBytes=TRUE)})
  download("st_report_download","design_study_report.md",function(file){r<-current();req(r);writeLines(study_report(r),file,useBytes=TRUE)})
  list(result=result,current=current,job=job)
}
study_reproduction_script <- function(r) {
  # dput preserves vector/list classes; repeat seeds do not depend on requested B.
  keys<-r$rows[,c("SCENARIO","SIMID"),drop=FALSE]
  c('# 在相同版本event_pred根目录执行。保留完整情景顺序，重放已保存最大轮次的前缀。',
    'for(f in c("units","models","forecast","inputs","simulation","nph","sequential","study"))source(paste0("R/",f,".R"))',
    paste0('cfg <- ',paste(capture.output(dput(r$config,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")),
    paste0('saved_keys <- ',paste(capture.output(dput(keys)),collapse="\n")),
    'replay_cfg <- cfg',
    'if(nrow(saved_keys))replay_cfg$reps <- max(20L,max(saved_keys$SIMID))',
    'res <- run_design_study(replay_cfg,should_cancel=function()nrow(saved_keys)==0L)',
    'res$config <- cfg',
    'key <- function(x)paste(x$SCENARIO,x$SIMID,sep="/")',
    'idx <- match(key(saved_keys),key(res$rows)); if(anyNA(idx))stop("Saved replicate keys not reproduced")',
    'res$rows <- res$rows[idx,,drop=FALSE]', 'res$looks <- res$looks[key(res$looks) %in% key(saved_keys),,drop=FALSE]',
    paste0('res$status <- ',encodeString(r$status,quote='"')),
    'res$completed <- nrow(res$rows); res$overview <- study_overview(res$rows,cfg)',
    'write.csv(res$rows,"study_trials.csv",row.names=FALSE)',
    'write.csv(res$overview,"study_summary.csv",row.names=FALSE)', 'write.csv(study_contrast_overview(res),"study_contrasts.csv",row.names=FALSE)','write.csv(res$looks,"study_looks.csv",row.names=FALSE)','write.csv(gs_overview(res),"study_stopping.csv",row.names=FALSE)')
}
study_report <- function(r) {
  o<-r$overview;md<-if(nrow(o))c('|情景|N|效应情景|完成/计划|有效决策|拒绝率|MCSE|无效决策|','|---|---:|---:|---|---:|---:|---:|---:|',sprintf('|%s|%s|%s|%s/%s|%s|%.4f|%.4f|%s|',o$SCENARIO,o$n,if(study_is_ph(r$config))o$true_hr else o$effect_label,o$completed,o$requested,o$decision_valid,o$rejection_conditional,o$mcse,o$invalid)) else '尚无完成轮次。'
  c('# 重复试验与设计评价记录','',paste('版本：',r$version),paste('状态：',r$status),paste(if(study_is_sequential(r$config))'完成/计划试验：' else '完成/计划截点分析：',r$completed,'/',r$total),paste('运行时间：',r$created_at),
    '', '## 生成与决策','',paste0('Control分布：',r$config$control_model$id,'；Treatment按照保存的HR(t)对Control风险积分。简单随机分组、组内独立脱落；每个N/效应情景使用独立重复试验，多个截点共享轨迹。'),
    paste0('主检验：',r$config$primary,'；方向：',r$config$sided,'；alpha=',r$config$alpha,'；未达D*规则：',r$config$miss_policy,'。'),
    '','## 结果','',md,'',if(!study_is_ph(r$config))c('NPH情景不报告恒定HR真值偏倚/覆盖。',capture.output(print(r$scenarios,row.names=FALSE))),if(r$config$primary %in% c('rmst','survival'))c('','## 预设差值估计与恢复',capture.output(print(study_contrast_overview(r),row.names=FALSE))),'',
    '曲线检验H0为两组分布相同；RMST/固定时点H0为预设tau下差值为0。情景按对应真值标记H0/H1；H0评价Ⅰ类错误，其余评价指定备择拒绝率。有效决策拒绝率/Wilson区间以有效轮次为分母；无效轮次分别视为不拒绝/拒绝，得到概率上下界。未达D*保留，Cox估计失败另记录。取消或中断的结果只代表已完成轮次。',
    'PH下Cox恢复及双侧95%覆盖仅在有效估计中计算；NPH只描述拟合Cox HR。RMST/固定时点恢复使用预设目标量。MCSE仅表示重复模拟误差；有效轮次为0时概率未定义，全0/全1仍报告Wilson区间。',
    '','## 拒绝率区间与截点','', '|情景|概率下界|概率上界|Wilson下限|Wilson上限|平均事件|平均DCO(日)|达标比例|', '|---|---:|---:|---:|---:|---:|---:|---:|', if(nrow(o))sprintf('|%s|%.4f|%.4f|%.4f|%.4f|%.2f|%.2f|%.4f|',o$SCENARIO,o$rejection_lower,o$rejection_upper,o$wilson_lower,o$wilson_upper,o$mean_events,o$mean_dco_day,o$target_reached) else '无结果。',
    if(r$config$estimate_hr&&study_is_ph(r$config))c('', '## Cox恢复与覆盖（有效估计条件下）', '', '|情景|有效估计|logHR偏倚|RMSE|经验SD|平均SE|95%覆盖|覆盖MCSE|', '|---|---:|---:|---:|---:|---:|---:|---:|', if(nrow(o))sprintf('|%s|%s|%.4f|%.4f|%.4f|%.4f|%.4f|%.4f|',o$SCENARIO,o$cox_valid,o$beta_bias,o$beta_rmse,o$beta_empirical_sd,o$mean_se_beta,o$coverage,o$coverage_mcse) else '无结果。'),
    if(study_is_sequential(r$config))c('', '## 组序贯规则与停止结果', paste0('PH标准log-rank，正Z表示Treatment获益。',gs_beta_plan_note(r$config),'终末未达下一目标不追加检验。'),capture.output(print(r$config$gs_plans)),capture.output(print(gs_overview(r),row.names=FALSE))),
    '', '## 失败原因计数', '', capture.output(print(study_failure_reasons(r),row.names=FALSE)),
    '', '## 配置快照','', '```json',jsonlite::toJSON(r$config,auto_unbox=TRUE,pretty=TRUE,digits=NA),'```',
    '','## 依赖','',capture.output(print(r$dependencies,row.names=FALSE)),
    '','连续时间用于检验；样例ADTTE日期向下取整、AVAL含首日。同日起止记录保留。每轮结果使用SCENARIO/SIMID及replicate_seed追踪；恢复脚本保留情景顺序、重建所需轮次前缀后选取已保存轮次。')
}

study_failure_reasons <- function(r) {
  out<-list()
  for(field in c("generation_note","logrank_note","cox_note","fh_note","contrast_note")) {
    if(field=="cox_note"&&!r$config$estimate_hr)next
    x<-r$rows[r$rows[[field]]!="",c("SCENARIO",field),drop=FALSE]
    if(!nrow(x))next
    names(x)[2]<-"原因";x$类型<-c(generation_note="生成/处理",logrank_note="log-rank",cox_note="Cox",fh_note="加权log-rank",contrast_note="RMST/固定时点")[[field]]
    out[[length(out)+1]]<-aggregate(list(轮次=rep(1,nrow(x))),x,sum)
  }
  if(length(out))do.call(rbind,out) else data.frame(SCENARIO=integer(),原因=character(),类型=character(),轮次=integer())
}
