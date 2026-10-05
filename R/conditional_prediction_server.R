conditional_prediction_input <- function(input,cfg,labels) {
  cfg$purpose<-if(identical(input$ci_purpose,"history"))"rejection" else input$ci_purpose %||% "survival"
  if(!conditional_is_prediction(cfg))return(cfg)
  cfg$prediction_design<-if(identical(input$ci_purpose,"history"))"sequential" else input$ci_pr_design %||% "fixed"
  cfg$prediction_control<-input$ci_pr_control %||% head(labels,1)
  cfg$prediction_sided<-input$ci_pr_sided %||% "benefit";cfg$prediction_alpha<-input$ci_pr_alpha %||% .025
  cfg$prediction_miss<-if(cfg$prediction_design=="sequential")"no_reject" else input$ci_pr_miss %||% "no_reject"
  if(cfg$prediction_design=="sequential") {
    cfg$prediction_timing<-parse_unit_numbers(input$ci_pr_timing %||% "0.5,1");cfg$prediction_spending<-input$ci_pr_spending %||% "asOF"
    cfg$prediction_futility<-if(cfg$prediction_sided=="benefit")input$ci_pr_futility %||% "none" else "none"
    cfg$prediction_futility_z<-if(cfg$prediction_futility=="z")parse_unit_numbers(input$ci_pr_futility_z) else numeric()
    if(cfg$prediction_futility=="beta"){
      cfg$prediction_allocation<-input$ci_pr_allocation;cfg$prediction_beta<-input$ci_pr_beta
      cfg$prediction_beta_spending<-input$ci_pr_beta_spending;cfg$prediction_beta_gamma<-input$ci_pr_beta_gamma;cfg$prediction_design_hr<-input$ci_pr_design_hr
    }
  }
  cfg
}
register_conditional_prediction_server <- function(input,output,session,res,selected,subsel,labels,source_data) {
  output$ci_result_purpose<-renderText({r<-res();if(is.null(r))"" else r$config$purpose %||% "survival"});outputOptions(output,"ci_result_purpose",suspendWhenHidden=FALSE)
  observe({labs<-labels();old<-isolate(input$ci_pr_control);updateSelectInput(session,"ci_pr_control",choices=labs,selected=if(length(old)==1&&old %in% labs)old else head(labs,1))})
  table<-function(d){labels<-c(scenario_q="统一q",requested="请求轮次",valid="有效决策",invalid="无效轮次",rejection_conditional="有效决策下拒绝概率",mcse="MCSE",wilson_lower="Wilson95%下限",wilson_upper="Wilson95%上限",failure_bound_lower="全部请求概率下界",failure_bound_upper="全部请求概率上界",target_probability="生成成功下达最终D*比例",mean_future_analyses="平均未来检验次数",mean_events="平均停止事件数",stop_reason="停止原因",decision_valid="决策有效",decision_reject="拒绝H0",future_analysis_count="未来检验次数",control="Control组",treatment="Treatment组",nominal_p="阶段名义p",phase="记录来源",lower_z="反方向效力Z",SIMID="模拟序号",stop_look="停止分析次序",analysis_count="累计检验次数",generated="生成成功",note="说明",n="观察人数")
    if("phase" %in% names(d))d$phase<-ifelse(d$phase=="observed_history","已观察历史","条件模拟未来")
    if("stop_reason" %in% names(d))d$stop_reason<-gs_action_label(d$stop_reason)
    if("action" %in% names(d)){d$action[d$action=="not_tested"]<-"单次Final：IA未检验";ok<-d$action %in% c("continue","efficacy_benefit","efficacy_reverse","futility","final_no_reject","window_unreached","invalid");d$action[ok]<-gs_action_label(d$action[ok])}
    if("action" %in% names(d))names(d)[names(d)=="action"]<-"决策"
    for(id in intersect(names(d),names(labels)))names(d)[names(d)==id]<-labels[[id]]
    gs_table(d)
  }
  output$ci_prediction_ia<-renderDT({r<-res();req(r,conditional_is_prediction(r$config));d<-r$ia_decision;names(d)[names(d)=="valid"]<-"IA检验可计算";d$DCO_DAY<-d$DCO_DAY/time_factor(r$config$display_unit);names(d)[names(d)=="DCO_DAY"]<-paste0("IA研究时间（",time_label(r$config$display_unit),"）");table(d)})
  output$ci_prediction_overview<-renderDT({r<-res();req(r,conditional_is_prediction(r$config));d<-conditional_prediction_overview(r);table(d[,c("scenario_q","requested","valid","invalid","rejection_conditional","mcse","wilson_lower","wilson_upper","failure_bound_lower","failure_bound_upper"),drop=FALSE])})
  output$ci_prediction_resources<-renderDT({r<-res();req(r,conditional_is_prediction(r$config));d<-conditional_prediction_overview(r);d$mean_stop_day<-d$mean_stop_day/time_factor(r$config$display_unit);names(d)[names(d)=="mean_stop_day"]<-paste0("平均停止时间（",time_label(r$config$display_unit),"）");d<-d[,setdiff(names(d),c("requested","valid","invalid","rejection_conditional","mcse","wilson_lower","wilson_upper","failure_bound_lower","failure_bound_upper")),drop=FALSE];table(d)})
  output$ci_prediction_note<-renderUI({r<-res();req(r,conditional_is_prediction(r$config));p(class="field-note",paste0("基于原IA历史，",switch(r$config$uncertainty,plugin="给定事件参数",bootstrap="Bootstrap重拟合事件参数",gamma="事件Gamma后验抽样",bayes_weibull="Weibull后验抽样"),"并模拟未来患者。报告有效决策下拒绝概率与全部请求概率界；MCSE/Wilson区间描述模拟误差。它不是从研究起点计算的功效或Ⅰ类错误，也不授权改变原分析设计。",if(r$config$prediction_design=="sequential"&&r$ia_decision$action!="continue")"所选IA已停止，结果为已有决策；未来不再检验或拟合。" else ""))})
  output$ci_prediction_path<-renderDT({s<-selected();req(conditional_is_prediction(s$r$config));d<-s$r$looks[s$r$looks$SIMID==s$b&s$r$looks$scenario_q==s$q,,drop=FALSE];if(!nrow(d))return(table(data.frame(记录="该轮生成失败，未伪造分析路径。")));d$DCO_DAY<-d$DCO_DAY/time_factor(s$r$config$display_unit);names(d)[names(d)=="DCO_DAY"]<-paste0("研究时间（",time_label(s$r$config$display_unit),"）");table(d)})
  output$ci_prediction_trial<-renderDT({s<-selected();req(conditional_is_prediction(s$r$config));d<-s$r$decisions[s$r$decisions$SIMID==s$b&s$r$decisions$scenario_q==s$q,,drop=FALSE];table(d)})
  output$ci_prediction_plan<-renderDT({req(input$ci_purpose %in% c("rejection","history"),input$ci_pr_design=="sequential"||input$ci_purpose=="history");d<-tryCatch({cfg<-conditional_prediction_input(input,list(target=input$ci_target,reps=input$ci_reps),labels());p<-conditional_prediction_plan(cfg);d0<-sum(source_data()$data$event);p$IA事件数<-d0;j<-if(identical(input$ci_purpose,"history"))as.integer(input$ci_history_look) else 1L;p$当前IA目标一致<-p$target_events[j]==d0;p},error=function(e)data.frame(说明=conditionMessage(e)));table(d)})
  for(pair in list(c("ci_prediction_trials_download","decisions"),c("ci_prediction_looks_download","looks"),c("ci_prediction_summary_download","overview")))local({id<-pair[1];key<-pair[2];output[[id]]<-downloadHandler(filename=paste0("conditional_prediction_",key,".csv"),content=function(file){r<-res();req(r,conditional_is_prediction(r$config));write.csv(if(key=="overview")conditional_prediction_overview(r) else r[[key]],file,row.names=FALSE)})})
  observe({req(identical(input$ci_purpose,"history"));x<-tryCatch(raw_history_choices(input),error=function(e)integer());old<-isolate(input$ci_history_look);updateSelectInput(session,"ci_history_look",choices=x,selected=if(length(old)==1&&old %in% as.character(x))old else tail(x,1))})
  raw_history_choices<-function(input) {req(input$ci_file,input$ci_endpoint);ia_history_choices(read_adtte(input$ci_file$datapath,input$ci_file$name),input$ci_endpoint,input$ci_flag %||% "",input$ci_flag_value %||% "Y")}
  history_preview<-reactive(tryCatch({req(input$ci_purpose=="history");z<-source_data();c<-conditional_prediction_input(input,list(target=input$ci_target,reps=1,cut_mode="target"),labels());list(path=conditional_history_path(z$snapshots,c))},error=function(e)list(error=conditionMessage(e))))
  observe({z<-if(identical(input$ci_purpose,"history"))history_preview() else NULL;if(!is.null(z)&&is.null(z$error)&&!is.null(z$path)&&tail(z$path$action,1)!="continue")nav_hide("ci_tabs","settings",session=session) else nav_show("ci_tabs","settings",session=session)})
  output$ci_history_state<-renderText({if(!identical(input$ci_purpose,"history"))return("");z<-history_preview();if(!is.null(z$error)||is.null(z$path))"invalid" else if(tail(z$path$action,1)=="continue")"continue" else "stopped"});outputOptions(output,"ci_history_state",suspendWhenHidden=FALSE)
  output$ci_history_note<-renderUI({z<-history_preview();if(!is.null(z$error))return(p(class="field-note",z$error));p(class="field-note",paste0("截至原分析",nrow(z$path),"的历史快照通过完整性核对；当前决策：",gs_action_label(tail(z$path$action,1)),"。",if(tail(z$path$action,1)!="continue")"已经停止，运行仅返回既有决策，不需填写未来模型、窗口或重复数。" else "未来从下一原计划分析继续，历史记录不重抽样。"))})
  outputOptions(output,"ci_history_note",suspendWhenHidden=FALSE)
  output$ci_history_preview<-renderDT({z<-history_preview();if(!is.null(z$error))return(table(data.frame()));d<-z$path;d$DCO_DAY<-d$DCO_DAY/time_factor(input$ci_unit %||% "months");names(d)[names(d)=="DCO_DAY"]<-paste0("研究时间（",time_label(input$ci_unit %||% "months"),"）");table(d)})
  output$ci_result_has_history<-renderText({r<-res();if(!is.null(r)&&conditional_has_history(r$config))"yes" else "no"});outputOptions(output,"ci_result_has_history",suspendWhenHidden=FALSE)
  output$ci_history_result<-renderDT({r<-res();req(r,conditional_has_history(r$config));d<-r$history_path;d$DCO_DAY<-d$DCO_DAY/time_factor(r$config$display_unit);names(d)[names(d)=="DCO_DAY"]<-paste0("研究时间（",time_label(r$config$display_unit),"）");table(d)})
  output$ci_history_download<-downloadHandler(filename="observed_ia_history.csv",content=function(file){r<-res();req(r,conditional_has_history(r$config));write.csv(r$history_path,file,row.names=FALSE)})
  output$ci_history_template<-downloadHandler(filename="adtte_ia_history_template.csv",content=function(file)write.csv(ia_history_template(),file,row.names=FALSE))

}
