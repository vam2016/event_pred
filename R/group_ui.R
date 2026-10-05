# Group input fields share the pooled parameter definitions and unit conversions.
group_input_defaults <- function(unit="months", index=1) {
  f <- time_factor(unit)
  m <- (if(index==1)12 else 18)*30.4375/f
  list(exp_input="median",exp_rate=log(2)/m,weibull_input="median",eta=m/log(2)^(1/1.2),log_input="median",log_mu=log(m),drop_input="rate",drop_period=30.4375/f,drop_prob=-expm1(-.0075),name=LETTERS[index],active_n=0,known_n=0,age_mode="fixed",duration=8*30.4375/f,
    design_model="weibull",fit_method="",median=(if(index==1)12 else 18)*30.4375/f,shape=1.2,scale=.8,cure=.2,median2=20*30.4375/f,shape2=1.2,mix=.5,g_rate=.06*f/30.4375,g_shape=.03*f/30.4375,
    parameter_cuts=paste(c(3,6,12)*30.4375/f,collapse=","),parameter_rates=paste(c(.03,.06,.09,.06)*f/30.4375,collapse=","),cuts=paste(c(3,6,12)*30.4375/f,collapse=","),tail_rate=.06*f/30.4375,
    future_n=150,enroll_mode="constant",enroll_rate=7.5*f/30.4375,enroll_cuts=paste(c(3,6)*30.4375/f,collapse=","),enroll_rates=paste(c(4.5,9,6)*f/30.4375,collapse=","),dropout_rate=.0075*f/30.4375,multiplier=1,lag=0)
}
group_input_card <- function(index,label,mode,unit) {
  prefix <- paste0("g",index,"_"); id <- function(x)paste0(prefix,x); d <- group_input_defaults(unit,index); u <- time_label(unit)
  cond <- function(js,...) conditionalPanel(gsub("GROUP",paste0("input.",prefix),js,fixed=TRUE),...)
  section(if(mode=="parameters") paste("组",index) else paste("组",index,"·",label),
    if(mode=="parameters") tagList(textInput(id("name"),"组名",d$name),
      fields(numericInput(id("active_n"),"IA 仍随访人数 Nactive",0,min=0,step=1),numericInput(id("known_n"),"已记录事件数 D0",0,min=0,step=1)),
      cond("GROUPactive_n > 0",fields(selectInput(id("age_mode"),"当前随访年龄设定",c("相同"="fixed","均匀"="uniform")),numericInput(id("duration"),paste0("已无事件随访时间 a / 最大值（",u,"）"),d$duration,min=0))),
      event_parameter_fields(prefix,unit,d))
    else tagList(selectInput(id("fit_method"),"本组拟合模型",c("使用公共候选模型"="",choices)),tagList(cond("GROUPfit_method === 'pwe' || (GROUPfit_method === '' && input.methods.indexOf('pwe') >= 0)",textInput(id("cuts"),paste0("PWE 切点（随访",u,"）"),d$cuts)),cond("GROUPfit_method === 'km_tail' || (GROUPfit_method === '' && input.methods.indexOf('km_tail') >= 0)",numericInput(id("tail_rate"),paste0("KM 尾部风险率（每",u,"）"),d$tail_rate,min=.000001)))),
    fields(numericInput(id("future_n"),"计划继续入组人数",150,min=0,max=1000,step=1),cond("GROUPfuture_n > 0",selectInput(id("enroll_mode"),"未来入组率",c("恒定"="constant","分段"="piecewise")))),
    cond("GROUPfuture_n > 0 && GROUPenroll_mode === 'constant'",numericInput(id("enroll_rate"),paste0("入组率 r（人/",u,"）"),d$enroll_rate,min=0)),
    cond("GROUPfuture_n > 0 && GROUPenroll_mode === 'piecewise'",fields(textInput(id("enroll_cuts"),paste0("入组切点（DCO后",u,"）"),d$enroll_cuts),textInput(id("enroll_rates"),paste0("各段入组率（人/",u,"）"),d$enroll_rates))),
    dropout_parameter_fields(prefix,unit,d),
    numericInput(id("multiplier"),"未来风险调整 q",1,min=.01),
    conditionalPanel("input.clock === 'reported'",numericInput(id("lag"),paste0("新增事件上报延迟 L（",u,"）"),0,min=0)))
}
