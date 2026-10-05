event_parameter_fields <- function(prefix="",unit="months",d=group_input_defaults(unit,1),simulation=FALSE) {
  id <- function(x)paste0(prefix,x); u <- time_label(unit)
  cond <- function(js,...) conditionalPanel(gsub("GROUP",paste0("input.",prefix),js,fixed=TRUE),...)
  div(class="model-fields",
    selectInput(id("design_model"),"事件时间分布",parameter_choices,"weibull"),
    cond("GROUPdesign_model === 'exponential'",selectInput(id("exp_input"),"指数模型输入参数",c("中位时间 m"="median","风险率 lambda"="rate",if(simulation)c("固定时点生存率"="survival")))),
    cond("['weibull','cure_weibull','mixture_weibull'].includes(GROUPdesign_model)",selectInput(id("weibull_input"),"Weibull 成分输入参数",c("中位时间 m + 形状 k"="median","尺度 eta + 形状 k"="eta"))),
    cond("['lognormal','loglogistic'].includes(GROUPdesign_model)",selectInput(id("log_input"),"对数时间分布输入参数",c("中位时间 m + sigma"="median","对数位置 mu + sigma"="mu"))),
    cond("(GROUPdesign_model === 'exponential' && !['rate','survival'].includes(GROUPexp_input)) || (['weibull','cure_weibull','mixture_weibull'].includes(GROUPdesign_model) && GROUPweibull_input !== 'eta') || (['lognormal','loglogistic'].includes(GROUPdesign_model) && GROUPlog_input !== 'mu')",numericInput(id("median"),paste0("中位时间 mPFS / mOS（",u,"；治愈模型为未治愈成分）"),d$median,min=.01)),
    cond("GROUPdesign_model === 'exponential' && GROUPexp_input === 'rate'",numericInput(id("exp_rate"),paste0("指数风险率 lambda（每",u,"）"),d$exp_rate,min=.000001)),
    cond("['weibull','cure_weibull','mixture_weibull'].includes(GROUPdesign_model) && GROUPweibull_input === 'eta'",numericInput(id("eta"),paste0("Weibull 尺度 eta（",u,"）"),d$eta,min=.01)),
    cond("['weibull','cure_weibull','mixture_weibull'].includes(GROUPdesign_model)",numericInput(id("shape"),"Weibull 形状 k",d$shape,min=.01)),
    cond("['lognormal','loglogistic'].includes(GROUPdesign_model) && GROUPlog_input === 'mu'",numericInput(id("log_mu"),paste0("对数时间位置 mu = log(m/",u,")"),d$log_mu)),
    cond("['lognormal','loglogistic'].includes(GROUPdesign_model)",numericInput(id("scale"),"对数时间尺度 sigma",d$scale,min=.01)),
    cond("GROUPdesign_model === 'gompertz'",fields(numericInput(id("g_rate"),paste0("初始风险率 b（每",u,"）"),d$g_rate,min=.000001),numericInput(id("g_shape"),paste0("Gompertz 形状 g（每",u,"）"),d$g_shape))),
    cond("GROUPdesign_model === 'cure_weibull'",numericInput(id("cure"),"治愈比例 pi",d$cure,min=0,max=.99)),
    cond("GROUPdesign_model === 'mixture_weibull'",fields(numericInput(id("median2"),paste0("第二成分 mPFS / mOS（",u,"）"),d$median2,min=.01),numericInput(id("shape2"),"第二成分形状 k2",d$shape2,min=.01)),numericInput(id("mix"),"第一成分比例",d$mix,min=.01,max=.99)),
    cond("GROUPdesign_model === 'pwe'",fields(textInput(id("parameter_cuts"),paste0("风险切点（",u,"）"),d$parameter_cuts),cond("GROUPpwe_input !== 'survival'",textInput(id("parameter_rates"),paste0("各段事件风险率（每",u,"）"),d$parameter_rates)))),
    if(simulation) tagList(
      cond("GROUPdesign_model === 'exponential' && GROUPexp_input === 'survival'",fields(numericInput(id("survival_time"),paste0("随访时点（",u,"）"),d$survival_time,min=.01),numericInput(id("survival_prob"),"该时点生存率",d$survival_prob,min=.000001,max=.999999))),
      cond("GROUPdesign_model === 'pwe'",selectInput(id("pwe_input"),"PWE输入形式",c("分段风险率"="rates","切点生存率 + 末段风险率"="survival")),
        cond("GROUPpwe_input === 'survival'",fields(textInput(id("pwe_survivals"),"风险切点对应生存率",d$pwe_survivals),numericInput(id("pwe_last_rate"),paste0("末段风险率（每",u,"）"),d$pwe_last_rate,min=0))))),
    uiOutput(id("parameter_conversion")))
}
dropout_parameter_fields <- function(prefix="",unit="months",d=group_input_defaults(unit,1)) {
  id <- function(x)paste0(prefix,x); u <- time_label(unit)
  cond <- function(js,...) conditionalPanel(gsub("GROUP",paste0("input.",prefix),js,fixed=TRUE),...)
  tagList(selectInput(id("drop_input"),"永久脱落参数输入",c("风险率 mu"="rate","指定时间内的脱落概率"="probability")),
    cond("GROUPdrop_input !== 'probability'",numericInput(id("dropout_rate"),paste0("独立永久脱落风险率 mu（每",u,"）"),d$dropout_rate,min=0)),
    cond("GROUPdrop_input === 'probability'",fields(numericInput(id("drop_prob"),"窗口内脱落概率",d$drop_prob,min=0,max=.9999,step=.01),numericInput(id("drop_period"),paste0("脱落概率对应窗口（",u,"）"),d$drop_period,min=.000001))),uiOutput(id("drop_conversion")))
}
