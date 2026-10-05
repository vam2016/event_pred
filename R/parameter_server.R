register_parameter_controls <- function(input,output,session,input_unit,prefixes=c("",paste0("g",1:6,"_")),defaults=group_input_defaults,model_builder=parameter_input_model) {
  for(prefix in prefixes) local({
    pre <- prefix; id <- function(x)paste0(pre,x)
    values <- reactive({d <- defaults(input_unit()); setNames(lapply(names(d),function(x)input[[id(x)]] %||% d[[x]]),names(d))})
    for(basis in c("exp_input","weibull_input","log_input","drop_input")) local({
      field <- basis; previous <- reactiveVal(NULL)
      observeEvent(input[[id(field)]],{
        now <- input[[id(field)]]; old <- previous(); previous(now)
        if(is.null(old)||identical(now,old)) return()
        v <- isolate(values()); value <- NULL; target <- NULL
        if(field=="exp_input") {
          rate <- if(old=="survival") -log(v$survival_prob)/v$survival_time else if(old=="rate")v$exp_rate else log(2)/v$median
          if(now=="rate") {target <- "exp_rate"; value <- rate} else if(now=="survival") {target <- "survival_prob"; value <- exp(-rate*v$survival_time)} else {target <- "median"; value <- log(2)/rate}
        } else if(field=="weibull_input") {
          if(now=="eta") {target <- "eta"; value <- v$median/log(2)^(1/v$shape)} else {target <- "median"; value <- v$eta*log(2)^(1/v$shape)}
        } else if(field=="log_input") {
          if(now=="mu") {target <- "log_mu"; value <- log(v$median)} else {target <- "median"; value <- exp(v$log_mu)}
        } else {
          if(now=="probability") {target <- "drop_prob"; value <- -expm1(-v$dropout_rate*v$drop_period)} else {target <- "dropout_rate"; value <- -log1p(-v$drop_prob)/v$drop_period}
        }
        if(length(value)==1 && is.finite(value)) updateNumericInput(session,id(target),value=value)
      },ignoreNULL=TRUE)
    })
    output[[id("parameter_conversion")]] <- renderUI({
      v <- values(); method <- input[[id("design_model")]]; req(method)
      tryCatch({d <- model_equivalents(model_builder(method,v,input_unit()),input_unit());
        # Component and overall quantiles are deliberately distinguished.
        d <- d[!d$parameter %in% c("均值"),,drop=FALSE]
        div(class="conversion-box",span(class="conversion-title","等价参数"),div(class="conversion-values",lapply(seq_len(nrow(d)),function(j)div(span(d$parameter[j]),strong(paste(if(is.infinite(d$value[j]))"∞" else format(signif(d$value[j],5),trim=TRUE),d$unit[j]))))))
      },error=function(e)p(class="field-note",conditionMessage(e)))
    })
    output[[id("drop_conversion")]] <- renderUI({
      v <- values(); tryCatch({mu <- dropout_input_rate(v); dt <- v$drop_period; req(is.finite(mu),mu>=0,is.finite(dt),dt>0)
        div(class="conversion-box",span(class="conversion-title","脱落换算"),p(paste0("mu = ",signif(mu,5)," /",time_label(input_unit()),"；",signif(dt,5),time_label(input_unit()),"内概率 = ",sprintf("%.2f%%",100*(-expm1(-mu*dt))))),p(class="field-note","p = 1 − exp(−mu × 窗口)。对应独立脱落时间，不包含事件的竞争终止。"))
      },error=function(e)p(class="field-note",conditionMessage(e)))
    })
  })
}
