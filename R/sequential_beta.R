# v0.20 development: binding one-sided alpha/beta spending.
# Parameters and code only; numerical and patient-level calibration are deferred.
gs_is_binding <- function(cfg) identical(cfg$gs_futility,"beta")
gs_validate_beta <- function(cfg) {
  scalar<-function(x,lo,hi)is.numeric(x)&&length(x)==1&&!is.na(x)&&is.finite(x)&&x>=lo&&x<=hi
  if(!gs_is_binding(cfg))return(invisible(TRUE))
  if(!identical(cfg$sided,"benefit"))stop("约束性beta消耗当前只用于单侧Treatment获益设计。")
  if(!scalar(cfg$gs_beta,.01,.5))stop("设计beta需在0.01–0.5，对应目标功效0.5–0.99。")
  if(length(cfg$gs_beta_spending)!=1||!cfg$gs_beta_spending %in% c("bsOF","bsP","bsHSD"))stop("请选择OBF型、Pocock型或HSD型beta消耗。")
  if(identical(cfg$gs_beta_spending,"bsHSD")&&!scalar(cfg$gs_beta_gamma,-10,5))stop("HSD beta形状参数gamma需在−10至5。")
  if(!scalar(cfg$gs_design_hr,.01,1)||cfg$gs_design_hr>=1)stop("设计备择HR需在[0.01,1)，用于所需事件数参照。")
  if(!scalar(cfg$treatment_fraction,.01,.99))stop("beta设计需要Treatment分配概率0.01–0.99。")
  invisible(TRUE)
}
# Reconstruct exactly the predeclared probability design, including binding lower bounds.
gs_design_object <- function(cfg,information_rates) {
  args<-list(informationRates=information_rates,alpha=cfg$alpha,sided=if(cfg$sided=="two")2L else 1L,typeOfDesign=cfg$gs_spending,tolerance=1e-10)
  if(gs_is_binding(cfg)) {
    gs_validate_beta(cfg)
    args$beta<-cfg$gs_beta;args$typeBetaSpending<-cfg$gs_beta_spending
    args$bindingFutility<-TRUE;args$directionUpper<-TRUE
    if(cfg$gs_beta_spending=="bsHSD")args$gammaB<-cfg$gs_beta_gamma
  }
  gs_preserve_rng(do.call(rpact::getDesignGroupSequential,args))
}
gs_beta_plan_metadata <- function(cfg,design,target) {
  # rpact prototype H1 tests mean 1; its final information equals squared final drift.
  characteristics<-gs_preserve_rng(rpact::getDesignCharacteristics(design))
  prototype_information<-as.numeric(tail(characteristics$information,1))
  delta<--log(cfg$gs_design_hr);p<-cfg$treatment_fraction
  if(length(prototype_information)!=1||!is.finite(prototype_information)||prototype_information<=0)stop("beta设计未返回有效的最大标准化信息量。")
  beta_spent<-as.numeric(design$betaSpent)
  if(length(beta_spent)!=length(design$criticalValues)||any(!is.finite(beta_spent)))stop("beta设计未返回完整累计beta消耗。")
  list(binding_futility=TRUE,beta_target=cfg$gs_beta,beta_spending=cfg$gs_beta_spending,beta_spent=beta_spent,
    design_hr=cfg$gs_design_hr,standardized_final_drift=sqrt(prototype_information),
    reference_required_information=prototype_information/delta^2,
    reference_required_events=prototype_information/(delta^2*p*(1-p)),
    reference_events_ceiling=ceiling(prototype_information/(delta^2*p*(1-p))),
    implied_design_hr=exp(-sqrt(prototype_information/(target*p*(1-p)))))
}
gs_beta_plan_note <- function(cfg) {
  if(!gs_is_binding(cfg))return("D*为最终事件目标。非约束性Z规则不参与效力界校准；窗口未达下一目标时关闭试验。")
  "约束性beta界与效力界联合生成，跨无效界必须停止。所填D*保留；目标功效对应表中隐含设计HR，设计备择HR用于所需事件数参照。事件数参照不是患者模拟保证。v0.20新增计算待复核。"
}
gs_beta_plan_summary <- function(cfg) {
  if(!gs_is_binding(cfg))return(data.frame())
  do.call(rbind,lapply(names(cfg$gs_plans),function(key) {
    x<-cfg$gs_plans[[key]]
    data.frame(D_FINAL=as.numeric(key),design_hr=x$design_hr[1],target_power=1-x$beta_target[1],
      reference_required_information=x$reference_required_information[1],reference_required_events=x$reference_required_events[1],
      reference_events_ceiling=x$reference_events_ceiling[1],implied_design_hr=x$implied_design_hr[1],
      standardized_final_drift=x$standardized_final_drift[1],binding_futility=TRUE)
  }))
}
