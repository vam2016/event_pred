register_round_two_help <- function() {
 h<-get("parameter_help",envir=parent.frame())
 for(prefix in c("ci_pr_","hi_")){
 h[paste0(prefix,"beta")]<-"原方案beta0.01–0.5，常用0.2。与alpha界联合构建约束性无效界；须按原规则停止，不按当前结果修改。"
 h[paste0(prefix,"beta_spending")]<-"原beta消耗选OBF型、Pocock型或HSD型。填写原方案类型；HSD额外填写gamma。"
 h[paste0(prefix,"beta_gamma")]<-"仅HSD型使用，原gamma−10至5；负值较晚消耗。应沿用原方案数值。"
 h[paste0(prefix,"design_hr")]<-"原设计备择HR在[0.01,1)，用于所需事件数参照；原D*保持，表中另列对应隐含HR，不用此HR替换未来预测模型。"
 }
 h["ci_pr_allocation"]<-"原Treatment随机化概率0.01–0.99，用于事件信息近似和beta设计参照，1:1填0.5。"
 h["ci_pr_futility"]<-"单侧沿用不设、非约束性Z或约束性beta；beta跨无效界必须停止，已停止历史不追加检验。双侧只不设。"
 h["hi_futility"]<-"沿用原不设/Z/beta无效规则；单侧beta已接入。完整原设计及实际连续快照必需，停止后的记录不进入正式推断。"
 assign("parameter_help",h,envir=parent.frame())
}
