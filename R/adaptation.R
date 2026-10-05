# Two independent canonical score stages; weights fixed before observing Z1.
adaptive_validate <- function(cfg, simulation=FALSE) {
  finite <- function(x) is.numeric(x) && length(x)==1L && is.finite(x)
  integer <- function(x) finite(x) && x==floor(x)
  for(n in c("d1","d_plan","d_max")) if(!integer(cfg[[n]]) || cfg[[n]]<1 || cfg[[n]]>100000) stop("事件数须为1–100000的整数。")
  if(cfg$d1>=cfg$d_plan || cfg$d_plan>cfg$d_max) stop("需满足 IA事件数 < 原定Final事件数 ≤ Final上限。")
  if(cfg$d_max-cfg$d_plan>2000) stop("本版Final事件上限最多比原计划增加2000。")
  if(!finite(cfg$p) || cfg$p<=0 || cfg$p>=1) stop("Treatment分配比例须在0与1之间。")
  if(!finite(cfg$alpha) || cfg$alpha<.0001 || cfg$alpha>.2) stop("单侧alpha须为0.0001–0.2。")
  if(!finite(cfg$hr_assumed) || cfg$hr_assumed<=0 || cfg$hr_assumed>=1) stop("重估采用的未来HR须在0与1之间。")
  if(!finite(cfg$cp_target) || cfg$cp_target<=.5 || cfg$cp_target>=1) stop("目标CP须大于0.5且小于1。")
  if(!cfg$rule %in% c("bounded","promising")) stop("选择上下限规则或promising-zone规则。")
  if(cfg$rule=="promising" && (!finite(cfg$cp_min) || cfg$cp_min<=0 || cfg$cp_min>=cfg$cp_target)) stop("promising-zone最低CP须大于0且小于目标CP。")
  if(!simulation && (!finite(cfg$z1) || abs(cfg$z1)>12)) stop("IA获益方向Z1须为−12至12。")
  if(simulation) {
    if(!integer(cfg$reps) || cfg$reps<1000 || cfg$reps>200000) stop("重复数须为1000–200000的整数。")
    if(!integer(cfg$seed) || cfg$seed<0 || cfg$seed>.Machine$integer.max) stop("随机种子须为非负整数且不超过2147483647。")
    if(!finite(cfg$hr_true) || cfg$hr_true<=0 || cfg$hr_true>5) stop("模拟真实HR须大于0且不超过5。")
  }
  invisible(cfg)
}
adaptive_plan <- function(cfg) {
  adaptive_validate(cfg,simulation=identical(cfg$purpose,"simulation"))
  list(w1=sqrt(cfg$d1/cfg$d_plan),w2=sqrt(1-cfg$d1/cfg$d_plan),
       critical=qnorm(1-cfg$alpha),d2_min=cfg$d_plan-cfg$d1,d2_max=cfg$d_max-cfg$d1,
       theta=-log(cfg$hr_assumed),allocation=cfg$p*(1-cfg$p))
}
adaptive_cp <- function(z1,d2,cfg) {
  a <- adaptive_plan(cfg)
  pnorm(a$theta*sqrt(d2*a$allocation)-(a$critical-a$w1*z1)/a$w2)
}
adaptive_decision <- function(z1,cfg) {
  a <- adaptive_plan(cfg)
  c2 <- (a$critical-a$w1*z1)/a$w2
  # Analytic solution, then clamp; ceil precision guard preserves exact integers.
  required <- pmax(0,c2+qnorm(cfg$cp_target))^2/(a$theta^2*a$allocation)
  integer_required <- ceiling(required-32*.Machine$double.eps*pmax(1,required))
  d2 <- pmin(a$d2_max,pmax(a$d2_min,integer_required))
  cp_original <- pnorm(a$theta*sqrt(a$d2_min*a$allocation)-c2)
  cp_max <- pnorm(a$theta*sqrt(a$d2_max*a$allocation)-c2)
  too_low <- if(cfg$rule=="promising") cp_max<cfg$cp_min else rep(FALSE,length(z1))
  d2[too_low] <- a$d2_min
  cp_selected <- pnorm(a$theta*sqrt(d2*a$allocation)-c2)
  reason <- ifelse(too_low,"maximum_below_cp_min",ifelse(cp_original>=cfg$cp_target,"original_sufficient",
    ifelse(cp_max<cfg$cp_target,"maximum_target_unattainable","increase_to_target")))
  data.frame(z1=z1,conditional_error=pnorm(c2,lower.tail=FALSE),conditional_critical=c2,
    cp_original=cp_original,cp_max=cp_max,required_d2=integer_required,selected_d2=d2,
    final_events=cfg$d1+d2,cp_selected=cp_selected,target_reached=cp_selected>=cfg$cp_target-1e-12,reason=reason)
}
adaptive_exact <- function(cfg,hr) {
  a <- adaptive_plan(cfg);theta <- -log(hr);mu1 <- theta*sqrt(cfg$d1*a$allocation)
  cp <- function(z) {
    d2 <- adaptive_decision(z,cfg)$selected_d2
    pnorm(theta*sqrt(d2*a$allocation)-(a$critical-a$w1*z)/a$w2)
  }
  # Integrate within every event-count discontinuity; one-dimensional adaptive
  # normal quadrature, finite-tail omission bounded by 2*pnorm(-10).
  transitions <- (a$critical+a$w2*(qnorm(cfg$cp_target)-a$theta*sqrt((a$d2_min:a$d2_max)*a$allocation)))/a$w1
  if(cfg$rule=="promising") transitions<-c(transitions,(a$critical+a$w2*(qnorm(cfg$cp_min)-a$theta*sqrt(a$d2_max*a$allocation)))/a$w1)
  ends <- sort(unique(c(mu1-10,mu1+10,transitions[transitions>mu1-10 & transitions<mu1+10])))
  sum(vapply(seq_len(length(ends)-1L),function(j) integrate(function(z)cp(z)*dnorm(z,mu1),ends[j],ends[j+1L],rel.tol=1e-9)$value,numeric(1)))
}
adaptive_summary <- function(rows,cfg) {
  a<-adaptive_plan(cfg)
  do.call(rbind,lapply(split(rows,rows$true_hr),function(r) {
    do.call(rbind,lapply(c("adaptive","original"),function(design) {
      x<-r[[paste0("reject_",design)]];n<-length(x);p<-mean(x);z<-qnorm(.975)
      center<-(p+z*z/(2*n))/(1+z*z/n);half<-z*sqrt(p*(1-p)/n+z*z/(4*n*n))/(1+z*z/n)
      data.frame(true_hr=r$true_hr[1],hypothesis=if(r$true_hr[1]==1)"H0" else "HR scenario",design=design,
        repetitions=n,rejections=sum(x),rejection_rate=p,mcse=sqrt(p*(1-p)/n),wilson_low=center-half,wilson_high=center+half,
        expected_reference=if(design=="original")pnorm(-log(r$true_hr[1])*sqrt(cfg$d_plan*a$allocation)-a$critical) else if(r$true_hr[1]==1)cfg$alpha else adaptive_exact(cfg,r$true_hr[1]),
        mean_events=if(design=="original")cfg$d_plan else mean(r$final_events),
        increased_fraction=if(design=="original")0 else mean(r$final_events>cfg$d_plan),
        target_unattained_fraction=if(design=="original")NA_real_ else mean(!r$target_reached))
    }))
  }))
}
run_adaptive_simulation <- function(cfg) {
  cfg$purpose<-"simulation";adaptive_validate(cfg,TRUE);a<-adaptive_plan(cfg)
  # Leave the session RNG unchanged; seed belongs to this saved run.
  had<-exists(".Random.seed",.GlobalEnv,inherits=FALSE);if(had)old<-get(".Random.seed",.GlobalEnv)
  on.exit(if(had)assign(".Random.seed",old,.GlobalEnv) else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv),add=TRUE)
  set.seed(cfg$seed)
  scenarios<-unique(c(1,cfg$hr_true))
  rows<-do.call(rbind,lapply(scenarios,function(hr) {
    theta<--log(hr);z1<-rnorm(cfg$reps,theta*sqrt(cfg$d1*a$allocation));noise2<-rnorm(cfg$reps)
    r<-adaptive_decision(z1,cfg)
    r$SIMID<-seq_len(cfg$reps);r$true_hr<-hr
    r$stage2_z<-noise2+theta*sqrt(r$selected_d2*a$allocation)
    r$combination_z<-a$w1*z1+a$w2*r$stage2_z
    r$reject_adaptive<-r$combination_z>=a$critical
    r$original_combination_z<-a$w1*z1+a$w2*(noise2+theta*sqrt(a$d2_min*a$allocation))
    r$reject_original<-r$original_combination_z>=a$critical
    r
  }))
  list(config=cfg,plan=a,trials=rows,summary=adaptive_summary(rows,cfg),version="0.30.0")
}
adaptive_reproduction <- function(result) {
  cfg<-paste(capture.output(dput(result$config,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  paste0('# Run from the event_pred source directory. Canonical independent-stage simulation.\nsource("R/adaptation.R")\ncfg <- ',cfg,
    '\nr <- run_adaptive_simulation(cfg)\nwrite.csv(r$trials,"adaptive_trials.csv",row.names=FALSE)\nwrite.csv(r$summary,"adaptive_summary.csv",row.names=FALSE)\n')
}
