# Independently verify handbook examples and newly expanded likelihood derivations.
local({
  html <- e$handbook_html()
  index <- do.call(c,unname(e$handbook_index()))
  check(length(index)==44 && !anyDuplicated(names(index)) && all(vapply(names(index),function(id)grepl(paste0('id="',id,'"'),html,fixed=TRUE),logical(1))), 'handbook index resolves to 44 distinct chapters')
  check(grepl('type="math/tex',html,fixed=TRUE)&&!grepl('HANDBOOKMATHTOKEN',html,fixed=TRUE),'expanded handbook inline math survives Markdown conversion')
  p <- list(median=12,shape=1.2,scale=.8,cure=.3,median2=20,shape2=1.5,mix=.4,rate=.06,rates=c(.03,.06,.09,.06))
  refs <- data.frame(id=c('exponential','weibull','pwe','lognormal','loglogistic','gompertz','cure_weibull','mixture_weibull'),p6=c(.2928932,.3349461,.3812166,.3895030,.2757524,.3943329,.2022300,.2500071),u50=c(12,9.889565,9.552453,8.441369,14.58796,8.036042,23.26479,13.22876))
  for(i in seq_len(nrow(refs))) {
    pp <- p; if(refs$id[i]=='gompertz') pp$shape <- .03
    m <- parameter_model(refs$id[i],pp,c(3,6,12))
    check(abs(1-model_survival(m,14)/model_survival(m,8)-refs$p6[i])<1e-6 && abs(sample_conditional(m,8,u=.5)-8-refs$u50[i])<1e-5,paste(refs$id[i],'handbook probability and residual median example'))
  }
  t <- c(2,8,20); mu <- log(12); sigma <- .8; z <- (log(t)-mu)/sigma
  check(near(sum(dnorm(z)/(sigma*t)),sum(dlnorm(t,mu,sigma)),1e-12),'handbook Log-normal Jacobian density matches built-in reference')
  k <- 1/sigma; eta <- exp(mu); w <- (log(t)-mu)/sigma
  check(near(sum(exp(w)/(sigma*t*(1+exp(w))^2)),sum((k/eta)*(t/eta)^(k-1)/(1+(t/eta)^k)^2),1e-12),'handbook Log-logistic density agrees in both parameterizations')
  y <- c(2,8,20,28); de <- c(1,0,1,0)
  ll <- function(th)sum(ifelse(de==1,dlnorm(y,th[1],exp(th[2]),log=TRUE),plnorm(y,th[1],exp(th[2]),lower.tail=FALSE,log.p=TRUE)))
  z <- (log(y)-mu)/sigma; rho <- dnorm(z)/pnorm(z,lower.tail=FALSE)
  score <- c(sum(de*z+(1-de)*rho)/sigma,sum(de*(z*z-1)+(1-de)*z*rho))
  th <- c(mu,log(sigma)); differences <- vapply(1:2,function(j){up <- down <- th;up[j] <- up[j]+1e-6;down[j] <- down[j]-1e-6;(ll(up)-ll(down))/2e-6},numeric(1))
  check(near(differences,score,1e-6),'handbook censored Log-normal scores match independent density differences')
  k <- 1.2; d <- sum(de)
  prof <- function(kk){eta <- (sum(y^kk)/d)^(1/kk);sum(ifelse(de==1,dweibull(y,kk,eta,log=TRUE),pweibull(y,kk,eta,lower.tail=FALSE,log.p=TRUE)))}
  check(near((prof(k+1e-6)-prof(k-1e-6))/2e-6,d/k+sum(de*log(y))-d*sum(y^k*log(y))/sum(y^k),1e-6),'handbook Weibull profile score matches independent likelihood differences')
  check(abs(log(12/log(2)^(1/1.2))-2.790335)<1e-6 && abs(1/(1+exp(-2))-.8807971)<1e-6 && abs(.6/(1+exp(-2))+.3/(1+exp(2))-.5642391)<1e-6,'handbook prior-scale and AIC weighted probability examples')
  ui <- as.character(e$handbook_ui())
  check(grepl('工作手册章节',ui,fixed=TRUE)&&grepl('handbook_download',ui,fixed=TRUE)&&grepl('window.print()',ui,fixed=TRUE),'handbook exposes chapter navigation, source download and browser print')
})
