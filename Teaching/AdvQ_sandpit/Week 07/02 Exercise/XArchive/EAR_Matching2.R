rm(list=ls())
setwd("C:/Users/CYCheng/Desktop/Dropbox/Collaboration/Elections and Autocracy/Data")
setwd("~/Dropbox/Collaboration/Elections and Autocracy/Data")
library(Matching); library(ebal); library(rbounds)

load("EAR_final.RData")

#table(dta$dpi_system)
#dta$fh_PR_n = dta$vdem_PR
#dta$fh_CL_n = dta$vdem_CL
#dta$lg_fh_PR_n = dta$lg_vdem_PR
#dta$lg_fh_CL_n = dta$lg_vdem_CL

dta$fh_PR_n = dta$fh_PR
dta$fh_CL_n = dta$fh_CL



##### Outcomes #####
out_pr = c("fh_PR_n", "lg_fh_PR_n")
out_cl = c("fh_CL_n", "lg_fh_CL_n")
out = c(out_pr, out_cl)

##### Treatment #####
dta$pr = ifelse(dta$elecs == "pr", 1, 0)
dta$pm = ifelse(dta$elecs == "pm", 1, 0)
treat = c("leg_elec", "pm", "pr")

##### Observed covariates #####
cov = c("lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac")

##### Subset #####

# Election vs None
dta1 = dta
vars = c(cov, treat, out); dta1 = dta1[,vars]; dta1 = na.omit(dta1)
table(dta1$leg_elec)

dta1$leg_none = ifelse(dta1$leg_elec == 1, 0, 1)

# PR vs None
sel = c("pr", "none")
dta2 = dta[dta$elecs %in% sel,]  
vars = c(cov, treat, out); dta2 = dta2[,vars]; dta2 = na.omit(dta2)
table(dta2$pr, dta2$leg_elec)

# PM vs None
sel = c("pm", "none")
dta3 = dta[dta$elecs %in% sel,]  
vars = c(cov, treat, out); dta3 = dta3[,vars]; dta3 = na.omit(dta3)
table(dta3$pm, dta3$leg_elec)

# PR vs PM
sel = c("pm", "pr")
dta4 = dta[dta$elecs %in% sel,]  
vars = c(cov, treat, out); dta4 = dta4[,vars]; dta4 = na.omit(dta4)
table(dta4$pr, dta4$pm)

##### SOO (Bias correction matching) #####

### Step 1: Pre-test (skip)
step1 = function(treat, ldv, data){
  summary(lm(treat ~ ldv + lg_epr_gdpcapl + lg_grow + lg_ross_net_oil_exports_valuePOP + ross_population + gwf_party + gwf_military + gwf_personal + arc_turn + epr_ethfrac, data=data))
}

step1(dta1$leg_elec, dta1$lg_fh_PR_n, dta1)
step1(dta2$pr, dta2$lg_fh_PR_n, dta2)
step1(dta3$pm, dta3$lg_fh_PR_n, dta3)
step1(dta4$pr, dta4$lg_fh_PR_n, dta4)

step1(dta1$leg_elec, dta1$lg_fh_CL_n, dta1)
step1(dta2$pr, dta2$lg_fh_CL_n, dta2)
step1(dta3$pm, dta3$lg_fh_CL_n, dta3)
step1(dta4$pr, dta4$lg_fh_CL_n, dta4)

### Step 2: Balance test (before, skip)
step2 = function(treat, ldv, ldvn, data){
  vars = c(ldvn, "GPDpc", "growth", "oil", "population", "party", "military", "personal", "leader turnover", "efl")
  mb = MatchBalance(treat ~ ldv + lg_epr_gdpcapl + lg_grow + lg_ross_net_oil_exports_valuePOP + ross_population + gwf_party + gwf_military + gwf_personal + arc_turn + epr_ethfrac, data=data)
  btest = baltest.collect(mb, var.names=vars, after=F)
  round(btest[, c("mean.Tr","mean.Co","T pval","KS pval")], 3)
}

step2(dta1$leg_elec, dta1$lg_fh_PR_n, "lg_fh_PR_n", dta1)
step2(dta2$pr, dta2$lg_fh_PR_n, "lg_fh_PR_n", dta2)
step2(dta3$pm, dta3$lg_fh_PR_n, "lg_fh_PR_n", dta3)
step2(dta4$pr, dta4$lg_fh_PR_n, "lg_fh_PR_n", dta4)

step2(dta1$leg_elec, dta1$lg_fh_CL_n, "lg_fh_CL_n", dta1)
step2(dta2$pr, dta2$lg_fh_CL_n, "lg_fh_CL_n", dta2)
step2(dta3$pm, dta3$lg_fh_CL_n, "lg_fh_CL_n", dta3)
step2(dta4$pr, dta4$lg_fh_CL_n, "lg_fh_CL_n", dta4)

### Step 3: Matching
step3 = function(data, ldvn, treat, outcome, m=1){
  vars = c(ldvn, "lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac", treat, outcome)
  X = data[,vars]
  #exactmatch = c(rep(FALSE, 5), rep(TRUE, 4), FALSE)
  exactmatch = rep(FALSE, 10)
  matchout = Match(Y=X[,12], Tr=X[,11], X=X[,1:10], M=m, exact=exactmatch, estimand="ATT", BiasAdjust=TRUE)
  res = c(matchout$est, matchout$se)
  temp = matchout$mdata
  mdta = cbind(temp$Y, temp$Tr, temp$X)
  colnames(mdta)[1:2] = c("Y", "Tr")
  return(list(out=matchout, res=res, mdta=mdta))
}

res1 = step3(dta1, "lg_fh_PR_n", "leg_elec", "fh_PR_n", 1)
res1.pr = step3(dta2, "lg_fh_PR_n", "pr", "fh_PR_n", 1)
res1.pm = step3(dta3, "lg_fh_PR_n", "pm", "fh_PR_n", 1)
res1.pmr = step3(dta4, "lg_fh_PR_n", "pr", "fh_PR_n", 1)

res2 = step3(dta1, "lg_fh_CL_n", "leg_elec", "fh_CL_n", 1)
res2.pr = step3(dta2, "lg_fh_CL_n", "pr", "fh_CL_n", 1)
res2.pm = step3(dta3, "lg_fh_CL_n", "pm", "fh_CL_n", 1)
res2.pmr = step3(dta4, "lg_fh_CL_n", "pr", "fh_CL_n", 1)

#save(res1.pr, res1.pm, res1.pmr, res2.pr, res2.pm, res2.pmr, file="EAR_final_mediation.RData")

### Step 4: Balance test (after)
step4 = function(matchout, treat, ldv, ldvn, data){
  vars = c(ldvn, "GPDpc", "growth", "oil", "population", "party", "military", "personal", "leader turnover", "efl")
  mb.out = MatchBalance(match.out=matchout, treat ~ ldv + lg_epr_gdpcapl + lg_grow + lg_ross_net_oil_exports_valuePOP + ross_population + gwf_party + gwf_military + gwf_personal + arc_turn + epr_ethfrac, data=data)
  btest_after = baltest.collect(mb.out, var.names=vars, after=TRUE)
  #round(btest_after[,c("mean.Tr","mean.Co","T pval","KS pval")],3)
  res = btest_after
  return(list(res=res))
}

resb1 = step4(res1$out, dta1$leg_elec, dta1$lg_fh_PR_n, "lg_fh_PR_n", dta1)$res
resb1.pr = step4(res1.pr$out, dta2$pr, dta2$lg_fh_PR_n, "lg_fh_PR_n", dta2)$res
resb1.pm = step4(res1.pm$out, dta3$pm, dta3$lg_fh_PR_n, "lg_fh_PR_n", dta3)$res
resb1.pmr = step4(res1.pmr$out, dta4$pr, dta4$lg_fh_PR_n, "lg_fh_PR_n", dta4)$res

resb2 = step4(res2$out, dta1$leg_elec, dta1$lg_fh_CL_n, "lg_fh_CL_n", dta1)$res
resb2.pr = step4(res2.pr$out, dta2$pr, dta2$lg_fh_CL_n, "lg_fh_CL_n", dta2)$res
resb2.pm = step4(res2.pm$out, dta3$pm, dta3$lg_fh_CL_n, "lg_fh_CL_n", dta3)$res
resb2.pmr = step4(res2.pmr$out, dta4$pr, dta4$lg_fh_CL_n, "lg_fh_CL_n", dta4)$res

res1.all = rbind(res1$res, res1.pr$res, res1.pm$res, res1.pmr$res)
res2.all = rbind(res2$res, res2.pr$res, res2.pm$res, res2.pmr$res)

#res1.all[,1] + 1.6* res1.all[,2]
#res2.all[,1] + 1.6* res2.all[,2]

### Step 5: Sensitivity analysis for matched data
sen1 = psens(res1$out, Gamma=2, GammaInc=0.1)$bounds
sen1.pr = psens(res1.pr$out, Gamma=2, GammaInc=0.1)$bounds
sen1.pm = psens(res1.pm$out, Gamma=2, GammaInc=0.1)$bounds
sen1.pmr = psens(res1.pmr$out, Gamma=2, GammaInc=0.1)$bounds

sen2 = psens(res2$out, Gamma=2, GammaInc=0.1)$bounds
sen2.pr = psens(res2.pr$out, Gamma=2, GammaInc=0.1)$bounds
sen2.pm = psens(res2.pm$out, Gamma=2, GammaInc=0.1)$bounds
sen2.pmr = psens(res2.pmr$out, Gamma=2, GammaInc=0.1)$bounds

##### SOO (Propensity Score) #####

### Step 1: Estimate propensity score
step1 = function(treat, ldv, data){
  pi.out = glm(treat ~ ldv + lg_epr_gdpcapl + lg_grow + lg_ross_net_oil_exports_valuePOP + ross_population + gwf_party + gwf_military + gwf_personal + arc_turn + epr_ethfrac, data=data, family=binomial(link="probit"))
  #summary(pi.out$fit)
  plot(density(pi.out$fit[treat==1]), lwd=2, main="Distribution of p-scores")
  lines(density(pi.out$fit[treat==0]), lwd=2, lty=2)
  legend("topleft", legend=c("treated","controls"), lty=c(1,2), lwd=2)
  return(list(res=pi.out$fit))
}

par(mfrow=c(2,4))

ps1 = step1(dta1$leg_elec, dta1$lg_fh_PR_n, dta1)
ps1.pr = step1(dta2$pr, dta2$lg_fh_PR_n, dta2)
ps1.pm = step1(dta3$pm, dta3$lg_fh_PR_n, dta3)
ps1.pmr = step1(dta4$pr, dta4$lg_fh_PR_n, dta4)

ps2 = step1(dta1$leg_elec, dta1$lg_fh_CL_n, dta1)
ps2.pr = step1(dta2$pr, dta2$lg_fh_CL_n, dta2)
ps2.pm = step1(dta3$pm, dta3$lg_fh_CL_n, dta3)
ps2.pmr = step1(dta4$pr, dta4$lg_fh_CL_n, dta4)

par(mfrow=c(1,1))

### Step 2: Matching by propensity score
step2 = function(treat, outcome, ps, m=1){
  matchout.pi = Match(Y=outcome, Tr=treat, X=ps, M=m, exact=FALSE, estimand="ATT", BiasAdjust=TRUE)
  #summary(matchout.pi)
  res = c(matchout.pi$est, matchout.pi$se)
  return(list(res=res, out=matchout.pi))
}

res1.ps = step2(dta1$leg_elec, dta1$fh_PR_n, ps1$res, 1)
res1.ps.pr = step2(dta2$pr, dta2$fh_PR_n, ps1.pr$res, 1)
res1.ps.pm = step2(dta3$pm, dta3$fh_PR_n, ps1.pm$res, 1)
res1.ps.pmr = step2(dta4$pr, dta4$fh_PR_n, ps1.pmr$res, 1)

res2.ps = step2(dta1$leg_elec, dta1$fh_CL_n, ps1$res, 1)
res2.ps.pr = step2(dta2$pr, dta2$fh_CL_n, ps1.pr$res, 1)
res2.ps.pm = step2(dta3$pm, dta3$fh_CL_n, ps1.pm$res, 1)
res2.ps.pmr = step2(dta4$pr, dta4$fh_CL_n, ps1.pmr$res, 1)

### Step 3: Balance test (after)
step3 = function(matchout, treat, ldv, ldvn, data){
  vars = c(ldvn, "GPDpc", "growth", "oil", "population", "party", "military", "personal", "leader turnover", "efl")
  mb.out.pi = MatchBalance(match.out=matchout, treat ~ ldv + lg_epr_gdpcapl + lg_grow + lg_ross_net_oil_exports_valuePOP + ross_population + gwf_party + gwf_military + gwf_personal + arc_turn + epr_ethfrac, data=data)
  btest_after_pi = baltest.collect(mb.out.pi, var.names=vars, after=TRUE)
  #round(btest_after_pi[,c("mean.Tr","mean.Co","T pval","KS pval")],3)
  #res = cbind(btest_after_pi[,1]-btest_after_pi[,2], btest_after_pi[,3])
  res = btest_after_pi
  return(list(res=res))
}

resb1.ps = step3(res1.ps$out, dta1$leg_elec, dta1$lg_fh_PR_n, "lg_fh_PR_n", dta1)$res
resb1.ps.pr = step3(res1.ps.pr$out, dta2$pr, dta2$lg_fh_PR_n, "lg_fh_PR_n", dta2)$res
resb1.ps.pm = step3(res1.ps.pm$out, dta3$pm, dta3$lg_fh_PR_n, "lg_fh_PR_n", dta3)$res
resb1.ps.pmr = step3(res1.ps.pmr$out, dta4$pr, dta4$lg_fh_PR_n, "lg_fh_PR_n", dta4)$res

resb2.ps = step3(res2.ps$out, dta1$leg_elec, dta1$lg_fh_CL_n, "lg_fh_CL_n", dta1)$res
resb2.ps.pr = step3(res2.ps.pr$out, dta2$pr, dta2$lg_fh_CL_n, "lg_fh_CL_n", dta2)$res
resb2.ps.pm = step3(res2.ps.pm$out, dta3$pm, dta3$lg_fh_CL_n, "lg_fh_CL_n", dta3)$res
resb2.ps.pmr = step3(res2.ps.pmr$out, dta4$pr, dta4$lg_fh_CL_n, "lg_fh_CL_n", dta4)$res

res1.ps.all = rbind(res1.ps$res, res1.ps.pr$res, res1.ps.pm$res, res1.ps.pmr$res)
res2.ps.all = rbind(res2.ps$res, res2.ps.pr$res, res2.ps.pm$res, res2.ps.pmr$res)

### Extra: OLS, weighted by PS
#extra = function(treat, outcome, ldv, weight, ps, data){
#  require(texreg)
#  lm.out1 = lm(outcome ~ treat, weight=ps, data=data) 
#  lm.out2 = lm(outcome ~ ldv + treat + lg_epr_gdpcapl + lg_grow + lg_ross_net_oil_exports_valuePOP + ross_population + gwf_party + gwf_military + gwf_personal + arc_turn + epr_ethfrac, weight=ps, data=data)
#  texreg(list(lm.out1, lm.out2))
#}
#extra(dta$election, dta$fh_PR_n, dta$lg_fh_PR_n, ps=ps1$res, data=dta)
#extra(dta2$pm, dta2$fh_PR_n, dta2$lg_fh_PR_n, ps=ps1.pm$res, data=dta2)
#extra(dta3$pr, dta3$fh_PR_n, dta3$lg_fh_PR_n, ps=ps1.pr$res, data=dta3)

#extra(dta$election, dta$fh_CL_n, dta$lg_fh_CL_n, ps=ps2$res, data=dta)
#extra(dta2$pm, dta2$fh_CL_n, dta2$lg_fh_CL_n, ps=ps2.pm$res, data=dta2)
#extra(dta3$pr, dta3$fh_CL_n, dta3$lg_fh_CL_n, ps=ps2.pr$res, data=dta3)

### Step 5: Sensitivity analysis for matched data
sen1.ps = psens(res1.ps$out, Gamma=2, GammaInc=0.1)$bounds
sen1.ps.pr = psens(res1.ps.pr$out, Gamma=2, GammaInc=0.1)$bounds
sen1.ps.pm = psens(res1.ps.pm$out, Gamma=2, GammaInc=0.1)$bounds
sen1.ps.pmr = psens(res1.ps.pmr$out, Gamma=2, GammaInc=0.1)$bounds

sen2.ps = psens(res2.ps$out, Gamma=2, GammaInc=0.1)$bounds
sen2.ps.pr = psens(res2.ps.pr$out, Gamma=2, GammaInc=0.1)$bounds
sen2.ps.pm = psens(res2.ps.pm$out, Gamma=2, GammaInc=0.1)$bounds
sen2.ps.pmr = psens(res2.ps.pmr$out, Gamma=2, GammaInc=0.1)$bounds

##### SOO (Balacing by IPW) #####

### Step 1: Estimate the IPW
step1 = function(treat, ldv, data){
  pi.out = glm(treat ~ ldv + lg_epr_gdpcapl + lg_grow + lg_ross_net_oil_exports_valuePOP + ross_population + gwf_party + gwf_military + gwf_personal + arc_turn + epr_ethfrac, data=data, family=binomial(link="probit"))
  ps = pi.out$fit
  D = treat
  PrD = mean(D)
  IPW = (D*PrD+(1-D)*(1-PrD))/(D*ps+(1-D)*(1-ps))
  #plot(density(IPW))
  
  plot(density(pi.out$fit[D==1], weight=IPW[D==1]/sum(IPW[D==1])), lwd=2, main="Distribution of IPW")
  lines(density(pi.out$fit[D==0], weight=IPW[D==0]/sum(IPW[D==0])),lwd=2, lty=2)
  legend("topleft", legend=c("treated","controls"), lty=c(1,2), lwd=2)
  return(list(res=IPW))
}

par(mfrow=c(2,4))

ipw1 = step1(dta1$leg_elec, dta1$lg_fh_PR_n, dta1)$res
ipw1.pr = step1(dta2$pr, dta2$lg_fh_PR_n, dta2)$res
ipw1.pm = step1(dta3$pm, dta3$lg_fh_PR_n, dta3)$res
ipw1.pmr = step1(dta4$pr, dta4$lg_fh_PR_n, dta4)$res

ipw2 = step1(dta1$leg_elec, dta1$lg_fh_CL_n, dta1)$res
ipw2.pr = step1(dta2$pr, dta2$lg_fh_CL_n, dta2)$res
ipw2.pm = step1(dta3$pm, dta3$lg_fh_CL_n, dta3)$res
ipw2.pmr = step1(dta4$pr, dta4$lg_fh_CL_n, dta4)$res

par(mfrow=c(1,1))

### Step 2: Balance test (before, skip)
step2 = function(treatn, ldvn, data, ipw){
  vars = c(ldvn, "lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac", treatn)
  X = data[, vars]
  bout.ipw = MatchBalance(X[,11] ~ as.matrix(X[,1:10]), weights=ipw)
  bal.ipw = baltest.collect(matchbal.out=bout.ipw, var.names=vars[1:10], after=F)
  round(bal.ipw[,c("mean.Tr","mean.Co","T pval","KS pval")],3)
}

step2("leg_elec", "lg_fh_PR_n", dta1, ipw1)
step2("pr", "lg_fh_PR_n", dta2, ipw1.pr)
step2("pm", "lg_fh_PR_n", dta3, ipw1.pm)
step2("pr", "lg_fh_PR_n", dta4, ipw1.pmr)

step2("leg_elec", "lg_fh_CL_n", dta1, ipw2)
step2("pr", "lg_fh_CL_n", dta2, ipw2.pr)
step2("pm", "lg_fh_CL_n", dta3, ipw2.pm)
step2("pr", "lg_fh_CL_n", dta4, ipw2.pmr)

### Step 3: Matching by IPW
step3 = function(data, ldvn, treatn, outcomen, m, ipw){
  vars = c(ldvn, "lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac", treatn, outcomen)
  X = data[,vars]
  #exactmatch = c(rep(FALSE, 5), rep(TRUE, 4), FALSE)
  exactmatch = rep(FALSE, 10)
  matchout = Match(Y=X[,12], Tr=X[,11], X=X[,1:10], M=m, exact=exactmatch, estimand="ATT", BiasAdjust=TRUE, weights=ipw)
  #summary(matchout)
  res = c(matchout$est, matchout$se)
  return(list(out=matchout, res=res))
}

res1.ipw = step3(dta1, "lg_fh_PR_n", "leg_elec", "fh_PR_n", 1, ipw1)
res1.ipw.pr = step3(dta2, "lg_fh_PR_n", "pr", "fh_PR_n", 1, ipw1.pr)
res1.ipw.pm = step3(dta3, "lg_fh_PR_n", "pm", "fh_PR_n", 1, ipw1.pm)
res1.ipw.pmr = step3(dta4, "lg_fh_PR_n", "pr", "fh_PR_n", 1, ipw1.pmr)

res2.ipw = step3(dta1, "lg_fh_CL_n", "leg_elec", "fh_CL_n", 1, ipw2)
res2.ipw.pr = step3(dta2, "lg_fh_CL_n", "pr", "fh_CL_n", 1, ipw2.pr)
res2.ipw.pm = step3(dta3, "lg_fh_CL_n", "pm", "fh_CL_n", 1, ipw2.pm)
res2.ipw.pmr = step3(dta4, "lg_fh_CL_n", "pr", "fh_CL_n", 1, ipw2.pmr)

### Step 4: Balance test (after)
step4 = function(matchout, treat, ldv, ldvn, data){
  vars = c(ldvn, "GPDpc", "growth", "oil", "population", "party", "military", "personal", "leader turnover", "efl")
  mb.out = MatchBalance(match.out=matchout, treat ~ ldv + lg_epr_gdpcapl + lg_grow + lg_ross_net_oil_exports_valuePOP + ross_population + gwf_party + gwf_military + gwf_personal + arc_turn + epr_ethfrac, data=data)
  btest_after = baltest.collect(mb.out, var.names=vars, after=TRUE)
  #round(btest_after[,c("mean.Tr","mean.Co","T pval","KS pval")],3)
  #res = cbind(btest_after[,1]-btest_after[,2], btest_after[,3])
  res = btest_after
  return(list(res=res))
}

resb1.ipw = step4(res1.ipw$out, dta1$leg_elec, dta1$lg_fh_PR_n, "lg_fh_PR_n", dta1)$res
resb1.ipw.pr = step4(res1.ipw.pr$out, dta2$pr, dta2$lg_fh_PR_n, "lg_fh_PR_n", dta2)$res
resb1.ipw.pm = step4(res1.ipw.pm$out, dta3$pm, dta3$lg_fh_PR_n, "lg_fh_PR_n", dta3)$res
resb1.ipw.pmr = step4(res1.ipw.pmr$out, dta4$pr, dta4$lg_fh_PR_n, "lg_fh_PR_n", dta4)$res

resb2.ipw = step4(res2.ipw$out, dta1$leg_elec, dta1$lg_fh_CL_n, "lg_fh_CL_n", dta1)$res
resb2.ipw.pr = step4(res2.ipw.pr$out, dta2$pr, dta2$lg_fh_CL_n, "lg_fh_CL_n", dta2)$res
resb2.ipw.pm = step4(res2.ipw.pm$out, dta3$pm, dta3$lg_fh_CL_n, "lg_fh_CL_n", dta3)$res
resb2.ipw.pmr = step4(res2.ipw.pmr$out, dta4$pr, dta4$lg_fh_CL_n, "lg_fh_CL_n", dta4)$res

res1.ipw.all = rbind(res1.ipw$res, res1.ipw.pr$res, res1.ipw.pm$res, res1.ipw.pmr$res)
res2.ipw.all = rbind(res2.ipw$res, res2.ipw.pr$res, res2.ipw.pm$res, res2.ipw.pmr$res)

### Step 5: Sensitivity analysis for matched data
sen1.ipw = psens(res1.ipw$out, Gamma=2, GammaInc=0.1)$bounds
sen1.ipw.pr = psens(res1.ipw.pr$out, Gamma=2, GammaInc=0.1)$bounds
sen1.ipw.pm = psens(res1.ipw.pm$out, Gamma=2, GammaInc=0.1)$bounds
sen1.ipw.pmr = psens(res1.ipw.pmr$out, Gamma=2, GammaInc=0.1)$bounds

sen2.ipw = psens(res2.ipw$out, Gamma=2, GammaInc=0.1)$bounds
sen2.ipw.pr = psens(res2.ipw.pr$out, Gamma=2, GammaInc=0.1)$bounds
sen2.ipw.pm = psens(res2.ipw.pm$out, Gamma=2, GammaInc=0.1)$bounds
sen2.ipw.pmr = psens(res2.ipw.pmr$out, Gamma=2, GammaInc=0.1)$bounds

##### SOO (Balacing by weighting) #####

### Step 1: Derive entropy weights
step1 = function(data, ldvn, treatn){
  require(ebal)
  vars = c(ldvn, "lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac", treatn)
  X = data[, vars]
  X.ebal=X[,1:10]
  eb.out = ebalance(X[,11], X=X.ebal)
  #ls(eb.out); eb.out$w
  ebalw = replicate(length(X[,11]),1)
  ebalw[X[,11]==0]=eb.out$w # Weight for the control units.
  return(list(res=ebalw))
}

ew1 = step1(dta1, "lg_fh_PR_n", "leg_elec")$res
ew1.pr = step1(dta2, "lg_fh_PR_n", "pr")$res
ew1.pm = step1(dta3, "lg_fh_PR_n", "pm")$res
ew1.pmr = step1(dta4, "lg_fh_PR_n", "pr")$res

ew2 = step1(dta1, "lg_fh_CL_n", "leg_elec")$res
ew2.pr = step1(dta2, "lg_fh_CL_n", "pr")$res
ew2.pm = step1(dta3, "lg_fh_CL_n", "pm")$res
ew2.pmr = step1(dta4, "lg_fh_CL_n", "pr")$res

### Step 2: Balance test (before, skip)
step2 = function(treatn, ldvn, data, ebalw){
  vars = c(ldvn, "lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac", treatn)
  X = data[, vars]
  bout.ebalw = MatchBalance(X[,11] ~ as.matrix(X[,1:10]), weights=ebalw)
  bal.ebalw = baltest.collect(matchbal.out=bout.ebalw, var.names=vars[1:10], after=F)
  round(bal.ebalw[,c("mean.Tr","mean.Co","T pval","KS pval")],3)
}

step2("leg_elec", "lg_fh_PR_n", dta1, ew1)
step2("pr", "lg_fh_PR_n", dta2, ew1.pr)
step2("pm", "lg_fh_PR_n", dta3, ew1.pm)
step2("pr", "lg_fh_PR_n", dta4, ew1.pmr)

step2("leg_elec", "lg_fh_CL_n", dta1, ew2)
step2("pr", "lg_fh_CL_n", dta2, ew2.pr)
step2("pm", "lg_fh_CL_n", dta3, ew2.pm)
step2("pr", "lg_fh_CL_n", dta4, ew2.pmr)

### Step 3: Matching by entropy weight
step3 = function(data, ldvn, treatn, outcomen, m, ew){
  vars = c(ldvn, "lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac", treatn, outcomen)
  X = data[,vars]
  #exactmatch = c(rep(FALSE, 5), rep(TRUE, 4), FALSE)
  exactmatch = rep(FALSE, 10)
  matchout = Match(Y=X[,12], Tr=X[,11], X=X[,1:10], M=m, exact=exactmatch, estimand="ATT", BiasAdjust=TRUE, weights=ew)
  #summary(matchout)
  res = c(matchout$est, matchout$se)
  return(list(out=matchout, res=res))
}

res1.ew = step3(dta1, "lg_fh_PR_n", "leg_elec", "fh_PR_n", 1, ew1)
res1.ew.pr = step3(dta2, "lg_fh_PR_n", "pr", "fh_PR_n", 1, ew1.pr)
res1.ew.pm = step3(dta3, "lg_fh_PR_n", "pm", "fh_PR_n", 1, ew1.pm)
res1.ew.pmr = step3(dta4, "lg_fh_PR_n", "pr", "fh_PR_n", 1, ew1.pmr)

res2.ew = step3(dta1, "lg_fh_CL_n", "leg_elec", "fh_CL_n", 1, ew2)
res2.ew.pr = step3(dta2, "lg_fh_CL_n", "pr", "fh_CL_n", 1, ew2.pr)
res2.ew.pm = step3(dta3, "lg_fh_CL_n", "pm", "fh_CL_n", 1, ew2.pm)
res2.ew.pmr = step3(dta4, "lg_fh_CL_n", "pr", "fh_CL_n", 1, ew2.pmr)

### Step 4: Balance test (after)
step4 = function(matchout, treat, ldv, ldvn, data){
  vars = c(ldvn, "GPDpc", "growth", "oil", "population", "party", "military", "personal", "leader turnover", "efl")
  mb.out = MatchBalance(match.out=matchout, treat ~ ldv + lg_epr_gdpcapl + lg_grow + lg_ross_net_oil_exports_valuePOP + ross_population + gwf_party + gwf_military + gwf_personal + arc_turn + epr_ethfrac, data=data)
  btest_after = baltest.collect(mb.out, var.names=vars, after=TRUE)
  #round(btest_after[,c("mean.Tr","mean.Co","T pval","KS pval")],3)
  #res = cbind(btest_after[,1]-btest_after[,2], btest_after[,3])
  res = btest_after
  return(list(res=res))
}

resb1.ew = step4(res1.ew$out, dta1$leg_elec, dta1$lg_fh_PR_n, "lg_fh_PR_n", dta1)$res
resb1.ew.pr = step4(res1.ew.pr$out, dta2$pr, dta2$lg_fh_PR_n, "lg_fh_PR_n", dta2)$res
resb1.ew.pm = step4(res1.ew.pm$out, dta3$pm, dta3$lg_fh_PR_n, "lg_fh_PR_n", dta3)$res
resb1.ew.pmr = step4(res1.ew.pmr$out, dta4$pr, dta4$lg_fh_PR_n, "lg_fh_PR_n", dta4)$res

resb2.ew = step4(res2.ew$out, dta1$leg_elec, dta1$lg_fh_CL_n, "lg_fh_CL_n", dta1)$res
resb2.ew.pr = step4(res2.ew.pr$out, dta2$pr, dta2$lg_fh_CL_n, "lg_fh_CL_n", dta2)$res
resb2.ew.pm = step4(res2.ew.pm$out, dta3$pm, dta3$lg_fh_CL_n, "lg_fh_CL_n", dta3)$res
resb2.ew.pmr = step4(res2.ew.pmr$out, dta4$pr, dta4$lg_fh_CL_n, "lg_fh_CL_n", dta4)$res

res1.ew.all = rbind(res1.ew$res, res1.ew.pr$res, res1.ew.pm$res, res1.ew.pmr$res)
res2.ew.all = rbind(res2.ew$res, res2.ew.pr$res, res2.ew.pm$res, res2.ew.pmr$res)

### Step 5: Sensitivity analysis for matched data
sen1.ew = psens(res1.ew$out, Gamma=2, GammaInc=0.1)$bounds
sen1.ew.pr = psens(res1.ew.pr$out, Gamma=2, GammaInc=0.1)$bounds
sen1.ew.pm = psens(res1.ew.pm$out, Gamma=2, GammaInc=0.1)$bounds
sen1.ew.pmr = psens(res1.ew.pmr$out, Gamma=2, GammaInc=0.1)$bounds

sen2.ew = psens(res2.ew$out, Gamma=2, GammaInc=0.1)$bounds
sen2.ew.pr = psens(res2.ew.pr$out, Gamma=2, GammaInc=0.1)$bounds
sen2.ew.pm = psens(res2.ew.pm$out, Gamma=2, GammaInc=0.1)$bounds
sen2.ew.pmr = psens(res2.ew.pmr$out, Gamma=2, GammaInc=0.1)$bounds

##### Present the results of sensitivity analysis #####
library(xtable)

sen1.all = cbind(sen1[,-2], sen1[,3], sen1[,3], sen1[,3])
sen2.all = cbind(sen2[,-2], sen2[,3], sen2[,3], sen2[,3])
print(xtable(sen1.all, digits=2), include.rownames=FALSE)
print(xtable(sen2.all, digits=2), include.rownames=FALSE)

sen1.ps.all = cbind(sen1.ps[,-2], sen1.ps.pr[,3], sen1.ps.pm[,3], sen1.ps.pmr[,3])
sen2.ps.all = cbind(sen2.ps[,-2], sen2.ps.pr[,3], sen2.ps.pm[,3], sen2.ps.pmr[,3])
print(xtable(sen1.ps.all, digits=2), include.rownames=FALSE)
print(xtable(sen2.ps.all, digits=2), include.rownames=FALSE)

sen1.ipw.all = cbind(sen1.ipw[,-2], sen1.ipw.pr[,3], sen1.ipw.pm[,3], sen1.ipw.pmr[,3])
sen2.ipw.all = cbind(sen2.ipw[,-2], sen2.ipw.pr[,3], sen2.ipw.pm[,3], sen2.ipw.pmr[,3])
print(xtable(sen1.ipw.all, digits=2), include.rownames=FALSE)
print(xtable(sen2.ipw.all, digits=2), include.rownames=FALSE)

sen1.ew.all = cbind(sen1.ew[,-2], sen1.ew.pr[,3], sen1.ew.pm[,3], sen1.ew.pmr[,3])
sen2.ew.all = cbind(sen2.ew[,-2], sen2.ew.pr[,3], sen2.ew.pm[,3], sen2.ew.pmr[,3])
print(xtable(sen1.ew.all, digits=2), include.rownames=FALSE)
print(xtable(sen2.ew.all, digits=2), include.rownames=FALSE)

##### Plot the results (matching) #####
coef = c(res1.all[,1], res1.ps.all[,1], res1.ipw.all[,1], res1.ew.all[,1])
sb = c(res1.all[,2], res1.ps.all[,2], res1.ipw.all[,2], res1.ew.all[,2])
#ub = coef + 1.96 * sb; lb = coef - 1.96 * sb
ub = coef + 1.6 * sb; lb = coef - 1.6 * sb
specification = rep(1, length(coef))
mod = c(rep(c("Bias-corrected", "Propensity", "IPW", "Entropy"), times=1, each=4))
mod = factor(mod, levels=c("Bias-corrected", "Propensity", "IPW", "Entropy"))
var = c(rep(c("Elec vs None", "PR vs None", "PM vs None", "PR vs PM"), times=4, each=1))
var = factor(var, levels=c("Elec vs None", "PR vs None", "PM vs None", "PR vs PM"))
all1 = data.frame(var, coef, sb, lb, ub, specification, mod)

coef = c(res2.all[,1], res2.ps.all[,1], res2.ipw.all[,1], res2.ew.all[,1])
sb = c(res2.all[,2], res2.ps.all[,2], res2.ipw.all[,2], res2.ew.all[,2])
#ub = coef + 1.96 * sb; lb = coef - 1.96 * sb
ub = coef + 1.64 * sb; lb = coef - 1.64 * sb
specification = rep(1, length(coef))
mod = c(rep(c("Bias-corrected", "Propensity", "IPW", "Entropy"), times=1, each=4))
mod = factor(mod, levels=c("Bias-corrected", "Propensity", "IPW", "Entropy"))
var = c(rep(c("Elec vs None", "PR vs None", "PM vs None", "PR vs PM"), times=4, each=1))
var = factor(var, levels=c("Elec vs None", "PR vs None", "PM vs None", "PR vs PM"))
all2 = data.frame(var, coef, sb, lb, ub, specification, mod)

#all1 = all1[all1$mod != "Propensity",] 
#all2 = all2[all2$mod != "Propensity",] 

#coef = c(res1.ew.all[,1], res2.ew.all[,1])
#sb = c(res1.ew.all[,2], res2.ew.all[,2])
#ub = coef + 1.96 * sb; lb = coef - 1.96 * sb
#ub = coef + 1.64 * sb; lb = coef - 1.64 * sb
#specification = rep(1, length(coef))
#mod = c(rep(c("Press Freedom", "Civil Liberties"), times=1, each=3))
#var = c(rep(c("Elections", "Plurality/Majoritarian", "Proportional"), times=2, each=1))
#var = factor(var, levels=c("Elections", "Plurality/Majoritarian", "Proportional"))
#allew = data.frame(var, coef, sb, lb, ub, specification, mod)

coef.plot = function(data){
  lower = min(data[,4])-0.05
  upper = max(data[,5])+0.05
  library(ggplot2)
  pd = position_dodge(width=0.5)
  ggplot(data, aes(var, coef, color=mod)) +
    geom_point(aes(shape=mod),size=4, position=pd) +
    #scale_color_manual(name="Method",values=c("red", "blue", "darkgreen")) +
    #scale_shape_manual(name="Method",values=c(16,17,18)) +
    scale_color_manual(name="",values=c("red", "blue", "darkgreen", "orange")) +
    scale_shape_manual(name="",values=c(15,16,17,18)) +
    theme_bw() + xlab("") + ylab("ATT") + ylim(lower, upper) +
    geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1, position=pd) +
    geom_hline(yintercept = 0, colour="grey", linetype = "longdash") +
    theme(text = element_text(size=20))
}
pdf("soo_press.pdf", width=10, height=5); coef.plot(all1); dev.off()
pdf("soo_civil.pdf", width=10, height=5); coef.plot(all2); dev.off()
#coef.plot(allew)

##### Plot the results (balance test) #####
b.plot = function(d){
  d.vnames = rownames(d)
  rownames(d) = NULL
  colnames(d) = "sdiff.pooled"
  d = data.frame(d)
  d$vname = d.vnames
  d$vname = factor(d$vname, levels = unique(d$vname)[ncol(X):1], labels = unique(d$vname)[ncol(X):1])
  d$gr = rep(c("Bias correction", "Propensity", "IPW", "Entrophy"), each=ncol(X))
  #d$gr = rep(c("Bias correction", "IPW", "Entrophy"), each=ncol(X))
  
  require(lattice); require(RColorBrewer)
  Cex = 1.2
  Cex2 = 1
  #mypal=brewer.pal(4,"Set2")
  mypal=brewer.pal(3, "Set2")
  
  print(
    xyplot(vname~sdiff.pooled/100,data=d,groups=gr,
           #xlim=c(min(d$sdiff.pooled/100),max(d$sdiff.pooled/100)),
           xlim=c(-1.1,1.1),
           panel = function(x,y,...)
           {
             panel.abline(v=0, lwd = 1 , lty="solid")
             panel.abline(v=c(-1,-.5,.5,1), lwd = 2 , lty="dotted")
             panel.abline(h=c(1:nrow(d)), lwd = 1 , lty="dashed", col="gray95")
             #panel.grid(v=0,h=-1)
             panel.xyplot(x,y,...)
           }
           ,par.settings = list(superpose.symbol = list(pch = c(1,15,16,17),col=mypal,cex=1.2))
           #,par.settings = list(superpose.symbol = list(pch = c(15,18,19),col=mypal,cex=1.2))
           ,xlab=list("Standardized Difference in Means",cex=Cex),ylab="",auto.key=T,scales=list(y=list(cex=Cex),x=list(cex=Cex2,at=c(-1,-.5,0,.5,1),labels=c("-1","-.5","0",".5","1")))
    )
  )
}

varnames = c("lg_fh_PR_n", "lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac")
X = dta[,varnames]
dall.press = rbind(as.matrix(resb1[,4]), as.matrix(resb1.ps[,4]), as.matrix(resb1.ipw[,4]), as.matrix(resb1.ew[,4]))
dpr.press = rbind(as.matrix(resb1.pr[,4]), as.matrix(resb1.ps.pr[,4]), as.matrix(resb1.ipw.pr[,4]), as.matrix(resb1.ew.pr[,4]))
dpm.press = rbind(as.matrix(resb1.pm[,4]), as.matrix(resb1.ps.pm[,4]), as.matrix(resb1.ipw.pm[,4]), as.matrix(resb1.ew.pm[,4]))
dpmr.press = rbind(as.matrix(resb1.pmr[,4]), as.matrix(resb1.ps.pmr[,4]), as.matrix(resb1.ipw.pmr[,4]), as.matrix(resb1.ew.pmr[,4]))
pdf("balance_all_press.pdf", width=7, height=7); b.plot(dall.press); dev.off()
pdf("balance_pr_press.pdf", width=7, height=7); b.plot(dpr.press); dev.off()
pdf("balance_pm_press.pdf", width=7, height=7); b.plot(dpm.press); dev.off()
pdf("balance_pmr_press.pdf", width=7, height=7); b.plot(dpmr.press); dev.off()

varnames = c("lg_fh_CL_n", "lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac")
X = dta[,varnames]
dall.civil = rbind(as.matrix(resb2[,4]), as.matrix(resb2.ps[,4]), as.matrix(resb2.ipw[,4]), as.matrix(resb2.ew[,4]))
dpr.civil = rbind(as.matrix(resb2.pr[,4]), as.matrix(resb2.ps.pr[,4]), as.matrix(resb2.ipw.pr[,4]), as.matrix(resb2.ew.pr[,4]))
dpm.civil = rbind(as.matrix(resb2.pm[,4]), as.matrix(resb2.ps.pm[,4]), as.matrix(resb2.ipw.pm[,4]), as.matrix(resb2.ew.pm[,4]))
dpmr.civil = rbind(as.matrix(resb2.pmr[,4]), as.matrix(resb2.ps.pmr[,4]), as.matrix(resb2.ipw.pmr[,4]), as.matrix(resb2.ew.pmr[,4]))
pdf("balance_all_civil.pdf", width=7, height=7); b.plot(dall.civil); dev.off()
pdf("balance_pr_civil.pdf", width=7, height=7); b.plot(dpr.civil); dev.off()
pdf("balance_pm_civil.pdf", width=7, height=7); b.plot(dpr.civil); dev.off()
pdf("balance_pmr_civil.pdf", width=7, height=7); b.plot(dpm.civil); dev.off()
