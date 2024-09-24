rm(list=ls())
setwd("C:/Users/polar/Downloads/Sandbox/Teaching/AdvQ/Week 07 08/02 Exercise")
library(Matching); library(ebal); library(rbounds); library(DataCombine)

load("EAR_final.RData")
dta$coln = dta$qog_ht_colonial
dta$fh_PR_n = scale(dta$fh_PR, center=T)
dta$fh_CL_n = scale(dta$fh_CL, center=T)

#dta$fh_PR_n = (dta$fh_PR-min(dta$fh_PR, na.rm=T))/(max(dta$fh_PR, na.rm=T)-min(dta$fh_PR, na.rm=T))
#dta$fh_CL_n = (dta$fh_CL-min(dta$fh_CL, na.rm=T))/(max(dta$fh_CL, na.rm=T)-min(dta$fh_CL, na.rm=T))

dta$hrs_n = (dta$hrs_right-min(dta$hrs_right, na.rm=T))/(max(dta$hrs_right, na.rm=T)-min(dta$hrs_right, na.rm=T))
dta$hrs_n = -1*(dta$hrs_n)

#dta$fh_PR_n = -1*(dta$vdem_PR)
#dta$fh_CL_n = -1*(dta$vdem_CL)

repm = dta[,c("hrs_n", "fh_PR_n", "fh_CL_n")]
cor(repm, use="na.or.complete")

dta = dta[order(dta$cowcode, dta$year),]
fh_PR_n1 = slide(dta, Var="fh_PR_n", GroupVar="cowcode", slideBy=-1)
dta$lg_fh_PR_n = fh_PR_n1$"fh_PR_n-1"
rm(fh_PR_n1)

dta = dta[order(dta$cowcode, dta$year),]
fh_CL1 = slide(dta, Var="fh_CL", GroupVar="cowcode", slideBy=-1)
dta$lg_fh_CL = fh_CL1$"fh_CL-1"
rm(fh_CL1)

dta = dta[order(dta$cowcode, dta$year),]
fh_CL_n1 = slide(dta, Var="fh_CL_n", GroupVar="cowcode", slideBy=-1)
dta$lg_fh_CL_n = fh_CL_n1$"fh_CL_n-1"
rm(fh_CL_n1)

dta = dta[order(dta$cowcode, dta$year),]
hrs_n1 = slide(dta, Var="hrs_n", GroupVar="cowcode", slideBy=-1)
dta$lg_hrs_n = hrs_n1$"hrs_n-1"
rm(hrs_n1)

#table(dta$dpi_system)

#dta$fh_PR_n = dta$vdem_PR
#dta$fh_CL_n = dta$vdem_CL
#dta$lg_fh_PR_n = dta$lg_vdem_PR
#dta$lg_fh_CL_n = dta$lg_vdem_CL

#dta$fh_PR_n = dta$fh_PR
#dta$fh_CL_n = dta$fh_CL
#dta$lg_fh_PR_n = dta$lg_fh_PR
#dta$lg_fh_CL_n = dta$lg_fh_CL

##### Outcomes #####
out_pr = c("fh_PR_n", "lg_fh_PR_n")
out_cl = c("fh_CL_n", "lg_fh_CL_n", "fh_CL", "lg_fh_CL")
out_hrs = c("hrs_n", "lg_hrs_n")
out = c(out_pr, out_cl, out_hrs)

##### Treatment #####
dta$pr = ifelse(dta$elecs == "pr", 1, 0)
dta$pm = ifelse(dta$elecs == "pm", 1, 0)
treat = c("leg_elec", "pm", "pr")

##### Observed covariates #####
cov = c("lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac", "coln")

##### Subset #####

# Election vs None
dta1 = dta
vars = c(cov, treat, out); dta1 = dta1[,vars]; dta1 = na.omit(dta1)
table(dta1$leg_elec)

dta1$leg_none = ifelse(dta1$leg_elec == 1, 0, 1)

saveRDS(dta1, "EAR_selection.RData")

##### SOO (Bias correction matching) #####

### Step 1: Pre-test (skip)
step1 = function(treat, ldv, data){
  summary(lm(treat ~ ldv + lg_epr_gdpcapl + lg_grow + lg_ross_net_oil_exports_valuePOP + ross_population + gwf_party + gwf_military + gwf_personal + arc_turn + epr_ethfrac + as.factor(coln), data=data))
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
  vars = c(ldvn, "lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac", "coln", treat, outcome)
  X = data[,vars]
  exactmatch = c(rep(FALSE, 5), rep(TRUE, 4), FALSE, TRUE)
  #exactmatch = rep(FALSE, 10)
  matchout = Match(Y=X[,13], Tr=X[,12], X=X[,1:11], M=m, exact=exactmatch, estimand="ATT", BiasAdjust=TRUE)
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

res3 = step3(dta1, "lg_hrs_n", "leg_elec", "hrs_n", 1)
res3.pr = step3(dta2, "lg_hrs_n", "pr", "hrs_n", 1)
res3.pm = step3(dta3, "lg_hrs_n", "pm", "hrs_n", 1)
res3.pmr = step3(dta4, "lg_hrs_n", "pr", "hrs_n", 1)

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
res3.all = rbind(res3$res, res3.pr$res, res3.pm$res, res3.pmr$res)

#res1.all[,1] + 1.6* res1.all[,2]
#res2.all[,1] + 1.6* res2.all[,2]

### Step 5: Sensitivity analysis for matched data
sen1 = psens(res1$out, Gamma=3, GammaInc=0.1)$bounds
sen1.pr = psens(res1.pr$out, Gamma=3, GammaInc=0.1)$bounds
sen1.pm = psens(res1.pm$out, Gamma=3, GammaInc=0.1)$bounds
sen1.pmr = psens(res1.pmr$out, Gamma=3, GammaInc=0.1)$bounds

sen2 = psens(res2$out, Gamma=3, GammaInc=0.1)$bounds
sen2.pr = psens(res2.pr$out, Gamma=3, GammaInc=0.1)$bounds
sen2.pm = psens(res2.pm$out, Gamma=3, GammaInc=0.1)$bounds
sen2.pmr = psens(res2.pmr$out, Gamma=3, GammaInc=0.1)$bounds

##### Plot the results (matching) #####
coef = c(res1.all[,1], res1.ipw.all[,1], res1.ew.all[,1])
sb = c(res1.all[,2], res1.ipw.all[,2], res1.ew.all[,2])
ub = coef + 1.96 * sb; lb = coef - 1.96 * sb
#ub = coef + 1.6 * sb; lb = coef - 1.6 * sb
specification = rep(1, length(coef))
mod = c(rep(c("Bias-corrected", "IPW", "Entropy"), times=1, each=4))
mod = factor(mod, levels=c("Bias-corrected", "IPW", "Entropy"))
var = c(rep(c("Elec vs None", "PR vs None", "PM vs None", "PR vs PM"), times=3, each=1))
var = factor(var, levels=c("Elec vs None", "PR vs None", "PM vs None", "PR vs PM"))
all1 = data.frame(var, coef, sb, lb, ub, specification, mod)

coef = c(res2.all[,1], res2.ipw.all[,1], res2.ew.all[,1])
sb = c(res2.all[,2], res2.ipw.all[,2], res2.ew.all[,2])
ub = coef + 1.96 * sb; lb = coef - 1.96 * sb
#ub = coef + 1.64 * sb; lb = coef - 1.64 * sb
specification = rep(1, length(coef))
mod = c(rep(c("Bias-corrected", "IPW", "Entropy"), times=1, each=4))
mod = factor(mod, levels=c("Bias-corrected", "IPW", "Entropy"))
var = c(rep(c("Elec vs None", "PR vs None", "PM vs None", "PR vs PM"), times=3, each=1))
var = factor(var, levels=c("Elec vs None", "PR vs None", "PM vs None", "PR vs PM"))
all2 = data.frame(var, coef, sb, lb, ub, specification, mod)

save(all1, all2, file="X2020/RES_Matching.RData")

coef = c(res3.all[,1], res3.ipw.all[,1], res3.ew.all[,1])
sb = c(res3.all[,2], res3.ipw.all[,2], res3.ew.all[,2])
ub = coef + 1.9 * sb; lb = coef - 1.9 * sb
#ub = coef + 1.64 * sb; lb = coef - 1.64 * sb
specification = rep(1, length(coef))
mod = c(rep(c("Bias-corrected", "IPW", "Entropy"), times=1, each=4))
mod = factor(mod, levels=c("Bias-corrected", "IPW", "Entropy"))
var = c(rep(c("Elec vs None", "PR vs None", "PM vs None", "PR vs PM"), times=3, each=1))
var = factor(var, levels=c("Elec vs None", "PR vs None", "PM vs None", "PR vs PM"))
all3 = data.frame(var, coef, sb, lb, ub, specification, mod)

coef.plot = function(data){
  lower = min(data[,4])-0.005
  upper = max(data[,5])+0.005
  library(ggplot2)
  pd = position_dodge(width=0.5)
  ggplot(data, aes(var, coef, color=mod)) +
    geom_point(aes(shape=mod),size=4, position=pd) +
    scale_color_manual(name="Method",values=c("red", "blue", "darkgreen")) +
    scale_shape_manual(name="Method",values=c(16,17,18)) +
    #scale_color_manual(name="",values=c("red", "blue", "darkgreen", "orange")) +
    #scale_shape_manual(name="",values=c(15,16,17,18)) +
    theme_bw() + xlab("") + ylab("ATT") + ylim(lower, upper) +
    geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1, position=pd) +
    geom_hline(yintercept = 0, colour="grey", linetype = "longdash") +
    theme(text = element_text(size=16))
}
pdf("soo_press.pdf", width=8, height=5); coef.plot(all1); dev.off()
pdf("soo_civil.pdf", width=8, height=5); coef.plot(all2); dev.off()
#pdf("soo_hrs.pdf", width=8, height=5); coef.plot(all3); dev.off()
#coef.plot(allew)

##### Plot the results (balance test) #####
b.plot = function(d){
  d.vnames = rownames(d)
  rownames(d) = NULL
  colnames(d) = "sdiff.pooled"
  d = data.frame(d)
  d$vname = d.vnames
  d$vname = factor(d$vname, levels = unique(d$vname)[ncol(X):1], labels = unique(d$vname)[ncol(X):1])
  #d$gr = rep(c("Bias correction", "Propensity", "IPW", "Entrophy"), each=ncol(X))
  d$gr = rep(c("Bias correction", "IPW", "Entrophy"), each=ncol(X))
  
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

dall.press = rbind(as.matrix(resb1[,4]), as.matrix(resb1.ipw[,4]), as.matrix(resb1.ew[,4]))
dpr.press = rbind(as.matrix(resb1.pr[,4]), as.matrix(resb1.ipw.pr[,4]), as.matrix(resb1.ew.pr[,4]))
dpm.press = rbind(as.matrix(resb1.pm[,4]), as.matrix(resb1.ipw.pm[,4]), as.matrix(resb1.ew.pm[,4]))
dpmr.press = rbind(as.matrix(resb1.pmr[,4]), as.matrix(resb1.ipw.pmr[,4]), as.matrix(resb1.ew.pmr[,4]))

#dall.press = rbind(as.matrix(resb1[,4]), as.matrix(resb1.ps[,4]), as.matrix(resb1.ipw[,4]), as.matrix(resb1.ew[,4]))
#dpr.press = rbind(as.matrix(resb1.pr[,4]), as.matrix(resb1.ps.pr[,4]), as.matrix(resb1.ipw.pr[,4]), as.matrix(resb1.ew.pr[,4]))
#dpm.press = rbind(as.matrix(resb1.pm[,4]), as.matrix(resb1.ps.pm[,4]), as.matrix(resb1.ipw.pm[,4]), as.matrix(resb1.ew.pm[,4]))
#dpmr.press = rbind(as.matrix(resb1.pmr[,4]), as.matrix(resb1.ps.pmr[,4]), as.matrix(resb1.ipw.pmr[,4]), as.matrix(resb1.ew.pmr[,4]))

pdf("balance_all_press_new.pdf", width=7, height=7); b.plot(dall.press); dev.off()
pdf("balance_pr_press_new.pdf", width=7, height=7); b.plot(dpr.press); dev.off()
pdf("balance_pm_press_new.pdf", width=7, height=7); b.plot(dpm.press); dev.off()
pdf("balance_pmr_press_new.pdf", width=7, height=7); b.plot(dpmr.press); dev.off()

varnames = c("lg_fh_CL_n", "lg_epr_gdpcapl", "lg_grow", "lg_ross_net_oil_exports_valuePOP", "ross_population", "gwf_party", "gwf_military", "gwf_personal", "arc_turn", "epr_ethfrac")
X = dta[,varnames]

#dall.civil = rbind(as.matrix(resb2[,4]), as.matrix(resb2.ps[,4]), as.matrix(resb2.ipw[,4]), as.matrix(resb2.ew[,4]))
#dpr.civil = rbind(as.matrix(resb2.pr[,4]), as.matrix(resb2.ps.pr[,4]), as.matrix(resb2.ipw.pr[,4]), as.matrix(resb2.ew.pr[,4]))
#dpm.civil = rbind(as.matrix(resb2.pm[,4]), as.matrix(resb2.ps.pm[,4]), as.matrix(resb2.ipw.pm[,4]), as.matrix(resb2.ew.pm[,4]))
#dpmr.civil = rbind(as.matrix(resb2.pmr[,4]), as.matrix(resb2.ps.pmr[,4]), as.matrix(resb2.ipw.pmr[,4]), as.matrix(resb2.ew.pmr[,4]))

dall.civil = rbind(as.matrix(resb2[,4]), as.matrix(resb2.ipw[,4]), as.matrix(resb2.ew[,4]))
dpr.civil = rbind(as.matrix(resb2.pr[,4]), as.matrix(resb2.ipw.pr[,4]), as.matrix(resb2.ew.pr[,4]))
dpm.civil = rbind(as.matrix(resb2.pm[,4]), as.matrix(resb2.ipw.pm[,4]), as.matrix(resb2.ew.pm[,4]))
dpmr.civil = rbind(as.matrix(resb2.pmr[,4]), as.matrix(resb2.ipw.pmr[,4]), as.matrix(resb2.ew.pmr[,4]))

pdf("balance_all_civil_new.pdf", width=7, height=7); b.plot(dall.civil); dev.off()
pdf("balance_pr_civil_new.pdf", width=7, height=7); b.plot(dpr.civil); dev.off()
pdf("balance_pm_civil_new.pdf", width=7, height=7); b.plot(dpr.civil); dev.off()
pdf("balance_pmr_civil_new.pdf", width=7, height=7); b.plot(dpm.civil); dev.off()
