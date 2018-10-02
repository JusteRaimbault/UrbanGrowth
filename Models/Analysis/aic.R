
library(knitr)

# source calibration for data
setwd('..')

source('../Analysis/functions.R')

## comparison gibrat/intgib

systems = c('ZA','CN','BR','IN','RU')

#system='CN'
res=data.frame()
for(system in systems){
  show(system)
  cpop = pop[pop$system==system,]

  bestintgib = data.frame(cpop[cpop$model=='intgib'&cpop$logmse[cpop$model=='intgib']==min(cpop$logmse[cpop$model=='intgib']),])
  bestgib = data.frame(cpop[cpop$model=='gibrat'&cpop$logmse[cpop$model=='gibrat']==min(cpop$logmse[cpop$model=='gibrat']),])
  M1 = runGibrat(system,bestgib$growthRate)
  M2 = runIntgib(system,bestintgib$growthRate,bestintgib$gravityWeight,bestintgib$gravityGamma,bestintgib$gravityDecay)
  p1 = polFit(data.frame(X=M1$X,Y=M1$Y),1)
  p2 = polFit(data.frame(X=M2$X,Y=M2$Y),4)
  daiclogmse = AIC(p1) - AIC(p2);dbiclogmse = BIC(p1) - BIC(p2)
  
  bestintgib = data.frame(cpop[cpop$model=='intgib'&cpop$mselog[cpop$model=='intgib']==min(cpop$mselog[cpop$model=='intgib']),])
  bestgib = data.frame(cpop[cpop$model=='gibrat'&cpop$mselog[cpop$model=='gibrat']==min(cpop$mselog[cpop$model=='gibrat']),])
  M1 = runGibrat(system,bestgib$growthRate)
  M2 = runIntgib(system,bestintgib$growthRate,bestintgib$gravityWeight,bestintgib$gravityGamma,bestintgib$gravityDecay)
  p1 = polFit(data.frame(X=M1$X,Y=M1$Y),1)
  p2 = polFit(data.frame(X=M2$X,Y=M2$Y),4)
  daicmselog = AIC(p1) - AIC(p2);dbicmselog = BIC(p1) - BIC(p2)
  
  res=rbind(res,c(daiclogmse,dbiclogmse,daicmselog,dbicmselog))
}

knitr::kable(cbind(systems,res))



systems = c('US')

resmarius=data.frame()
for(system in systems){
  show(system)
  cpop = pop[pop$system==system,]
  
  bestmarius = data.frame(cpop[cpop$model=='marius'&cpop$logmse[cpop$model=='marius']==min(cpop$logmse[cpop$model=='marius']),])
  bestmariusrestr = data.frame(cpop[cpop$model=='mariusrestr'&cpop$logmse[cpop$model=='mariusrestr']==min(cpop$logmse[cpop$model=='mariusrestr']),])
  M1 = runRestrMarius(system,bestmariusrestr$economicMultiplier,bestmariusrestr$sizeEffectOnSupply,bestmariusrestr$sizeEffectOnDemand,bestmariusrestr$distanceDecay)
  M2 = runMarius(system,bestmarius$economicMultiplier,bestmarius$sizeEffectOnSupply,bestmarius$sizeEffectOnDemand,bestmarius$distanceDecay,bestmarius$populationToWealthExponent,bestmarius$wealthToPopulationExponent)
  p1 = polFit(data.frame(X=M1$X,Y=M1$Y),4)
  p2 = polFit(data.frame(X=M2$X,Y=M2$Y),6)
  daiclogmse = AIC(p1) - AIC(p2);dbiclogmse = BIC(p1) - BIC(p2)
  
  bestmarius = data.frame(cpop[cpop$model=='marius'&cpop$mselog[cpop$model=='marius']==min(cpop$mselog[cpop$model=='marius']),])
  bestmariusrestr = data.frame(cpop[cpop$model=='mariusrestr'&cpop$mselog[cpop$model=='mariusrestr']==min(cpop$mselog[cpop$model=='mariusrestr']),])
  M1 = runRestrMarius(system,bestmariusrestr$economicMultiplier,bestmariusrestr$sizeEffectOnSupply,bestmariusrestr$sizeEffectOnDemand,bestmariusrestr$distanceDecay)
  M2 = runMarius(system,bestmarius$economicMultiplier,bestmarius$sizeEffectOnSupply,bestmarius$sizeEffectOnDemand,bestmarius$distanceDecay,bestmarius$populationToWealthExponent,bestmarius$wealthToPopulationExponent)
  p1 = polFit(data.frame(X=M1$X,Y=M1$Y),4)
  p2 = polFit(data.frame(X=M2$X,Y=M2$Y),6)
  daicmselog = AIC(p1) - AIC(p2);dbicmselog = BIC(p1) - BIC(p2)
  
  resmarius=rbind(resmarius,c(daiclogmse,dbiclogmse,daicmselog,dbicmselog))
}

#knitr::kable(cbind(systems,resmarius))
# -> select points on pareto front ?



