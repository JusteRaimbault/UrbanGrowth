
library(sp)


runmodel<-function(args){
  system(paste0('sbt "run ',args,'"'))
  sim = read.csv(file='res/popsim.csv',header = F)
  X=c();Y=c()
  for(j in 2:ncol(sim)){X=append(X,sim[,j-1]);Y=append(Y,sim[,j])}
  # read.csv(file='res/poptarget.csv',header=F)
  return(list(X=X,Y=Y))
}

runRestrMarius<-function(system,economicMultiplier,sizeEffectOnSupply,sizeEffectOnDemand,distanceDecay){
  return(runmodel(paste0(c("Marius ",system,economicMultiplier,sizeEffectOnSupply,sizeEffectOnDemand,distanceDecay),collapse = ' ')))
}

runMarius<-function(system,economicMultiplier,sizeEffectOnSupply,sizeEffectOnDemand,distanceDecay,wealthToPopulationExponent,populationToWealthExponent){
  return(runmodel(paste0(c("Marius ",system,economicMultiplier,sizeEffectOnSupply,sizeEffectOnDemand,distanceDecay,wealthToPopulationExponent,populationToWealthExponent),collapse = ' ')))
}

runGibrat<-function(system,growthRate){
  return(runmodel(paste0(c("Coevolution ",system,growthRate),collapse=' ')))
}

runIntgib<-function(system,growthRate,gravityWeight,gravityGamma,gravityDecay){
  return(runmodel(paste0(c("Coevolution ",system,growthRate,gravityWeight,gravityGamma,gravityDecay),collapse=' ')))
}

#'
#' Statistical polynomial fit
polFit<-function(data,params){
  form = "Y~a1+a2*X"
  start=list(a1=0,a2=0)
  for(k in 1:(params-2)){
    form=paste0(form,"+a",(k+2),"*X^",(k+1))
    start[[paste0("a",k+2)]]=0
  }
  statfit = nls(as.formula(form),data,start=start)
  return(statfit)
  #return(AIC(statfit))
}



#'
#' Get successive distances between pareto fronts
#'
frontDiffs <- function(popdirectory,objectives=c('logmse','mselog')){
  files = sort(list.files(popdirectory))
  #show(files)
  gens = sapply(strsplit(files,'population'),function(l){sapply(strsplit(l[2],split = '.',fixed = T),function(l){as.numeric(l[1])})})
  dists = c()
  for(i in 2:length(files)){
    prevfront=read.csv(file=paste0(popdirectory,'/',files[i-1]))
    currentfront=read.csv(file=paste0(popdirectory,'/',files[i]))
    #dists = append(dists,sum(c(dist(as.matrix(prevfront[,objectives]),as.matrix(currentfront[,objectives])))))
    owndistprev = mean(c(dist(as.matrix(prevfront[,objectives]))))
    owndistcurrent = mean(c(dist(as.matrix(currentfront[,objectives]))))
    betweendist = mean(c(spDists(as.matrix(prevfront[,objectives]),as.matrix(currentfront[,objectives]))))
    dists = append(dists,2*betweendist/(owndistprev+owndistcurrent))
  }
  return(list(gens=gens[2:length(gens)],dists=dists))
}




