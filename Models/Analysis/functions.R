
library(sp)
#library(hypervolume)


hierarchy<-function(x){
  xx = x[x>0]
  if(length(xx)==0){return(0)}else{
    reg = lm(data=data.frame(logx = sort(log(xx),decreasing = T),logrank=log(1:length(xx))),logx~logrank)
    return(reg$coefficients[2])
  }
}

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
  gens=gens[gens>1000]
  dists = c()
  for(i in 2:length(gens)){
    show(i)
    prevfront=read.csv(file=paste0(popdirectory,'/population',gens[i-1],'.csv'))
    currentfront=read.csv(file=paste0(popdirectory,'/population',gens[i],'.csv'))
    #dists = append(dists,sum(c(dist(as.matrix(prevfront[,objectives]),as.matrix(currentfront[,objectives])))))
    owndistprev = mean(c(dist(as.matrix(prevfront[,objectives]))))
    owndistcurrent = mean(c(dist(as.matrix(currentfront[,objectives]))))
    betweendist = mean(c(spDists(as.matrix(prevfront[,objectives]),as.matrix(currentfront[,objectives]))))
    dists = append(dists,2*betweendist/(owndistprev+owndistcurrent))
  }
  return(list(gens=gens[2:length(gens)],dists=dists))
}



#'
#' Approximate hypervolumes for successive fronts
hypervolumes<-function(popdirectory,indics){
  files=list.files(popdirectory)
  
  #alldata = data.frame()
  #for(file in files){alldata=rbind(alldata,read.csv(file=paste0(popdirectory,'/',file)))}
  #sres=alldata[,indics]
  #for(j in 1:ncol(sres)){sres[,j]=(sres[,j]-min(sres[,j]))/(max(sres[,j])-min(sres[,j]))}
  #pca=prcomp(sres[,indics])
  
  gens = sort(sapply(strsplit(files,'population'),function(l){sapply(strsplit(l[2],split = '.',fixed = T),function(l){as.numeric(l[1])})}))
  gens=gens[gens>1000]
  vols = c()
  for(i in 1:length(gens)){
    show(i)
    currentfront=read.csv(file=paste0(popdirectory,'/population',gens[i],'.csv'))
    sres=currentfront[,indics]
    for(j in 1:ncol(sres)){sres[,j]=(sres[,j]-min(sres[,j]))/(max(sres[,j])-min(sres[,j]))}
    #rot=as.matrix(currentfront[,indics])%*%pca$rotation
    #vols=append(vols,get_volume(hypervolume(data=rot[,1:4],method = "box")))
    show(dim(sres))
    vols=append(vols,get_volume(hypervolume(data=sres,method = "box")))
  }
  
  return(list(gens=gens,vols=vols))
}


#'
#' Number of patterns
patterns<-function(popdirectory){
  files=list.files(popdirectory)
  gens = sort(sapply(strsplit(files,'population'),function(l){sapply(strsplit(l[2],split = '.',fixed = T),function(l){as.numeric(l[1])})}))
  pattnum=c()
  for(i in 2:length(gens)){ 
    show(i)
    currentfront=read.csv(file=paste0(popdirectory,'/population',gens[i],'.csv'))
    pattnum=append(pattnum,nrow(currentfront))
  }
  return(list(gens=gens[2:length(gens)],patterns=pattnum))
}


#'
#'
polygonVolumes <-function(popdirectory,bounds=c(Inf,Inf),objectives=c('logmse','mselog')){
  files=list.files(popdirectory)
  gens = sort(sapply(strsplit(files,'population'),function(l){sapply(strsplit(l[2],split = '.',fixed = T),function(l){as.numeric(l[1])})}))
  volumes=c()
  for(i in 2:length(gens)){ 
    show(i)
    currentfront=read.csv(file=paste0(popdirectory,'/population',gens[i],'.csv'))
    currentfront = currentfront[,objectives]
    for(j in 1:ncol(currentfront)){if(nrow(currentfront)>0){currentfront=currentfront[currentfront[,j]<bounds[j],]}}
    vol=0
    if(nrow(currentfront)>0){
      currentpol = Polygon(currentfront[c(chull(x=currentfront[,objectives[1]],y=currentfront[,objectives[2]]),1),],hole = F)
      vol=currentpol@area
    }
    volumes=append(volumes,nrow(currentfront))
  }
  return(list(gens=gens[2:length(gens)],volumes=volumes))
}







