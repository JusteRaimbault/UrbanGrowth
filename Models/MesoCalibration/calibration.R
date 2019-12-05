
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoCalibration'))

library(dplyr)
library(ggplot2)

source('functions.R')

resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/MesoCalibration/');
dir.create(resdir);


ucdbsf <- loadUCDBData(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0')

morphos <- read.csv(file='configs/morphologies.csv',header = T,sep = ";")
inds = c(1:946,948:1000);areasmorph = ucdbsf[inds,];areasmorph$areaid = inds
areasmorph = as.data.frame(left_join(areasmorph,morphos,by=c('areaid'='areaid')))


latestgen <- function(dir){
  if(is.null(dir)){return(NULL)}
  else{return(max(as.integer(sapply(strsplit(sapply(strsplit(list.files(dir,pattern=".csv"),"population"),function(s){s[2]}),".csv"),function(s){s[1]}))))}
}
latestdir <- function(row,year){
  dirs = list.dirs('calibration')
  rows = unlist(sapply(strsplit(sapply(strsplit(dirs[2:length(dirs)],split = '/'),function(r){r[2]}),split='_'),
                       function(r){if(as.numeric(r[4])==row&as.numeric(r[5])==year){return(list(r=r,sort=paste0(r[6],r[7])))}}
  ))
  if(is.null(rows)){return(NULL)}
  m = matrix(rows,ncol=8,byrow=T)
  latest = order(m[,8],decreasing = T)[1]
  return(paste0('calibration/',paste(m[latest,1:7],collapse='_')))
}
# latestgen(latestdir(row,year))

# missing: 1-1975; 204-1990; 275-2000; 309-2015; 336-1990; 385-2000; 414-2015
allrestmp = data.frame()
for(row in 415:nrow(morphos)) {for(year in c(1990,2000,2015)){
  dir = latestdir(row,year);gen=latestgen(dir)
  show(paste0(row,'-',year,':',gen))
  if(!is.null(gen)){
    currentdata = read.csv(file=paste0(dir,'/population',gen,'.csv'))
    allrestmp = rbind(allrestmp,cbind(areaid = rep(morphos[row,"areaid"],nrow(currentdata)),year = rep(year,nrow(currentdata)),currentdata))
  }
}}


timedf = as_temporal_df(areasmorph,indics)
allres <- left_join(allrestmp,timedf[timedf$year>1975,],by=c('areaid'='areaid','year'='year')) # %>%
allres <- allres %>%  mutate(relpopfit = popfit / (totalPop^2))

#####
# Examples of Pareto fronts
# -> on 100-200 seems fine - even if few replications
dir.create(paste0(resdir,'Examples'))

ids = c(1,10,50,100,200,300)
for(currentid in ids){
  currentd = getParetoFront(allres[allres$areaid==currentid,],"indicsfit","relpopfit")
  areaname = strsplit(areasmorph$UC_NM_LST[areasmorph$areaid==currentid],",")[[1]][1]
  
  g=ggplot(currentd,aes(x=relpopfit,y=indicsfit,color=alpha.x,size=evolution.samples))
  g+geom_point(alpha=0.5)+xlab("Population fit")+ylab("Indicators fit")+scale_size_continuous(name="Samples")+scale_color_continuous(name=expression(alpha))+ggtitle(areaname)+stdtheme
  ggsave(file=paste0(resdir,'Examples/pareto_indics-relpop_',currentid,'_coloralpha.png'),width=20,height=18,units='cm')
  
  g=ggplot(currentd,aes(x=relpopfit,y=indicsfit,color=beta,size=evolution.samples))
  g+geom_point(alpha=0.5)+xlab("Population fit")+ylab("Indicators fit")+scale_size_continuous(name="Samples")+scale_color_continuous(name=expression(alpha))+ggtitle(areaname)+stdtheme
  ggsave(file=paste0(resdir,'Examples/pareto_indics-relpop_',currentid,'_colorbeta.png'),width=20,height=18,units='cm')
  
  g=ggplot(currentd,aes(x=relpopfit,y=indicsfit,color=tsteps,size=evolution.samples))
  g+geom_point(alpha=0.5)+xlab("Population fit")+ylab("Indicators fit")+scale_size_continuous(name="Samples")+scale_color_continuous(name=expression(alpha))+ggtitle(areaname)+stdtheme
  ggsave(file=paste0(resdir,'Examples/pareto_indics-relpop_',currentid,'_colortsteps.png'),width=20,height=18,units='cm')
}

#####
# calibrated params

# Q should filter on number of samples? for indics only
sres = allres %>% group_by(areaid,year) %>% summarize(
  alphapop = mean(alpha.x[popfit<quantile(popfit,0.1)]),alphapopsd = sd(alpha.x[popfit<quantile(popfit,0.1)]),
  betapop =  mean(beta[popfit<quantile(popfit,0.1)]),betapopsd =  sd(beta[popfit<quantile(popfit,0.1)]),
  tstepspop =  mean(tsteps[popfit<quantile(popfit,0.1)]),tstepspop =  sd(tsteps[popfit<quantile(popfit,0.1)]),
  alphaindics = mean(alpha.x[indicsfit<quantile(indicsfit,0.1)&evolution.samples>2]),sdalphaindics = sd(alpha.x[indicsfit<quantile(indicsfit,0.1)&evolution.samples>2]),
  betaindics = mean(beta[indicsfit<quantile(indicsfit,0.1)&evolution.samples>2]),sdbetaindics = sd(beta[indicsfit<quantile(indicsfit,0.1)&evolution.samples>2]),
  tstepsindics = mean(tsteps[indicsfit<quantile(indicsfit,0.1)&evolution.samples>2]),sdtstepsindics = sd(tsteps[indicsfit<quantile(indicsfit,0.1)&evolution.samples>2]),
  meanalpha = mean(alpha.x),sdalpha = sd(alpha.x),
  meanbeta = mean(beta),sdbeta = sd(beta),
  meantsteps = mean(tsteps),sdtsteps = sd(tsteps)
)

# todo validation: stability/ variability/ saturation

#####
## Maps

dir.create(paste0(resdir,'Calibration'))

areasmorphesttmp <- left_join(timedf[timedf$year>1975,],sres,by=c('areaid'='areaid','year'='year'))
areasmorphest <- left_join(areasmorphesttmp,areasmorph,by=c("areaid"="areaid"))

years=c(1990,2000,2015)

params = c("alphapop","betapop","tstepspop","alphaindics","betaindics","tstepsindics","meanalpha","meanbeta","meantsteps")
paramnames = list("alphapop"=expression(alpha[P]),"betapop"=expression(beta[P]),"tstepspop"=expression(t[P]),"alphaindics"=expression(alpha[mu]),"betaindics"=expression(beta[mu]),"tstepsindics"=expression(t[mu]),"meanalpha"=expression(bar(alpha)),"meanbeta"=expression(bar(beta)),"meantsteps"=expression(bar(t)))

for(param in params){
  for(year in years){
  currentdata=areasmorphest[areasmorphest$year==year,]
  if(param=='meanalpha'){currentdata=currentdata[currentdata[,'meanalpha']<3,]}
  map(
    currentdata,
    param,
    "totalPop",
    paste0(resdir,'Calibration/mapparam_',param,'_',year,'.png'),
    legendtitle = paramnames[[param]],
    legendsizetitle = paste0('Population\n',year)
  )
  }
}
 
####
## Correlations

years = c(1990,2000,2015)

vars=c('P','B','E','G','DP','DB','DE','DG')
currenttimedf <- data.frame()
for(year in years){
  if(year>1990){currentvars = vars}else{currentvars = vars[c(-4,-8)]}
  currentd = areasmorph[,paste0(currentvars,substr(year,3,4))];names(currentd)<-currentvars
  if(year==1990){currentd$G=rep(NA,nrow(currentd));currentd$DG=rep(NA,nrow(currentd))}
  currenttimedf = rbind(currenttimedf,cbind(areaid = areasmorph$areaid,currentd,year=rep(year,nrow(currentd))))
}

cordata <- left_join(sres,currenttimedf,by=c("areaid"="areaid","year"="year"))

corvars = c('P','B','E','G','meanalpha','meanbeta','meantsteps')
corvarsnames = c('Pop','Built','Emissions','GDP','alpha','beta','tsteps')
correlations(cordata,corvars,corvarsnames,years,fileprefix = paste0(resdir,'Calibration/var-params'),mode="long")

corvars = c('DP','DG','DE','DB','meanalpha','meanbeta','meantsteps')
corvarsnames = c('DeltaPop','DeltaGDP','DeltaEm','DeltaBuilt','alpha','beta','tsteps')
correlations(cordata,corvars,corvarsnames,years,fileprefix = paste0(resdir,'Calibration/deltavar-params'),mode="long")




####
## Trajectories in phase diag

areanames = sapply(strsplit(areasmorph$UC_NM_LST,";"),function(r){r[1]})

# load phase diags: (alpha,beta) stratified by tf

phasediag <- as.tbl(read.csv('exploration/20191204_225527_EXPLORATION_LOCAL4.csv'))
sphasediag <- phasediag %>% group_by(alpha,beta,tsteps) %>% summarize(
  avgDistance=mean(avgDistance),
  entropy=mean(entropy),
  moran=mean(moran),
  slope=mean(slope)
)

# restrict phase diag to beta < 0.1 (boundary for calibration)
sphasediag = sphasediag[sphasediag$beta<=0.1&sphasediag$alpha<=2,]

indics = c("avgDistance","entropy","moran","slope")
#indicnames =list("avgDistance","entropy","moran","slope")

tsteps= unique(sphasediag$tsteps);dists=sapply(cordata$meantsteps,function(t){min(abs(t-tsteps))})
for(indic in indics){
  for(tstep in tsteps){
    currentest = cordata[abs(cordata$meantsteps-tstep)==dists&cordata$meanalpha<max(sphasediag$alpha),]
    # summarize: average in each quadran
    qest = currentest%>% group_by(areaid) %>% summarize(deltaalpha = meanalpha[year==max(year)] - meanalpha[year==min(year)],deltabeta = meanbeta[year==max(year)] - meanbeta[year==min(year)]) %>% mutate(alphaf = sign(deltaalpha),betaf= sign(deltabeta)) %>% mutate(qid = paste0(alphaf,betaf))
    qavg = left_join(currentest,qest)%>%filter(qid!="00")%>% group_by(year,qid) %>% summarize(alpha=mean(meanalpha),beta=mean(meanbeta),pop=sum(P,na.rm=T))
    ggplot()+geom_raster(data=sphasediag[sphasediag$tsteps==tstep,],aes_string(x='alpha',y='beta',fill=indic))+scale_fill_distiller(palette = 'Spectral',na.value ='white')+
    #facet_wrap(~tsteps,scales = 'free') # better not to wrap for color scale
    #geom_point(data=currentest,aes(x=meanalpha,y=meanbeta,size=meantsteps),alpha=0.5)+
    #geom_line(data=currentest,aes(x=meanalpha,y=meanbeta,group=areaid),alpha=0.5)+
      geom_path(data=qavg,aes(x=alpha,y=beta,group=qid),arrow=arrow(angle=20, type = "closed",length = unit(0.05,'inches')))
      stdtheme
  ggsave(file=paste0(resdir,'Calibration/phasediag_',indic,'_tsteps',tstep,'.png'),width=20,height=18,units='cm')

  p15 = currentest[currentest$year==2015,]
  ids = p15$areaid[p15$P>=quantile(p15$P,0.98)]
  
  ggplot()+geom_raster(data=sphasediag[sphasediag$tsteps==tstep,],aes_string(x='alpha',y='beta',fill=indic))+scale_fill_distiller(palette = 'Spectral',na.value ='white')+
    geom_path(data=currentest[currentest$areaid%in%ids,],aes(x=meanalpha,y=meanbeta,group=areaid),arrow=arrow(angle=20, type = "closed",length = unit(0.05,'inches')))+
    #geom_text(data=cbind(p15[p15$areaid%in%ids,],name=areanames[ids]),aes(x=meanalpha,y=meanbeta,label=name))+
  stdtheme
  ggsave(file=paste0(resdir,'Calibration/phasediag_largestcities_',indic,'_tsteps',tstep,'.png'),width=20,height=18,units='cm')
  
  
  }
}






