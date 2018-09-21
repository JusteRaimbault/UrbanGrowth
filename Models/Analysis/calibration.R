
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))

# parameters : where calibration results are stored and where to store result figures
sourcedir = 'calibration/'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Calibration/')

latestgen <- function(dir){max(as.integer(sapply(strsplit(sapply(strsplit(list.files(dir),"population"),function(s){s[2]}),".csv"),function(s){s[1]})))}

# population dirs listed by hand

models = c('intgib','innovation','gibrat','innovationext')
systems = c('ZA','CN','US','BR','EU','IN','RU')

popdirs = list(
  'intgib_ZA'='CALIB_intgib_ZA_20180919_052829',
  'intgib_CN'='CALIB_intgib_CN_20180918_231457',
  'intgib_US'='CALIB_intgib_US_20180918_175315',
  'intgib_BR'='CALIB_intgib_BR_20180919_143214',
  'intgib_EU'='CALIB_intgib_EU_20180919_163321',
  'intgib_IN'='CALIB_intgib_IN_20180919_183427',
  'intgib_RU'='CALIB_intgib_RU_20180919_203608',
  'innovation_ZA'='CALIB_innovation_ZA_20180920_064141',
  'innovation_CN'='CALIB_innovation_CN_20180920_003846',
  'innovation_US'='CALIB_innovation_US_20180920_084300',
  'innovation_BR'='CALIB_innovation_BR_20180919_223752',
  #'innovation_EU'='',
  'innovation_IN'='CALIB_innovation_IN_20180920_023945',
  'innovation_RU'='CALIB_innovation_RU_20180920_044043',
  'gibrat_ZA'='CALIB_gibrat_ZA_20180920',
  #'gibrat_CN'='',
  'gibrat_US'='CALIB_gibrat_US_20180920',
  'gibrat_BR'='CALIB_gibrat_BR_20180920',
  'gibrat_EU'='CALIB_gibrat_EU_20180920',
  'gibrat_IN'='CALIB_gibrat_IN_20180920',
  'gibrat_RU'='CALIB_gibrat_RU_20180920',
  'innovationext_ZA'='CALIB_innovationext_ZA_20180921_081251',
  'innovationext_CN'='CALIB_innovationext_CN_20180921_020951',
  #'innovationext_US'='',
  'innovationext_BR'='CALIB_innovationext_BR_20180921_000851',
  #'innovationext_EU'='',
  'innovationext_IN'='CALIB_innovationext_IN_20180921_041049',
  'innovationext_RU'='CALIB_innovationext_RU_20180921_061152'
)

systembounds=list(
  'BR'=c('logmse'=40,'mselog'=1500),
  'IN'=c('logmse'=33,'mselog'=300),
  'ZA'=c('logmse'=40,'mselog'=25000),
  'US'=c('logmse'=40,'mselog'=21000),
  'RU'=c('logmse'=50,'mselog'=65000),
  'EU'=c('logmse'=40,'mselog'=1000),
  'CN'=c('logmse'=40,'mselog'=2000)
)

### CV check

for(popdir in paste0(sourcedir,popdirs)){
  fdiff = frontDiffs(popdir)
  plot(fdiff$gens,fdiff$dists)
}



### Analysis

populations = list()

popcolnames=c()
for(popname in names(popdirs)){
  populations[[popname]] = read.csv(paste0(sourcedir,popdirs[[popname]],'/population',latestgen(paste0(sourcedir,popdirs[[popname]])),'.csv'))
  popcolnames=append(popcolnames,colnames(populations[[popname]]))
}
popcolnames=unique(c(popcolnames,'model','system'))

# aggregate with model and system
pop = data.frame(matrix(rep(0,length(popcolnames)),ncol=length(popcolnames)))
colnames(pop)=popcolnames
for(model in models){
  for(system in systems){
    currentdata = populations[[paste0(model,'_',system)]]
    if(!is.null(currentdata)){
      show(paste0(model,'_',system))
      show(dim(currentdata))
      for(col in popcolnames[!popcolnames%in%colnames(currentdata)]){currentdata[,col]=rep(NA,nrow(currentdata))}
      currentdata[,"system"]=rep(system,nrow(currentdata));currentdata[,"model"]=rep(model,nrow(currentdata))
      #filter with system bounds
      currentdata = currentdata[currentdata$logmse<systembounds[[system]][['logmse']]&currentdata$mselog<systembounds[[system]][['mselog']],]
      #pop=rbind(pop,cbind(currentdata,model=rep(model,nrow(currentdata)),system=rep(system,nrow(currentdata))))
      pop=rbind(pop,currentdata)
    }
  }
}
pop=as.tbl(pop[2:nrow(pop),])


#######

# compare all models and systems

g = ggplot(pop,aes(x = mselog,y=logmse,color=model))
g+geom_point()+facet_wrap(~system,scales = 'free')

g = ggplot(pop,aes(x = mselog,y=logmse,color=as.character(gravityWeight>0)))
g+geom_point()+facet_wrap(~system,scales = 'free')

# -> checking when baseline is better than interaction model



## exploration of influence of parameters

g=ggplot(pop[pop$gravityWeight>0,],aes(x=gravityWeight))
g+geom_point(aes(y=logmse,color=gravityDecay))+scale_x_log10()#+geom_line(aes(y=mselog),col=2)

g=ggplot(pop,aes(x=gravityDecay))
g+geom_point(aes(y=logmse,color=gravityWeight))+#+scale_x_log10()#+geom_line(aes(y=mselog),col=2)
  facet_wrap(~cut(pop$growthRate,5))

g=ggplot(pop[pop$gravityWeight>0,],aes(x=gravityWeight))
g+geom_point(aes(y=mselog,color=gravityDecay))+scale_x_log10()+#+geom_line(aes(y=mselog),col=2)
  facet_wrap(~cut(pop[pop$gravityWeight>0,]$growthRate,5))

g=ggplot(pop,aes(x=gravityDecay))
g+geom_point(aes(y=mselog,color=gravityGamma))+#+scale_x_log10()#+geom_line(aes(y=mselog),col=2)
  facet_wrap(~cut(pop$growthRate,5))
  # decay has no impact on fit ?






