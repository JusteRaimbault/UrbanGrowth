
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole/calibration'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))

# parameters : where calibration results are stored and where to store result figures
sourcedir = ''
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Calibration/all/')

latestgen <- function(dir){
  max(as.integer(sapply(strsplit(sapply(strsplit(
    list.files(dir,pattern=".csv"),"population"),function(s){s[2]}),".csv"),function(s){s[1]})))}

# population dirs listed by hand

models = c('intgib','innovation','gibrat','innovationext','marius','intgibphysical','mariusrestr')
systems = c('ZA','CN','US','BR','EU','IN','RU')

# could take only the latest here ? not viable as older can be better
popdirs = list(
  'intgib_ZA'='CALIB_intgib_ZA_20181004_115105',
  'intgib_CN'='CALIB_intgib_CN_20181003_204750',
  'intgib_US'='CALIB_intgib_US_20181004_154542',
  'intgib_BR'='CALIB_GRID_intgib_BR_20181209_185930',
  'intgib_EU'='CALIB_intgib_EU_20181004_163326',
  'intgib_IN'='CALIB_intgib_IN_20181004_014849',
  'intgib_RU'='CALIB_intgib_RU_20181004_064958',
  'innovation_ZA'='CALIB_innovation_ZA_20181013_032337',
  'innovation_CN'='CALIB_innovation_CN_20181012_012821',
  'innovation_US'='CALIB_innovation_US_20181013_115954',
  'innovation_BR'='CALIB_innovation_BR_20181011_171420',
  'innovation_EU'='CALIB_innovation_EU_20181013_204045',
  'innovation_IN'='CALIB_innovation_IN_20181012_101107',
  'innovation_RU'='CALIB_innovation_RU_20181012_184655',
  'gibrat_ZA'='CALIB_gibrat_ZA_20181004',
  'gibrat_CN'='CALIB_gibrat_CN_20180922',
  'gibrat_US'='CALIB_gibrat_US_20181004',
  'gibrat_BR'='CALIB_gibrat_BR_20180922',
  'gibrat_EU'='CALIB_gibrat_EU_20181004',
  'gibrat_IN'='CALIB_gibrat_IN_20180922',
  'gibrat_RU'='CALIB_gibrat_RU_20180922',
  'innovationext_ZA'='CALIB_innovationext_ZA_20180922_082545',
  'innovationext_CN'='CALIB_innovationext_CN_20181014_133044',
  'innovationext_US'='CALIB_innovationext_US_20180922_092709',
  'innovationext_BR'='CALIB_innovationext_BR_20181014_050644',
  'innovationext_EU'='CALIB_innovationext_EU_20180922_102836',
  'innovationext_IN'='CALIB_innovationext_IN_20181014_220028',
  'innovationext_RU'='CALIB_innovationext_RU_20180922_072418',
  'marius_ZA'='CALIB_marius_ZA_20180923_012414',
  'marius_CN'='CALIB_marius_CN_20180922_235109',
  'marius_US'='CALIB_marius_US_20180923_015517',
  'marius_BR'='CALIB_marius_BR_20180922_232008',
  'marius_EU'='CALIB_marius_EU_20180923_022621',
  'marius_IN'='CALIB_marius_IN_20180923_002208',
  'marius_RU'='CALIB_marius_RU_20180923_005308',
  'intgibphysical_ZA'='CALIB_intgibphysical_ZA_20180922_145319',
  'intgibphysical_CN'='CALIB_intgibphysical_CN_20180922_132014',
  'intgibphysical_US'='CALIB_intgibphysical_US_20180922_152420',
  'intgibphysical_BR'='CALIB_intgibphysical_BR_20180922_124915',
  'intgibphysical_EU'='CALIB_intgibphysical_EU_20180922_155520',
  'intgibphysical_IN'='CALIB_intgibphysical_IN_20180922_135116',
  'intgibphysical_RU'='CALIB_intgibphysical_RU_20180922_142217',
  'mariusrestr_ZA'='CALIB_mariusrestr_ZA_20180922_214658',
  'mariusrestr_CN'='CALIB_mariusrestr_CN_20180922_201353',
  'mariusrestr_US'='CALIB_mariusrestr_US_20180922_221801',
  'mariusrestr_BR'='CALIB_mariusrestr_BR_20180922_194248',
  'mariusrestr_EU'='CALIB_mariusrestr_EU_20180922_224907',
  'mariusrestr_IN'='CALIB_mariusrestr_IN_20180922_204500',
  'mariusrestr_RU'='CALIB_mariusrestr_RU_20180922_211602'
)

# systembounds=list(
#   'BR'=c('logmse'=40,'mselog'=1500),
#   'IN'=c('logmse'=33,'mselog'=300),
#   'ZA'=c('logmse'=40,'mselog'=25000),
#   'US'=c('logmse'=40,'mselog'=21000),
#   'RU'=c('logmse'=50,'mselog'=65000),
#   'EU'=c('logmse'=40,'mselog'=1000),
#   'CN'=c('logmse'=40,'mselog'=2000)
# )
systembounds=list(
  'BR'=c('logmse'=1000,'mselog'=1e10),
  'IN'=c('logmse'=1000,'mselog'=1e10),
  'ZA'=c('logmse'=1000,'mselog'=1e10),
  'US'=c('logmse'=1000,'mselog'=1e10),
  'RU'=c('logmse'=1000,'mselog'=1e10),
  'EU'=c('logmse'=1000,'mselog'=1e10),
  'CN'=c('logmse'=1000,'mselog'=1e10)
)

### CV check

#for(popdir in paste0(sourcedir,popdirs)){
#  fdiff = frontDiffs(popdir)
#  plot(fdiff$gens,fdiff$dists)
#}



### Analysis

populations = list()

popcolnames=c()
for(popname in names(popdirs)){
  currentfile=paste0(sourcedir,popdirs[[popname]],'/population',latestgen(paste0(sourcedir,popdirs[[popname]])),'.csv')
  show(currentfile)
  currentdata = read.csv(currentfile)
  show(dim(currentdata))
  populations[[popname]] = currentdata
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
g+geom_point(pch='+',alpha=0.9)+facet_wrap(~system,scales = 'free')+
  stdtheme+ theme(legend.justification=c(1,0), legend.position=c(0.5,0.0))
ggsave(file=paste0(resdir,'allmodels_allsystems.png'),width=30,height = 20,units='cm')

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



####
# targeted plots

g = ggplot(pop[pop$system=='IN'&pop$logmse<32.5&pop$mselog<100,],aes(x = mselog,y=logmse,color=model))
g+geom_point(alpha=0.5)+stdtheme+ggtitle('India (zoomed)')
ggsave(file=paste0(resdir,'IN_zoomed.png'),width=22,height = 18,units='cm')

g = ggplot(pop[pop$system=='BR'&pop$mselog<200,],aes(x = mselog,y=logmse,color=model))
g+geom_point(alpha=0.5)+stdtheme+ggtitle('Brazil (zoomed)')
ggsave(file=paste0(resdir,'BR_zoomed.png'),width=22,height = 18,units='cm')

g = ggplot(pop[pop$system=='CN'&pop$mselog<220&pop$logmse<33.5,],aes(x = mselog,y=logmse,color=model))
g+geom_point(alpha=0.5)+stdtheme+ggtitle('China (zoomed)')
ggsave(file=paste0(resdir,'CN_zoomed.png'),width=22,height = 18,units='cm')



