
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

# parameters : where calibration results are stored and where to store result figures
sourcedir = 'calibration/'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Calibration/')

latestgen <- function(dir){max(as.integer(sapply(strsplit(sapply(strsplit(list.files(dir),"population"),function(s){s[2]}),".csv"),function(s){s[1]})))}

# population dirs listed by hand

models = c('intgib')
systems = c('ZA','CN','US')

popdirs = list(
  'intgib_ZA'='CALIB_intgib_ZA_20180919_052829',
  'intgib_CN'='CALIB_intgib_CN_20180918_231457',
  'intgib_US'='CALIB_intgib_US_20180918_175315'
)

populations = list()

for(popname in names(popdirs)){
  populations[[popname]] = read.csv(paste0(sourcedir,popdirs[[popname]],'/population',latestgen(paste0(sourcedir,popdirs[[popname]])),'.csv'))
}


# aggregate with model and system
pop = data.frame()
for(model in models){
  for(system in systems){
    currentdata = populations[[paste0(model,'_',system)]]
    if(!is.null(currentdata)){
      pop=rbind(pop,cbind(currentdata,model=rep(model,nrow(currentdata)),system=rep(system,nrow(currentdata))))
    }
  }
}
pop=as.tbl(pop)


#######

# compare all models and systems
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






