
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Calibration'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))

# parameters : where calibration results are stored and where to store result figures
sourcedir = ''
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Calibration/world/');dir.create(resdir)

latestgen <- function(dir){
  max(as.integer(sapply(strsplit(sapply(strsplit(
    list.files(dir,pattern=".csv"),"population"),function(s){s[2]}),".csv"),function(s){s[1]})))}

# population dirs listed by hand

allmodels = c("gibrat","innovation","innovationext","intgib","intgibphysical","marius","mariusrestr")
models = c('intgib','innovation','gibrat','mariusrestr','innovationext','marius')
#systems = c('ZA','CN','US','BR','EU','IN','RU')
systems=c('world')

# could take only the latest here ? not viable as older can be better
popdirs = list(
  'intgib_world'='20190904_172108_CALIB_GRID_intgib_world',
  'innovation_world'='20190904_152508_CALIB_GRID_innovation_world',
  'gibrat_world'='20190904_CALIB_GRID_gibrat_world',
  'mariusrestr_world'='20190904_171935_CALIB_GRID_mariusrestr_world',
  'marius_world'='20190906_182401_CALIB_GRID_marius_world',
  'innovationext_world'='20190906_182354_CALIB_GRID_innovationext_world'
)


systembounds=list(
  'world'=c('logmse'=1000,'mselog'=1e10)
)


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

pop$model = factor(pop$model,levels = allmodels)

#######

# compare all models and systems

#g = ggplot(pop,aes(x = mselog,y=logmse,color=model))
#g+geom_point(pch='+',alpha=0.9)+
#  stdtheme#+ theme(legend.justification=c(1,0), legend.position=c(0.5,0.0))
#ggsave(file=paste0(resdir,'allmodels_world.png'),width=30,height = 20,units='cm')

#g = ggplot(pop,aes(x = mselog,y=logmse,color=as.character(gravityWeight>0)))
#g+geom_point()+facet_wrap(~system,scales = 'free')

# -> checking when baseline is better than interaction model




## exploration of influence of parameters

#g=ggplot(pop[pop$gravityWeight>0,],aes(x=gravityWeight))
#g+geom_point(aes(y=logmse,color=gravityDecay))+scale_x_log10()#+geom_line(aes(y=mselog),col=2)

#g=ggplot(pop[pop$gravityWeight>0,],aes(x=gravityWeight))
#g+geom_point(aes(y=mselog,color=gravityDecay))+scale_x_log10()#+geom_line(aes(y=mselog),col=2)


#g=ggplot(pop[!is.na(pop$gravityGamma),],aes(x=gravityDecay))
#g+geom_point(aes(y=mselog,color=gravityGamma))

#g=ggplot(pop[!is.na(pop$gravityGamma),],aes(x=gravityDecay))
#g+geom_point(aes(y=logmse,color=gravityGamma))
# counter intuitive ?


####
# Result plots

cols = gg_color_hue(length(levels(pop$model)));names(cols)<-levels(pop$model)
pop$modelcolor = cols[pop$model]

currentcols = pop$modelcolor;names(currentcols)<-pop$model
g = ggplot(pop,aes(x = mselog,y=logmse,color=model))
g+geom_point(alpha=0.5)+stdtheme+
  xlab('Mean square error on log of populations')+ylab("Log of mean square error on populations")+
  stdtheme+guides(colour = guide_legend(override.aes = list(size=4,alpha=1),title = 'Model'))+
  scale_color_manual(values = currentcols)
ggsave(file=paste0(resdir,'allmodels_world.png'),width=22,height = 18,units='cm')


######
pop$normalizedGravityDecay = rep(NA,nrow(pop))
for(model in models){
  ma = max(pop$gravityDecay[pop$model==model]);mi=min(pop$gravityDecay[pop$model==model])
  if(!is.na(mi)){pop$normalizedGravityDecay[pop$model==model] = (pop$gravityDecay - mi) / (ma - mi)}
}

g = ggplot(pop[pop$system=='world'&!is.na(pop$normalizedGravityDecay),],aes(x = mselog,y=logmse,color=normalizedGravityDecay,shape=model))
g+geom_point(alpha=0.8)+stdtheme
ggsave(file=paste0(resdir,'allmodels_world_colorNormalizedGravityDecay.png'),width=22,height = 18,units='cm')









