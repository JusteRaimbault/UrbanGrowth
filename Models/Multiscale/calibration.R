
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole'))

library(dplyr)
library(ggplot2)
library(reshape2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))
source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))


resPrefix = '20190930_0418_CALIBRATION_GRID'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Multiscale/',resPrefix,'/');dir.create(resdir)

# TODO getLatestGen
res <- as.tbl(read.csv(paste0(resdir,'data/population4100.csv'),stringsAsFactors = F))

params = c("macroGrowthRate","macroInteractionDecay","macroInteractionGamma","macroInteractionWeight",
           "mesoAlpha","mesoBeta","mesoNdiff","mesoTimeSteps",
           "macroMesoAlphaUpdateMax","macroMesoBetaUpdateMax","mesoMacroCongestionCost","mesoMacroDecayUpdateMax"
)

objectives = c("macroHierarchy","mesoAggregation","mesoDistance")

#plot(res[,objectives])
# strange : - how can mesoDistance be negative ? ; - aggreg should be absolute value (and minimized : here maximize hierarchy)
# -> stop anyway
# errors : Caused by: java.lang.AssertionError: assertion failed: existing null meso pop grids
# 1y1m17d5h

#g=ggplot(res,aes(x=macroHierarchy,y=mesoAggregation,size=evolution.samples,color=macroMesoAlphaUpdateMax))
#g+geom_point(alpha=0.4)

g=ggplot(res,aes(x=macroHierarchy,y=mesoAggregation,size=evolution.samples,color=macroMesoBetaUpdateMax))
g+geom_point(alpha=0.6)+stdtheme+
  scale_size_continuous(name='Samples')+scale_color_continuous(name=expression(delta[beta]))+
  xlab('Macroscopic populations hierarchy')+ylab('Mesoscopic average hierarchy')
ggsave(file=paste0(resdir,'pareto_macroHierarchy-mesoHierarchy.png'),width=20,height=18,units='cm')


g=ggplot(res[res$evolution.samples>5,],aes(x=macroHierarchy,y=mesoAggregation,size=evolution.samples,color=macroMesoBetaUpdateMax))
g+geom_point(alpha=0.6)+stdtheme+
  scale_size_continuous(name='Samples')+scale_color_continuous(name=expression(delta[beta]))+
  xlab('Macroscopic populations hierarchy')+ylab('Mesoscopic average hierarchy')
ggsave(file=paste0(resdir,'pareto_macroHierarchy-mesoHierarchy_5samples.png'),width=20,height=18,units='cm')



