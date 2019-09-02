
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))
source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))


resPrefix = '20190429_193158_MULTISCALE_GRID_GRID'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Multiscale/',resPrefix,'/');dir.create(resdir)

res <- as.tbl(read.csv(paste0('exploration/',resPrefix,'.csv')))

params = c("macroGrowthRate","macroInteractionDecay","macroInteractionGamma",
           "mesoAlpha","mesoBeta","mesoTimeSteps",
           "macroMesoAlphaUpdateMax","macroMesoBetaUpdateMax","mesoMacroCongestionCost","mesoMacroDecayUpdateMax"
           )

macroindics = c("macroAccessibilities","macroClosenesses","macroPopulations")
mesoindics = c("mesoSlopes","mesoMorans","mesoEntropy","mesoDistances")

ncities = 20
tsteps = 5

for(macroindic in macroindics){
  res[[paste0("deltaHierarchy",macroindic)]] = apply(res[,paste0(macroindic,0:(ncities-1))],2,hierarchy) - apply(res[,paste0(macroindic,(ncities*(tsteps-1)):(ncities*tsteps - 1))],2,hierarchy)
}

for(mesoindic in mesoindics){
  res[[paste0("delta",mesoindic)]] = apply(res[,paste0(mesoindic,0:(ncities-1))],2,mean) - apply(res[,paste0(mesoindic,(ncities*(tsteps-1)):(ncities*tsteps - 1))],2,mean)
}

macroindics=paste0("deltaHierarchy",macroindics)
mesoindics=paste0("delta",mesoindics)


sres = res %>% group_by(
  macroGrowthRate,macroInteractionDecay,macroInteractionGamma,mesoAlpha,mesoBeta,mesoTimeSteps,
  macroMesoAlphaUpdateMax,macroMesoBetaUpdateMax,mesoMacroCongestionCost,mesoMacroDecayUpdateMax
) %>% summarise(deltaHierarchymacroAccessibilities=mean(deltaHierarchymacroAccessibilities),
deltaHierarchymacroClosenesses=mean(deltaHierarchymacroClosenesses),
deltaHierarchymacroPopulations=mean(deltaHierarchymacroPopulations),
deltamesoSlopes=mean(deltamesoSlopes),deltamesoMorans=mean(deltamesoMorans),
deltamesoEntropy=mean(deltamesoEntropy),deltamesoDistances=mean(deltamesoDistances)
)


sres=sres[sres$macroGrowthRate==0.0&sres$macroMesoAlphaUpdateMax==0.1&sres$macroInteractionGamma==5.0&sres$mesoAlpha==0.5&sres$mesoBeta==0.1&sres$mesoMacroCongestionCost==2.0,]


g=ggplot(sres,aes(x=macroInteractionDecay,y=deltaHierarchymacroAccessibilities,group=mesoTimeSteps,color=mesoTimeSteps))
g+geom_line()+facet_grid(mesoMacroDecayUpdateMax~macroMesoBetaUpdateMax)+stdtheme
ggsave(file=paste0(resdir,'deltaHierarchyAccessibility.png'),width=20,height=18,units='cm')

g=ggplot(sres,aes(x=macroInteractionDecay,y=deltamesoMorans,group=mesoTimeSteps,color=mesoTimeSteps))
g+geom_line()+facet_grid(mesoMacroDecayUpdateMax~macroMesoBetaUpdateMax,scales="free")
ggsave(file=paste0(resdir,'deltaMoran.png'),width=20,height=18,units='cm')

g=ggplot(sres,aes(x=macroInteractionDecay,y=deltamesoDistances,group=mesoTimeSteps,color=mesoTimeSteps))
g+geom_line()+facet_grid(mesoMacroDecayUpdateMax~macroMesoBetaUpdateMax,scales="free")
ggsave(file=paste0(resdir,'deltaDistances.png'),width=20,height=18,units='cm')





