
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole'))

library(dplyr)
library(ggplot2)

#source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))
source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))


#resPrefix = '20190429_193158_MULTISCALE_GRID_GRID'
#resPrefix = '20190506_135221_MULTISCALE_TARGETEDGRID_GRID'
resPrefix = '20190919_161009_MULTISCALE_TARGETEDGRID_GRID'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Multiscale/',resPrefix,'/');dir.create(resdir)

#res <- as.tbl(read.csv(paste0('exploration/',resPrefix,'.csv')))
res <- as.tbl(read.csv(paste0(resdir,'data/',resPrefix,'.csv'),stringsAsFactors = F))

params = c("macroGrowthRate","macroInteractionDecay","macroInteractionGamma",
           "mesoAlpha","mesoBeta","mesoTimeSteps",
           "macroMesoAlphaUpdateMax","macroMesoBetaUpdateMax","mesoMacroCongestionCost","mesoMacroDecayUpdateMax"
           )

macroindics = c("macroAccessibilities","macroClosenesses","macroPopulations")
mesoindics = c("mesoSlopes","mesoMorans","mesoEntropy","mesoDistances")

#ncities = 20
#tsteps = 5
ncities = 20
tsteps = 20

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
) %>% summarise(
  deltaHierarchymacroAccessibilitiesSd=sd(deltaHierarchymacroAccessibilities),
  deltaHierarchymacroAccessibilities=mean(deltaHierarchymacroAccessibilities),
  deltaHierarchymacroClosenessesSd=sd(deltaHierarchymacroClosenesses),
  deltaHierarchymacroClosenesses=mean(deltaHierarchymacroClosenesses),
  deltaHierarchymacroPopulationsSd=sd(deltaHierarchymacroPopulations),
  deltaHierarchymacroPopulations=mean(deltaHierarchymacroPopulations),
  deltamesoSlopesSd=sd(deltamesoSlopes),
  deltamesoSlopes=mean(deltamesoSlopes),
  deltamesoMoransSd=sd(deltamesoMorans),
  deltamesoMorans=mean(deltamesoMorans),
  deltamesoEntropySd=sd(deltamesoEntropy),
  deltamesoEntropy=mean(deltamesoEntropy),
  deltamesoDistancesSd=sd(deltamesoDistances),
  deltamesoDistances=mean(deltamesoDistances)
)


#save(sres,file=paste0(resdir,'data/',resPrefix,'_summary.RData'))
load(paste0(resdir,'data/',resPrefix,'_summary.RData'))

#sres=sres[sres$macroGrowthRate==0.0&sres$macroMesoAlphaUpdateMax==0.1&sres$macroInteractionGamma==5.0&sres$mesoAlpha==0.5&sres$mesoBeta==0.1&sres$mesoMacroCongestionCost==2.0,]

###
# compute sharpe ratios (cv ?)
sharpes <- sres %>% transmute(
  deltaHierarchymacroAccessibilitiesSharpe = abs(deltaHierarchymacroAccessibilities / deltaHierarchymacroAccessibilitiesSd),
  deltaHierarchymacroClosenessesSharpe = abs(deltaHierarchymacroClosenesses / deltaHierarchymacroClosenessesSd),
  deltaHierarchymacroPopulationsSharpe = abs(deltaHierarchymacroPopulations / deltaHierarchymacroPopulationsSd),
  deltamesoSlopesSharpe = abs(deltamesoSlopes / deltamesoSlopesSd),
  deltamesoMoransSharpe = abs(deltamesoMorans / deltamesoMoransSd),
  deltamesoEntropySharpe = abs(deltamesoEntropy / deltamesoEntropySd),
  deltamesoDistancesSharpe = abs(deltamesoDistances / deltamesoDistancesSd)
)
summary(sharpes)
# -> well converged with 50 replications given the relative size of stds


g=ggplot(sres,aes(x=macroInteractionDecay,y=deltaHierarchymacroAccessibilities,group=mesoTimeSteps,color=mesoTimeSteps))
g+geom_line()+facet_grid(mesoMacroDecayUpdateMax~macroMesoBetaUpdateMax)+stdtheme
#ggsave(file=paste0(resdir,'deltaHierarchyAccessibility-macroInteractionDecay_col-mesoTimeSteps_facet-mesoMacroDecayUpdateMax-macroMesoBetaUpdateMax.png'),width=20,height=18,units='cm')
# 

g=ggplot(sres,aes(x=macroInteractionDecay,y=deltamesoMorans,group=mesoTimeSteps,color=mesoTimeSteps))
g+geom_line()+facet_grid(mesoMacroDecayUpdateMax~macroMesoBetaUpdateMax,scales="free")
# ggsave(file=paste0(resdir,'deltaMesoMoran-macroInteractionDecay_col-mesoTimeSteps_facet-mesoMacroDecayUpdateMax-macroMesoBetaUpdateMax.png'),width=20,height=18,units='cm')
# 
g=ggplot(sres,aes(x=macroInteractionDecay,y=deltamesoDistances,group=mesoTimeSteps,color=mesoTimeSteps))
g+geom_line()+facet_grid(mesoMacroDecayUpdateMax~macroMesoBetaUpdateMax,scales="free")
# ggsave(file=paste0(resdir,'deltaMesoDistances-macroInteractionDecay_col-mesoTimeSteps_facet-mesoMacroDecayUpdateMax-macroMesoBetaUpdateMax.png'),width=20,height=18,units='cm')
# 


###
# targeted calibration from this ? : minimize sprawl while maximizing access ?
# compromise "meso opt / macro opt" ? (macro access ?)








