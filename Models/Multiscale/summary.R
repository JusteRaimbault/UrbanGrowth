
#####
# summary of large result datasets

setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole'))

library(dplyr)
library(ggplot2)

#source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))
source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))


#resPrefix = '20190429_193158_MULTISCALE_GRID_GRID'
#resPrefix = '20190506_135221_MULTISCALE_TARGETEDGRID_GRID'
#resPrefix = '20190919_161009_MULTISCALE_TARGETEDGRID_GRID'
resPrefix = '20190919_141607_MULTISCALE_TARGETEDGRID_GRID'
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

macrocols = c()
for(macroindic in macroindics){
  macrocols=append(macrocols,paste0(macroindic,0:(ncities-1)))
  macrocols=append(macrocols,paste0(macroindic,(ncities*tsteps):(ncities*(tsteps+1) - 1)))
}
mesocols = c()
for(mesoindic in mesoindics){
  mesocols=append(mesocols,paste0(mesoindic,0:(ncities-1)))
  mesocols=append(mesocols,paste0(mesoindic,(ncities*tsteps):(ncities*(tsteps+1) - 1)))
}

filtered = res[,c("id","replication",params,macrocols,mesocols)]

write.csv(filtered,file=paste0(resdir,'data/',resPrefix,'_filtered.csv'),row.names = F)

# 
# for(macroindic in macroindics){
#   res[[paste0("deltaHierarchy",macroindic)]] = apply(res[,paste0(macroindic,0:(ncities-1))],2,hierarchy) - apply(res[,paste0(macroindic,(ncities*(tsteps-1)):(ncities*tsteps - 1))],2,hierarchy)
# }
# 
# for(mesoindic in mesoindics){
#   res[[paste0("delta",mesoindic)]] = apply(res[,paste0(mesoindic,0:(ncities-1))],2,mean) - apply(res[,paste0(mesoindic,(ncities*(tsteps-1)):(ncities*tsteps - 1))],2,mean)
# }
# 
# hierarchiesMacroindics=paste0("deltaHierarchy",macroindics)
# deltaMesoindics=paste0("delta",mesoindics)
# 
# 
# sres = res %>% group_by(
#   macroGrowthRate,macroInteractionDecay,macroInteractionGamma,mesoAlpha,mesoBeta,mesoTimeSteps,
#   macroMesoAlphaUpdateMax,macroMesoBetaUpdateMax,mesoMacroCongestionCost,mesoMacroDecayUpdateMax
# ) %>% summarise(
#   deltaHierarchymacroAccessibilitiesSd=sd(deltaHierarchymacroAccessibilities),
#   deltaHierarchymacroAccessibilities=mean(deltaHierarchymacroAccessibilities),
#   deltaHierarchymacroClosenessesSd=sd(deltaHierarchymacroClosenesses),
#   deltaHierarchymacroClosenesses=mean(deltaHierarchymacroClosenesses),
#   deltaHierarchymacroPopulationsSd=sd(deltaHierarchymacroPopulations),
#   deltaHierarchymacroPopulations=mean(deltaHierarchymacroPopulations),
#   deltamesoSlopesSd=sd(deltamesoSlopes),
#   deltamesoSlopes=mean(deltamesoSlopes),
#   deltamesoMoransSd=sd(deltamesoMorans),
#   deltamesoMorans=mean(deltamesoMorans),
#   deltamesoEntropySd=sd(deltamesoEntropy),
#   deltamesoEntropy=mean(deltamesoEntropy),
#   deltamesoDistancesSd=sd(deltamesoDistances),
#   deltamesoDistances=mean(deltamesoDistances),
#   count=n()
# )
# 
# 
# 
# save(sres,file=paste0(resdir,'data/',resPrefix,'_summary.RData'))
# 
