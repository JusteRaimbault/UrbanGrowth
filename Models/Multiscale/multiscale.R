
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))
source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))


#resPrefix = '20190429_193158_MULTISCALE_GRID_GRID'
#resPrefix = '20190506_135221_MULTISCALE_TARGETEDGRID_GRID'
resPrefix = '20190919_161009_MULTISCALE_TARGETEDGRID_GRID'
#resPrefix = '20190919_141607_MULTISCALE_TARGETEDGRID_GRID'
#resPrefix = '20190926_114834_MULTISCALE_LHS_TEST'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Multiscale/',resPrefix,'/');dir.create(resdir)

#res <- as.tbl(read.csv(paste0('exploration/',resPrefix,'.csv')))
#res <- as.tbl(read.csv(paste0(resdir,'data/',resPrefix,'.csv'),stringsAsFactors = F))
#res <- as.tbl(read.csv(paste0(resdir,'data/',resPrefix,'_sample.csv'),stringsAsFactors = F))
res <- as.tbl(read.csv(paste0(resdir,'data/',resPrefix,'_filtered.csv'),stringsAsFactors = F))

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

## test
#macroindic="macroPopulations"
#macroindic="macroAccessibilities"
#macroindic="macroClosenesses"
#i=10
#P0 = unlist(res[i,paste0(macroindic,0:(ncities-1))])
#Pf = unlist(res[i,paste0(macroindic,(ncities*tsteps - 1):(ncities*(tsteps + 1) - 1))])
#plot(log(1:length(Pf)),sort(log(Pf),decreasing = T))
#plot(log(1:length(P0)),sort(log(P0),decreasing = T),col='red')
#hierarchy(Pf) - hierarchy(P0)


for(macroindic in macroindics){
  res[[paste0("deltaHierarchy",macroindic)]] = apply(res[,paste0(macroindic,0:(ncities-1))],1,hierarchy) - apply(res[,paste0(macroindic,(ncities*tsteps):(ncities*(tsteps+1) - 1))],1,hierarchy)
}
for(mesoindic in mesoindics){
  res[[paste0("delta",mesoindic)]] = apply(res[,paste0(mesoindic,0:(ncities-1))],1,mean) - apply(res[,paste0(mesoindic,(ncities*tsteps):(ncities*(tsteps+1) - 1))],1,mean)
}

hierarchiesMacroindics=paste0("deltaHierarchy",macroindics)
deltaMesoindics=paste0("delta",mesoindics)

#length(which(!duplicated(res[,hierarchiesMacroindics])))
#length(unique(res$replication))
# only 20 distincts runs ?
#g=ggplot(res,aes(x=deltaHierarchymacroPopulations,group=as.character(replication),color=as.character(replication)))
#g+geom_density(alpha=0.4)

#res%>% group_by(replication)%>%summarize(diffnumber=length(unique(deltaHierarchymacroPopulations)))
#summary(res%>% group_by(id)%>%summarize(diffnumber=length(unique(deltaHierarchymacroPopulations)),min=min(deltaHierarchymacroPopulations)))


####
# hists
#set.seed(10)
#ids = sample(unique(res$id),100)
#ids = unique(res$id)
#g=ggplot(res[res$id%in%ids,],aes(x=deltaHierarchymacroPopulations,group=as.character(id),color=as.character(id)))
#g+geom_density(alpha=0.4)

#g=ggplot(res,aes(x=deltaHierarchymacroPopulations,group=as.character(id),color=as.character(id)))
#g+geom_density(alpha=0.4)

#for(param in params){
#  if(length(unique(unlist(res[,param])))>1){
#    g=ggplot(res,aes_string(x="deltaHierarchymacroPopulations",group="as.character(id)",color=paste0("as.character(",param,")")))
#    g+geom_density()
#    ggsave(file=paste0(resdir,'hist_deltaHierarchymacroPopulations_color',param,'.png'),width=18,height=15,units='cm')
#  }
#}

#####

sres = res %>% group_by(
  macroGrowthRate,macroInteractionDecay,macroInteractionGamma,mesoAlpha,mesoBeta,mesoTimeSteps,
  macroMesoAlphaUpdateMax,macroMesoBetaUpdateMax,mesoMacroCongestionCost,mesoMacroDecayUpdateMax,id
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
  deltamesoDistances=mean(deltamesoDistances),
  count=n()
)


#####

#load(paste0(resdir,'data/',resPrefix,'_summary.RData'))

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


#g=ggplot(sres,aes(x=macroInteractionDecay,y=deltaHierarchymacroAccessibilities,group=mesoTimeSteps,color=mesoTimeSteps))
#g+geom_line()+facet_grid(mesoMacroDecayUpdateMax~macroMesoBetaUpdateMax)+stdtheme
#ggsave(file=paste0(resdir,'deltaHierarchyAccessibility-macroInteractionDecay_col-mesoTimeSteps_facet-mesoMacroDecayUpdateMax-macroMesoBetaUpdateMax.png'),width=20,height=18,units='cm')
# 


for()

g=ggplot(sres,aes(x=mesoTimeSteps,y=deltaHierarchymacroPopulations,group=macroInteractionDecay,color=macroInteractionDecay))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=deltaHierarchymacroPopulations-deltaHierarchymacroPopulationsSd,ymax=deltaHierarchymacroPopulations+deltaHierarchymacroPopulationsSd),width=1)+
  facet_grid(mesoMacroDecayUpdateMax~macroMesoBetaUpdateMax,scales='free')+stdtheme


#g=ggplot(sres,aes(x=macroInteractionDecay,y=deltaHierarchymacroPopulations,group=mesoTimeSteps,color=mesoTimeSteps))
#g+geom_line()+facet_grid(mesoMacroDecayUpdateMax~macroMesoAlphaUpdateMax)+stdtheme


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








