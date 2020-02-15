
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole'))

library(dplyr)
library(ggplot2)
library(reshape2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))
source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))


#resPrefix = '20190429_193158_MULTISCALE_GRID_GRID'
#resPrefix = '20190506_135221_MULTISCALE_TARGETEDGRID_GRID'
#resPrefix = '20190919_161009_MULTISCALE_TARGETEDGRID_GRID';filtered=T;fulltrajs=T;oneFactor=F
#resPrefix = '20190919_141607_MULTISCALE_TARGETEDGRID_GRID'
#resPrefix = '20190926_114834_MULTISCALE_LHS_TEST'
#resPrefix = '20190926_173110_MULTISCALE_ONEFACTOR_GRID';filtered=F;fulltrajs=F;oneFactor=T
resPrefix = '20190927_131754_MULTISCALE_TARGETEDGRID_GRID';filtered=F;fulltrajs=F;oneFactor=F

resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Multiscale/',resPrefix,'/');dir.create(resdir)

if(!filtered){
  res <- as.tbl(read.csv(paste0(resdir,'data/',resPrefix,'.csv'),stringsAsFactors = F))
}else{
  #res <- as.tbl(read.csv(paste0(resdir,'data/',resPrefix,'_sample.csv'),stringsAsFactors = F))
  res <- as.tbl(read.csv(paste0(resdir,'data/',resPrefix,'_filtered.csv'),stringsAsFactors = F))
}
  
params = c("macroGrowthRate","macroInteractionDecay","macroInteractionGamma","macroInteractionWeight",
           "mesoAlpha","mesoBeta","mesoNdiff","mesoTimeSteps",
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

if(!fulltrajs){tsteps=1}# shitty naming system for arrays on rows
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
  macroGrowthRate,macroInteractionDecay,macroInteractionGamma,macroInteractionWeight,
  mesoAlpha,mesoBeta,mesoNdiff,
  mesoTimeSteps,
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



####
## one factor sampling plots


if(oneFactor){

nominals = list(
  "macroGrowthRate"=0.02,"macroInteractionDecay"=250,"macroInteractionWeight"=0.005,"macroInteractionGamma"=1.5,
  "mesoAlpha"=1.5,"mesoBeta"=0.05,"mesoNdiff"=1,"mesoTimeSteps"=11,"macroMesoAlphaUpdateMax"=0.05,"macroMesoBetaUpdateMax"=0.05,
  "mesoMacroCongestionCost"=1.0,"mesoMacroDecayUpdateMax"=0.05
)



msres = melt(sres[,c(params,"id",hierarchiesMacroindics,deltaMesoindics)],id.vars = c(params,"id"),measure.vars = c(hierarchiesMacroindics,deltaMesoindics),variable.name = "var")
msressd = melt(sres[,c(params,"id",paste0(c(hierarchiesMacroindics,deltaMesoindics),'Sd'))],id.vars = c(params,"id"),measure.vars = paste0(c(hierarchiesMacroindics,deltaMesoindics),'Sd'),variable.name = "var")
msres$valueSd = msressd$value

# length(which(msres$macroGrowthRate==0.02&msres$macroInteractionDecay==250&msres$macroInteractionWeight==0.005&msres$macroInteractionGamma==1.5&
#                msres$mesoAlpha==1.5&msres$mesoBeta==0.05&msres$mesoNdiff==1&msres$mesoTimeSteps==11&msres$macroMesoAlphaUpdateMax==0.05&
#                msres$macroMesoBetaUpdateMax==0.05&msres$mesoMacroCongestionCost==1&msres$mesoMacroDecayUpdateMax==0.05
# )
# )

for(param in params){
  nomonly = rep(T,nrow(msres));for(param2 in params){if(param2!=param){nomonly=nomonly&msres[,param2]==nominals[[param2]]}}
  g=ggplot(msres[nomonly,],aes_string(x=param,y='value'))
  g+geom_point()+geom_line()+facet_wrap(~var,scales='free',nrow = 2)+ylab('Indicator')+stdtheme
  ggsave(file=paste0(resdir,'onefactor_allindics_',param,'.png'),width=42,height=25,units='cm')
  
  g=ggplot(msres[nomonly,],aes_string(x=param,y='value'))
  g+geom_point()+geom_line()+geom_errorbar(aes(ymin=value-valueSd,ymax=value+valueSd))+
    facet_wrap(~var,scales='free',nrow = 2)+ylab('Indicator')+stdtheme
  ggsave(file=paste0(resdir,'onefactor_allindics_',param,'_errorbars.png'),width=42,height=25,units='cm')
}


###
# some histograms

mres = melt(res[,c(params,"id",hierarchiesMacroindics,deltaMesoindics)],id.vars = c(params,"id"),measure.vars = c(hierarchiesMacroindics,deltaMesoindics),variable.name = "var")

set.seed(15)
ids = sample(unique(res$id),10)

g=ggplot(mres[mres$id%in%ids,],aes(x=value,color=as.character(id),group=as.character(id)))
g+geom_density()+facet_wrap(~var,scales='free',nrow = 2)+scale_color_discrete(name='id')
ggsave(file=paste0(resdir,'allindics_hist.png'),width=30,height = 20,units='cm')

}



#######
## Full grid plots

for(param in params){show(param);show(length(unique(unlist(res[,param]))))}

for(beta in unique(sres$mesoBeta)){
for(indic in c(hierarchiesMacroindics,deltaMesoindics)){
  #show(indic)
  #g=ggplot(sres,aes_string(x="mesoTimeSteps",y=indic,group="macroInteractionDecay",color="macroInteractionDecay"))
  #g+geom_point()+geom_line()+geom_errorbar(aes_string(ymin=paste0(indic," - ",indic,"Sd"),ymax=paste0(indic," + ",indic,"Sd")),width=1)+
  #facet_grid(mesoMacroDecayUpdateMax~macroMesoBetaUpdateMax)+scale_color_continuous(name=expression(d[G]))+stdtheme
  #ggsave(file=paste0(resdir,indic,'-mesoTimeSteps_colorMacroInteractionDecay_facetmesoMacroDecayUpdateMax-macroMesoBetaUpdateMax.png'),width=30,height=25,units='cm')

  show(indic)
  g=ggplot(sres[sres$mesoBeta==beta&sres$mesoMacroDecayUpdateMax%in%c(-0.2,-0.1,0,0.1,0.2),],
           aes_string(x="macroMesoAlphaUpdateMax",y=indic,group="macroMesoBetaUpdateMax",color="macroMesoBetaUpdateMax"))
  g+geom_point()+geom_line()+geom_errorbar(aes_string(ymin=paste0(indic," - ",indic,"Sd"),ymax=paste0(indic," + ",indic,"Sd")))+
    facet_grid(mesoMacroCongestionCost~mesoMacroDecayUpdateMax)+scale_color_continuous(name=expression(delta[d]))+xlab(expression(delta[beta]))+stdtheme
  ggsave(file=paste0(resdir,indic,'-macroMesoAlphaUpdateMax_colormacroMesoBetaUpdateMax_facetmesoMacroCongestionCost-mesoMacroDecayUpdateMax_mesoBeta',beta,'.png'),width=40,height=25,units='cm')
  
}
}


###
# targeted calibration from this ? : minimize sprawl while maximizing access ?
# compromise "meso opt / macro opt" ? (macro access ?)








