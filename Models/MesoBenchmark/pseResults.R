
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoBenchmark'))

library(ggplot2)
library(dplyr)
library(GGally)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

#resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/MesoBenchmark/Simple/')
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/MesoBenchmark/Replications/')
dir.create(resdir)

resdirs = list(
  #'expmixture' = '20200828_1515_PSE_EXPMIXTURE_res'
  'expmixture' = 'openmole/pse/20201125_1115_PSE_EXPMIXTURE_REPLICATIONS_GRID',
  #'reactiondiffusion' = 'openmole/pse/20200828_1624_PSE_REACTIONDIFFUSION_res',
  'reactiondiffusion' = 'openmole/pse/20210531_0803_PSE_REACTIONDIFFUSION_REPLICATIONS_LOCAL_res/',
  # 'correlatedperco' = '20200829_1330_PSE_CORRELATEDPERCO_res'
  'correlatedperco' = 'openmole/pse/20201124_1816_PSE_CORRELATEDPERCO_REPLICATIONS_GRID/',
  #'gravity' = 'openmole/pse/20200829_1526_PSE_GRAVITY_res'
  'gravity' = 'openmole/pse/20210507_1744_PSE_GRAVITY_REPLICATIONS_LOCAL/'
)

stochastic = FALSE

gen<-function(filename){as.integer(sapply(strsplit(sapply(strsplit(filename,"population"),function(s){s[2]}),".csv"),function(s){s[1]}))}

latestgen <- function(dir){
  if(is.null(dir)){return(NULL)}
  else{return(max(sapply(list.files(dir,pattern=".csv"),gen)))}
}

indics = c('moran','distance','entropy','slope')

#####
# Convergence

allres = data.frame()
for(model in names(resdirs)){
  currentdir = resdirs[[model]]
  currentfiles = paste0(currentdir,'/',list.files(currentdir,pattern = '.csv'))
  for(currentfile in currentfiles){
    currentres = as.tbl(read.csv(currentfile));currentgen=gen(currentfile)
    # toggle for stochastic runs
    if (stochastic){
      allres = rbind(allres,cbind(currentres[,c(indics,'evolution.samples')],model=rep(model,nrow(currentres)),generation=rep(currentgen,nrow(currentres))))
    }else{
      allres = rbind(allres,cbind(currentres[,indics],model=rep(model,nrow(currentres)),generation=rep(currentgen,nrow(currentres))))
    }
  }
}

sres = allres %>% group_by(model,generation) %>% summarise(patterns = n())

g=ggplot(sres[sres$model=='correlatedperco',],aes(x=generation,y=patterns,color=model,group=model))
g+geom_point()+geom_line()

if (stochastic){
  sres = allres[allres$evolution.samples>=10,] %>% group_by(model,generation) %>% summarise(patterns = n())
  g=ggplot(sres[sres$model=='correlatedperco',],aes(x=generation,y=patterns,color=model,group=model))
  g+geom_point()+geom_line()
}

# for correlated perco: 20200829_1330_PSE_CORRELATEDPERCO has much more patterns but is not robust to stochasticity; 
# 20201121_1802_PSE_CORRELATEDPERCO_GRID still increasing at gen 20000 for patterns with more than 10 samples
# -> test with repetitions - rq: does not have antecedent param in that case (but not used anyway)


#####
# PSE plots



res = data.frame()
for(model in names(resdirs)){
  currentdir = resdirs[[model]];currentgen = latestgen(currentdir)
  currentres = as.tbl(read.csv(paste0(currentdir,'/population',currentgen,'.csv')))
  if (stochastic){
    res = rbind(res,cbind(currentres[,c(indics,'evolution.samples')],model=rep(model,nrow(currentres))))
  }else{
    res = rbind(res,cbind(currentres[,indics],model=rep(model,nrow(currentres))))
  }
}

#plot(res[res$model=='correlatedperco'&res$slope>-4,indics])


ggsave(plot = ggpairs(res[res$slope>-4,],aes(color=model),columns = indics,
                      lower = list(continuous = wrap("points", alpha = 0.6,size=0.2,shape='.')),
                      diag = list(continuous = wrap("densityDiag", alpha = 0.4))
                      )+stdtheme,filename = paste0(resdir,'scatter_models.png'),width = 40,height=30,units='cm')


# with real points
morphos <- as.tbl(read.csv(file='../MesoCalibration/configs/morphologies.csv',header = T,sep = ";"))
real = morphos[,c("moran2015","avgDist2015","entropy2015","alpha2015")]
names(real)<- indics

res = rbind(res,cbind(real,evolution.samples=rep(0,nrow(real)),model=rep('real',nrow(real))))

ggsave(plot = ggpairs(res[res$slope>-4,],aes(color=model),columns = indics,
                      lower = list(continuous = wrap("points", alpha = 0.7,size=0.15,shape='.')),
                      diag = list(continuous = wrap("densityDiag", alpha = 0.4))
)+stdtheme,filename = paste0(resdir,'scatter_all.png'),width = 40,height=30,units='cm')



#####
# Hypervolumes
# for hypervolume intersection: https://rdrr.io/cran/hypervolume/man/hypervolume_set.html

library(hypervolume)
library(reshape2)

models = c(names(files),'real')
res=res[res$slope>-4,]

hvs = list()
for(model in models){
  show(model)
  start = as.numeric(Sys.time())
  hvs[[model]] = hypervolume_gaussian(res[res$model==model,indics])
  show(as.numeric(Sys.time()) - start)
}
sapply(hvs,function(hv){hv@Volume})

overlaps = matrix(data=rep(1,length(models)*length(models)),nrow=length(models))
for(model1 in 1:length(models)){
  for (model2 in 1:length(models)){
    if(model1!=model2){
      show(paste0(models[model1],' / ',models[model2]))
      hvset = hypervolume_set(hvs[[models[model1]]],hvs[[models[model2]]],check.memory=FALSE,num.points.max=100000)
      overlaps[model1,model2] = hvset@HVList$Intersection@Volume / hvs[[models[model2]]]@Volume
    }
  }
}
rownames(overlaps)<-models
colnames(overlaps)<-models
diag(overlaps)=NA
save(overlaps,hvs,file='hvs.RData')

g=ggplot(melt(overlaps),aes(x=Var1,y=Var2,fill=value))
g+geom_raster()+xlab('')+ylab('')+scale_fill_continuous(name='Overlap')+stdtheme
ggsave(file=paste0(resdir,'overlap.png'),width=30,height=26,units='cm')



