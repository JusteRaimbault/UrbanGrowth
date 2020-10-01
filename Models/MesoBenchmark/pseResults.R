
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoBenchmark'))

library(ggplot2)
library(dplyr)
library(GGally)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/MesoBenchmark/')

files = list(
  'expmixture' = 'openmole/pse/20200829_1514_PSE_EXPMIXTURE_res/population100000.csv',
  'reactiondiffusion' = 'openmole/pse/20200828_1624_PSE_REACTIONDIFFUSION_res/population50000.csv',
  'correlatedperco' = 'openmole/pse/20200829_1330_PSE_CORRELATEDPERCO_res/population100000.csv',
  'gravity' = 'openmole/pse/20200829_1526_PSE_GRAVITY_res/population37000.csv'
)

indics = c('moran','distance','entropy','slope')

res = data.frame()
for(model in names(files)){
  currentres = as.tbl(read.csv(files[[model]]))
  res = rbind(res,cbind(currentres[,c(indics,'evolution.samples')],model=rep(model,nrow(currentres))))
}


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



####
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



