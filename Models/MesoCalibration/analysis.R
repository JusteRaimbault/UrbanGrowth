

setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoCalibration'))

library(dplyr)
library(sf)
library(rgdal)
library(raster)
library(reshape2)
library(ggplot2)
library(maps)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

source('functions.R')
source('morphology.R')

ucdbsf <- st_as_sf(loadUCDBData(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'))

resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/GHS/Morphology/')

years = c('1975','1990','2000','2015')
indics=c('totalPop','maxPop','minPop','moran','avgDist','entropy','alpha','alphaRSquared')
indicnames = list('totalPop'='Total population','maxPop'='Maximal population','minPop'='Minimal population',
                  'moran'='Spatial autocorrelation','avgDist'='Average distance','entropy'='Entropy',
                  'alpha'='Hierarchy','alphaRSquared'='Fierarchy fit')

if(!file.exists('configs/morphologies.csv')){
  # real morphologies
  load(file='morphologies_tmp.RData')

  which(sapply(res,length)==1)
  # 528 947
  res[[528]]=extractDataAndComputeMorphology(528) # Helsinki was out because of missing DEM
  res[[947]]=NULL # 2x1 cell extent -> not worth it fitting

  morphos = data.frame(matrix(unlist(res),nrow=length(res),byrow = T))
  names(morphos)<-names(res[[1]])
  morphos$areaid = c(1:946,948:1000)
  morphos = morphos[!apply(morphos,1,function(r){length(which(is.na(r)))>0}),] # indices are shifted above 204!
  write.table(morphos,file='configs/morphologies.csv',col.names = T,row.names = F,sep = ";")
}

morphos <- read.csv(file='configs/morphologies.csv',header = T,sep = ";")


# filter bad fit slope
#morphos = morphos[morphos$alphaRSquared1975>0.5,]
# better with outliers
morphos = morphos[morphos$alpha1975>-4&morphos$alpha1990>-4&morphos$alpha2000>-4&morphos$alpha2015>-4,]

timedf <- data.frame()
for(year in years){
  currentd = morphos[,paste0(indics,year)];names(currentd)<-indics
  timedf = rbind(timedf,cbind(area = 1:nrow(currentd),currentd,year=rep(year,nrow(currentd))))
}

classifindics = c('moran','avgDist','entropy','alpha')
classdata = data.frame(id = 1:nrow(morphos))
for(yearind in 2:length(years)){
  classdata=cbind(classdata,morphos[,paste0(classifindics,years[yearind])] - morphos[,paste0(classifindics,years[yearind-1])])
}
# normalize for clustering !
for(j in 2:ncol(classdata)){classdata[,j] = (classdata[,j] - min(classdata[,j]))/(max(classdata[,j])-min(classdata[,j]))}

# classify relative variation TS
set.seed(42)
clusts = list()
for(k in 2:15){
  show(k)
  clusts[[as.character(k)]] = kmeans(classdata[,2:ncol(classdata)],centers = k,iter.max = 10000,nstart = 1000)
}
#plot(2:15,sapply(clusts,function(km){km$tot.withinss/km$totss}),type='l')
# rq: number of cluster could also be a function of disjoint signif intervals for averages ?

set.seed(42)
k = 4
km = kmeans(classdata[,2:ncol(classdata)],centers = k,iter.max = 10000,nstart = 1000)


classdata$cluster = km$cluster
timedf$cluster = rep(km$cluster,length(years))

# remove outliers for slope ?
for(indic in indics){
  g=ggplot(timedf,aes_string(x='year',y=indic,color='as.character(cluster)',group='cluster'))
  g+geom_point(pch='.')+geom_smooth(method = 'loess')+xlab('Year')+ylab(indicnames[[indic]])+scale_color_discrete(name = 'Cluster')+stdtheme
  ggsave(file=paste0(resdir,'clusteringTS_',indic,'.png'),width=20,height=15,units='cm')
}


for(indic in indics){
  g=ggplot(timedf,aes_string(x=indic,color='year'))
  g+geom_density()+scale_color_discrete(name='Year')+xlab(indicnames[[indic]])+ylab('Density')+stdtheme
  ggsave(file=paste0(resdir,'distribution_',indic,'.png'),width=20,height=15,units='cm')
}


timeclassdata = data.frame()
for(year in years[2:length(years)]){
  for(classifindic in classifindics){
    currentd = classdata[,c(paste0(classifindic,year),"cluster")];names(currentd)<-c("value","cluster")
    timeclassdata = rbind(timeclassdata,cbind(area = 1:nrow(currentd),currentd,year=rep(year,nrow(currentd)),indic=rep(indicnames[[classifindic]],nrow(currentd))))
  }
}

g=ggplot(timeclassdata,aes(x=value,color=factor(cluster),linetype=year,group=interaction(cluster,year)))
g+geom_density()+facet_wrap(~indic,scales='free')+scale_linetype_discrete(name='Year')+scale_color_discrete(name='Cluster')+xlab("")+ylab("")+stdtheme
ggsave(file=paste0(resdir,'distributions_cluster-year.png'),width=30,height=20,units='cm')

# -> there seems to have enough significant variability for a relevant calibration


###
# quick maps

inds = c(1:946,948:1000) # 
#morphosall = data.frame(matrix(unlist(res),nrow=length(res),byrow = T))
#names(morphosall)<-names(res[[1]])
morphos <- read.csv(file='configs/morphologies.csv',header = T,sep = ";")
rownames(morphos)<-morphos$areaid

areasmorph = as.tbl(ucdbsf[inds,])
areasmorph$areaid = inds

areasmorph = left_join(areasmorph,morphos,by=c('areaid'='areaid'))

map<- function(var){
  WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
  g=ggplot() + geom_map(data = WorldData, map = WorldData,aes(group = group, map_id=region),
             fill = "white", colour = "#7f7f7f", size=0.5) + 
    geom_point(data=areasmorph,aes(x=GCPNT_LON,y=GCPNT_LAT,color=moran2015))+
    #geom_map(data = areasmorph, map=WorldData,
    #         aes(fill=moran2015),#, map_id=region),
    #         colour="#7f7f7f", size=0.5) +
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
    #scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
    scale_y_continuous(breaks=c()) +
    scale_x_continuous(breaks=c()) +
    labs(fill="legend", title="Title", x="", y="") +
    theme_bw()
  g
}


g=ggplot(areasmorph,aes(x=GCPNT_LON,y=GCPNT_LAT,color=moran2015))
g+geom_point()

# check clusters
areasmorph = areasmorph[!apply(areasmorph[,names(morphosall)],1,function(r){length(which(is.na(r)))>0}),]
for(classindic in names(classdata)){
  areasmorph[[paste0("class",classindic)]] = classdata[[classindic]]
}
areasmorph=areasmorph[areasmorph$alpha1975>-2,]

g=ggplot(areasmorph,aes(x=GCPNT_LON,y=GCPNT_LAT,color=factor(classcluster)))
g+geom_point()

g=ggplot(areasmorph,aes(x=GCPNT_LON,y=GCPNT_LAT,color=classalpha1990))
g+geom_point()

g=ggplot(areasmorph,aes(x=GCPNT_LON,y=GCPNT_LAT,color=cut(classalpha1990,breaks=c(-2,-0.1,0.1,1))))
g+geom_point() # -> proportion of each per country ?






