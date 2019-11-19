

setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoCalibration'))

library(dplyr)
library(sf)
library(rgdal)
library(raster)
library(reshape2)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

source('functions.R')
source('morphology.R')

ucdbsf <- st_as_sf(loadUCDBData(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'))

years = c('1975','1990','2000','2015')
indics=c('totalPop','maxPop','minPop','moran','avgDist','entropy','alpha','alphaRSquared')

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

# filter bad fit slope
#morphos = morphos[morphos$alphaRSquared1975>0.5,]

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


# classify relative variation TS
set.seed(42)
clusts = list()
for(k in 2:15){
  show(k)
  clusts[[as.character(k)]] = kmeans(classdata[,2:ncol(classdata)],centers = k,iter.max = 10000,nstart = 1000)
}
plot(2:15,sapply(clusts,function(km){km$tot.withinss/km$totss}),type='l')
k = 4
km = kmeans(classdata[,2:ncol(classdata)],centers = k,iter.max = 10000,nstart = 1000)


classdata$cluster = km$cluster
timedf$cluster = rep(km$cluster,length(years))

g=ggplot(timedf,aes(x=year,y=alpha,color=cluster,group=cluster))
g+geom_point(pch='.')+geom_smooth(method = 'loess')
# should remove outliers - at least for slope ?

plots=list()
for(indic in indics){
  g=ggplot(timedf,aes_string(x=indic,color='year'))
  plots[[indic]]=g+geom_density()
}
multiplot(plots,cols = 4)


timeclassdata = data.frame()
for(year in years[2:length(years)]){
  for(classifindic in classifindics){
    currentd = classdata[,c(paste0(classifindic,year),"cluster")];names(currentd)<-c("value","cluster")
    timeclassdata = rbind(timeclassdata,cbind(area = 1:nrow(currentd),currentd,year=rep(year,nrow(currentd)),indic=rep(classifindic,nrow(currentd))))
  }
}

g=ggplot(timeclassdata,aes(x=value,color=year,linetype=factor(cluster),group=interaction(cluster,year)))
g+geom_density()+facet_wrap(~indic,scales='free')

# -> there seems to have enough significant variability for a relevant calibration


###
# quick maps

inds = c(1:946,948:1000)

morphosall = data.frame(matrix(unlist(res),nrow=length(res),byrow = T))
names(morphosall)<-names(res[[1]])

areasmorph = as.tbl(ucdbsf[inds,])

for(indicmorph in names(morphosall)){
  areasmorph[[indicmorph]] = morphosall[[indicmorph]]
}

g=ggplot(areasmorph,aes(x=GCPNT_LON,y=GCPNT_LAT,color=urbFromScratch2015))
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






