
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoCalibration'))

library(dplyr)
library(sf)
library(rgdal)
library(raster)

source('functions.R')
source('morphology.R')

ucdb <- loadUCDBData(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0')

#dem <- raster(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/DEM/raw/DEM_geotiff/alwdgg.tif')) # shitty DEM
#dem <- raster(paste0(Sys.getenv('CS_HOME'),'/Data/SRTM/raw/srtm_01_02/srtm_01_02.tif'))
#dem <- raster(paste0(Sys.getenv('CS_HOME'),'/Data/SRTM/SRTM_1km_GRD/srtmv4_30s/w001001.adf')) # bug with projection
dem <- raster(paste0(Sys.getenv('CS_HOME'),'/Data/SRTM/SRTM_1km.tif'))
# citation for the srtm data
# https://cgiarcsi.community/data/srtm-90m-digital-elevation-database-v4-1/
# Jarvis, A., H.I. Reuter, A. Nelson, E. Guevara, 2008, Hole-filled SRTM for the globe Version 4, available from the CGIAR-CSI SRTM 90m Database (http://srtm.csi.cgiar.org).


years = c('1975','1990','2000','2015')
pops = list()
for(year in years){
  pops[[year]] = raster(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_POP_GPW4',year,'_GLOBE_R2015A_54009_1k_v1_0/GHS_POP_GPW4',year,'_GLOBE_R2015A_54009_1k_v1_0.tif'))
}

ucdbsf <- st_as_sf(ucdb)

# get approximate centroids # better locally project
centroids <- st_centroid(ucdbsf) %>% arrange(desc(P15))
ucdbsf <- ucdbsf %>% arrange(desc(P15))

# 1000th area, in terms of pop ? 
# ucdbsf[1000,c("P75","P00")] # 134064.2 348947.3

# check extents of areas
#getExtent<- function(i){
#  currentarea = ucdbsf[i,]
#  currentprojbbox = st_bbox(st_transform(currentarea,CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')))
#  return(c(xext = 1.25*(currentprojbbox$xmax - currentprojbbox$xmin) / 1000, yext = 1.25*(currentprojbbox$ymax - currentprojbbox$ymin) / 1000))
#}

#extents <- sapply(1:nrow(ucdbsf),getExtent)
#summary(t(extents))
#ucdbsf[extents[1,]==max(extents[1,]),] # New York 
#ucdbsf[extents[2,]==max(extents[2,]),] # Miami
#ucdbsf[extents[1,]==min(extents[1,]),] # Harrai random 2pixels area in India -> pb in dataset ? # ucdbsf[6903,c("P75","P00")]
#ucdbsf[extents[2,]==min(extents[2,]),]
#summary(extents[1,1:1000])
#quantile(extents[1,1:1000],c(0.1,0.25,0.75,0.8,0.9,0.99))
#   75%    80%    90%    99% 
#  38.75  42.50  55.00 138.75

extractDataAndComputeMorphology <- function(i){
  #i = 528
  show(paste0("Computing for area ",i," - ",ucdbsf$UC_NM_MN[i]))
  currentarea = ucdbsf[i,]
  #currentcentroid = centroids[i,]
  #currentcoords = st_coordinates(currentcentroid)
  #currentareaproj = st_transform(currentarea,CRS(paste0('+proj=tmerc +lat_0=',currentcoords[1,2],' +lon_0=',currentcoords[1,1],' +ellps=GRS80 +units=km')))
  # plot(currentarea[,"geometry"]);plot(currentareaproj[,"geometry"]); # intermediate reproj not needed

  currentareaprojbbox = st_bbox(st_transform(currentarea,#CRS( # sf version 0.5-4 needs a string 
	'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'))#)

  #centrprojcoords = st_coordinates(st_centroid(currentareaproj))
  #xmin=centrprojcoords[1,1]-25;xmax = centrprojcoords[1,1]+25;ymin = centrprojcoords[1,2]-25;ymax=centrprojcoords[1,2]+25
  
  #xext = currentareaprojbbox$xmax - currentareaprojbbox$xmin; yext = currentareaprojbbox$ymax - currentareaprojbbox$ymin
  #xmin = currentareaprojbbox$xmin - 0.25*xext; xmax = currentareaprojbbox$xmax + 0.25*xext; ymin = currentareaprojbbox$ymin - 0.25*yext; ymax = currentareaprojbbox$ymax + 0.25*yext
  xext = currentareaprojbbox[3] - currentareaprojbbox[1]; yext = currentareaprojbbox[4] - currentareaprojbbox[2]
  xmin = currentareaprojbbox[1] - 0.25*xext; xmax = currentareaprojbbox[3] + 0.25*xext; ymin = currentareaprojbbox[2] - 0.25*yext; ymax = currentareaprojbbox[4] + 0.25*yext  

  currentbbox = SpatialPolygonsDataFrame(
    SpatialPolygons(Srl=list(
    Polygons(list(
      Polygon(matrix(c(xmin,ymin,xmin,ymax,xmax,ymax,xmax,ymin,xmin,ymin),ncol=2,byrow=T))),"ID")),proj4string=CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')#CRS(st_crs(currentareaprojbbox)$proj4string)
  ),data=data.frame(ID=c(1)),match.ID = F)

  demcells <- cellFromPolygon(dem,currentbbox)
  demvalues=NULL
  if(!is.null(demcells[[1]])){
    demrows<-rowFromCell(dem,demcells[[1]]);demcols<-colFromCell(dem,demcells[[1]])
    demvalues <-getValuesBlock(dem,row=min(demrows),nrows=(max(demrows)-min(demrows)),col=min(demcols),ncols=(max(demcols)-min(demcols)),format='matrix')
    # plot(raster(demvalues))
    # plot(raster(demvalues<0))
    # plot(raster(is.na(demvalues)))
    # need to extend a bit the coastline (bord cells, plus extension sligthly possible in the sea)
    demvalues = as.matrix(focal(raster(demvalues),matrix(rep(1,9),nrow=3),mean,na.rm=T))
  }
  
  res = list()
  prevconfig = NULL
  for(year in years){
    show(year)
  
    cells <- cellFromPolygon(pops[[year]],currentbbox)
    rows <- rowFromCell(pops[[year]],cells[[1]])
    cols <- colFromCell(pops[[year]],cells[[1]])
    popvalues <- getValuesBlock(pops[[year]],row=min(rows),nrows=(max(rows)-min(rows)),col=min(cols),ncols=(max(cols)-min(cols)),format='matrix')
    
    # compute underwater cells
    #for(i in 1:nrow(popvalues)){for(j in 1:ncol(popvalues)){
    #  if(is.na(demvalues[floor((i-1)*nrow(demvalues)/nrow(popvalues))+1,floor((j-1)*ncol(demvalues)/ncol(popvalues))+1])){popvalues[i,j]=NA}
    #}} # -> shit - dem is not precise enough
    # with reprojected dem, can directly do the product (same dimension)
    if(!is.null(demvalues)){popvalues = ifelse(is.na(demvalues),demvalues,popvalues)}
    
    # plot(raster(popvalues))
    
    write.table(popvalues,file = paste0('configs/',i,'_',year,'.csv'),col.names = F,row.names = F,sep=";")
    
    t = proc.time()
    pop = summaryPopulation(popvalues);mor = moran(popvalues);avgdist = averageDistance(popvalues); entr = entropy(popvalues);slope = rankSizeSlope(popvalues)
    show(proc.time()[3] - t[3])
    # note : with na.rm, focal functions are two times slower !
    
    res[[paste0("totalPop",year)]]=pop$totalPop
    res[[paste0("maxPop",year)]]=pop$maxPop
    res[[paste0("minPop",year)]]=pop$minPop
    res[[paste0("moran",year)]]=mor
    res[[paste0("avgDist",year)]]=unname(unlist(avgdist)) # this is not nicely formatted
    res[[paste0("entropy",year)]]=unname(unlist(entr))
    res[[paste0("alpha",year)]]=unname(slope$rankSizeAlpha)
    res[[paste0("alphaRSquared",year)]]=slope$rankSizeRSquared
    
    if(!is.null(prevconfig)){# same size matrices
      res[[paste0("urbFromScratch",year)]] = length(which(prevconfig==0&abs(prevconfig-popvalues)>0))
    }
    # plot(raster((prevconfig==0&prevconfig-popvalues!=0)*popvalues))
    # plot(raster(as.numeric(prevconfig==0&prevconfig-popvalues!=0)*(prevconfig-popvalues)/abs(prevconfig-popvalues)))
    # summary(c(as.numeric(prevconfig==0&prevconfig-popvalues!=0)*(prevconfig-popvalues)/abs(prevconfig-popvalues)))
    # 
    
    prevconfig = popvalues
  }
  res[["width"]]=ncol(popvalues)
  res[["height"]]=nrow(popvalues)
  res[["water"]] = length(which(is.na(demvalues)))/length(c(demvalues))

  return(res)
}


###### run

# test
#for(i in 950:1000){
#  show(extractDataAndComputeMorphology(i))
#}



library(doParallel)
cl <- makeCluster(20,outfile='log')
#cl <- makeCluster(8,outfile='logtest')
registerDoParallel(cl)

startTime = proc.time()[3]

#res <- foreach(i=1:nrow(ucdbsf)) %dopar% {
res <- foreach(i=1:1000) %dopar% {
  library(dplyr)
  library(sf)
  library(rgdal)
  library(raster)
  source('morphology.R')
  
  morph = tryCatch({extractDataAndComputeMorphology(i)
 	},error=function(e){show(e);return(res=NA)})
  show(morph)
  return(morph)
}
 
show(proc.time()[3] - startTime)
 
stopCluster(cl)
  

save(res,file='morphologies_tmp.RData')


# extractDataAndComputeMorphology(528)
# extractDataAndComputeMorphology(947) 



### generate range - year file for calibration
inds = 1:99
years = c(1990,2000,2015)

write.table(
  data.frame(ind = c(t(matrix(rep(inds,length(years)),ncol=length(years)))),year = rep(years,length(inds))),
  row.names = F,col.names = F,sep=",",file='runYears1-99'
)


inds = 509:985
years = c(1990,2000,2015)
write.table(data.frame(ind = c(t(matrix(rep(inds,length(years)),ncol=length(years)))),year = rep(years,length(inds))),row.names = F,col.names = F,sep=",",file='runYears509-985')

inds = 204:500
years = c(1990,2000,2015)

write.table(
  data.frame(ind = c(t(matrix(rep(inds,length(years)),ncol=length(years)))),year = rep(years,length(inds))),
  row.names = F,col.names = F,sep=",",file='runYears204-500'
)

inds = 593:985
years = c(1990,2000,2015)
write.table(data.frame(ind = c(t(matrix(rep(inds,length(years)),ncol=length(years)))),year = rep(years,length(inds))),row.names = F,col.names = F,sep=",",file='runYears593-985')





