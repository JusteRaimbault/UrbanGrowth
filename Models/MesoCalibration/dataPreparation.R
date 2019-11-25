
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoCalibration'))

library(dplyr)
library(sf)
library(rgdal)
library(raster)

source('functions.R')

ucdb <- loadUCDBData(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0')

pop = raster(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_POP_GPW41975_GLOBE_R2015A_54009_1k_v1_0/GHS_POP_GPW41975_GLOBE_R2015A_54009_1k_v1_0.tif'))

ucdbsf <- st_as_sf(ucdb)

# get approximate centroids # better locally project
centroids <- st_centroid(ucdbsf) %>% arrange(desc(P15))
ucdbsf <- ucdbsf %>% arrange(desc(P15))

# 1000th area, in terms of pop ? 
# ucdbsf[1000,c("P75","P00")] # 134064.2 348947.3

# check extents of areas
getExtent<- function(i){
  currentarea = ucdbsf[i,]
  currentprojbbox = st_bbox(st_transform(currentarea,CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')))
  return(c(xext = 1.25*(currentprojbbox$xmax - currentprojbbox$xmin) / 1000, yext = 1.25*(currentprojbbox$ymax - currentprojbbox$ymin) / 1000))
}

extents <- sapply(1:nrow(ucdbsf),getExtent)
summary(t(extents))
ucdbsf[extents[1,]==max(extents[1,]),] # New York 
ucdbsf[extents[2,]==max(extents[2,]),] # Miami
ucdbsf[extents[1,]==min(extents[1,]),] # Harrai random 2pixels area in India -> pb in dataset ? # ucdbsf[6903,c("P75","P00")]
ucdbsf[extents[2,]==min(extents[2,]),]
summary(extents[1,1:1000])
quantile(extents[1,1:1000],c(0.1,0.25,0.75,0.8,0.9,0.99))
#   75%    80%    90%    99% 
#  38.75  42.50  55.00 138.75

i = 1
currentcentroid = centroids[i,];currentarea = ucdbsf[i,]
#currentcoords = st_coordinates(currentcentroid)
#currentareaproj = st_transform(currentarea,CRS(paste0('+proj=tmerc +lat_0=',currentcoords[1,2],' +lon_0=',currentcoords[1,1],' +ellps=GRS80 +units=km')))
# plot(currentarea[,"geometry"]);plot(currentareaproj[,"geometry"]); # intermediate reproj not needed
currentareaproj = st_transform(currentarea,CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'))

centrprojcoords = st_coordinates(st_centroid(currentareaproj))

xmin=centrprojcoords[1,1]-25;xmax = centrprojcoords[1,1]+25;ymin = centrprojcoords[1,2]-25;ymax=centrprojcoords[1,2]+25

currentbbox = spTransform(
  SpatialPolygonsDataFrame(
  SpatialPolygons(Srl=list(
  Polygons(list(
    Polygon(matrix(c(xmin,ymin,xmin,ymax,xmax,ymax,xmax,ymin,xmin,ymin),ncol=2,byrow=T))),"ID")),proj4string=CRS(st_crs(currentareaproj)$proj4string)
),
data=data.frame(ID=c(1)),match.ID = F)
,CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'))

cells <- cellFromPolygon(pop,currentbbox)
getValuesBlock()



