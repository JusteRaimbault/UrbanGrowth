
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/DataProcessing'))

library(raster)
#library(sf)
library(rgdal)

source('functions.R')

targetDir <- paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/processed/');dir.create(targetDir)

#countries <- st_read(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/gis'),'countries')
countries <- readOGR(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/gis'),'countries')
countries = spTransform(countries,"+proj=longlat +ellps=clrk66 +no_defs")

dem <- raster(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/DEM/raw/DEM_geotiff/alwdgg.tif'))

#country = as_Spatial(countries[countries$CNTR_ID=='FR',])

countrycodes = c('FR')#c('FR','BR','CN','IN','RU','ZA','US')
cityfiles = list('FR'='France','BR'='Brazil','CN'='China','IN'='India','RU'='Russia','ZA'='South-Africa','US'='USA')

for(countrycode in countrycodes){
  show(paste0('Constructing for country ',countrycode))
  extrdem = extractRaster(dem,countries,countrycode)
  graph = geographicGraph(extrdem)
  cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
  vertices = apply(cities[,c("Long","Lat")],1,function(r){
    d=(V(graph)$x-r[1])^2+(V(graph)$y-r[2])^2
    show(min(d))
    return(which(d==min(d)))
  })
  
  # there should not be any duplicate ?
  #cities=cities[!duplicated(vertices),]
  # vertices[duplicated(vertices)]
  # cities[which(vertices==10334),]
  
  distances(graph,v = vertices[!duplicated(vertices)],to=vertices[!duplicated(vertices)])
  
  # TODO put euclidian distance for duplicated
  
  # not needed, coordinates are exctly the same (longlat)
  #spc=spTransform(SpatialPoints(cities[,c("Long","Lat")],proj4string =CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),"+proj=longlat +ellps=clrk66 +no_defs")
  #plot(extrdem)
  # g=ggplot(data.frame(x=V(graph)$x,y=V(graph)$y))
  #g+geom_point(aes(x=x,y=y),pch='.')+geom_point(data=cities,aes(x=Long,y=Lat,size=X1999/1000000),col='red')+
  #  geom_point(data=data.frame(spc@coords),aes(x=Long,y=Lat),col='blue')
  
  
}


