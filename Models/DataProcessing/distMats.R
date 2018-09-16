
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

#countrycodes = c('FR','BR','CN','IN','RU','ZA','US')
#countrycodes = c('CN','IN','RU','ZA','US')
countrycodes=c('ZA','US')
cityfiles = list('FR'='France','BR'='Brazil','CN'='China','IN'='India','RU'='Russia','ZA'='South-Africa','US'='USA')

for(countrycode in countrycodes){
  show(paste0('Constructing for country ',countrycode))
  extrdem = extractRaster(dem,countries,countrycode)
  graph = geographicGraph(extrdem)
  cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/clean/',cityfiles[[countrycode]],'.csv'))
  
  vertices = apply(cities[,c("Long","Lat")],1,function(r){
    d=(V(graph)$x-r[1])^2+(V(graph)$y-r[2])^2
    #show(min(d))
    return(which(d==min(d))[1])
  })
  cities$vertices=vertices
  
  cities <- mergeDuplicatedCities(cities)
  
  slopes = atan(abs(E(graph)$slope)/(E(graph)$length*1000))*360/(2*pi)
  
  #alpha0 = 3;n0 = 3
  for(alpha0 in 2:4){
    for(n0 in 2:4){
      impedances = E(graph)$length*(1 + (slopes/alpha0)^n0)
      graphdists = distances(graph,v = cities$vertices,to=cities$vertices,weights = impedances)
      write.table(graphdists,file=paste0(targetDir,countrycode,'_gdist_alpha0',alpha0,'_n0',n0,'.csv'),row.names = F,col.names = F,sep=',')
    }
  }
  
  # euclidian distances
  eucldists = spDists(SpatialPoints(cities[,c("Long","Lat")],proj4string = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
  write.table(eucldists,file=paste0(targetDir,countrycode,'_dist.csv'),row.names = F,col.names = F,sep=',')
  
  
  # write merged cities
  write.table(cities,file=paste0(targetDir,countrycode,'.csv'),row.names = F,sep=",")
  # drop vertices
  cities$vertices=NULL;cities$ID=NULL;cities$Long=NULL;cities$Lat=NULL;cities$Name=NULL
  write.table(cities,file=paste0(targetDir,countrycode,'_pops.csv'),row.names = F,col.names = F,sep=",")
  
  # dates
  write.table(as.numeric(substring(colnames(cities),2)),file=paste0(targetDir,countrycode,'_dates.csv'),row.names = F,col.names = F,sep=",")
  
}







