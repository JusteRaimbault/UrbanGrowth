
library(raster)
library(rgdal)


extractRaster <- function(dem,countries,countrycode){
  country = countries[countries$CNTR_ID==countrycode,]
  if(countrycode!='EU'){country = SpatialPolygons(list(Polygons(list(country@polygons[[1]]@Polygons[[1]]),ID="id")),proj4string = country@proj4string)}
  ext <- extent(country)
  if(countrycode=='EU'){ext = extent(matrix(c(-17,28,33,66 ),nrow=2))}
  
  rows=rowFromY(dem,ext@ymax):rowFromY(dem,ext@ymin)
  cols=colFromX(dem,ext@xmin):colFromX(dem,ext@xmax)
  cells = c()
  for(i in rows){
    cells <- append(cells,cellFromRowCol(dem, i, cols))
  }
  extrdem <- raster(nrows = length(rows),ncols=length(cols),ext=ext,crs = crs(dem))
  rawvals = extract(dem,cells);rawvals[rawvals<0]=0
  values(extrdem) <- rawvals
  return(extrdem)
}


#'
#' Merge by vertice of the geographical graph to which it is connected
#' (cities assumed to have a $vertice field)
#'
mergeDuplicatedCities <- function(cities){
  
  
  # there should not be any duplicate ?
  #cities=cities[!duplicated(vertices),]
  # vertices[duplicated(vertices)]
  # cities[which(vertices==10334),]
  # cities[duplicated(cities[,c("Long","Lat")]),]
  # -> merge these
  
  #cities[cities$vertices%in%vertices[duplicated(vertices)],]
  
  #  put euclidian distance for duplicated ? no remove ! (some at same position !)
  
  # not needed, coordinates are exctly the same (longlat)
  #spc=spTransform(SpatialPoints(cities[,c("Long","Lat")],proj4string =CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),"+proj=longlat +ellps=clrk66 +no_defs")
  #plot(extrdem)
  # g=ggplot(data.frame(x=V(graph)$x,y=V(graph)$y))
  #g+geom_point(aes(x=x,y=y),pch='.')+geom_point(data=cities,aes(x=Long,y=Lat,size=X1999/1000000),col='red')+
  #  geom_point(data=data.frame(spc@coords),aes(x=Long,y=Lat),col='blue')
  
  
  newcities = cities[!(cities$vertices%in%cities$vertices[duplicated(cities$vertices)]),]
  for(v in unique(cities$vertices[duplicated(cities$vertices)])){
    #show(v)
    currentmerging=cities[cities$vertices==v,]
    #show(currentmerging)
    newcity = cbind(currentmerging[1,1:2],t(colSums(currentmerging[,3:ncol(cities)])))
    newcity[,c("Lat","Long","vertices")]=newcity[,c("Lat","Long","vertices")]/nrow(currentmerging)
    #show(newcity)
    newcities=rbind(newcities,newcity)
  }
  #newcities[newcities$vertices%in%newcities$vertices[duplicated(newcities$vertices)],]
  return(newcities)
}


#'
#'
#'
geographicGraph <- function(mnt){
  
  prevrow=c();prevnames=c();previnds=c();edf=data.frame();vdf=data.frame()
  for(i in 1:nrow(mnt)){
    show(i)
    if(length(which(!is.na(getValuesBlock(mnt,row=i))))>0){
      xcors = xmin(mnt) + (0:(ncol(mnt)-1))*xres(mnt) + (xres(mnt) / 2)
      ycor = ymin(mnt) + (nrow(mnt) - i )*yres(mnt) + (yres(mnt) / 2)
      
      xreskm = spDists(SpatialPoints(xyFromCell(extrdem,1:2),proj4string = crs(mnt)),longlat = T)[1,2]
      yreskm = spDists(SpatialPoints(xyFromCell(extrdem,c(1,cellFromRowCol(mnt,2,1))),proj4string = crs(mnt)),longlat = T)[1,2]
      diagkm = sqrt(xreskm^2 + yreskm^2)
      
      vnames=paste0(as.character(xcors),'-',ycor)
      currentrow=getValuesBlock(mnt,row=i);
      inds=which(!is.na(currentrow));
      hinds=which(!is.na(currentrow[1:(length(currentrow)-1)])&!is.na(currentrow[2:length(currentrow)]))
      vdf=rbind(vdf,data.frame(vnames[inds],xcors[inds],rep(ycor,length(inds))))
      edf=rbind(edf,data.frame(from=vnames[hinds],to=vnames[hinds+1],slope = currentrow[hinds]-currentrow[hinds+1],length=xreskm))
      edf=rbind(edf,data.frame(from=vnames[hinds+1],to=vnames[hinds],slope = currentrow[hinds+1]-currentrow[hinds],length=xreskm))
      if(length(prevrow)>0){
        vinds = intersect(inds,previnds)
        edf=rbind(edf,data.frame(from=vnames[vinds],to=prevnames[vinds],slope=prevrow[vinds]-currentrow[vinds],length=yreskm))
        edf=rbind(edf,data.frame(from=prevnames[vinds],to=vnames[vinds],slope=currentrow[vinds]-prevrow[vinds],length=yreskm))
        d1inds = which(!is.na(currentrow[1:(length(currentrow)-1)])&!is.na(prevrow[2:length(prevrow)]))
        edf=rbind(edf,data.frame(from=vnames[d1inds],to=prevnames[d1inds+1],slope=prevrow[d1inds+1]-currentrow[d1inds],length=diagkm))
        edf=rbind(edf,data.frame(from=prevnames[d1inds+1],to=vnames[d1inds],slope=currentrow[d1inds]-prevrow[d1inds+1],length=diagkm))
        d2inds = which(!is.na(prevrow[1:(length(prevrow)-1)])&!is.na(currentrow[2:length(currentrow)]))
        edf=rbind(edf,data.frame(from=prevnames[d2inds],to=vnames[d2inds+1],slope=currentrow[d2inds+1]-prevrow[d2inds],length=diagkm))
        edf=rbind(edf,data.frame(from=vnames[d2inds+1],to=prevnames[d2inds],slope=prevrow[d2inds]-currentrow[d2inds+1],length=diagkm))
      }
      prevrow=currentrow;prevnames=vnames;previnds=inds
    }
  }
  
  names(vdf)<-c("name","x","y")
  
  g=graph_from_data_frame(d = edf,directed=TRUE,vertices = vdf)
  
  return(g)
  
}

