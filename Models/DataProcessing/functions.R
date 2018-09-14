
library(raster)
library(rgdal)


extractRaster <- function(dem,countries,countrycode){
  country = countries[countries$CNTR_ID==countrycode,]
  country = SpatialPolygons(list(Polygons(list(country@polygons[[1]]@Polygons[[1]]),ID="id")),proj4string = country@proj4string)
  ext <- extent(country)
  
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
#'
#'
geographicGraph <- function(mnt){
  
  prevrow=c();prevnames=c();previnds=c();edf=data.frame();vdf=data.frame()
  for(i in 1:nrow(mnt)){
    show(i)
    if(length(which(!is.na(getValuesBlock(mnt,row=i))))>0){
      xcors = xmin(mnt) + (0:(ncol(mnt)-1))*xres(mnt) + (xres(mnt) / 2)
      ycor = ymin(mnt) + (nrow(mnt) - i )*yres(mnt) + (yres(mnt) / 2)
      vnames=paste0(as.character(xcors),'-',ycor)
      currentrow=getValuesBlock(mnt,row=i);
      inds=which(!is.na(currentrow));
      hinds=which(!is.na(currentrow[1:(length(currentrow)-1)])&!is.na(currentrow[2:length(currentrow)]))
      vdf=rbind(vdf,data.frame(vnames[inds],xcors[inds],rep(ycor,length(inds))))
      edf=rbind(edf,data.frame(from=vnames[hinds],to=vnames[hinds+1],slope = currentrow[hinds]-currentrow[hinds+1],length=1))
      edf=rbind(edf,data.frame(from=vnames[hinds+1],to=vnames[hinds],slope = currentrow[hinds+1]-currentrow[hinds],length=1))
      if(length(prevrow)>0){
        vinds = intersect(inds,previnds)
        edf=rbind(edf,data.frame(from=vnames[vinds],to=prevnames[vinds],slope=prevrow[vinds]-currentrow[vinds],length=1))
        edf=rbind(edf,data.frame(from=prevnames[vinds],to=vnames[vinds],slope=currentrow[vinds]-prevrow[vinds],length=1))
        d1inds = which(!is.na(currentrow[1:(length(currentrow)-1)])&!is.na(prevrow[2:length(prevrow)]))
        edf=rbind(edf,data.frame(from=vnames[d1inds],to=prevnames[d1inds+1],slope=prevrow[d1inds+1]-currentrow[d1inds],length=sqrt(2)))
        edf=rbind(edf,data.frame(from=prevnames[d1inds+1],to=vnames[d1inds],slope=currentrow[d1inds]-prevrow[d1inds+1],length=sqrt(2)))
        d2inds = which(!is.na(prevrow[1:(length(prevrow)-1)])&!is.na(currentrow[2:length(currentrow)]))
        edf=rbind(edf,data.frame(from=prevnames[d2inds],to=vnames[d2inds+1],slope=currentrow[d2inds+1]-prevrow[d2inds],length=sqrt(2)))
        edf=rbind(edf,data.frame(from=vnames[d2inds+1],to=prevnames[d2inds],slope=prevrow[d2inds]-currentrow[d2inds+1],length=sqrt(2)))
      }
      prevrow=currentrow;prevnames=vnames;previnds=inds
    }
  }
  
  names(vdf)<-c("name","x","y")
  
  g=graph_from_data_frame(d = edf,directed=TRUE,vertices = vdf)
  
  return(g)
  
}

