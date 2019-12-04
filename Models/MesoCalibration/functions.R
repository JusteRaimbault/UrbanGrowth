


library(scales)
library(sf)
#library(maps)
#library(mapproj)


as_temporal_df <- function(data,indics){
  currenttimedf <- data.frame()
  for(year in years){
    currentd = data[,paste0(indics,year)];names(currentd)<-indics
    currenttimedf = rbind(currenttimedf,cbind(areaid = data$areaid,currentd,year=rep(year,nrow(currentd))))
  }
  return(currenttimedf)
}

getParetoFront<-function(d,c1,c2){
  inds=c()
  for(i in 1:nrow(d)){
    if(length(which(d[i,c1]>d[,c1]&d[i,c2]>d[,c2]))==0){inds=append(inds,i)}
  }
  return(d[inds,])
}


library(corrplot)

correlations<-function(currentdata,corvars,corvarsnames,years,mode,fileprefix,classifindics=NULL){
  
  for(year in years){
    rho = matrix(0,length(corvars),length(corvars));colnames(rho)<-corvarsnames;rownames(rho)<-corvarsnames
    rhomin = rho;rhomax = rho
    for(i in 1:length(corvars)){
      for(j in 1:length(corvars)){
        if(mode=="wide"){
        ivar= paste0(corvars[i],year);jvar=paste0(corvars[j],year)
        if(!corvars[i]%in%classifindics){ivar= paste0(corvars[i],substr(year,3,4))}
        if(!corvars[j]%in%classifindics){jvar=paste0(corvars[j],substr(year,3,4))}
        x=unlist(currentdata[,ivar]);y=unlist(currentdata[,jvar])
        }
        if(mode=="long"){
          x=unlist(currentdata[currentdata$year==year,corvars[i]])
          y=unlist(currentdata[currentdata$year==year,corvars[j]])
        }
        rhotest = cor.test(x,y)
        rho[i,j]=rhotest$estimate;rhomin[i,j]=rhotest$conf.int[1];rhomax[i,j]=rhotest$conf.int[2]
      }
    }
    png(filename = paste0(fileprefix,'_',year,'.png'),width = 25,height = 20,units='cm',res = 300)
    corrplot(rho,lowCI.mat = rhomin, uppCI.mat = rhomax,type = 'upper',title = year,bg='lightgrey',
             plotCI = 'circle',
             addCoef.col = "black",
             mar = c(0,0,1,0)
             #order='hclust',
             #method='ellipse'
    )
    dev.off()
  }
}


countries <- st_read(paste0(Sys.getenv('CS_HOME'),'/Data/Countries/'),'countries')

map<- function(data,var,sizevar,filename,discrete=FALSE,legendtitle=NULL,legendsizetitle=NULL,xlim=c(-130,150),ylim=c(-50, 60)){
  #WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
  #sizes = log10(data[[sizevar]]);sizes = (sizes - min(sizes,na.rm = T)) / (max(sizes,na.rm = T) - min(sizes,na.rm = T))
  #data[['sizes']]=sizes
  g=ggplot()+
    #geom_map(data = WorldData, map = WorldData,aes(group = group, map_id=region),fill = "white", colour = "#7f7f7f", size=0.1) + 
    geom_sf(data=countries,fill = "white", colour = "#7f7f7f", size=0.1)+
    geom_point(data=data,aes_string(x='GCPNT_LON',y='GCPNT_LAT',color=var,size=sizevar),alpha=0.6)+
    scale_size_area(name=ifelse(is.null(legendsizetitle),sizevar,legendsizetitle))+
    #geom_map(data = areasmorph, map=WorldData,
    #         aes(fill=moran2015),#, map_id=region),
    #         colour="#7f7f7f", size=0.5) +
    #coord_map("mollweide")+ # coord_map("rectangular",lat0=lat0, xlim=xlim, ylim=ylim)
    #coord_sf("rectangular",xlim=xlim, ylim=ylim)+
    #scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
    theme_bw()+xlab("")+ylab("")+
    xlim(xlim)+ylim(ylim)+theme(axis.text = element_blank(),axis.ticks = element_blank())
  #scale_y_continuous(limits=ylim,breaks=c(),labels = c()) +
  #scale_x_continuous(limits=xlim,breaks=c(),labels = c())
  if(discrete){
    g+scale_color_discrete(name=ifelse(is.null(legendtitle),var,legendtitle))
  }else{
    g+scale_color_distiller(palette = 'Spectral',na.value ='white',name=ifelse(is.null(legendtitle),var,legendtitle))
  }
  ggsave(filename = filename,width=30,height=12,units='cm',dpi = 600)
}



loadUCDBData <- function(dsn,layer){
  ucdb <- readOGR(dsn,layer,stringsAsFactors = F)
  
  areas = ucdb@data
  for(j in 1:ncol(areas)){if(length(which(areas[,j]=="NAN"))>0){
    areas[areas[,j]=="NAN",j]="0"
    areas[,j]=as.numeric(areas[,j])
  }
  }
  
  # compute aggreg indicators
  # co2 emissions
  areas$E75 = areas$E_EC2E_A75 + areas$E_EC2E_T75 + areas$E_EC2E_I75 + areas$E_EC2E_R75 +  areas$E_EC2E_E75 +
    areas$E_EC2O_A75 + areas$E_EC2O_T75 + areas$E_EC2O_I75 + areas$E_EC2O_R75 +  areas$E_EC2O_E75
  areas$E90 = areas$E_EC2E_A90 + areas$E_EC2E_T90 + areas$E_EC2E_I90 + areas$E_EC2E_R90 +  areas$E_EC2E_E90 +
    areas$E_EC2O_A90 + areas$E_EC2O_T90 + areas$E_EC2O_I90 + areas$E_EC2O_R90 +  areas$E_EC2O_E90
  areas$E00 = areas$E_EC2E_A00 + areas$E_EC2E_T00 + areas$E_EC2E_I00 + areas$E_EC2E_R00 +  areas$E_EC2E_E00 +
    areas$E_EC2O_A00 + areas$E_EC2O_T00 + areas$E_EC2O_I00 + areas$E_EC2O_R00 +  areas$E_EC2O_E00
  areas$E15 = areas$E_EC2E_A12 + areas$E_EC2E_T12 + areas$E_EC2E_I12 + areas$E_EC2E_R12 +  areas$E_EC2E_E12 +
    areas$E_EC2O_A12 + areas$E_EC2O_T12 + areas$E_EC2O_I12 + areas$E_EC2O_R12 +  areas$E_EC2O_E12
  
  areas$G15 = areas$GDP15_SM 
  areas$G00 = areas$GDP00_SM
  areas$G90 = areas$GDP90_SM
  
  # deltas
  areas$DP90 = ifelse(areas$P75>0,(areas$P90 - areas$P75)/areas$P75,NA)
  areas$DP00 = ifelse(areas$P90>0,(areas$P00 - areas$P90)/areas$P90,NA)
  areas$DP15 = ifelse(areas$P00>0,(areas$P15 - areas$P00)/areas$P00,NA)
  
  areas$DB90 = ifelse(areas$B75>0,(areas$B90 - areas$B75)/areas$B75,NA)
  areas$DB00 = ifelse(areas$B90>0,(areas$B00 - areas$B90)/areas$B90,NA)
  areas$DB15 = ifelse(areas$B00>0,(areas$B15 - areas$B00)/areas$B00,NA)
  
  areas$DG00 = ifelse(areas$GDP90_SM>0,(areas$GDP00_SM - areas$GDP90_SM)/areas$GDP90_SM,NA)
  areas$DG15 = ifelse(areas$GDP00_SM>0,(areas$GDP15_SM - areas$GDP00_SM)/areas$GDP00_SM,NA)
  
  areas$DE90 = ifelse(areas$E75>0,(areas$E90 - areas$E75)/areas$E75,NA)
  areas$DE00 = ifelse(areas$E90>0,(areas$E00 - areas$E90)/areas$E90,NA)
  areas$DE15 = ifelse(areas$E00>0,(areas$E15 - areas$E00)/areas$E00,NA)
  
  areas$country = as.character(areas$CTR_MN_NM)
  
  ucdb@data = areas
  
  ucdbsf <- st_as_sf(ucdb)
  ucdbsf <- ucdbsf %>% arrange(desc(P15))
  
  return(ucdbsf)
}

