

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

ucdbsf <- loadUCDBData(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0')

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

inds = c(1:946,948:1000) # 
#morphosall = data.frame(matrix(unlist(res),nrow=length(res),byrow = T))
#names(morphosall)<-names(res[[1]])
#morphos <- read.csv(file='configs/morphologies.csv',header = T,sep = ";")
#rownames(morphos)<-morphos$areaid
areasmorph = ucdbsf[inds,]
areasmorph$areaid = inds

areasmorph = left_join(areasmorph,morphos,by=c('areaid'='areaid'))



# filter bad fit slope
#morphos = morphos[morphos$alphaRSquared1975>0.5,]
# better with outliers
morphos = morphos[morphos$alpha1975>-4&morphos$alpha1990>-4&morphos$alpha2000>-4&morphos$alpha2015>-4,]

timedf <- as_temporal_df(morphos,indics,years)

classifindics = c('moran','avgDist','entropy','alpha')
classdata = data.frame(id=morphos$areaid)
for(yearind in 2:length(years)){
  classdata=cbind(classdata,morphos[,paste0(classifindics,years[yearind])] - morphos[,paste0(classifindics,years[yearind-1])])
}
# normalize for clustering !
for(j in 2:ncol(classdata)){classdata[,j] = (classdata[,j] - min(classdata[,j]))/(max(classdata[,j])-min(classdata[,j]))}

# classify relative variation TS
#set.seed(42)
#clusts = list()
#for(k in 2:15){
#  show(k)
#  clusts[[as.character(k)]] = kmeans(classdata[,2:ncol(classdata)],centers = k,iter.max = 10000,nstart = 1000)
#}
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
    timeclassdata = rbind(timeclassdata,cbind(area = classdata$id,currentd,year=rep(year,nrow(currentd)),indic=rep(indicnames[[classifindic]],nrow(currentd))))
  }
}

g=ggplot(timeclassdata,aes(x=value,color=factor(cluster),linetype=factor(year),group=interaction(cluster,year)))
g+geom_density()+facet_wrap(~indic,scales='free')+scale_linetype_discrete(name='Year')+scale_color_discrete(name='Cluster')+xlab("")+ylab("")+stdtheme
ggsave(file=paste0(resdir,'distributions_cluster-year.png'),width=30,height=20,units='cm')

# -> there seems to have enough significant variability for a relevant calibration


###
# quick maps


years=c(1975,1990,2000,2015)

for(indic in indics){for(year in years){
  currentdata=areasmorph
  if(indic=='moran'){currentdata=areasmorph[areasmorph[,paste0('moran',year)]>0,]}
  if(indic=='alpha'){currentdata=areasmorph[areasmorph[,paste0('alpha',year)]>-2,]}
  map(
    currentdata,
    paste0(indic,year),
    paste0("totalPop",year),
    paste0(resdir,'mapindic_',indic,'_',year,'.png'),
    legendtitle = paste0(indicnames[[indic]],'\n',year),
    legendsizetitle = paste0('Population\n',year)
)}}



# clusters map
areasmorphfilt = areasmorph[!apply(areasmorph[,names(morphos)],1,function(r){length(which(is.na(r)))>0})&areasmorph$alpha1975>-4&areasmorph$alpha1990>-4&areasmorph$alpha2000>-4&areasmorph$alpha2015>-4,]
for(classindic in names(classdata)){
  currentd = classdata[[classindic]];names(currentd)<-classdata$id
  areasmorphfilt[[paste0("class",classindic)]] = currentd[areasmorphfilt$areaid]
}
areasmorphfilt$classcluster=as.character(areasmorphfilt$classcluster)

map(
  areasmorphfilt[!is.na(areasmorphfilt$classcluster),],
  'classcluster',
  "totalPop2015",
  paste0(resdir,'mapindic_cluster.png'),
  discrete=T,
  legendtitle = 'Cluster',
  legendsizetitle = 'Population\n2015'
)


#####
### Correlations
# + supp mat: cors dpop etc

years = c(1990,2000,2015)

corvars = c('P','B','E','G',classifindics)
corvarsnames = c('Pop','Built','Emissions','GDP',unlist(indicnames[classifindics]))

correlations(areasmorphfilt,corvars,corvarsnames,classifindics,fileprefix=paste0(resdir,'correlations_vars-morpho'),mode="wide")


years = c(2000,2015)

corvars = c('DP','DG','DE','DB',classifindics)
corvarsnames = c('DeltaPop','DeltaGDP','DeltaEm','DeltaBuilt',unlist(indicnames[classifindics]))
for(year in years){
  rho = matrix(0,length(corvars),length(corvars));colnames(rho)<-corvarsnames;rownames(rho)<-corvarsnames
  rhomin = rho;rhomax = rho
  for(i in 1:length(corvars)){
    for(j in 1:length(corvars)){
      ivar= paste0(corvars[i],year);jvar=paste0(corvars[j],year)
      if(!corvars[i]%in%classifindics){ivar= paste0(corvars[i],substr(year,3,4))}
      if(!corvars[j]%in%classifindics){jvar=paste0(corvars[j],substr(year,3,4))}
      rhotest = cor.test(unlist(areasmorphfilt[,ivar]),unlist(areasmorphfilt[,jvar]))
      rho[i,j]=rhotest$estimate;rhomin[i,j]=rhotest$conf.int[1];rhomax[i,j]=rhotest$conf.int[2]
    }
  }
  png(filename = paste0(resdir,'correlations_deltavars-morpho_',year,'.png'),width = 25,height = 20,units='cm',res = 300)
  corrplot(rho,lowCI.mat = rhomin, uppCI.mat = rhomax,type = 'upper',title = year,bg='lightgrey',
           plotCI = 'circle',
           addCoef.col = "black",
           mar = c(0,0,1,0)
           #order='hclust',
           #method='ellipse'
  )
  dev.off()
}



######
# trajectories of cluster centers in PC space

#summary(classdata)

years = c(1975,1990,2000,2015)

timedf <- as_temporal_df(as.data.frame(areasmorphfilt),indics)
pcadata = timedf
for(ind in classifindics){pcadata[,ind] = (pcadata[,ind] - min(pcadata[,ind]))/(max(pcadata[,ind])-min(pcadata[,ind]))}

pca <- prcomp(pcadata[,classifindics])

#                           PC1    PC2     PC3     PC4
#Standard deviation     0.2002 0.1890 0.08542 0.05261
#Proportion of Variance 0.4667 0.4160 0.08499 0.03223
#Cumulative Proportion  0.4667 0.8828 0.96777 1.00000
# more variance captured

#pca$rotation
#                PC1        PC2        PC3        PC4
#moran   0.85080930 -0.3122690  0.1625213 -0.3901262
#avgDist 0.05508002  0.8256783  0.4291656 -0.3619922
#entropy 0.50776371  0.3602970 -0.1041703  0.7755712
#alpha   0.12355460  0.3015450 -0.8823561 -0.3394887


rotated = data.frame(as.matrix(pcadata[,classifindics])%*%pca$rotation)

for(pcname in names(rotated)){pcadata[,pcname]=rotated[,pcname]}

clusts = classdata$cluster;names(clusts)<-classdata$id

pcadata$cluster=as.character(clusts[pcadata$area])



g=ggplot(pcadata,aes(x=PC1,y=PC2,color=cluster,group=area))
g+geom_path(alpha=0.2,arrow=arrow(angle=20, type = "closed",length = unit(0.05,'inches')))+
  geom_path(data=pcadata%>%group_by(cluster,year)%>%summarise(PC1=mean(PC1),PC2=mean(PC2)),
            aes(x=PC1,y=PC2,color=cluster,group=cluster),size=2,
            arrow=arrow(angle=30, type = "closed",length = unit(0.1,'inches')))+
  geom_label(data=data.frame(x=0.28,y=0.01,s=paste0("PC1=",format(pca$rotation[1,1],digits=2),"*I+",format(pca$rotation[2,1],digits=2),"*d+",format(pca$rotation[3,1],digits=2),"*e+",format(pca$rotation[4,1],digits=2),
                                              "*s\nPC2=",format(pca$rotation[1,2],digits=2),"*I+",format(pca$rotation[2,2],digits=2),"*d+",format(pca$rotation[3,2],digits=2),"*e+",format(pca$rotation[4,2],digits=2),"*s"))
             ,mapping=aes(x=x,y=y,label=s),inherit.aes = F)+
  stdtheme
ggsave(file=paste0(resdir,'PCA_trajectories.png'),width=20,height=18,units='cm')





