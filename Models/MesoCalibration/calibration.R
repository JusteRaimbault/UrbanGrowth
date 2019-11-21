
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoCalibration'))

library(dplyr)
library(ggplot2)


latestgen <- function(dir){max(as.integer(sapply(strsplit(sapply(strsplit(list.files(dir,pattern=".csv"),"population"),function(s){s[2]}),".csv"),function(s){s[1]})))}
latestdir <- function(row,year){
  dirs = list.dirs('calibration')
  m = matrix(unlist(sapply(strsplit(sapply(strsplit(dirs[2:length(dirs)],split = '/'),function(r){r[2]}),split='_'),
         function(r){if(as.numeric(r[4])==row&as.numeric(r[5])==year){return(list(r=r,sort=paste0(r[6],r[7])))}}
  )),ncol=8,byrow=T)
  latest = order(m[,8],decreasing = T)[1]
  return(paste0('calibration/',paste(m[latest,1:7],collapse='_')))
}
# latestgen(latestdir(row,year))

for(row in 1:985){for(year in c(1990,2000,2015)){show(latestgen(latestdir(row,year)))}}

#####
# test Pareto fronts
# -> on 100-200 seems fine - even if few replications

resdir = 'calibration/MESOCALIB_CALIB_LOCAL10_204_1990_20191119_051844/'
res <- as.tbl(read.csv(paste0(resdir,'population',latestgen(resdir),'.csv')))

g=ggplot(res,aes(x=popfit,y=indicsfit,color=alpha))
g+geom_point()

g=ggplot(res[res$evolution.samples>2,],aes(x=popfit,y=indicsfit,color=alpha))
g+geom_point()

g=ggplot(res,aes(x=popfit,y=indicsfit,color=beta))
g+geom_point()

g=ggplot(res[res$evolution.samples>2,],aes(x=popfit,y=indicsfit,color=beta))
g+geom_point()

g=ggplot(res[res$evolution.samples>2,],aes(x=popfit,y=indicsfit,color=tsteps))
g+geom_point()
