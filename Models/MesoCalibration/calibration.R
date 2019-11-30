
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoCalibration'))

library(dplyr)
library(ggplot2)


latestgen <- function(dir){max(as.integer(sapply(strsplit(sapply(strsplit(list.files(dir,pattern=".csv"),"population"),function(s){s[2]}),".csv"),function(s){s[1]})))}

#####
# test Pareto fronts
# -> on 100-200 seems fine - even if few replications

resdir = 'calibration/MESOCALIB_CALIB_LOCAL20_200_2000_20191129_122332/'
res <- as.tbl(read.csv(paste0(resdir,'population',latestgen(resdir),'.csv')))

g=ggplot(res[res$evolution.samples>2,],aes(x=popfit,y=indicsfit,color=alpha))
g+geom_point()

g=ggplot(res[res$evolution.samples>2,],aes(x=popfit,y=indicsfit,color=beta))
g+geom_point()

g=ggplot(res[res$evolution.samples>2,],aes(x=popfit,y=indicsfit,color=tsteps))
g+geom_point()
