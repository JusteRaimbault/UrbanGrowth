
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole/calibration'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))

sourcedir = 'CALIB_GRID_intgib_BR_20181209_185930/'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Calibration/',sourcedir);dir.create(resdir)

indics=c('logmse','mselog')

vols=hypervolumes(sourcedir,indics)
g=ggplot(data=data.frame(generation=vols$gens,volume=vols$vols),aes(x=generation,y=volume))
g+geom_point()+geom_line()
#ggsave(file=paste0(figdir,'hypervolume.png'),width=15,height=10,units='cm')

g=ggplot(data=data.frame(generation=vols$gens[2:length(vols$gens)],volumediff=diff(vols$vols)/max(vols$vols)),aes(x=generation,y=volumediff))
g+geom_point()+geom_line()
#ggsave(file=paste0(figdir,'hypervolumediff.png'),width=15,height=10,units='cm')

dists = frontDiffs(sourcedir)
g=ggplot(data=data.frame(generation=dists$gens,distance=dists$dists),aes(x=generation,y=distance))
g+geom_point()+geom_line()
ggsave(file=paste0(resdir,'frontdiffs.png'),width=15,height=10,units='cm')


## 
# specific measure for 2d pareto fronts ?


pop29000 = read.csv(file=paste0(sourcedir,'population29000.csv'))
pop30000 = read.csv(file=paste0(sourcedir,'population30000.csv'))
pop31000 = read.csv(file=paste0(sourcedir,'population31000.csv'))

g=ggplot(data.frame(rbind(cbind(pop29000,gen=rep("29",nrow(pop29000))),cbind(pop30000,gen=rep("30",nrow(pop30000))),cbind(pop31000,gen=rep("31",nrow(pop31000))))),aes(x=logmse,y=mselog,color=gen))
g+geom_point()


p29 = Polygon(pop29000[c(chull(x=pop29000$logmse,y=pop29000$mselog),1),indics],hole = F)
p30 = Polygon(pop30000[c(chull(x=pop30000$logmse,y=pop30000$mselog),1),indics],hole = F)
p31 = Polygon(pop31000[c(chull(x=pop31000$logmse,y=pop31000$mselog),1),indics],hole = F)

# -> delta polygon area ? - in min area

vols = polygonVolumes(sourcedir,bounds=c(33.5,800))
g=ggplot(data=data.frame(generation=dists$gens,volume=vols$volumes),aes(x=generation,y=volume))
g+geom_point()+geom_line()
ggsave(file=paste0(resdir,'volumes.png'),width=15,height=10,units='cm')

vols = polygonVolumes(sourcedir)
g=ggplot(data=data.frame(generation=dists$gens,volume=vols$volumes),aes(x=generation,y=volume))
g+geom_point()+geom_line()
ggsave(file=paste0(resdir,'volumes_nobounds.png'),width=15,height=10,units='cm')


# compare with no grid
oldcalib = 'CALIB_intgib_BR_20181003_154646'
#'CALIB_intgib_BR_20181003_154646/population42744.csv'

d = rbind(cbind(pop31000,type='grid'),cbind(read.csv(file=paste0(oldcalib,'/population42744.csv')),type='local'))
g=ggplot(d,aes(x=logmse,y=mselog,color=type))
g+geom_point()
# -> what the fuck ? check num of cities







