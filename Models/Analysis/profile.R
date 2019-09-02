setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/urbangrowth/openmole/calibration'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

source(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/Analysis/functions.R'))

# parameters : where calibration results are stored and where to store result figures
#sourcedir = 'PROFILE_GRID_intgib_BR_20181219_150953/'
sourcedir = 'PROFILE_GRID_intgib_BR_20181221_103649/'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/Calibration/',sourcedir);dir.create(resdir)


#res=as.tbl(read.csv(file=paste0(sourcedir,'population6899.csv')))
res=as.tbl(read.csv(file=paste0(sourcedir,'population20000.csv')))

# g=ggplot(res[res$gravityDecay<=1000,],aes(x=gravityDecay,y=logmse))
g=ggplot(res,aes(x=gravityDecay,y=logmse))
g+geom_point()+geom_line()+stdtheme
ggsave(file=paste0(resdir,'profile_logmse-gravityDecay_gen20000.png'),width=15,height = 10,units='cm')

# Q : 
#  - more precise profile in the [0,100] interval ? -> relaunch more precise
#    -> 0 : exp(-d/d0) -> 0 : Gibrat model : if is better, model does not improve ?
#
#  - difference with grid results ? try a parcimonious grid

