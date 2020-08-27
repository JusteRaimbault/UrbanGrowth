setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoBenchmark'))

library(ggplot2)
library(rgdal)
library(dplyr)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))
source('../MesoCalibration/functions.R')
source('../MesoCalibration/morphology.R')

ucdbsf <- loadUCDBData(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0')

years = c('1975','1990','2000','2015')
indics=c('totalPop','maxPop','minPop','moran','avgDist','entropy','alpha','alphaRSquared')
indicnames = list('totalPop'='Total population','maxPop'='Maximal population','minPop'='Minimal population',
                  'moran'='Spatial autocorrelation','avgDist'='Average distance','entropy'='Entropy',
                  'alpha'='Hierarchy','alphaRSquared'='Fierarchy fit')

# load morphologies: see ../MesoCalibration/dataPreparation.R and ../MesoCalibration/analysis.R
morphos <- read.csv(file='../MesoCalibration/configs/morphologies.csv',header = T,sep = ";")
inds = c(1:946,948:1000)
areasmorph = ucdbsf[inds,]
areasmorph$areaid = inds
areasmorph = left_join(areasmorph,morphos,by=c('areaid'='areaid'))








