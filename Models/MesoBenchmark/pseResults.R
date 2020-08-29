
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/MesoBenchmark'))

library(ggplot2)
library(dplyr)
library(GGally)


files = list(
  'expmixture' = '20200828_1515_PSE_EXPMIXTURE_res/population20000.csv',
  'reactiondiffusion' = '20200828_1624_PSE_REACTIONDIFFUSION_res/population50000.csv'
)

indics = c('moran','distance','entropy','slope')

res = data.frame()
for(model in names(files)){
  currentres = as.tbl(read.csv(paste0('openmole/pse/',files[[model]])))
  res = rbind(res,cbind(currentres[,c(indics,'evolution.samples')],model=rep(model,nrow(currentres))))
}

ggpairs(res[res$slope>-4,indics],aes(color=model))


# for hypervolume intersection: https://rdrr.io/cran/hypervolume/man/hypervolume_set.html
