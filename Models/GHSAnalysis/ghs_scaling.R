
####
## Empirical analysis of GHS database

setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/GHSAnalysis'))

library(dplyr)
library(rgdal)
library(raster)
library(ggplot2)
library(reshape2)
library(corrplot)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

source('functions.R')


ucdb <- readOGR(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0',stringsAsFactors = F)

###
# Analysis
#  - summary
#  - basic scaling (log-log) for countries, continents, global (still cutoff ?)
#  - two param scaling
#  - powerlawness (check paper Harisson)
#  - variable urban area (k*radius)
#  - endogenous scaling ?

resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/GHS/')


#### Data formatting

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


## export for simulation
targetdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/GHSL/processed/');dir.create(targetdir)
areastoexport = areas[areas$P15>quantile(areas$P15,c(0.96)),]
# distance matrix
eucldists = spDists(SpatialPoints(areastoexport[,c("GCPNT_LON","GCPNT_LAT")],proj4string = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),longlat = T)
write.table(eucldists,file=paste0(targetdir,'world_dist.csv'),row.names = F,col.names = F,sep=',')
write.table(areastoexport[,c('P75','P90','P00','P15')],file=paste0(targetdir,'world_pops.csv'),row.names = F,col.names = F,sep=",")
write.table(c(1975,1990,2000,2015),file=paste0(targetdir,'world_dates.csv'),row.names = F,col.names = F,sep=",")




#####
# 1) Summary statistics

dir.create(paste0(resdir,'Summary'))

# countries
countrycount = as.tbl(ucdb@data) %>% group_by(CTR_MN_NM) %>% summarize(count=n())
#countrycount[countrycount$count > 50,]
summary(countrycount$count)

allcountries = as.character(unlist(countrycount[countrycount$count > area_num_threshold,"CTR_MN_NM"]))



######

allpops = melt(areas,measure.vars = c('P75','P90','P00','P15'),id.vars = c('ID_HDC_G0','CTR_MN_NM'))
allpops$year = substr(allpops$variable,2,4)
allpops$year = ifelse(allpops$year%in%c("75","90"),paste0("19",allpops$year),paste0("20",allpops$year))
allpops$population = allpops$value

summary(ucdb$P15);summary(ucdb$P00);summary(ucdb$P90);summary(ucdb$P75)

g=ggplot(allpops[allpops$population>50000,],aes(x=year,y=population,group=year))
g+geom_boxplot(outlier.size = 1)+scale_y_continuous(trans="log10")+xlab("Year")+ylab("Population")+stdtheme
ggsave(filename = paste0(resdir,'Summary/population_distributions.png'),width=20,height=15,units='cm')


### Correlations

# corrmats in 2000 and 2015
corvars = c('DP','DG','DE','DB','P','B','E','G')
corvarsnames = c('DeltaPop','DeltaGDP','DeltaEm','DeltaBuilt','Pop','Built','Emissions','GDP')
for(year in c('00','15')){
  rho = matrix(0,length(corvars),length(corvars));colnames(rho)<-corvarsnames;rownames(rho)<-corvarsnames
  rhomin = rho;rhomax = rho
  for(i in 1:length(corvars)){
    for(j in 1:length(corvars)){
      rhotest = cor.test(areas[,paste0(corvars[i],year)],areas[,paste0(corvars[j],year)])
      rho[i,j]=rhotest$estimate;rhomin[i,j]=rhotest$conf.int[1];rhomax[i,j]=rhotest$conf.int[2]
    }
  }
  png(filename = paste0(resdir,'Summary/correlations_',year,'.png'),width = 20,height = 20,units='cm',res = 300)
  corrplot(rho,lowCI.mat = rhomin, uppCI.mat = rhomax,type = 'upper',title = paste0('20',year),bg='lightgrey',
           plotCI = 'circle',
           addCoef.col = "black",
           mar = c(0,0,1,0)
           #order='hclust',
           #method='ellipse'
           )
  dev.off()
}








#####
# 2) Rank-size / summary / scaling for regions and countries

dir.create(paste0(resdir,'SimpleScaling'))


# EU countries : comparison EU/Europe
#europe = as.character(unique(ucdb$CTR_MN_NM[ucdb$GRGN_L1=='Europe']))
eu=c('Finland','Sweden','Estonia','United Kingdom','Denmark','Latvia','Lithuania',
            'Germany','Poland','Ireland','Netherlands','Belgium','France','Czech Republic','Luxembourg',         
             'Slovakia','Austria','Hungary','Slovenia','Italy','Bulgaria',
             'Spain','Portugal','Greece','Romania','Malta')

# does not make sense : overlap !
#simpleScaling(areas,list('Europe'=europe,'EU'=eu),resdir,withPlot = T)


#####
# cybergeo paper
# "reproduction" of rank-size plot in geodivercity paper
# Urban systems : Brazil, China, Europe, Former Soviet Union, India, South Africa, United States

cybresults = simpleScaling(areasdata = areas,regions = list('Europe'=eu,
                         'China'=c('China'),
                         'Brazil'=c('Brazil'),
                         'India'=c('India'),
                         'SouthAfrica'=c('South Africa'),
                         'UnitedStates'=c('United States'),
                         'FormerSovietUnion'=c('Kazakhstan','Kyrgyzstan','Tajikistan','Turkmenistan',
                                               'Uzbekistan','Belarus','Moldova','Ukraine','Russia','Armenia',
                                               'Azerbaijan','Georgia')),
              figresdir = resdir,withPlot = T)

write.table(cybresults,row.names = F,sep="&",file = paste0(resdir,'SimpleScaling/geodivercity.csv'))


####
# Continents




asean=c('Indonesia','Thailand','Malaysia','Singapore','Philippines','Vietnam','Myanmar','Cambodia','Laos','Brunei')
mercosur=c('Argentina','Brazil','Paraguay','Uruguay','Bolivia','Chile','Colombia','Ecuador','Guyana','Peru','Suriname')

#areas = ucdb@data[as.character(ucdb$CTR_MN_NM)=='France',]
countries=c('France')







#####
## 3) Two param scaling (with xmin cutoff)
# (package powerlaw ?)













