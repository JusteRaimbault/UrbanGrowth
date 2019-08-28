
####
## Empirical analysis of GHS database

setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/GHSAnalysis'))

library(dplyr)
library(rgdal)
library(raster)
library(ggplot2)
library(reshape2)

source('functions.R')

ucdb <- readOGR(paste0(Sys.getenv('CS_HOME'),'/Data/JRC_EC/GHS/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0'),'GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0')

###
# Analysis
#  - summary
#  - basic scaling (log-log) for countries, continents, global (still cutoff ?)
#  - two param scaling
#  - powerlawness (check paper Harisson)
#  - variable urban area (k*radius)
#  - endogenous scaling ?


#####
# 1) Summary statistics

# countries
countrycount = as.tbl(ucdb@data) %>% group_by(CTR_MN_NM) %>% summarize(count=n())
countrycount[countrycount$count > 50,]

summary(ucdb$P15)

#####
# 2) Raw scaling

area_num_threshold = 50
#pop_threshold = 

countries = as.character(unlist(countrycount[countrycount$count > area_num_threshold,"CTR_MN_NM"]))

areas = as.tbl(ucdb@data) %>% filter(CTR_MN_NM%in%countries)

pops = melt(areas,measure.vars = c('P75','P90','P00','P15'),id.vars = c('ID_HDC_G0','CTR_MN_NM'))
#popcounts = pops %>% group_by(CTR_MN_NM,variable) %>% summarize(count=n())
pops = pops[pops$value>0,]

ranks = rep(NA,nrow(pops))
for(popvar in c('P75','P90','P00','P15')){for(country in countries){ranks[pops$variable==popvar&as.character(pops$CTR_MN_NM)==country]=order(pops$value[pops$variable==popvar&as.character(pops$CTR_MN_NM)==country],decreasing = T)}}
pops$logranks = log(ranks,base = 10)
pops$logpop = log(pops$value,base=10)

g=ggplot(pops,aes(x=logranks,y=logpop,color=CTR_MN_NM,linetype=variable,group=interaction(CTR_MN_NM,variable)))
g+stat_smooth(method = 'lm')
#g+geom_point()

# terrible - test to fit log-log linear models
regs = pops%>%group_by(variable,CTR_MN_NM)%>% summarise(alpha = lmreg(logranks,logpop)$alpha,rsquared = lmreg(logranks,logpop)$rsquared)

# seems very far from rank-size law - need other tests.

# check same with pop/gdp

gdpvars = c("GDP90_SM"='90',"GDP00_SM"='00',"GDP15_SM"='15')
popvars = c('P90'='90','P00'='00','P15'='15')
pops = melt(areas,measure.vars = names(popvars),id.vars = c('ID_HDC_G0','CTR_MN_NM'))
pops = pops[pops$value>0,]
pops$year = popvars[pops$variable]
gdps = melt(areas,measure.vars = names(gdpvars),id.vars = c('ID_HDC_G0','CTR_MN_NM'))
gdps$year = gdpvars[gdps$variable]
gdps = gdps[gdps$value>0,]

popgdp = left_join(pops,gdps,by=c('ID_HDC_G0'='ID_HDC_G0','CTR_MN_NM'='CTR_MN_NM','year'='year'))
names(popgdp)<-c('id','country','popvar','pop','year','gdpvar','gdp')
popgdp = popgdp[popgdp$pop>0&!is.na(popgdp$gdp),]
popgdp$logpop = log(popgdp$pop,base = 10)
popgdp$loggdp = log(popgdp$gdp,base = 10)

regs = popgdp%>%group_by(year,country)%>% summarise(alpha = lmreg(logpop,loggdp)$alpha,rsquared = lmreg(logpop,loggdp)$rsquared)

# -> better than rank-size but still not crazy.

####
# Targeted analysis

# european countries
unique(ucdb$CTR_MN_NM[ucdb$GRGN_L1=='Europe'])
countries=c('Finland','Norway','Sweden','Estonia','United Kingdom','Denmark','Latvia','Lithuania',
            'Germany','Poland','Ireland','Netherlands','Belgium','France','Czech Republic','Luxembourg',         
             'Slovakia','Austria','Hungary','Switzerland','Slovenia','Italy','Bulgaria',
             'Spain','Portugal','Greece')
# 'Belarus', Ukraine, 'Romania'
areas = as.tbl(ucdb@data) %>% filter(as.character(CTR_MN_NM)%in%countries)

g=ggplot(popgdp,aes(x=logpop,y=loggdp,color=country,linetype=popvar,group=interaction(country,popvar)))
g+stat_smooth(method = 'lm',se = F)

g=ggplot(popgdp,aes(x=logpop,color=country,linetype=popvar,group=interaction(country,popvar)))
g+geom_density()

g=ggplot(popgdp,aes(x=logpop,y=loggdp,color=country,shape=popvar,group=interaction(country,popvar)))
g+geom_point()+stat_smooth(method = 'lm',se = F,alpha=0.6)


#####
# "reproduction" of rank-size plot in geodivercity paper
# Urban systems : Brazil, China, Europe, Former Soviet Union, India, South Africa, United States





#####
## 3) Two param scaling (with xmin cutoff)
# (package powerlaw ?)













