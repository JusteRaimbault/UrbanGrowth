setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/StylizedFacts'))

library(dplyr)
library(ggplot2)
library(sp)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))
source('functions.R')

resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Results/StylizedFacts/');dir.create(resdir)

countrycodes = c('EU','BR','CN','IN','RU','ZA','US')
cityfiles = list('EU'='Europe','BR'='Brazil','CN'='China','IN'='India','RU'='Russia','ZA'='South-Africa','US'='USA')

# rq : all of this may be quite sensitive to filtering done in data cleaning -> to be checked

dates=c("1960","1970","1980","1990","2000","2010")

populations = list()

for(countrycode in countrycodes){
  cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/clean/',cityfiles[[countrycode]],'.csv'))
  cities$ID=NULL;cities$Long=NULL;cities$Lat=NULL;cities$Name=NULL
  colnames(cities)=dates[(length(dates)-ncol(cities)+1):length(dates)]
  populations[[countrycode]]=cities
}


## distrib of populations
flatpops = data.frame(pop=unlist(populations),system=unlist(sapply(countrycodes,function(countrycode){rep(countrycode,length(unlist(c(populations[[countrycode]]))))})))

g=ggplot(flatpops[flatpops$pop>100,],aes(x=pop,group=system,color=system))
g+geom_density()+scale_x_log10()+xlab("Population")+ylab("Density")+scale_color_discrete(name="System")+stdtheme
ggsave(file=paste0(resdir,'popdistribs.png'),width=20,height=18,units='cm')

sortedpops=data.frame()
for(countrycode in countrycodes){
  pops = populations[[countrycode]]
  for(year in colnames(pops)){
    sortedpops=rbind(sortedpops,data.frame(pop=sort(pops[[year]],decreasing = T),rank=1:nrow(pops),system=rep(countrycode,nrow(pops)),year=rep(year,nrow(pops))))
  }
}

g=ggplot(sortedpops[sortedpops$pop>100,],aes(x=rank,y=pop,group=interaction(system,year),color=system,shape=year))
g+geom_point(alpha=0.3)+scale_x_log10()+scale_y_log10()+
  stat_smooth(method='lm',aes(linetype=year),alpha=0.7,se = F)+
  xlab("Rank")+ylab("Population")+scale_color_discrete(name="System")+stdtheme
ggsave(file=paste0(resdir,'popranksize.png'),width=20,height=18,units='cm')


# type of distrib of pop by year and system ?
distrs=c();systems=c();years=c()
for(system in countrycodes){
  for(year in dates){
    currentpop=sortedpops$pop[sortedpops$system==system&sortedpops$year==year&sortedpops$pop>100]
    if(length(currentpop)>0){
      # criteria : cvm,ad,ks,aic,bic
      bestdistr = getDistribType(currentpop,criteria = 'aic')$name
      distrs=append(distrs,bestdistr)
      years=append(years,year);systems=append(systems,system)
    }
  }
}

table(distrs)
table(data.frame(distrs,systems,years))
#hist(sortedpops$pop[sortedpops$system=='US'&sortedpops$year=='1990'])
# -> remove the zeros !
#hist(sortedpops$pop[sortedpops$system=='RU'&sortedpops$year=='2010'&sortedpops$pop>100])


growthrates = sapply(populations,
                     function(pops){
                       t(apply(pops,1,function(r){diff(r)/r[2:(length(r))]}))
                     }
                     )

flatgrowthrates = data.frame()
for(countrycode in countrycodes){
  pops = growthrates[[countrycode]]
  flatgrowthrates=rbind(flatgrowthrates,data.frame(g=unlist(c(pops)),system=rep(countrycode,length(unlist(c(pops))))))
}

#g=ggplot(flatgrowthrates[flatgrowthrates$g<quantile(flatgrowthrates$g,0.9),],aes(x=g,group=system,color=system))
g=ggplot(flatgrowthrates,aes(x=g,group=system,color=system))
g+geom_density()+xlab("Normalized growth rate")+ylab("Density")+scale_color_discrete(name="System")+stdtheme
ggsave(file=paste0(resdir,'growthrates.png'),width=20,height=18,units='cm')


# evol of growth rates

tgrowthrates=data.frame()
for(countrycode in countrycodes){
  gr = growthrates[[countrycode]]
  for(year in colnames(gr)){
    tgrowthrates=rbind(tgrowthrates,data.frame(gr=unlist(c(gr[,year])),system=rep(countrycode,nrow(gr)),year=rep(year,nrow(gr))))
    # do not remove empty ? ok negligible
  }
}

g=ggplot(tgrowthrates,aes(x=gr,group=system,color=system))
g+geom_density()+facet_wrap(~year)+xlab("Normalized growth rate")+ylab("Density")+
  scale_color_discrete(name="System")+stdtheme + theme(legend.justification=c(1,0), legend.position=c(0.95,-0.05))
ggsave(file=paste0(resdir,'growthrates_byyear.png'),width=30,height=20,units='cm')


####
# stat distrib of growth rates
distrs=c();systems=c();years=c()
for(system in countrycodes){
  for(year in dates){
    currentg=tgrowthrates$g[tgrowthrates$system==system&tgrowthrates$year==year]
    if(length(currentg)>0){
      # criteria : cvm,ad,ks,aic,bic
      bestdistr = getDistribType(currentg,criteria = 'ks')$name
      show(bestdistr)
      distrs=append(distrs,bestdistr)
      years=append(years,year);systems=append(systems,system)
    }
  }
}
table(distrs)
table(data.frame(distrs,systems,years))



######
# correlation of growth rates as a function of distance - in time

countrycode = 'US'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))

#apply(cities,2,function(col){length(which(is.na(col)))})

cities = cities[!is.na(cities$X2010),c("Long","Lat","X1960","X1970","X1980","X1990","X2000","X2010")]
cities=cities[apply(cities,1,function(r){length(which(is.na(r)))==0}),]

distmat = spDists(as.matrix(cities[,c('Long','Lat')]),longlat = T)

cities=cities[,c("X1960","X1970","X1980","X1990","X2000","X2010")]

growthrates = t(apply(cities,1,function(r){diff(r)/r[2:(length(r))]}))

quantiles = c(0.0,0.25,0.5,0.75,1)

quantile(c(distmat),quantiles)

#which(distmat<774)


