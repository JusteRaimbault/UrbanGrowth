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

#countrycode = 'ZA'
countrycodes = c('EU','BR','CN','IN','RU','ZA','US')
#countrycodes = c('BR','CN','IN','RU','ZA','US')

# hand def colsnames (different number and names for europe)
popcolnames=list('EU'=c("X1961","X1971","X1981","X1991","X2001","X2011"),
                 'US'=c("X1960","X1970","X1980","X1990","X2000","X2010"),
                 'BR'=c("X1960","X1970","X1980","X1991","X2000","X2010"),
                 'CN'=c("X1964","X1982","X1990","X2000"),
                 'IN'=c("X1951","X1961","X1981","X1991","X2001","X2011"),
                 'RU'=c("X1959","X1970","X1979","X1989","X2002","X2010"),
                 'ZA'=c("X1960","X1970","X1980","X1991","X1996","X2001")
                 )

corresdir=paste0(resdir,'corrs/');dir.create(corresdir)

generatePeriods<-function(cols){
  res=list()
  for(l in 1:length(cols)){
    currentcols=list()
    for(i in 1:(length(cols)-l+1)){
      currentcols[[substring(cols[(i+l-1)],2)]]=cols[i:(i+l-1)]
    }
    res[[as.character(l)]]=currentcols
  }
  return(res)
}

for(countrycode in countrycodes){
  show(countrycode)
  if(countrycode!='EU'){
    cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
  }else{
    cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/tradeve/agglo.csv'),sep = ";",dec=',')
    colnames(cities)[6:13]=c('Long','Lat',"X1961","X1971","X1981","X1991","X2001","X2011")
  }
  #apply(cities,2,function(col){length(which(is.na(col)))})
  cities = cities[,c('Long','Lat',popcolnames[[countrycode]])]
  
  #cities = cities[!is.na(cities$X2010),c("Long","Lat","X1960","X1970","X1980","X1990","X2000","X2010")]
  cities=cities[apply(cities,1,function(r){length(which(is.na(r)))==0}),]
  #plot(SpatialPoints(cities[,c('Long','Lat')]))
  
  #distmat = spDists(SpatialPoints(cities[,c('Long','Lat')],proj4string = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
  distmat = spDists(as.matrix(cities[,c('Long','Lat')]),longlat = T)
  
  cities=cities[,popcolnames[[countrycode]]]
  deltats = diff(as.numeric(substring(colnames(cities),2)))
  
  growthrates = t(apply(cities,1,function(r){diff(r)/(r[2:(length(r))]*deltats)}))
  flatmat = c(distmat)
  
  #quantiles = c(0.0,0.25,0.5,0.75,1)
  for(quantiles in list(seq(0.0,1.0,0.25),seq(0.0,1.0,0.1),seq(0.0,1.0,0.05))){
    
    #periods = list('full'=colnames(growthrates))
    #periods = list('1970'="X1970",'1980'="X1980",'1990'="X1990",'2000'="X2000",'2010'="X2010")
    # generate periods
    for(periods in generatePeriods(colnames(growthrates))){
      
      dists = c();corrs=c();corrmin=c();corrmax=c();cperiods=c()
      # TODO add N
      for(i in 2:length(quantiles)){
        show(paste0(i,'/',length(quantiles)))
        # include both extrs to include max
        flatinds = which(flatmat>=quantile(flatmat[flatmat>0],quantiles[i-1])&flatmat<=quantile(flatmat[flatmat>0],quantiles[i]))
        rows = floor(flatinds / ncol(distmat))+1;cols=flatinds%%ncol(distmat)+1
        currentdists = flatmat[flatinds]
        #uids = unique(paste0(rows,"-",cols))
        # quite dirty
        uids = unique(sapply(1:length(rows),function(i){paste0(sort(c(rows[i],cols[i])),collapse='-')}))
        suids = strsplit(uids,'-')
        urows=sapply(suids,function(l){as.numeric(l[1])});ucols=sapply(suids,function(l){as.numeric(l[2])})
        for(periodname in names(periods)){
          show('\tperiodname')
          currentg = growthrates[,periods[[periodname]]]
          # data on which correlation is estimated is x = x_i_t, y = x_j_t
          if(!is.null(dim(currentg))){# shitty behavior to project by default
            rho = cor.test(x=unlist(c(currentg[urows,])),y=unlist(c(currentg[ucols,])))
          }else{
            rho = cor.test(x=unlist(c(currentg[urows])),y=unlist(c(currentg[ucols])))
          }
          dists=append(dists,mean(currentdists));corrs=append(corrs,rho$estimate);
          corrmin=append(corrmin,rho$conf.int[1]);corrmax=append(corrmax,rho$conf.int[2]);cperiods=append(cperiods,periodname)
        }
      }
      # -> time window and distance bin which maximize the quality of estimation of correlation : which criteria ?
      # ex. : average ratio rho / size of confidence int ; quantity of overlap between two successive points
      
      g=ggplot(data.frame(rho=corrs,rhomin=corrmin,rhomax=corrmax,distance=dists,period=cperiods),aes(x=distance,y=rho,ymin=rhomin,ymax=rhomax,group=period,color=period))
      g+geom_line()+geom_point()+geom_errorbar()+
        xlab("Distance")+ylab("Correlation")+scale_color_discrete(name="Period")+ggtitle(countrycode)+stdtheme
      ggsave(file=paste0(corresdir,'corrsdist_',countrycode,'_quantiles',length(quantiles),'_periods',length(periods),'.png'),width=20,height=18,units='cm')
    }
  }
}



##### Summary stats (pop in time)
# -> ok done in geodivercity papers

## developments : - nonstat in time for some city systems ?
# - calib at the second order on correlation curves (-> new methodo approach - link with correlated synthetic data !)
# (many things are connected)

