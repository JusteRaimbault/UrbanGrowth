
library(fitdistrplus)
library(poweRlaw)


#powerlawfit <- function(x,y)


#areasdata=areas
#regions = list('China'=c('China'),'NorthAmerica'=c('United States','Canada'))
# regions = list('Europe'=europe,'EU'=eu)
rm(areasdata,regions,figresdir)
simpleScaling <- function(areasdata, 
                          regions, #list(reg1 = c(countries),...)
                          figresdir,
                          countrycol = 'country',
                          withPlot = F,
                          area_num_threshold=10,
                          popthreshold=50000, # do not include areas which were smaller at previous dates
                          popvars = c('P90','P00','P15'), #c('P75','P90','P00','P15'),
                          years = c('90','00','15'),#c('75','90','00','15'),
                          scalingvars = list('Built'='B','GDP'='G','Emissions'='E'),
                          areasids = c('ID_HDC_G0','country'),
                          regionnames=NULL
){
  allregions = unlist(regions)
  if(is.null(regionnames)){regionnames = paste(names(regions),sep="",collapse = "-")}
  allyears = paste(years,sep="",collapse = "-")
  
  pops = melt(areasdata[areasdata[,countrycol]%in%allregions,],measure.vars = popvars,id.vars = areasids)
  pops = pops[pops$value>popthreshold,]
  
  # ranks and summary by year and region
  ranks = rep(NA,nrow(pops))
  cyears=c();cregions=c();cpops=c();cranksizealpha=c();cranksizersquared=c();cranksizerelci=c();ccities = c()
  cprimacy=c();
  for(popvar in popvars){
    for(region in names(regions)){
      currentyear = substr(popvar,2,4);ifelse(currentyear%in%c("75","90"),paste0("19",currentyear),paste0("20",currentyear))
      currentregion = regions[[region]]
      regionrows = pops$variable==popvar&pops[,countrycol]%in%currentregion
      currentpops = pops$value[regionrows]
      currentranks = rank(-currentpops)
      #ranks[regionrows]=(1:length(which(regionrows)))[order(pops$value[regionrows],decreasing = T)]
      ranks[regionrows]=currentranks
      
      # summary stats
      ranksizefit = lmreg(log(currentranks,base=10),log(currentpops,base=10))
      cyears=append(cyears,currentyear);cregions=append(cregions,region);
      cpops=append(cpops,sum(currentpops));
      cranksizealpha=append(cranksizealpha,ranksizefit$alpha);cranksizersquared=append(cranksizersquared,ranksizefit$rsquared);cranksizerelci=append(cranksizerelci,abs(ranksizefit$alpha/ranksizefit$sigmaalpha))
      ccities = append(ccities,length(currentpops));
      cprimacy = append(cprimacy,currentpops[currentranks==1]/currentpops[currentranks==2])
    }
  }
  summarydf = data.frame(
    year=cyears,region=cregions,population=cpops,cities=ccities,primacy=cprimacy,ranksizealpha=cranksizealpha,
    ranksizersquared=cranksizersquared,ranksizerelci=cranksizerelci
  )
  
  pops$pops = pops$value
  pops$ranks = ranks
  #pops$logranks = log(ranks,base = 10)
  #pops$logpop = log(pops$value,base=10)
  pops$year = substr(pops$variable,2,4)
  pops$year = ifelse(pops$year%in%c("75","90"),paste0("19",pops$year),paste0("20",pops$year))
  
  # add region attribute
  pops$region = rep(NA,nrow(pops))
  for(i in 1:nrow(pops)){for(region in names(regions)){if(pops[i,countrycol]%in%regions[[region]]){pops$region[i] = region}}}
  
  if(withPlot){
    
    g=ggplot(pops,aes(x=ranks,y=pops,color=region,linetype=year,shape=year,group=interaction(region,year)))
    g+stat_smooth(method = 'lm',se = F)+geom_point(alpha=0.5)+scale_x_log10()+scale_y_log10()+
      scale_color_discrete(name='Region')+scale_linetype_discrete(name='Year')+scale_shape_discrete(name='Year')+
      xlab('Rank')+ylab('Population')+stdtheme
    ggsave(file=paste0(figresdir,'SimpleScaling/RankSize-fitted_',regionnames,'_years-',allyears,'.png'),width=25,height=22,units='cm')
    
    # also rank size plot without fitted
    g=ggplot(pops,aes(x=ranks,y=pops,color=region,linetype=year,shape=year,group=interaction(region,year)))
    g+geom_line(size=0.7)+geom_point(alpha=0.5)+scale_x_log10()+scale_y_log10()+
      scale_color_discrete(name='Region')+scale_linetype_discrete(name='Year')+scale_shape_discrete(name='Year')+
      xlab('Rank')+ylab('Population')+stdtheme
    ggsave(file=paste0(figresdir,'SimpleScaling/RankSize_',regionnames,'_years-',allyears,'.png'),width=25,height=22,units='cm')
    
  }
  
  
  # Variables scaling

  for(scalingvarname in names(scalingvars)){
    currentscalingvars = paste0(scalingvars[[scalingvarname]],years)
    
    pops = melt(areasdata[areasdata[,countrycol]%in%allregions,],measure.vars = popvars,id.vars = areasids)
    pops = pops[pops$value>popthreshold,]
    pops$year = substr(pops$variable,2,4);pops$year=ifelse(pops$year%in%c("75","90"),paste0("19",pops$year),paste0("20",pops$year))
    
    scaling = melt(areasdata[areasdata[,countrycol]%in%allregions,],measure.vars = currentscalingvars,id.vars = areasids)
    scaling$year = substr(scaling$variable,2,4);scaling$year=ifelse(scaling$year%in%c("75","90"),paste0("19",scaling$year),paste0("20",scaling$year))
    scaling = scaling[scaling$value>0,]
  
    joinnames = c('year'='year');for(id in areasids){joinnames[id]=id}
    allvars = left_join(pops,scaling,by=joinnames)
    names(allvars)<-c('id','country','popvar','pop','year','scalingvar','scaling')
    allvars = allvars[allvars$pop>0&!is.na(allvars$scaling),]
    
    allvars$region = rep(NA,nrow(allvars))
    for(i in 1:nrow(allvars)){for(region in names(regions)){if(allvars[i,countrycol]%in%regions[[region]]){allvars$region[i] = region}}}
    
    
    summarydf[,paste0(scalingvarname,'alpha')]=rep(NA,nrow(summarydf))
    summarydf[,paste0(scalingvarname,'rsquared')]=rep(NA,nrow(summarydf))
    summarydf[,paste0(scalingvarname,'relci')]=rep(NA,nrow(summarydf))
    for(popvar in popvars){
      for(region in names(regions)){
        currentyear = substr(popvar,2,4);ifelse(currentyear%in%c("75","90"),paste0("19",currentyear),paste0("20",currentyear))
        currentregion = regions[[region]]
        regionrows = allvars$popvar==popvar&allvars[,countrycol]%in%currentregion
        currentpops = allvars$pop[regionrows];currentscaling = allvars$scaling[regionrows]
        show(currentyear);show(currentregion);show(scalingvarname)
        reg = lmreg(log(currentpops,base=10),log(currentscaling,base=10))
        
        summarydf[summarydf$year==currentyear&summarydf$region==region,paste0(scalingvarname,'alpha')]=reg$alpha
        summarydf[summarydf$year==currentyear&summarydf$region==region,paste0(scalingvarname,'rsquared')]=reg$rsquared
        summarydf[summarydf$year==currentyear&summarydf$region==region,paste0(scalingvarname,'relci')]=abs(reg$alpha/reg$sigmaalpha)
      }
    }
  
    if(withPlot){
      g=ggplot(allvars,aes(x=pop,y=scaling,color=region,linetype=year,shape=year,group=interaction(region,year)))
      g+stat_smooth(method = 'lm')+geom_point(alpha=0.3,size=0.5)+scale_x_log10()+scale_y_log10()+
        scale_color_discrete(name='Region')+scale_linetype_discrete(name='Year')+scale_shape_discrete(name='Year')+
        xlab('Population')+ylab(scalingvarname)+stdtheme
      ggsave(file=paste0(figresdir,'SimpleScaling/',scalingvarname,'-fitted_',regionnames,'_years-',allyears,'.png'),width=25,height=22,units='cm')
    }
      
  }
  return(summarydf)
  
}





lmreg <- function(x,y){
  reg = lm(y~x,data=data.frame(x,y))
  return(list(alpha = summary(reg)$coefficients[2,1],
              rsquared = summary(reg)$adj.r.squared,
              sigmaalpha = summary(reg)$coefficients[2,2]
              )
         )
}




getDistribType <- function(x,distrnames = c("norm", "lnorm","pois", "exp", "gamma", "nbinom","geom", "unif","logis"),criteria="ks"){
  # other potential distribs (need additional parameters depending on the estimation method)
  #,"weibull")
  fits = list()
  for(distrname in distrnames){
    tofit=x-min(x)+1
    #if(min(tofit)<0&distrname%in%c("lnorm","exp","gamma")){tofit=tofit-min(tofit)+1}# rq : should generally normalize ?
    fits[[distrname]] = fitdistrplus::fitdist(tofit,distr = distrname,method = "mme")
    #fits[[distrname]] = fitdistrplus::fitdist(x,distr = distrname,method = "mge",gof='KS')
    # mme is definitively the most generic (regarding distribs)
  }
  
  # available goodness of fit statistics
  #gofstat(fits)
  #gofstat(fits)$kstest
  #gofstat(fits)$cvmtest
  #gofstat(fits)$adtest
  
  gof = gofstat(fits)[[criteria]]
  #show(sapply(gof,is.finite))
  gof=gof[is.finite(gof)]
  bestdistr= distrnames[which(gof==min(gof))[1]]
  return(list(
    name=bestdistr,
    fit = fits[[bestdistr]]
  )
  )
  
}
