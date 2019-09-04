
library(fitdistrplus)
library(poweRlaw)


#powerlawfit <- function(x,y)



simpleScaling <- function(areasdata,
                          withPlot = F,
                          area_num_threshold=10,
                          popvars = c('P75','P90','P00','P15'),
                          years = c('75','90','00','15'),
                          scalingvars = c('B','G','E'),
                          areasids = c('ID_HDC_G0','CTR_MN_NM')
){
  
  pops = melt(areas,measure.vars = popvars,id.vars = areasids)
  #popcounts = pops %>% group_by(CTR_MN_NM,variable) %>% summarize(count=n())
  pops = pops[pops$value>0,]
  
  ranks = rep(NA,nrow(pops))
  for(popvar in popvars){
    for(country in countries){
      ranks[pops$variable==popvar&as.character(pops$CTR_MN_NM)==country]=order(pops$value[pops$variable==popvar&as.character(pops$CTR_MN_NM)==country],decreasing = T)
      }
    }
  pops$pops = pops$value
  pops$ranks = ranks
  pops$logranks = log(ranks,base = 10)
  pops$logpop = log(pops$value,base=10)
  
  if(withPlot){
    g=ggplot(pops,aes(x=logranks,y=logpop,color=CTR_MN_NM,linetype=variable,group=interaction(CTR_MN_NM,variable)))
    g+stat_smooth(method = 'lm')+geom_point()
  }
  
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
