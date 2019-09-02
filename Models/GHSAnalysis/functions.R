
library(fitdistrplus)
library(poweRlaw)


powerlawfit <- function(x,y)


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
