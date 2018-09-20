
library(sp)

#'
#' Get successive distances between pareto fronts
#'
frontDiffs <- function(popdirectory,objectives=c('logmse','mselog')){
  files = sort(list.files(popdirectory))
  #show(files)
  gens = sapply(strsplit(files,'population'),function(l){sapply(strsplit(l[2],split = '.',fixed = T),function(l){as.numeric(l[1])})})
  dists = c()
  for(i in 2:length(files)){
    prevfront=read.csv(file=paste0(popdirectory,'/',files[i-1]))
    currentfront=read.csv(file=paste0(popdirectory,'/',files[i]))
    #dists = append(dists,sum(c(dist(as.matrix(prevfront[,objectives]),as.matrix(currentfront[,objectives])))))
    owndistprev = mean(c(dist(as.matrix(prevfront[,objectives]))))
    owndistcurrent = mean(c(dist(as.matrix(currentfront[,objectives]))))
    betweendist = mean(c(spDists(as.matrix(prevfront[,objectives]),as.matrix(currentfront[,objectives]))))
    dists = append(dists,2*betweendist/(owndistprev+owndistcurrent))
  }
  return(list(gens=gens[2:length(gens)],dists=dists))
}


