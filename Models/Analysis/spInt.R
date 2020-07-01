
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/UrbanGrowth-model'))

systems = c('ZA','CN','US','BR','EU','IN','RU')

for(system in systems){
  show(system)
  populations = read.csv(paste0('data/processed/',system,'_pops.csv'),header = F)
  dmat = read.csv(paste0('data/processed/',system,'_dist.csv'),header = F)
  dists = rowSums(dmat)
  y=c();x=c();d=c()
  for(t in 2:ncol(populations)){
    y = append(y,log(populations[,t]))
    x = append(x,log(populations[,t-1]))
    d = append(d,dists)
    #model = lm(data = data.frame(y=y,x=x,d=dists),y~x+d)
    #sum((predict(model) - y)^2)
  }
  model = lm(data = data.frame(y=y,x=x,d=d),y~x+d)
  show(sum((predict(model) - y)^2)/length(y))
  show(summary(model))
}

