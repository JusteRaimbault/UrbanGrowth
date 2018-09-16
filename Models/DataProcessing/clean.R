
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/DataProcessing'))

targetDir<- paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/clean/');dir.create(targetDir)

cityfiles = list('FR'='France','BR'='Brazil','CN'='China','IN'='India','RU'='Russia','ZA'='South-Africa','US'='USA')

# france : data ok
countrycode = 'FR'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")

# china : idem
countrycode = 'CN'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")

# brazil : remove altitude
countrycode = 'BR'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
cities = cities[,c("ID","Name","Long","Lat","X1960","X1970","X1980","X1991","X2000","X2010")]
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")


# india : keep data only after 1961
countrycode = 'IN'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
cities = cities[,c("ID","Name","Long","Lat","X1961","X1981","X1991","X2001","X2011")]
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")


# Russia
countrycode = 'RU'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
# can assume the 0 are real 0 for Russia (new soviet towns)
# just put negligible number instead as the model can not deal with empty towns
#cities = cities[,c("ID","Name","Long","Lat","X1840","X1856","X1897","X1926","X1939","X1959","X1970","X1979","X1989","X2002","X2010")]
# keep after 1960 for comparability between systems of cities
cities = cities[,c("ID","Name","Long","Lat","X1959","X1970","X1979","X1989","X2002","X2010")]
for(j in 5:ncol(cities)){cities[,j]=as.numeric(as.character(cities[,j]))}
cities[cities==0.0]=0.1
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")

# South Africa
countrycode='ZA'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
# remove nas and <10000 in 2001
cities=cities[!is.na(cities$X2001),];cities=cities[cities$X2001>10000,]
# keep after 1960 and put negligible values instead of nas (Bantoustans)
cities = cities[,c("ID","Name","Long","Lat","X1960","X1970","X1980","X1991","X1996","X2001")]
cities[is.na(cities)]=0.1
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")

# US
countrycode='US'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
# idem south africa
cities=cities[!is.na(cities$X2010),];cities=cities[cities$X2010>10000,]
# keep after 1960 (should do non stationary version of the model otherwise)
# -> model with appearing cities ? much more complicated..
cities = cities[,c("ID","Name","Long","Lat","X1960","X1970","X1980","X1990","X2000","X2010")]
cities[is.na(cities)]=0.1
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")



