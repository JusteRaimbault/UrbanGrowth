
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Models/DataProcessing'))

targetDir<- paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/clean/');dir.create(targetDir)

cityfiles = list('FR'='France','BR'='Brazil','CN'='China','IN'='India','RU'='Russia','ZA'='South-Africa','US'='USA')

#' TODO : option to remove small or not

# france : data ok
#countrycode = 'FR'
#cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
#write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")
# work on Europe instead

# europe
countrycode = 'EU'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/tradeve/agglo.csv'),sep=";",dec = ",")
cities = cities[,c("Unit_Code_Ori","Name","X_WGS","Y_WGS","Pop_1961","Pop_1971","Pop_1981","Pop_1991","Pop_2001","Pop_2011")]
colnames(cities)<-c("ID","Name","Long","Lat","X1961","X1971","X1981","X1991","X2001","X2011")
# dim = 3982   10
#cities[apply(cities,1,function(r){length(which(is.na(r)))>0}),]
# remove nas (20 rows)
cities=cities[apply(cities,1,function(r){length(which(is.na(r)))==0}),]
#cities = cities[cities$X2011>=median(cities$X2011),]
# for memory purposes, diminish significantly the number of cities
cities = cities[cities$X2011>=quantile(cities$X2011,0.75),]
write.table(cities,file=paste0(targetDir,'Europe.csv'),row.names = F,sep=",")


# china : idem
# for china do not remove below medium-sized as already well aggregated, no small areas in the db
countrycode = 'CN'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")

# brazil : remove altitude
# and small areas
countrycode = 'BR'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
cities = cities[,c("ID","Name","Long","Lat","X1960","X1970","X1980","X1991","X2000","X2010")]
cities = cities[cities$X2010>=median(cities$X2010),]
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")


# india : keep data only after 1961
countrycode = 'IN'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
cities = cities[,c("ID","Name","Long","Lat","X1961","X1981","X1991","X2001","X2011")]
cities=cities[apply(cities[,c("X1961","X1981","X1991","X2001","X2011")],1,function(r){length(which(is.na(r)))==0}),]
cities = cities[cities$X2011>=median(cities$X2011),]
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
cities = cities[cities$X2010>=median(cities$X2010),]
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")

# South Africa
countrycode='ZA'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
# remove nas and <10000 in 2001
cities=cities[!is.na(cities$X2001),];
#cities=cities[cities$X2001>10000,]
# keep after 1960 and put negligible values instead of nas (Bantoustans)
cities = cities[,c("ID","Name","Long","Lat","X1960","X1970","X1980","X1991","X1996","X2001")]
cities[is.na(cities)]=0.1
cities = cities[cities$X2001>=median(cities$X2001),]
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")

# US
countrycode='US'
cities <- read.csv(paste0(Sys.getenv('CS_HOME'),'/UrbanGrowth/Data/Geodivercity/data/',cityfiles[[countrycode]],'.csv'))
# idem south africa
cities=cities[!is.na(cities$X2010),];
#cities=cities[cities$X2010>10000,]
# keep after 1960 (should do non stationary version of the model otherwise)
# -> model with appearing cities ? much more complicated..
cities = cities[,c("ID","Name","Long","Lat","X1960","X1970","X1980","X1990","X2000","X2010")]
cities[is.na(cities)]=0.1
cities = cities[cities$X2010>=median(cities$X2010),]
write.table(cities,file=paste0(targetDir,cityfiles[[countrycode]],'.csv'),row.names = F,sep=",")



