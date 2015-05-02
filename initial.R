train<-read.csv("train.csv",stringsAsFactors=F)
key<-read.csv("key.csv",stringsAsFactors=F)
str(key)

weather<-read.csv("weather.csv",stringsAsFactors=F)

#list<-strsplit(weather$codesum," ")
#codesum<-as.data.frame(unique(unlist(list, use.names = FALSE)))
#colnames(codesum)<-c('code')
#codesum<-subset(codesum,code != "")
#codesum<- data.frame(lapply(codesum, as.character), stringsAsFactors=FALSE)

#for (i in 1:nrow(codesum))
#{
#  weather[,codesum[i,1]]<-NA
#}

#rm(i)

#for (m in (ncol(weather)-nrow(codesum)+1):ncol(weather))
#{
#  for(i in 1:nrow(weather))
#  {
#    weather[i,m]<-ifelse((grepl(names(weather[m]),weather$codesum[i],fixed=T)),'1','0')
#  }
#}

#rm(list)
#rm(i)
#rm(m)
#rm(codesum)

weather[ weather == "M" ] = NA
weather[ weather == "-" ] = NA
weather[ weather == " " ] = NA



#for (i in 1:nrow(weather))

#{

#  m<-21

#  for(j in m:ncol(weather))

# {

#   weather[i,j]<- ifelse(is.na(weather$codesum[i]),NA,weather[i,j])
#  }
#}

null_df<-weather
for (i in 1:ncol(null_df))
{
  null_df[,i]<-NA
  
}

null_df<-na.omit(null_df)

for (i in 1:ncol(null_df))
{
  null_df[1,i]<-nrow(weather)
  null_df[2,i]<-sum(is.na(weather[,i]))
  null_df[3,i]<-round((null_df[2,i]/null_df[1,i])*100,2)
}

weather$date<-as.Date(weather$date)
weather$day<-weekdays(weather$date)
weather$month<-as.numeric(format(weather$date,"%m"))
weather$year<-as.numeric(format(weather$date,"%y"))
weather$weeknum<-as.numeric(format(weather$date,"%U"))


weather$codesum<-NULL
weather$depart<-NULL
weather$sunrise<-NULL
weather$sunset<-NULL
weather$codesum<-NULL
weather$snowfall<-NULL


########################################################


library(plyr)
weather_key<- join(weather, key, by = "station_nbr")

train$store_nbr_date<-paste(train$store_nbr,"_",train$date,sep="")

weather_key$store_nbr_date<-paste(weather_key$store_nbr,"_",weather_key$date,sep="")

str(weather_key)
str(train)

train_final<- join(weather_key, train, by = "store_nbr_date")

str(train_final)

#train_final$id<-paste(train_final$store_nbr,"_",train_final$item_nbr,"_",train_final$date,sep="")

train_final$date<-NULL
train_final$store_nbr_date<-NULL
train_final$store_nbr<-NULL
train_final$date<-NULL

rm(key)
rm(train)
rm(weather)
rm(weather_key)


train_final$tmax<-as.numeric(train_final$tmax)
train_final$tmin<-as.numeric(train_final$tmin)
train_final$tavg<-as.numeric(train_final$tavg)
train_final$dewpoint<-as.numeric(train_final$dewpoint)
train_final$wetbulb<-as.numeric(train_final$wetbulb)
train_final$heat<-as.numeric(train_final$heat)
train_final$cool<-as.numeric(train_final$cool)
train_final$preciptotal<-as.numeric(train_final$preciptotal)
train_final$stnpressure<-as.numeric(train_final$stnpressure)
train_final$sealevel<-as.numeric(train_final$sealevel)
train_final$resultspeed<-as.numeric(train_final$resultspeed)
train_final$resultdir<-as.numeric(train_final$resultdir)
train_final$avgspeed<-as.numeric(train_final$avgspeed)
train_final$day<-as.factor(train_final$day)

library(caTools)

set.seed(3000)
split = sample.split(train_final$units, SplitRatio = 0.7)
train = subset(train_final, split==TRUE)
test = subset(train_final, split==FALSE)


library(randomForest)

rf<-randomForest(units~.,data = train, ntree=200, nodesize=25)
PredictForest = predict(rf, newdata = test)
table(test$units, train$units) 
