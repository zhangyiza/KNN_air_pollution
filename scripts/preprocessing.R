################################
#KNN for air quality prediction
#Preprocessing
#author: yingzhi
################################

#import the dataset
total_data<-read.csv('airdata.csv', encoding = 'UTF-8')

#convert UMT_time to timestamp
total_data$UMT_time<-as.numeric(as.POSIXct(as.character(total_data$UMT_time), format = "%Y%m%d%H%M%S"))

#convert to numeriec
total_data[,c(6:13,15,16)]<-sapply(total_data[,c(6:13,15,16)] , function(x){as.numeric(as.character(x))})

#subset
subarea_data<-total_data[total_data$Latitude>35&total_data$Latitude<40&total_data$Longitude>115&total_data$Longitude<120,]
week_data<-total_data[total_data$UMT_time>=as.numeric(as.POSIXct("20141224000000", format = "%Y%m%d%H%M%S"))&total_data$UMT_time<as.numeric(as.POSIXct("20141231000000", format = "%Y%m%d%H%M%S")),]
month_data<-total_data[total_data$UMT_time>=as.numeric(as.POSIXct("20141101000000", format = "%Y%m%d%H%M%S"))&total_data$UMT_time<as.numeric(as.POSIXct("20141201000000", format = "%Y%m%d%H%M%S")),]
subarea_week_data<-subarea_data[subarea_data$UMT_time>=as.numeric(as.POSIXct("20141224000000", format = "%Y%m%d%H%M%S"))&subarea_data$UMT_time<as.numeric(as.POSIXct("20141231000000", format = "%Y%m%d%H%M%S")),]
subarea_month_data<-subarea_data[subarea_data$UMT_time>=as.numeric(as.POSIXct("20141101000000", format = "%Y%m%d%H%M%S"))&subarea_data$UMT_time<as.numeric(as.POSIXct("20141201000000", format = "%Y%m%d%H%M%S")),]