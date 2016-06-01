################################
#KNN for air quality prediction
##KNN with Longitude, Latitude, UMT_time, Temperature, Pressure, Humidity
#author: yingzhi
################################

#correlation
test_residuals<-cbind(test[12:16],residuals)
cor(test_residuals, use = 'pairwise.complete.obs', method = 'pearson')
library(corrgram)
corrgram(test_residuals)

#config
title<-'KNN with weather factors: one-week data'   #this is for the output
data<-week_data
distance<-2
#Factors to UMT_time weight ratio
L2T_weight<-500000000    #Longitude/Latitude to UMT_time weight ratio
Tem2T_weight<-1000000
P2T_weight<-1000000
H2T_weight<-1000000
#used for train.kknn
kernel_candidate<-c("rectangular", "triangular", "epanechnikov", "cos", "inv", "gaussian", "optimal")
kmax<-7

#remove NA values
data<-data[!is.na(data$UMT_time)&!is.na(data$Longitude)&!is.na(data$Latitude)&!is.na(data$PM2.5)&!is.na(data$Temperature)&!is.na(data$Pressure)&!is.na(data$Humidity),]


#add weight to Factors and UMT_time
data$Latitude<-L2T_weight*data$Latitude
data$Longitude<-L2T_weight*data$Longitude
data$Temperature<-Tem2T_weight*data$Temperature
data$Pressure<-P2T_weight*data$Pressure
data$Humidity<-H2T_weight*data$Humidity

#prepare train, test(choose the latest day layer) and build(24h before test data's time)
size<-dim(data)[1]
max_timestamp = max(data$UMT_time)
train<-data[data$UMT_time != max_timestamp,]
test<-data[data$UMT_time == max_timestamp,]
build<-data[data$UMT_time<max_timestamp-60*60*24,]

#train knn using leave-one-out cross validation, to get the best k and kernel
library(kknn)
fit<-train.kknn(PM2.5~Latitude+Longitude+UMT_time+Temperature+Pressure+Humidity, train, kmax = kmax, kernel = kernel_candidate, distance = distance, scale = FALSE)
best_k<-fit$best.parameters$k
best_kernel<-fit$best.parameters$kernel

#build and test the best model with data 24h before the test data's time
best_fit<-kknn(PM2.5~Latitude+Longitude+UMT_time+Temperature+Pressure+Humidity, build, test, k = best_k, kernel = best_kernel, distance = distance, scale = FALSE)
residuals<-test$PM2.5-best_fit$fitted.values
MAE<-mean(abs(residuals[!is.na(residuals)]))
MSE<-mean(residuals^2)

#update the output of the results
output<-function(){
  print('-------Data-------')
  print(paste(datasetname, 'with size',size))
  print('-------Parameters-------')
  print(paste('L2T_weight =', L2T_weight))
  print(paste('Tem2T_weight =', Tem2T_weight))
  print(paste('P2T_weight =', P2T_weight))
  print(paste('H2T_weight =', H2T_weight))
  print(paste('distance parameter=', distance))
  print(paste('k =', best_k))
  print(paste('kernel =', best_kernel))
  print('-------In-sample errors-------')
  print(paste('In-sample MAE =', min(fit$MEAN.ABS)))
  print(paste('In-sample MSE =', min(fit$MEAN.SQU)))
  print('-------Test dataset errors-------')
  print(paste('Test MAE =', MAE))
  print(paste('Test MSE =', MSE))
  #plot first 200 test dataset
  x<-1:min(100, length(test[,1]))
  plot(x, best_fit$fitted.values[1:min(100, length(test[,1]))], ylab = 'PM2.5', xlab = NA, xlim = c(1,120), type = 'l')
  lines(x, test$PM2.5[1:min(100, length(test[,1]))], col = 4, lty = 2)
  title('Test dataset fitted vs true')
  legend('topright', c('True value','Fitted value'), col = c(1,4), lty = c(1,2), merge = TRUE, cex = 0.8, xpd = TRUE)
}
output()

#Use another dataset to test
#preprocess
week_data2<-total_data[total_data$UMT_time>=as.numeric(as.POSIXct("20141201000000", format = "%Y%m%d%H%M%S"))&total_data$UMT_time<as.numeric(as.POSIXct("20141208000000", format = "%Y%m%d%H%M%S")),]
datasetname<-'week_data2'
data<-week_data2
data<-data[!is.na(data$UMT_time)&!is.na(data$Longitude)&!is.na(data$Latitude)&!is.na(data$PM2.5)&!is.na(data$Temperature)&!is.na(data$Pressure)&!is.na(data$Humidity),]
data$Latitude<-L2T_weight*data$Latitude
data$Longitude<-L2T_weight*data$Longitude
data$Temperature<-Tem2T_weight*data$Temperature
data$Pressure<-P2T_weight*data$Pressure
data$Humidity<-H2T_weight*data$Humidity
size<-dim(data)[1]
max_timestamp = max(data$UMT_time)
test<-data[data$UMT_time == max_timestamp,]
build<-data[data$UMT_time<max_timestamp-60*60*24,]
#build KNN and test
best_fit<-kknn(PM2.5~Latitude+Longitude+UMT_time+Temperature+Pressure+Humidity, build, test, k = best_k, kernel = best_kernel, distance = distance, scale = FALSE)
residuals<-test$PM2.5-best_fit$fitted.values
MAE<-mean(abs(residuals[!is.na(residuals)]))
MSE<-mean(residuals^2)
output()