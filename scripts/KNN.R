################################
#KNN for air quality prediction
#KNN with Longitude, Latitude and UMT_time
#author: yingzhi
################################

#config
title<-'KNN: one-week data'   #this is for the output
data<-week_data
distance<-2
#Longitude/Latitude to UMT_time weight ratio
L2T_weight<-500000000    
#used for train.kknn
kernel_candidate<-c("rectangular", "triangular", "epanechnikov", "cos", "inv", "gaussian", "optimal")
kmax<-7

#remove NA values
data<-data[!is.na(data$UMT_time)&!is.na(data$Longitude)&!is.na(data$Latitude)&!is.na(data$PM2.5),]


#add weight to Longitude, Latitude and UMT_time
data$Latitude<-L2T_weight*data$Latitude
data$Longitude<-L2T_weight*data$Longitude

#prepare train, test(choose the latest day layer) and build(24h before test data's time)
size<-dim(data)[1]
# test_index<-sample(1:size, size = min(round(size/5),2000), replace = FALSE, prob = rep(1/size, size))
# train<-data[-test_index,]
# test<-data[test_index,]
max_timestamp = max(data$UMT_time)
train<-data[data$UMT_time != max_timestamp,]
test<-data[data$UMT_time == max_timestamp,]
build<-data[data$UMT_time<max_timestamp-60*60*24,]

#train knn using leave-one-out cross validation, to get the best k and kernel
library(kknn)
fit<-train.kknn(PM2.5~Latitude+Longitude+UMT_time, train, kmax = kmax, kernel = kernel_candidate, distance = distance, scale = FALSE)
best_k<-fit$best.parameters$k
best_kernel<-fit$best.parameters$kernel

#build and test the best model with data 24h before the test data's time
best_fit<-kknn(PM2.5~Latitude+Longitude+UMT_time, build, test, k = best_k, kernel = best_kernel, distance = distance, scale = FALSE)
residuals<-test$PM2.5-best_fit$fitted.values
MAE<-mean(abs(residuals[!is.na(residuals)]))
MSE<-mean(residuals^2)

#output the results
output<-function(){
  print('-------Data-------')
  print(paste(title, 'with size',size))
  print('-------Parameters-------')
  print(paste('L2T_weight =', L2T_weight))
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
data<-data[!is.na(data$UMT_time)&!is.na(data$Longitude)&!is.na(data$Latitude)&!is.na(data$PM2.5),]
data$Latitude<-L2T_weight*data$Latitude
data$Longitude<-L2T_weight*data$Longitude
size<-dim(data)[1]
max_timestamp = max(data$UMT_time)
test<-data[data$UMT_time == max_timestamp,]
build<-data[data$UMT_time<max_timestamp-60*60*24,]
#build KNN and test
best_fit<-kknn(PM2.5~Latitude+Longitude+UMT_time, build, test, k = best_k, kernel = best_kernel, distance = distance, scale = FALSE)
residuals<-test$PM2.5-best_fit$fitted.values
MAE<-mean(abs(residuals[!is.na(residuals)]))
MSE<-mean(residuals^2)
output()