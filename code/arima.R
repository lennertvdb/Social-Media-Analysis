
load("./../data/basetable_final.RData")
library(forecast)
library(tseries)

basetable[1,"dependend"]<-78.05
#split in test and train set
len<-length(basetable[,1])
train_sample<-ceiling(0.6*len)
val_sample<-floor(0.2*len)
test_sample<-len-train_sample-val_sample

train<-basetable[1:train_sample,]
val<-basetable[(train_sample+1):(train_sample+val_sample),]
test<-basetable[(train_sample+val_sample+1):len,]

#test for stationary
#___________________
lag.length = 25
Box.test(basetable[,"dependend"], lag=lag.length, type="Ljung-Box")

options(warn=-1)
adf.test(basetable[,"dependend"])

kpss.test(basetable[,"dependend"], null="Trend")

plot.new()
frame()
par(mfcol=c(2,2))
# the stationary signal and ACF
plot(t,basetable[,"dependend"],
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Stationary signal")
acf(basetable[,"dependend"],lag.max = length(basetable[,"dependend"]),
    xlab = "lag #", ylab = 'ACF',main=' ')

#potentially differencing might help
differencing<- diff(basetable[,"dependend"], differences=1)

lag.length = 25
Box.test(differencing, lag=lag.length, type="Ljung-Box")

options(warn=-1)
adf.test(differencing)

kpss.test(differencing, null="Trend")

plot.new()
frame()
par(mfcol=c(2,2))
# the stationary signal and ACF
plot(t,differencing,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Stationary signal")
acf(differencing,lag.max = length(differencing),
    xlab = "lag #", ylab = 'ACF',main=' ')

##only trend is still clearly in the data (detrend)

#let arime determine the optimal model
features<-c("price_stock","volume_stock","topic_1_impact_normalised","topic_2_impact_normalised","sentiment_impact","sentiment_impact_std")
x_matrix<-as.matrix(train[,features])
model<-auto.arima(train["dependend"],xreg=x_matrix)

forecast<-forecast(model,h=1,xreg =as.matrix(val[,features]))

plot(forecast)
plot(forecast$residuals)
accuracy(model)
forecasted_train<-forecast$mean
forecasted_train<-data.frame(forecasted_train)

mae<-data.frame(abs(val[,"dependend"]-forecasted_train[,1]))
mae_total<-sum(mae)/length(mae[,1])


#without volume
features<-c("price_stock")
x_matrix<-as.matrix(train[,features])
model<-auto.arima(train["dependend"],xreg=x_matrix)

forecast<-forecast(model,h=1,xreg =as.matrix(val[,features]))

plot(forecast)
plot(forecast$residuals)
accuracy(model)
forecasted_train<-forecast$mean
forecasted_train<-data.frame(forecasted_train)

mae<-data.frame(abs(val[,"dependend"]-forecasted_train[,1]))
mae_total<-sum(mae)/length(mae[,1])
