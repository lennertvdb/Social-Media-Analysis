library(randomForest)

basetable[1,"dependend"]<-78.05
#split in test and train set
len<-length(basetable[,1])
train_sample<-ceiling(0.6*len)
val_sample<-floor(0.2*len)
test_sample<-len-train_sample-val_sample

train<-basetable[1:train_sample,]
val<-basetable[(train_sample+1):(train_sample+val_sample),]
test<-basetable[(train_sample+val_sample+1):len,]

features<-c("price_stock","volume_stock","topic_1_impact_normalised","topic_2_impact_normalised","sentiment_impact","sentiment_impact_std")

rf<-randomForest(y=train[,"dependend"],x=train[,features],ntree=100)

predictions<-predict(rf,val[,features])

mae<-data.frame(abs(val[3,"dependend"]-predictions[3]))
mae_total<-sum(mae)/length(mae[,1])

mae_dummy<-data.frame(abs(val[,"dependend"]-val[,"price_stock"]))
mae_total_dummy<-sum(mae_dummy)/length(mae_dummy[,1])

mae_dummy<-data.frame(abs(val[,"dependend"]-val[,"price_stock"]))
mae_total_dummy<-sum(mae_dummy)/length(mae_dummy[,1])
