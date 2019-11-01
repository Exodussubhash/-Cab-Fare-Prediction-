rm(list=ls())
setwd('D:/edwisor/Project/train_cab')
getwd()
CabCompany = read.csv('train_cab.csv')
str(CabCompany)
summary(CabCompany)
sum(is.na(CabCompany))
#Converting factor to numeric
CabCompany$fare_amount = as.integer(as.character(CabCompany$fare_amount))
library(DMwR)
CabCompany = knnImputation(CabCompany,k=1)
#Checking how data is related to each other
library(corrgram)
corrgram(CabCompany)
#*****Data Preprocessing**************
#**********Calculation for distance*********#
#a = sin?(????/2) + cos ??1 ??? cos ??2 ??? sin?(????/2)
#?? is latitude, ?? is longitude, 

distance = cbind(CabCompany['pickup_longitude'],CabCompany['pickup_latitude'],CabCompany['dropoff_longitude'],
                 CabCompany['dropoff_latitude'])
library(NISTunits)

distance$pickup_longitude=NISTdegTOradian(distance$pickup_longitude)
distance$pickup_latitude=NISTdegTOradian(distance$pickup_latitude)
distance$dropoff_longitude=NISTdegTOradian(distance$dropoff_longitude)
distance$dropoff_latitude=NISTdegTOradian(distance$dropoff_latitude)

distance$longitudediff = (distance$dropoff_longitude - distance$pickup_longitude)
distance$latitudediff = (distance$dropoff_latitude-distance$pickup_latitude)

a=(sin(distance$latitudediff/2)*sin(distance$latitudediff/2))+
  cos(distance$pickup_latitude)*cos(distance$dropoff_latitude)*
     sin(distance$longitudediff/2)*sin(distance$longitudediff/2)
c = 2*atan2(sqrt(a),sqrt(1-a))
distance$distance_covered = 6371*c
CabCompany$Distance_in_km = distance$distance_covered
###Removing multicollinearity and unwanted variables########
CabCompany$pickup_longitude = NULL
CabCompany$pickup_latitude = NULL
CabCompany$dropoff_longitude = NULL
CabCompany$dropoff_latitude = NULL
#****Extracting year from the data***********
Dateconvert = as.Date(strptime(CabCompany$pickup_datetime, "%Y-%m-%d %H:%M:%S",tz='UTC'))
CabCompany$Date = Dateconvert
year = as.numeric(format(CabCompany$Date,'%Y'))
CabCompany$year = year

CabCompany$Date = NULL
CabCompany$pickup_datetime = NULL

numeric_index = sapply(CabCompany,is.numeric) #selecting only numeric

numeric_data = CabCompany[,numeric_index]

cnames = colnames(numeric_data)
 
for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "fare_amount"), data = subset(CabCompany))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="fare_amount")+
            ggtitle(paste("Box plot of faremount for",cnames[i])))
 }
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
##Removing outliers###########
for(i in colnames(CabCompany)){
     print(i)
     val = CabCompany[,i][CabCompany[,i] %in% boxplot.stats(CabCompany[,i])$out]
     print(length(val))
     CabCompany = CabCompany[which(!CabCompany[,i] %in% val),]
}

boxplot(CabCompany$fare_amount) 
###Checking data after outlier removal##
hist(CabCompany$year , CabCompany$fare_amount, breaks = 20)
plot(CabCompany$fare_amount , CabCompany$passenger_count )
cor(CabCompany$Distance_in_km,CabCompany$passenger_count)
sum(is.na(CabCompany$year))
CabCompany = knnImputation(CabCompany,k=1)
#Checking VIF factor before applying linear regression
library(MASS)
library(usdm)
vifcor(CabCompany[,-1], th = 0.9)
##Multiple linear regression###
Linear_regression = lm(fare_amount ~. ,data = CabCompany)
summary(Linear_regression)
plot(Linear_regression)
prediction = predict(Linear_regression,CabCompany[,-1])
regr.eval(CabCompany$fare_amount,prediction, stats = c('mae','rmse','mape','mse'))
####Logistic regression######
Logistic_regression = glm(fare_amount~ . ,data = CabCompany)
summary(Logistic_regression)
plot(Logistic_regression)
prediction = predict(Logistic_regression,CabCompany[,-1])
regr.eval(CabCompany$fare_amount,prediction, stats = c('mae','rmse','mape','mse'))
####Applying decision tree on model########
library(rpart)
fit = rpart(fare_amount ~ ., data = CabCompany, method = "anova")
prediction = predict(fit,CabCompany[,-1])
regr.eval(CabCompany$fare_amount,prediction, stats = c('mae','rmse','mape','mse'))
####Best fit of the model Random Forest######
library(randomForest)
RF_model = randomForest(fare_amount ~ ., CabCompany, importance = TRUE, ntree = 52 ) #52
prediction = predict(RF_model,CabCompany[,-1])
regr.eval(CabCompany$fare_amount,prediction, stats = c('mae','rmse','mape','mse'))
####Loading test data###
CabCompany_test = read.csv('test.csv')
distance_test = cbind(CabCompany_test['pickup_longitude'],CabCompany_test['pickup_latitude'],CabCompany_test['dropoff_longitude'],
                 CabCompany_test['dropoff_latitude'])

library(NISTunits)

distance_test$pickup_longitude=NISTdegTOradian(distance_test$pickup_longitude)
distance_test$pickup_latitude=NISTdegTOradian(distance_test$pickup_latitude)
distance_test$dropoff_longitude=NISTdegTOradian(distance_test$dropoff_longitude)
distance_test$dropoff_latitude=NISTdegTOradian(distance_test$dropoff_latitude)

distance_test$longitudediff = (distance_test$dropoff_longitude - distance_test$pickup_longitude)
distance_test$latitudediff = (distance_test$dropoff_latitude-distance_test$pickup_latitude)

a=(sin(distance_test$latitudediff/2)*sin(distance_test$latitudediff/2))+
  cos(distance_test$pickup_latitude)*cos(distance_test$dropoff_latitude)*
  sin(distance_test$longitudediff/2)*sin(distance_test$longitudediff/2)
c = 2*atan2(sqrt(a),sqrt(1-a))
distance_test$distance_covered = 6371*c
CabCompany_test$Distance_in_km = distance_test$distance_covered
CabCompany_test$pickup_longitude = NULL
CabCompany_test$pickup_latitude = NULL
CabCompany_test$dropoff_longitude = NULL
CabCompany_test$dropoff_latitude = NULL

Dateconvert = as.Date(strptime(CabCompany_test$pickup_datetime, "%Y-%m-%d %H:%M:%S",tz='UTC'))
CabCompany_test$Date = Dateconvert
year = as.numeric(format(CabCompany_test$Date,'%Y'))
CabCompany_test$year = year

CabCompany_test$Date = NULL
CabCompany_test$pickup_datetime = NULL
######Predicitng RF model on test data############
prediction = predict(RF_model,CabCompany_test)
CabCompany_test$predicted_values = prediction

#*******Thankyou********************#
