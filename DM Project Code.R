install.packages('stats')
install.packages('catools')
install.packages('caret')
install.packages('car')
library(stats)
library(catools)
library(caret)
library(car)
# libraries used 
dataset = read.csv(file.choose()) #READING THE CSV FILE
data = dataset[,-c(1,2,14,15)] # REMOVED UNWANTED VARIALES LIKE INSTANT,DATETIME,CASUAL AND REGISTER
data
dim(data) # DIMENTION OF THE DATA
summary(data) # SUMMARY OF THE DATA
cor(data) # CORRELATION MATRIX OF THE DATA
pairs(data[,c(8,9,10,11,12)]) # DRAWING PAIR PLOTS OF THE TEMP,ATEMP,HUM,WIND SPEED AND CNT
# CODE FOR BOX PLOT
boxplot(cnt~season,data = data) 
boxplot(cnt~yr,data = data)
boxplot(cnt~holiday,data = data)
boxplot(cnt~mnth,data = data)
boxplot(cnt~weekday,data = data)
boxplot(cnt~workingday,data = data)
boxplot(cnt~weathersit,data = data)
# SPLITING THE DATA SET INTO TRAINING AND TESTING 
split=sample.split(data,0.8)
split
# TRAINING SET OF 80%
train = subset(data,split==TRUE)
# TESTING SET OF 20%
test = subset(data,split==FALSE)
# FITTING MULTIPLE LINEAR REGRESSION MODEL BY USING THE lm() function
model = lm(cnt~.,data)
model
summary(model)
# VIF of the model
vif(model)
# REMOVED ATEMP AND FITTED ANOTHER MODEL 
model1 = lm(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed,data)
summary(model1)
vif(model1)
plot(model1)
# Accuracy of train data
predi=predict(model1,train[,-12])
cor(train$cnt,predi)^2
# Accuracy of test data
pred=predict(model1,test)
pred
cor(test$cnt,pred)^2


