install.packages("stats")
install.packages("car")
install.packages("caTools")
install.packages("lmtest")
install.packages("stats")
install.packages("dplyr")
library(stats)
library(car)
library(caTools)
library(lmtest)
library(dplyr)
dataset = read.csv(file.choose())
data = dataset[,-c(1,2,14,15)]
data
#dimention of data
dim(data)
#summary of data
summary(data)
#correlation matrix
cor(data)
# by correlation matrix we can say that the holidat,weekday,workingday has least relationship with cnt(dependent variable)
#scatter plot matrix
pairs(data[,c(8,9,10,11,12)])
# we can see that there is a positive and negitive correlation with the cnt
# one thing to notice is that temp and atemp has highest correlation this might cause multicollinearity problem
#box plots for identifying the outliers
boxplot(cnt~season,data = data)
#one outlier
boxplot(cnt~yr,data = data)
# there is one outliers
boxplot(cnt~holiday,data = data)
#no outliers
boxplot(cnt~mnth,data = data)
# there is no outliers but we can see that the there is high bike rentals in the month of the 6 to 9
boxplot(cnt~weekday,data = data)
#there is no outliers but we can see that each day there are equal no of the bike rentals in the week days
boxplot(cnt~workingday,data = data)
#there are no outliers the count of bike rentals on working day as well as non working days is equal
boxplot(cnt~weathersit,data = data)
# in the 1st wheathersit the count of bike rental is high
split=sample.split(data,0.8)
split
train = subset(data,split==TRUE)
test = subset(data,split==FALSE)
model = lm(cnt~.,train)
model
summary(model)
# this gives us the summary of the model
vif(model)
# this will check the multicollinearity
# here we can see that the atemp has high vif value so we need to remove that variable
train
model1 = lm(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed,train)
summary(model1)
vif(model1) 
# in this model there is no multicollinearity prest
par(mfrow = c(2, 2))
plot(model1)
#Residuals  s Fitted
# no hetreosckedasticity present
# linearity assumption is satisfied
#Normal Q-Q
# the data is approximately normal
# Sclate Location and Residual vs leverage
# by running the model we get the influence points then we remove those points from our data set 
train=train %>%  filter(!row_number() %in% c(204,555,595,668,692))
train=train %>%  filter(!row_number() %in% c(499,153,413,517,139))
train=train %>%  filter(!row_number() %in% c(405,513,327,535,247))
train=train %>%  filter(!row_number() %in% c(531,530,241,329,369,156))
train=train %>%  filter(!row_number() %in% c(526,529,268))

model1 = lm(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed,train)
summary(model1)
par(mfrow = c(2, 2))
plot(model1)
#now the model has good estimators
# Accuracy of train data
predi=predict(model1,train[,-12])
cor(train$cnt,predi)^2
# Accuracy of test data
pred=predict(model1,test)
cor(test$cnt,pred)^2
# since training accuracy is 84%
# and testing is 79%
# so our model is good it doesn't over fit as well as it doesn't under fit


