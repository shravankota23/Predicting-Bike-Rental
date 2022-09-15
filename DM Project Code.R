dataset = read.csv(file.choose())
data = dataset[,-c(1,2,14,15)]
data
dim(data)
summary(data)
cor(data)
pairs(data[,c(8,9,10,11,12)])
boxplot(cnt~season,data = data)
boxplot(cnt~yr,data = data)
boxplot(cnt~holiday,data = data)
boxplot(cnt~mnth,data = data)
boxplot(cnt~weekday,data = data)
boxplot(cnt~workingday,data = data)
boxplot(cnt~weathersit,data = data)
split=sample.split(data,0.8)
split
train = subset(data,split==TRUE)
test = subset(data,split==FALSE)
model = lm(cnt~.,data)
model
summary(model)
vif(model)
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


