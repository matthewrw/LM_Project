#--------------------------------------------
#
#		Read in Data 
#
#--------------------------------------------

setwd("~/Desktop/github/LM_Project/data")
train = read.csv("train.csv")

#--------------------------------------------
#
#		Start fitting naive models
#		No interactions
#
#--------------------------------------------

#Remove point 89 - bad bad outlier 
train = train[-89, ]


m = lm(ISI ~., data = train[-89,])
summary(m)

par(mfrow = c(2,2))
plot(m)

train2 = train[,-1]
train2$lISI = log(train$ISI + 1)

m = lm(lISI ~., data = train2)
summary(m)
par(mfrow = c(2,2))
plot(m)

m = lm(ISI ~., data = train[-305,])
boxcox(m)

train3 = train[,-1]
train3$sqISI = (train$ISI)^(1/2)

m = lm(sqISI ~., data = train3[-89,])
summary(m)
par(mfrow = c(2,2))
plot(m)



