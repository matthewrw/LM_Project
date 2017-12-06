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

#fit initial model
m = lm(ISI ~., data = train)
summary(m)

#look at plots
par(mfrow = c(2,2))
plot(m)

#look at boxcox - 305 is zero - remove it for the moment
m = lm(ISI ~., data = train[-304,])
MASS::boxcox(m)

#try square root
train2 = train[,-1]
train2$sqISI = sqrt(train$ISI)

m = lm(sqISI ~., data = train2)
summary(m)
par(mfrow = c(2,2))
plot(m)

#--------------------------------------------
#
#		Added Variable Plots
#
#--------------------------------------------
library(car)
av.plots(m)




