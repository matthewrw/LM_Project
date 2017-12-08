#--------------------------------------------
#
#		Read in Data 
#
#--------------------------------------------

setwd("~/Desktop/github/LM_Project/data")
train = data.frame(read.csv("train.csv"))

#--------------------------------------------
#
#		Cast as factors		 
#
#--------------------------------------------

train$wkd = as.factor(train$wkd)
train$wkdM = as.factor(train$wkdM)
train$summer = as.factor(train$summer)
train$FFMCQuantile = as.factor(train$FFMCQuantile)
train$rainvnorain = as.factor(train$rainvnorain)
train$grid_group = as.factor(train$grid_group)

#--------------------------------------------
#
#		Start fitting naive models
#		No interactions
#
#--------------------------------------------

#Remove point 89 - bad bad outlier 
train = train[-89, ]

#fit initial model
m = lm(ISI ~., data = train[,-c(9,12)])
summary(m)

#look at plots
par(mfrow = c(2,2))
plot(m)

#look at boxcox - 304 is zero - remove it for the moment
m = lm(ISI ~., data = t[-304,])
MASS::boxcox(m)

#try square root
train2 = train[,-1]
train2$sqISI = sqrt(train$ISI)

m = lm(sqISI ~., data = train2)
summary(m)
par(mfrow = c(2,2))
plot(m)

#really doesn't help all that much 
#Wind and FFMC quanitule look really good every thing esle not so much
#Gonna try and fit just a simple bivariate model

m = lm(ISI~FFMCQuantile + wind, data = train)

#--------------------------------------------
#
#		Added Variable Plots
#
#--------------------------------------------
m = lm(sqISI~., data = train2)
car::avPlots(m)

#--------------------------------------------
#
#		Added Variable Plots
#
#--------------------------------------------






