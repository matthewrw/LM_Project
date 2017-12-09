#--------------------------------------------
#
#     Models we're considering 
#
#--------------------------------------------


#Read in data
setwd("~/Desktop/github/LM_Project/data")
train = data.frame(read.csv("train.csv"))

#transform ISI 
train$sqISI = sqrt(train$ISI)

#weights for FFMC 
train$tFFMC = ifelse(train$FFMC<80, 0, 1)

#cast as factors 
train$wkd = as.factor(train$wkd)
train$summer = as.factor(train$summer)
train$rainvnorain = as.factor(train$rainvnorain)
train$grid_group = as.factor(train$grid_group)
train$tFFMC = as.factor(train$tFFMC)

#No FFMC model
m1 = lm(sqISI ~ summer + wind + temp + rainvnorain 
        #,data = train[-89,])
       ,data = train)
par(mfrow = c(2,2));plot(m1)
car::avPlots(m1)
summary(m1)

#Weighted FFMC model
library(dplyr)
train = data.frame(train %>% group_by(tFFMC) %>% mutate(weight = n()))
m2 = lm(sqISI ~ summer + wind + temp + rainvnorain
        ,weights = weight
        ,data = train[-89,])
        #,data = train)
par(mfrow = c(2,2));plot(m2)
car::avPlots(m2)
summary(m2)

#tFFMC as covariate model
m3 = lm(sqISI ~ summer + wind + temp + rainvnorain + tFFMC
        ,data = train[-89,])
        #,data = train)
par(mfrow = c(2,2));plot(m3)
car::avPlots(m3)
summary(m3)






