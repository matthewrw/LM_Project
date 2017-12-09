#--------------------------------------------
#
#     Models we're considering 
#
#--------------------------------------------


#Read in data
setwd("~/Desktop/github/LM_Project/data")
train = data.frame(read.csv("data/train.csv"))

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
train2 = data.frame(train %>% group_by(tFFMC) %>% mutate(weight = n()))
m2 = lm(sqISI ~ summer + wind + temp + rainvnorain
        ,weights = weight
        ,data = train2[-89,])
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

#########
# Dear team,
# note error happens in lm whenever one of the factors is only 1 level
# i.e. all of the sampled values are in the summer, or all no_rain.
# As such, whenever this happens, we have to resample the bootstrap
# leading to a small bias.
# Note we have to reroll when all the sampled records have a factor with only 1 level
# -Aaron
#########

#--------------------------------------------
# Bootstrapping
# Citation https://www.statmethods.net/advstats/bootstrapping.html
#--------------------------------------------
library(boot)



residualStat <- function(formula, data, indices) { # private function for boot to get the residuals
  dfBoot <- data[indices,] 
  m <- lm(formula, data=dfBoot)
  return(summary(m)$residuals)
}
betaStat <- function(formula, data, indices) { # private function for boot to get the coefficients
  dfBoot <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=dfBoot) # fit the model based on bootstrapped records
  return(coef(fit)) # get coeficients
} 

# residual bootstraps
resultsM1 <- boot(data=train[-89,], statistic=residualStat, R=200, formula= sqISI ~ summer + wind + temp + rainvnorain)
plot(resultsM1)
resultsM2 <- boot(data=train2[-89,], statistic=residualStat, R=200, formula=sqISI ~ summer + wind + temp + rainvnorain)
plot(resultsM2)
resultsM3 <- boot(data=train[-89,], statistic=residualStat, R=200, formula=sqISI ~ summer + wind + temp + rainvnorain + tFFMC)
plot(resultsM3)

# Beta bootstraps
resultsM1B <- boot(data=train[-89,], statistic=betaStat, R=400, formula= sqISI ~ summer + wind + temp + rainvnorain)
plot(resultsM1B, index=1) # intercept 
plot(resultsM1B, index=2) # summer 
plot(resultsM1B, index=3) # wind 
plot(resultsM1B, index=4) # temp 
plot(resultsM1B, index=5) # rainvnorain 

resultsM2B <- boot(data=train2[-89,], statistic=betaStat, R=400, formula=sqISI ~ summer + wind + temp + rainvnorain)
plot(resultsM2B, index=1) # intercept 
plot(resultsM2B, index=2) # summer 
plot(resultsM2B, index=3) # wind 
plot(resultsM2B, index=4) # temp 
plot(resultsM2B, index=5) # rainvnorain


resultsM3B <- boot(data=train[-89,], statistic=betaStat, R=400, formula=sqISI ~ summer + wind + temp + rainvnorain + tFFMC)
plot(resultsM3B, index=1) # intercept 
plot(resultsM3B, index=2) # summer 
plot(resultsM3B, index=3) # wind 
plot(resultsM3B, index=4) # temp 
plot(resultsM3B, index=5) # rainvnorain
plot(resultsM3B, index=6) # tFFMC
