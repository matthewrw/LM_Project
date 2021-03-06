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
# Citation Solution to Homework 7
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



B <- 1000
ResidualBootstrapM1 <-t( replicate(B, {
  #yb <- fitted(m1) + resid(m1)[sample.int(nrow(train[-89,]), replace = TRUE)]
  yb <- fitted(m1) + resid(m1)[sample.int(nrow(train), replace = TRUE)]
  boot <- model.matrix(m1)
  coef(lm(yb ~ boot - 1))
}))
#qqnorm(ResidualBootstrapM1[,1, drop=FALSE]) # intercept
qqnorm(ResidualBootstrapM1[,1, drop=FALSE]) # summer
qqnorm(ResidualBootstrapM1[,2, drop=FALSE]) # wind 
qqnorm(ResidualBootstrapM1[,3, drop=FALSE]) # temp
qqnorm(ResidualBootstrapM1[,4, drop=FALSE]) # rainvnorain

#look at beta distributions
par(mfrow=c(2,2))
hist(ResidualBootstrapM1[,1], xlab = "summer")
hist(ResidualBootstrapM1[,2], xlab ="wind")
hist(ResidualBootstrapM1[,3], xlab ="temp")
hist(ResidualBootstrapM1[,4], xlab ="rainvnorain")

#assuming normality of errors
confint(m1)
#empirical CI 
t(apply(ResidualBootstrapM1, 2, quantile, c(.025, .975)))


ResidualBootstrapM2 <-t( replicate(B, {
  yb <- fitted(m2) + resid(m2)[sample.int(nrow(train2[-89,]), replace = TRUE)]
  boot <- model.matrix(m2)
  coef(lm(yb ~ boot-1))
}))
#qqnorm(ResidualBootstrapM2[,1, drop=FALSE]) # intercept
qqnorm(ResidualBootstrapM2[,1, drop=FALSE]) # summer
qqnorm(ResidualBootstrapM2[,2, drop=FALSE]) # wind 
qqnorm(ResidualBootstrapM2[,3, drop=FALSE]) # temp
qqnorm(ResidualBootstrapM2[,4, drop=FALSE]) # rainvnorain

#look at beta distributions
par(mfrow=c(2,2))
hist(ResidualBootstrapM2[,1], xlab = "summer")
hist(ResidualBootstrapM2[,2], xlab ="wind")
hist(ResidualBootstrapM2[,3], xlab ="temp")
hist(ResidualBootstrapM2[,4], xlab ="rainvnorain")

#assuming normality of errors
confint(m2)
#empirical CI 
t(apply(ResidualBootstrapM2, 2, quantile, c(.025, .975)))

ResidualBootstrapM3 <-t( replicate(B, {
  yb <- fitted(m3) + resid(m3)[sample.int(nrow(train[-89,]), replace = TRUE)]
  boot <- model.matrix(m3)
  coef(lm(yb ~ boot - 1))
}))
#qqnorm(ResidualBootstrapM3[,1, drop=FALSE]) # intercept
qqnorm(ResidualBootstrapM3[,1, drop=FALSE]) # summer
qqnorm(ResidualBootstrapM3[,2, drop=FALSE]) # wind
qqnorm(ResidualBootstrapM3[,3, drop=FALSE]) # temp
qqnorm(ResidualBootstrapM3[,4, drop=FALSE]) # rainvnorrain
qqnorm(ResidualBootstrapM3[,5, drop=FALSE]) # tFFMC

#look at beta distributions
par(mfrow=c(3,2))
hist(ResidualBootstrapM2[,1], xlab = "summer")
hist(ResidualBootstrapM2[,2], xlab ="wind")
hist(ResidualBootstrapM2[,3], xlab ="temp")
hist(ResidualBootstrapM2[,4], xlab ="rainvnorain")
hist(ResidualBootstrapM2[,5], xlab ="tFFMC")

#assuming normality of errors
confint(m3)
#empirical CI 
t(apply(ResidualBootstrapM3, 2, quantile, c(.025, .975)))



