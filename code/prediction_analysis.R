# Prediction Code on Test Set
library(ggplot2)

#Read in data
#setwd("~/Desktop/github/LM_Project/data")
test = data.frame(read.csv("test.csv"))

#transform ISI 
test$sqISI = sqrt(test$ISI)

#weights for FFMC 
test$tFFMC = ifelse(test$FFMC<80, 0, 1)

#cast as factors 
test$wkd = as.factor(test$wkd)
test$summer = as.factor(test$summer)
test$rainvnorain = as.factor(test$rainvnorain)
test$grid_group = as.factor(test$grid_group)
test$tFFMC = as.factor(test$tFFMC)

#No FFMC model
m1 = lm(sqISI ~ summer + wind + temp + rainvnorain 
        ,data = test)
#,data = test)
par(mfrow = c(2,2));plot(m1)
car::avPlots(m1)
summary(m1)

par(mfrow = c(1,1))

plotdf <- data.frame(cbind(test$sqISI^2,m1$fitted.values^2) )
names(plotdf) <-    c("ActualValues","FittedValues")

ggplot(plotdf, aes(x=FittedValues,y=ActualValues)) + geom_point(col = "steelblue", alpha = .7) + geom_segment(aes(x=0,y=0,xend=15,yend=15),col="black") + theme_bw()

#MSE for fitted predictor
1/length(test$sqISI)*sum((test$sqISI-m1$fitted.values)^2)
# MSE for ISI
1/length(test$sqISI)*sum((test$sqISI^2-m1$fitted.values^2)^2)


#Bootstrap for P values
# Bootstrap Code
#
#

B <- 1000
bootresults <-t( replicate(B, {
  yb <- fitted(m1) + resid(m1)[sample.int(nrow(test), replace = TRUE)]
  boot <- model.matrix(m1)
  coef(lm(yb ~ boot - 1))
}))

#empirical CI 
t(apply(bootresults, 2, quantile, c(.025, .975)))



library(nlme)
library(boot)
# Model to bootstrap on
m <- m1
B <- 100
bootCoefs  <- replicate(B, {
  yb <- unname(fitted(m)) + unname(resid(m))[sample(length(unname(m$fitted.values)), replace = TRUE)]
  boot <- model.matrix(m)
  coef(lm(yb ~ boot - 1))
})
bootCoefs <- t(bootCoefs)
t(apply(bootCoefs, 2, quantile, c(.025,.5, .975)))

#Get P values from bootstrap
apply(bootCoefs < 0 ,2,sum)/B


