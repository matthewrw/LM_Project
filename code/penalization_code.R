#---------------------------------------
#
#       LASSO Fits 
#
#---------------------------------------

#Read in data
setwd("~/Desktop/github/LM_Project/data")
train = data.frame(read.csv("train.csv"))

#transform ISI 
train$sqISI = sqrt(train$ISI)
train$lgISI = log(train$ISI + 1)

#transform FFMC 
train$tFFMC = log(max(train$FFMC) - train$FFMC + 1)
train$tFFMC = ifelse(train$FFMC<mean(train$FFMC) - 2*sd(train$FFMC), 0, 1)
train$tFFMC = ifelse(train$FFMC<80, 0, 1)
#train$tFFMC = as.factor(train$tFFMC)

#cast as factors 
train$wkd = as.factor(train$wkd)
train$wkdM = as.factor(train$wkdM)
train$summer = as.factor(train$summer)
train$FFMCQuantile = as.factor(train$FFMCQuantile)
train$rainvnorain = as.factor(train$rainvnorain)
train$grid_group = as.factor(train$grid_group)
train$month = as.factor(train$month)
train$day = as.factor(train$day)
train$X = as.factor(train$X)
train$Y = as.factor(train$Y) 
train$tFFMC = as.factor(train$tFFMC)

#---------------------------------------
#
#       GLMNET Penalization Fits
#
#---------------------------------------

#construct regression equation
f = formula(sqISI ~ 
            #tFFMC 
            # + day #3 iteration 
            #+ month # 1iteration 
            #+ X #2 iteration 
            #+ Y #2 iteration 
            #+ DMC #6 iteration 
            #+ DC #6iteration
            #+ FFMC
            + temp 
            #+ RH 
            + wind 
            + wkd 
            #+ wkdM #4 iteration  
            + summer
            #+ areaTrans 
            #+ wetness 
            + rainvnorain 
            #+ forest_ind 
            #+ grid_group 
            #+ tFFMC:rainvnorain 
            #+ FFMC:DMC #Iteration 5 
            #+ FFMC:DC #Iteration 5 
            #+ DC:DMC #Iteration 5 
            #+ summer:rainvnorain 
            #+ temp:RH 
            #+ wetness:RH 
            #+ tFFMC:wind 
            #+ forest_ind:grid_group
            )


#build model matrix
X = model.matrix(f,train)
Y = as.matrix(train$sqISI)
a = 1

cv = glmnet::cv.glmnet(X, Y, alpha = a)
plot(cv)
lambda_opt = cv$lambda.min

lasso = glmnet::glmnet(X, Y, alpha = a, lambda = lambda_opt)
tmp = sort(abs(coef(lasso)[,1]), decreasing = TRUE)
varImp = data.frame(VarNames = names(tmp), Beta = round(as.vector(tmp),3))
varImp

train$tISI =(train$ISI)^(1/3)
train[train$rainvnorain == 0, "rISI"] = train[train$rainvnorain == 0, "rISI"] - mean(train[train$rainvnorain == 0, "ISI"])
train[train$rainvnorain == 1, "rISI"] = train[train$rainvnorain == 1, "rISI"] - mean(train[train$rainvnorain == 1, "ISI"])

m = lm(sqISI ~
         tFFMC
        wkd
       + summer
       + wind 
       + temp
       + rainvnorain 
       #+ tFFMC:rainvnorain
       + summer:rainvnorain
       + grid_group 
       + forest_ind
       , data = train)
par(mfrow =c(2,2));plot(m)
car::avPlots(m)

#-------------------------------------------------------------------
#
#         Backwards/Forward Selection
#
#-------------------------------------------------------------------

#we should be careful here, AIC and BIC need residuals 
#of the model to be normal and these still have fat tails

n = nrow(train)
step(lm(sqISI ~  summer+ wind + temp + rainvnorain + RH 
        + DMC+ grid_group+ forest_ind, data = train)
     , direction = "backward", k = log(n))
#backwards BIC suggests sqISI ~ summer + wind + temp
step(lm(sqISI ~  summer+ wind + temp + rainvnorain + RH 
              + DMC+ grid_group+ forest_ind, data = train)
              , direction = "backward", k = 2)

#backwards AIC suggests sqISI ~ summer + wind + temp + rainvnorain + RH + DMC + forest_ind
full = lm(sqISI ~  summer+ wind + temp + rainvnorain + RH 
          + DMC+ grid_group+ forest_ind, data = train)
null = lm(sqISI~1,data = train)
step(null, scope = list(lower = null, upper = full)
     , direction = "forward", k = log(n))
#forwards BIC suggests sqISI ~ summer + wind + temp
step(null, scope = list(lower = null, upper = full)
     , direction = "forward", k = 2)
#forwards AIC suggests sqISI ~ summer + wind + temp + rainvnorain + RH + DMC + forest_ind

#-------------------------------------------------------------------
#
#         Weighted model(s) 
#
#-------------------------------------------------------------------

#Weighted based on FFMC uneven groupings
gp1 = which(train$FFMC<80)
#gp3 = which(80<= train$FFMC & train$FFMC <90)
gp4 = which(80<= train$FFMC & train$FFMC < 83)
gp5 = which(85<= train$FFMC)

train[gp1, "weight"] = length(gp1)
#train[gp2, "weight"] = length(gp2)
#train[gp3, "weight"] = length(gp3)
train[gp4, "weight"] = length(gp4)
train[gp5, "weight"] = length(gp5)

m = lm(sqISI ~
         summer
       + wind 
       + temp
       + rainvnorain 
       #+ RH 
       #+ DMC
       #+ grid_group
       #+ forest_ind
       ,weights = weight
       ,data = train[-89,])

par(mfrow = c(2,2));plot(m)
car::avPlots(m)
summary(m)
#Weighted based on spatial locations
library(dplyr)
train = data.frame(train %>% group_by(ra) %>% mutate(weight = n()))

#see new_spatial_data.R for the X2 and Y2 variables
m = lm(sgISI ~
         summer
       + wind 
       + temp
       + rainvnorain 
       #+ RH 
       #+ DMC
       #+ grid_group
       #+ forest_ind
       #,weights = weight
       ,data = train)
par(mfrow = c(2,2));plot(m)
car::avPlots(m)
summary(m)


#Weighted based on rain vs no rain
train = data.frame(train %>% group_by(rainvnorain) %>% mutate(weight = n()))

#see new_spatial_data.R for the X2 and Y2 variables
m = lm(sqISI ~
         summer
       + wind 
       + temp
       #+ rainvnorain 
       + tFFMC
       #+ DMC
       #+ grid_group
       #+ forest_ind
       ,weights = weight
       ,data = train[-89,])
par(mfrow = c(2,2));plot(m)
car::avPlots(m)
summary(m)

#Weighted based on tFFMC
train = data.frame(train %>% group_by(tFFMC) %>% mutate(weight = n()))
m = lm(lgISI ~
         summer
       + wind 
       + temp
       #+ rainvnorain 
       #+ tFFMC
       #+ DMC
       #+ grid_group
       #+ forest_ind
       ,weights = weight
       ,data = train)
par(mfrow = c(2,2));plot(m)
car::avPlots(m)
summary(m)


#Weighted based on tFFMC
lgtrain = data.frame(train %>% group_by(tFFMC) %>% mutate(weight = n()))
m = lm(lgISI ~
         summer
       + wind 
       + temp
       #+ rainvnorain 
       #+ tFFMC
       #+ DMC
       #+ grid_group
       #+ forest_ind
       ,weights = weight
       ,data = lgtrain)
par(mfrow = c(2,2));plot(m)
car::avPlots(m)
summary(m)
#-------------------------------------------------------------------
#
#         Random intercept model(s) 
#
#-------------------------------------------------------------------

train[train$X2 == 2 & train$Y2 == 4, "region"] = 1
train[train$X2 == 3 & train$Y2==4, "region"] = 2
train[train$X2 == 4 & train$Y2==4, "region"] = 3
train[train$X2 == 5 & train$Y2==4, "region"] = 4
train[train$X2 == 6 & train$Y2==4, "region"] = 5
train[train$X2 == 7 & train$Y2==4, "region"] = 6

train[train$X2 == 2 & train$Y2==5, "region"] = 7
train[train$X2 == 3 & train$Y2==5, "region"] = 8
train[train$X2 == 4 & train$Y2==5, "region"] = 9
train[train$X2 == 5 & train$Y2==5, "region"] = 10
train[train$X2 == 6 & train$Y2==5, "region"] = 11
train[train$X2 == 7 & train$Y2==5, "region"] = 12

train$region = as.factor(train$region)


ris_model = lme(sqISI ~ summer + wind + temp + rainvnorain + tFFMC
                ,random=~1|region
                , data =  train[-89,]
                , method = "REML")


