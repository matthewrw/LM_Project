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

#get rid of outlier
train = train[-89,]

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

#construct model matrix
f = formula(wISI ~ FFMC + DMC + DC + temp + RH + wind + wkd + wkdM +  summer
                + areaTrans + wetness + rainvnorain + forest_ind + grid_group 
                + FFMC:rainvnorain + FFMC:DMC + FFMC:DC+ DC:DMC+ summer:rainvnorain + temp:RH 
                + wetness:RH + FFMC:wind + forest_ind:grid_group)

X = model.matrix(f,train)
Y = as.matrix(train$wISI)

#---------------------------------------
#
#       GLMNET Penalization Fits
#
#---------------------------------------

cv = glmnet::cv.glmnet(X, Y, alpha = 1)
lambda_opt = cv$lambda.min

lasso = glmnet::glmnet(X, Y, alpha = 1, lambda = lambda_opt)


#expand search for time of week, time of year, spatial
#construct model matrix
f = formula(sqISI ~ 
            # tFFMC 
            # + day #3 iteration 
            #+ month # 1iteration 
            #+ X #2 iteration 
            #+ Y #2 iteration 
            + DMC #6 iteration 
            + DC #6iteration
            + temp 
            + RH 
            + wind 
            + wkd 
            #+ wkdM #4 iteration  
            + summer
            + areaTrans 
            + wetness 
            + rainvnorain 
            + forest_ind 
            + grid_group 
            #+ tFFMC:rainvnorain 
            #+ FFMC:DMC #Iteration 5 
            #+ FFMC:DC #Iteration 5 
            #+ DC:DMC #Iteration 5 
            + summer:rainvnorain 
            + temp:RH 
            + wetness:RH 
            #+ tFFMC:wind 
            + forest_ind:grid_group
            )

X = model.matrix(f,train)
Y = as.matrix(train$wISI)
a = 1

cv = glmnet::cv.glmnet(X, Y, alpha = a)
plot(cv)
lambda_opt = cv$lambda.min

lasso = glmnet::glmnet(X, Y, alpha = a, lambda = lambda_opt)
tmp = sort(abs(coef(lasso)[,1]), decreasing = TRUE)
varImp = data.frame(VarNames = names(tmp), Beta = as.vector(tmp))
varImp

train$tISI =(train$ISI)^(1/4)
train[train$rainvnorain == 0, "rISI"] = train[train$rainvnorain == 0, "rISI"] - mean(train[train$rainvnorain == 0, "ISI"])
train[train$rainvnorain == 1, "rISI"] = train[train$rainvnorain == 1, "rISI"] - mean(train[train$rainvnorain == 1, "ISI"])
m = lm(rISI ~
         tFFMC
       + wkd
       + summer
       + wind 
       + temp
       #+ wetness
       + rainvnorain 
       + tFFMC:rainvnorain
       + summer:rainvnorain
       + grid_group 
       + forest_ind
       , data = train)
par(mfrow =c(2,2));plot(m)
car::avPlots(m)


#-------------------------------------------------------------------
#
#         Weighted model 
#
#-------------------------------------------------------------------

m = lm(sqISI ~
        summer
       + wind 
       + temp
       + rainvnorain 
       #+ RH 
       #+ DMC
       #+ grid_group
       #+ forest_ind
       , data = train)
par(mfrow =c(2,2));plot(m)
car::avPlots(m)

gp1 = which(train$FFMC<80)
gp3 = which(80<= train$FFMC & train$FFMC <90)
gp4 = which(90<= train$FFMC & train$FFMC < 95)
gp5 = which(95<= train$FFMC)

train[gp1, "weights"] = length(gp1)
#train[gp2, "weights"] = length(gp2)
train[gp3, "weights"] = length(gp3)
train[gp4, "weights"] = length(gp4)
train[gp5, "weights"] = length(gp5)

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
       ,data = train)

par(mfrow = c(2,2));plot(m)
MASS::boxcox(m)




