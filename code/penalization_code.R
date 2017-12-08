#---------------------------------------
#
#       LASSO Fits 
#
#---------------------------------------

#Read in data
setwd("~/Desktop/github/LM_Project/data")
train = data.frame(read.csv("train.csv"))

#transform ISI 
train$wISI = sqrt(log(train$ISI + 1))

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
f = formula(wISI ~ day + month + X + Y 
            +  FFMC + DMC + DC + temp + RH + wind + wkd + wkdM +  summer
            + areaTrans + wetness + rainvnorain + forest_ind + grid_group 
            + FFMC:rainvnorain + FFMC:DMC + FFMC:DC+ DC:DMC+ summer:rainvnorain + temp:RH 
            + wetness:RH + FFMC:wind + forest_ind:grid_group)

X = model.matrix(f,train)
Y = as.matrix(train$wISI)

cv = glmnet::cv.glmnet(X, Y, alpha = 1)
lambda_opt = cv$lambda.min

lasso = glmnet::glmnet(X, Y, alpha = 1, lambda = lambda_opt)


