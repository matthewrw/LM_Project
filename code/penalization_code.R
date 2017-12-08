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

#construct model matrix
model_matrix = (wISI ~ FFMC + DMC + DC + temp + RH + wind + wkd + wkdM +  summer
                + areaTrans + wetness + rainvnorain + forest_ind + grid_group 
                + FFMC:Rain + FFMC:DMC + FFMC:DC+ DC:DMC+ Summer:Rain + Temperature:RH 
                + Wetness:RH + FFMC:wind + forest_ind:grid_group)
  
  
  
#fit the big model
m = glmnet(wISI ~ FFMC + DMC + DC + temp + RH + wind + wkd + wkdM +  summer + areaTrans + wetness + rainvnorain + forest_ind + grid_group 
       + FFMC:Rain + FFMC:DMC + FFMC:DC+ DC:DMC+ Summer:Rain + Temperature:RH + Wetness:RH + FFMC:wind + forest_ind:grid_group,
       alpha = 
       
       )