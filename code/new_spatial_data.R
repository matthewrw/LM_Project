#Read in data
setwd("~/Desktop/github/LM_Project/data")
train = data.frame(read.csv("train.csv"))

train$X2 = train$X
train$Y2 = train$Y
table(train$Y2, train$X2)


train$Y2[train$Y2 == 9] = 8
table(train$Y2, train$X2)

train$X2[train$X2 == 9] = 8
table(train$Y2, train$X2)

train$Y2[train$Y2 == 8] = 6
table(train$Y2, train$X2)

train$Y2[train$Y2 == 2] =3
table(train$Y2, train$X2)

train$X2[train$X2 == 1] = 2
table(train$Y2, train$X2)

train$X2[train$X2 == 8] = 7
table(train$Y2, train$X2)


train$Y2[train$Y2 == 6] = 5
table(train$Y2, train$X2)

train$Y2[train$Y2 == 3] = 4
z = table(train$Y2, train$X2)


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

#RANDOM INTERCEPT MODEL BASED ON NEW REGIONS 
data.frame(train %>% group_by(X,Y) %>% summarize(weight = mean(ISI)))

ris_model = lme(sqISI ~ summer + wind + temp + rainvnorain + tFFMC
                ,random=~1|region
                , data =  train[-89,]
                , method = "REML")

