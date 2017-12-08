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


train[train$X2 == 2 & train$Y2 == 4, "weights"] = z[1,1]
train[train$X2 == 3 & train$Y2==4, "weights"] = z[1,2]
train[train$X2 == 4 & train$Y2==4, "weights"] = z[1,3]
train[train$X2 == 5 & train$Y2==4, "weights"] = z[1,4]
train[train$X2 == 6 & train$Y2==4, "weights"] = z[1,5]
train[train$X2 == 7 & train$Y2==4, "weights"] = z[1,6]

train[train$X2 == 2 & train$Y2==5, "weights"] = z[2,1]
train[train$X2 == 3 & train$Y2==5, "weights"] = z[2,2]
train[train$X2 == 4 & train$Y2==5, "weights"] = z[2,3]
train[train$X2 == 5 & train$Y2==5, "weights"] = z[2,4]
train[train$X2 == 6 & train$Y2==5, "weights"] = z[2,5]
train[train$X2 == 7 & train$Y2==5, "weights"] = z[2,6]


library(dplyr)
train = data.frame(train %>% group_by(X2,Y2) %>% mutate(weight = n()))

data.frame(train %>% group_by(X,Y) %>% summarize(weight = mean(ISI)))



