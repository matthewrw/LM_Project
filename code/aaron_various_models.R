#Aaron Elliot
#MA 575 Final Project
#tried several models, and compared using BIC

setwd("/Home/Fall 2017/CS210/FinalProject/LM_Project")
fires<-read.csv("Data/forestfires.csv")
set.seed(575)
train.ind <- sample.int(n = nrow(fires), size = floor(nrow(fires) * 0.7), replace = FALSE)
train<- fires[train.ind, ]
test <- fires[-train.ind, ]
attach(train)
library(car)
scatterplotMatrix(train)

#_________________________
#Notes
#_________________________
#All multiplied variables
#result in models with all
#possible combinations of
#those variables.
#i.e. y~a*b*c >>> y = abc+ab+bc+ac+a+b+c+1

#Currently, I am trying things and seeing how
#they stick, consider these estimates biased 
#when evaluating the final model.

#Round1!
#         BIC
# M1.1 ,  2120.834
                  # M1.2 ,  2082.039
# M2.1 ,  2136.714
                  # M2.2 ,  2100.901
# M3.1 ,  2083.862
                  # M3.2 ,  2071.833

# MX.1 is using purely FFMC & Wind
# MX.2 is using using FFMC, Wind, temp, DMC, DC
# M1 is using the equation from the 1970's paper
# M2 is using just the FFMC part of the actual equation
# M3 is using Exp(FFMC) (overall winner for round 1 models)

# Round2
#         BIC
# M1.2 ,  2082.039
# M3.1 ,  2083.862
# M3.2 ,  2071.833
# M4   ,  2039.552
# M5   ,  2015.323
# M6   ,  2009.803
# M7   ,  2072.72

#Round2
# M4 is Exp(FFMC)*DMC +Exp(FFMC)*DC + temp
# M5 is Exp(FFMC)*DMC*DC+temp
# M6 is Exp(FFMC)*DMC*DC
# M7 is Exp(-1.386FFMC)*DMC*DC

windExp <- 2^(wind/19)
FineFuel<- exp(-.1386*FFMC)*(1+FFMC^5.31/(49300000))
FFEXP   <- exp(FFMC)
FFEXP2  <- exp(-.1386*FFMC)  

M1.1 <- lm(ISI ~ windExp*FineFuel, data = train)
summary(M1.1)
plot(M1.1,which=1:4)
BIC(M1.1)

M1.2 <- lm(ISI ~ windExp*FineFuel+temp+DMC+DC, data = train)
summary(M1.2)
plot(M1.2,which=1:4)
BIC(M1.2)

M2.1 <- lm(ISI ~ FineFuel)
summary(M2.1)
plot(M2.1,which=1:4)
BIC(M2.1)

M2.2 <- lm(ISI ~ FineFuel+temp+DMC+DC)
summary(M2.2)
plot(M2.2,which=1:4)
BIC(M2.2)

M3.1 <- lm(ISI ~ FFEXP)
summary(M3.1)
plot(M3.1,which=1:4)
BIC(M3.1)

M3.2 <- lm(ISI ~ FFEXP +DMC+DC+temp)
summary(M3.2)
plot(M3.2,which=1:4)
BIC(M3.2)



M4 <- lm(ISI ~ FFEXP*DMC+FFEXP*DC+temp)
summary(M4)
plot(M4,which=1:4)
BIC(M4)


M5 <- lm(ISI ~ FFEXP*DC*DMC+temp)
summary(M5)
plot(M5,which=1:4)
BIC(M5)

M6 <- lm(ISI ~ FFEXP*DC*DMC)
summary(M6)
plot(M6,which=1:4)
BIC(M6)

M7 <- lm(ISI ~ FFEXP2*DC*DMC)
summary(M7)
plot(M7,which=1:4)
BIC(M7)








