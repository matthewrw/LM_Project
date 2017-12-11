#----------------------------------------------
#
#   Plots doc 
#
#----------------------------------------------

setwd("~/Desktop/github/LM_Project/data")
train = data.frame(read.csv("train.csv"))
setwd("~/Desktop/github/LM_Project/report/report_figures")


library(ggplot2)
library(dplyr)

train$month = factor(train$month, levels = c("jan","feb",'mar','apr','may','jun','jul','aug','sep','oct','nov','dec'))
train$day = factor(train$day, levels = c('sun','mon','tue','wed','thu','fri','sat'))
df1 = data.frame(train %>% group_by(month) %>% summarize(lmonth = n()))
df2 = data.frame(train %>% group_by(day) %>% summarize(lday = n()))
train$sqISI = sqrt(train$ISI)


p1<-ggplot(data=df1, aes(x=month, y = lmonth)) +
  geom_bar(stat="identity", fill="orangered2")+
  theme_minimal()+ labs(y = "Number of Fires",x = "Month", main = "")
pdf("month_bar.pdf")
p1
dev.off()

p2<-ggplot(data=df2, aes(x=day, y = lday)) +
  geom_bar(stat="identity", fill="orangered2")+
  theme_minimal()+ labs(y = "Number of Fires",x = "Day", main = "")
pdf("day_bar.pdf")
p2
dev.off()

p3<-ggplot(data=train, aes(x = "",y=ISI)) +
  geom_boxplot(fill="blue", alpha = 0.5)+
  theme_minimal()+ labs(y = "ISI",x = "", main = "")+
  coord_flip()
pdf("ISI_box.pdf")
p3
dev.off()


p4<-ggplot(data=train, aes(x =FFMC,y=sqISI)) +
  geom_point(color="blue",alpha = 0.7)+
  theme_minimal()+ labs(y = "Square Root ISI",x = "FFMC", main = "")
pdf("FFMC_ISI_scatter.pdf")
p4
dev.off()

#construct regression equation
f1 = formula(sqISI ~ temp + wind + summer + rainvnorain)
f2 = formula(sqISI ~ temp + wind + summer + rainvnorain + FFMC)

#build model matrix
X1 = model.matrix(f1,train)
X2 = model.matrix(f2,train)
Y = as.matrix(train$sqISI)

fit1 = glmnet::glmnet(X1, Y, alpha = 1)
fit2 = glmnet::glmnet(X2, Y, alpha = 1)

pdf("penalization_plots.pdf")
par(mfrow=c(1,2), mai = c(1, 0.5, 0.1, 0.1))
plot_glmnet(fit2, ylim = c(0,.6))
plot_glmnet(fit1, ylim = c(0,.6)) 
dev.off()




