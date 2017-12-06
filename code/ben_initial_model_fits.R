setwd("~/Desktop/github/LM_Project/data")
train = read.csv("train.csv")

m = lm(ISI ~., data = train[-89,])
summary(m)
par(mfrow = c(2,2))
plot(m)

train2 = train[,-1]
train2$lISI = log(train$ISI + 1)

m = lm(lISI ~., data = train2)
summary(m)
par(mfrow = c(2,2))
plot(m)

m = lm(ISI ~., data = train[-305,])
boxcox(m)

train3 = train[,-1]
train3$sqISI = (train$ISI)^(1/2)

m = lm(sqISI ~., data = train3[-89,])
summary(m)
par(mfrow = c(2,2))
plot(m)

library("plotly")
tcbs = as.matrix(read.table("tcbs_cost_0101"))

mat = matrix(0, ncol = 9, nrow = 9)
for(i in 1:9){
	for(j in 1:9){
		indx = which(fires$X == i)
		indy = which(fires$X == j)
		ind = intersect(indx, indy)
		
		mat[i,j] = mean(fires[ind])
	}
}


lambda = c(0,.01,.02,.03,.04,.05,.06,.07,.08,.09, .1,.15,.2)
ell = seq(2,400,2)
cost = as.matrix(tcbs)


plot_ly(z =~cost ,y = ~ell,x = ~lambda, showscale = FALSE) %>% add_surface()
