#--------------------------------------------
#
#     Make graphic 
#
#--------------------------------------------
#Read in data
setwd("~/Desktop/github/LM_Project/data")
train = data.frame(read.csv("train.csv"))

#aggreate ISI means over (X,Y)
df = data.frame(train %>% group_by(X,Y) %>% summarize(mISI = mean(ISI)))
head(df)

z = matrix(0, ncol = 9, nrow = 9)
xnames = unique(df[,1])
ynames = unique(df[,2])

for(x in xnames){
  for(y in ynames){
    ind = which(df[,1] == x & df[,2] == y)
    if(length(ind) != 0)z[y,x] = df[ind,3]
  }
}

library(plotly)
p1 <- plot_ly(x = 1:9, y = 1:9, z = z) %>% add_surface()
p2 <- plot_ly(x = 1:9, y = 1:9, z = z, type = "heatmap")
p2

#aggreate ISI means over (X2,Y2)
df = data.frame(train %>% group_by(X2,Y2) %>% summarize(mISI = mean(ISI)))
head(df)

z = matrix(0, ncol = 6, nrow = 2)
xnames = unique(df[,1])
ynames = unique(df[,2])

for(x in xnames){
  for(y in ynames){
    ind = which(df[,1] == x & df[,2] == y)
    if(length(ind) != 0)z[y-3,x-1] = df[ind,3]
  }
}

library(plotly)
p3 <- plot_ly(x = 1:9, y = 1:9, z = z) %>% add_surface()
p4 <- plot_ly(x = 1:9, y = 1:9, z = z, type = "heatmap")
p3
p4