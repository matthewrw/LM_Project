#Kelly Kung
#MA 575 Final Project
#set up indicators and transform data

setwd("/Users/kkung/Documents/MA 575/project")
fires<-read.csv("forestfires.csv")

#################################################################
# Variables:
# wkd = weekend indicator for FRI - SUN
# wkdM = weekend indicator for FRI - MON
# summer = summer indicator for JUN - SEP
# areaTrans = transformed area log(area + 1) as per the paper
# FFMCQuantile = c(1:10) each for the 10 quantiles
# rainvnorain = rain indicator: 1 if not equal to 0
# wetness = wetness calculated using RH and temp 

# train = training data from the full data
# test = testing data from the full data
# forest_ind = indicator for forests 
# grid_group = grouped coordinates 
#################################################################


#create the indicators for weekend and summer
wkd<-rep(0,nrow(fires))
wkd[fires$day %in% c("fri", "sat", "sun")]<-1
wkdM<-rep(0,nrow(fires))
wkdM[fires$day %in% c("fri", "sat", "sun", "mon")]<-1
summer<-rep(0,nrow(fires))
summer[fires$month %in% c("jun", "jul", "aug", "sep")]<-1

#transform the area
areaTrans<-log(fires$area + 1)

#calculate the groupings of FFMC by quantile
ffmcQuant<-quantile(fires$FFMC, probs = seq(0, 1, .1))
FFMCQuantile<-rep(0, nrow(fires))
FFMCQuantile[fires$FFMC<=ffmcQuant[2]]<-1
FFMCQuantile[ffmcQuant[2]<fires$FFMC & fires$FFMC<=ffmcQuant[3]]<-2
FFMCQuantile[ffmcQuant[3]<fires$FFMC & fires$FFMC<=ffmcQuant[4]]<-3
FFMCQuantile[ffmcQuant[4]<fires$FFMC & fires$FFMC<=ffmcQuant[5]]<-5
FFMCQuantile[ffmcQuant[5]<fires$FFMC & fires$FFMC<=ffmcQuant[6]]<-6
FFMCQuantile[ffmcQuant[6]<fires$FFMC & fires$FFMC<=ffmcQuant[7]]<-7
FFMCQuantile[ffmcQuant[7]<fires$FFMC & fires$FFMC<=ffmcQuant[8]]<-8
FFMCQuantile[ffmcQuant[8]<fires$FFMC & fires$FFMC<=ffmcQuant[9]]<-9
FFMCQuantile[ffmcQuant[9]<fires$FFMC]<-10

#rain indicator
rainvnorain<-rep(0,nrow(fires))
rainvnorain[fires$rain != 0]<-1

#wetness metric
rel_humid100_temp <- c(0,5,10,15,17,19,20,22,24,26,29,32,35)
rel_humid100_water <- c(4.2,5.74,7.84,10.7,12.12,13.73,14.62,16.56,18.76,21.25,25.62,30.89,37.24)

water_at_full <- approxfun(x = rel_humid100_temp, y = rel_humid100_water )
est_wetness_metric <- function(Temp,rh){
  return(water_at_full(Temp)*rh)
}
wetness <- est_wetness_metric(fires$temp,fires$RH)

#grid for area
library(sp)
library(dplyr)

# forest_indicator
forest_coords <- c(1, 1, 1, 1, 1, 1, 1, 1, 1
                   , 0, 0, 1, 1, 1, 0, 1, 1, 1
                   , 0, 0, 1, 0, 0, 0, 1, 1, 0
                   , 0, 0, 1, 0, 0, 0, 1, 1, 0
                   , 0, 1, 0, 0, 1, 1, 1, 1, 1
                   , 0, 0, 0, 1, 0, 0, 0, 0, 1
                   , 1, 1, 1, 1, 0, 0, 0, 0, 1
                   , 1, 1, 1, 1, 1, 0, 0, 0, 0
                   , 1, 1, 1, 1, 1, 0, 0, 0, 0)
forest_coords <- matrix(forest_coords, nrow = 9, ncol = 9)

for(i in 1:nrow(fires)){
  fires[i, "forest_ind"] <- forest_coords[fires[i, "X"], fires[i, "Y"]]
}

# geo-spatial grid
fires <- fires %>% group_by(X, Y) %>% mutate(coords_count = n())
coords <- fires[-23, ]

coordinates(coords) <- ~ X + Y
sp.theme = TRUE
spplot(coords, "coords_count", colorkey = TRUE)
spplot(coords, "ISI", colorkey = TRUE)

fires[, "grid_group"] <- "other"                      # default (other)
fires[fires$X %in% c(1, 2, 3) & 
        fires$Y %in% c(2, 3, 4), "grid_group"] <- "tl" # top left mountain
fires[fires$X %in% c(3, 4, 5) & 
        fires$Y %in% c(3, 4, 5), "grid_group"] <- "ml" # middle left mountain
fires[fires$X %in% c(5, 6, 7) & 
        fires$Y %in% c(3, 4, 5), "grid_group"] <- "mr" # middle right mountain
fires[fires$X %in% c(7, 8) & 
        fires$Y %in% c(6, 7), "grid_group"] <- "br"    # bottom right mountain


#put everything together 
fires$wkd<-wkd
fires$wkdM<-wkdM
fires$summer<-summer
fires$areaTrans<-areaTrans
fires$FFMCQuantile<-FFMCQuantile
fires$wetness<-wetness
fires$rainvnorain<-rainvnorain

#create train data 
set.seed(575)
train.ind <- sample.int(n = nrow(fires), size = floor(nrow(fires) * 0.7), replace = FALSE)
train<- fires[train.ind, ]
test <- fires[-train.ind, ]

#save the data:
write.csv(fires, "/Users/kkung/Documents/GitHub/LM_Project/data/full_data_with_covariates", row.names=FALSE)
write.csv(test, "/Users/kkung/Documents/GitHub/LM_Project/data/test.csv", row.names=FALSE)
write.csv(train, "/Users/kkung/Documents/GitHub/LM_Project/data/train.csv", row.names=FALSE)
