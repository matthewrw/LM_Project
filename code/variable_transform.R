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

# firesNoOut = data without the outlier #23
# full data = data with only the columns we want, without the outlier
# train = training data from the full data
# test = testing data from the full data
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

#still need interaction terms 

#grid for area

#put everything together 
fires$wkd<-wkd
fires$wkdM<-wkdM
fires$summer<-summer
fires$areaTrans<-areaTrans
fires$FFMCQuantile<-FFMCQuantile
fires$wetness<-wetness
fires$rainvnorain<-rainvnorain

#take out the outlier
firesNoOut<-fires[-c(23),]

#clean data so we only have columns we need
fullData<-fires[c("ISI", "DMC", "DC", "temp", "RH", "wind", "wkd", "wkdM", 
                  "summer", "areaTrans", "FFMCQuantile", "wetness", "rainvnorain")]
#create train data 
set.seed(575)
train.ind <- sample.int(n = nrow(fullData), size = floor(nrow(fullData) * 0.7), replace = FALSE)
train<- fullData[train.ind, ]
test <- fullData[-train.ind, ]