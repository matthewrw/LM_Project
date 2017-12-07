library(sp)
library(dplyr)

setwd("/Users/njosephs/Desktop/PhD/575/final project/")
fires <- read.csv("forestfires.csv")

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


# split data
set.seed(575)
train.ind <- sample.int(n = nrow(fires), size = floor(nrow(fires) * 0.7), replace = FALSE)
train <- fires[train.ind, ]
test <- fires[-train.ind, ]

# justify throwing out record(s)
train_no_outliars <- train[-89, ]
