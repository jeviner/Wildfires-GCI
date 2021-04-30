#learning how to do SDM model with Wesley, Jessica, Liz, on April 29th
# using an R tutorial on plant data

###
#https://rspatial.org/raster/sdm/4_sdm_envdata.html
###

#load libraries
install.packages(c('raster', 'rgdal', 'dismo', 'rJava'))

library(dismo)
library(maptools)
library(dplyr)
library(raster)
library(rgdal)
library(rJava)
#library(tidyverse)

#load files we will need
file <- paste0(system.file(package="dismo"), "/ex/bradypus.csv")
# this is the file we will use:

#read in table form the file
bradypus <- read.table(file,  header=TRUE,  sep=",")
bradypus <- bradypus[,2:3]

#remove NA values
acgeo <- subset(acaule, !is.na(lon) & !is.na(lat))


#map out the species data
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-80,70), ylim=c(-60,10), axes=TRUE, col="light yellow")

# restore the box around the map
box()
# add the points
points(acgeo$lon, acgeo$lat, col='red', cex=0.75)

#Load Environmental data
path <- file.path(system.file(package="dismo"), 'ex')
files <- list.files(path, pattern='grd$', full.names=TRUE )
files

predictors <- stack(files)
predictors
names(predictors)
plot(predictors)

### let's overlay the point data now.

#focus on one map
plot(predictors, 3)
plot(wrld_simpl, add=TRUE)
# with the points function, "add" is implicit
points(bradypus, col='blue')

#or

plot(predictors, 4)
plot(wrld_simpl, add=TRUE)
# with the points function, "add" is implicit
points(acgeo$lon, acgeo$lat, col='blue')

# Predict based on pb: PRESENCE-ABSENCE MODEL
# - We can do this to predict how likely a wildfire is at a certain point
# - Give all medium-large fires a value of 1 and use certain features
# - Make a model and then make a predictive map
# - Where are fires most likely to occur? Then overlay with urban map because
#   we only care about places where people live
# - Also where do fires start v.s. where they spread
# Where do they start, how big to they get, how long to they last
# Mainly interested in fires that quickly spread
# Goal: Find environmental/urban data to combine with our fire data
# We should start combining codebases into a GitHub repository
presvals <- raster::extract(predictors, bradypus)
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- raster::extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])
head(sdmdata)

# analyze collinearity
# linear means linked I think?
# but we should be fine
pairs(sdmdata[,2:5], cex=0.1)

# save locally - optional
saveRDS(sdmdata, "sdm.Rds")
saveRDS(presvals, "pvals.Rds")

###
#https://rspatial.org/raster/sdm/5_sdm_models.html
###

# create model!
# GAUSSIAN linear model
m1 <- glm(pb ~ bio1 + bio5 + bio12, data=sdmdata)
class(m1)
# asterisks to the far right of coefficients indicate significance
summary(m1)

bc <- bioclim(presvals[,c('bio1', 'bio5', 'bio12')])
class(bc)
pairs(bc)

# Give 3 arbitrary values, and then predict the results?
bio1 = c(40, 150, 200)
bio5 = c(60, 115, 290)
bio12 = c(600, 1600, 1700)
pd = data.frame(cbind(bio1, bio5, bio12))

# print likelihoods for the 3 different scenarios
predict(m1, pd)
predict(bc, pd)

# create response plots - graph likelihoods of various values
response(bc)

predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern='grd$', full.names=TRUE ))
names(predictors)
p <- predict(predictors, m1)
plot(p)
