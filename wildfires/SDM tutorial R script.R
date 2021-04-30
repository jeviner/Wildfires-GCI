#learning how to do SDM model with Wesley, Jessica, Liz, on April 29th
# using an R tutorial on plant data

rm(list = ls())
par(mfrow = c(1,1))

setwd("/Users/jessicaviner/Desktop/wildfires")

#load libraries
library(dismo)
library(maptools)

#load files we will need
file <- paste0(system.file(package="dismo"), "/ex/bradypus.csv")
# this is the file we will use:

#read in table form the file
bradypus <- read.table(file,  header=TRUE,  sep=",")
bradypus <- bradypus[,2:3]


acaule <- gbif("solanum", "acaule*", geo=FALSE)
colnames(acaule)

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
plot(predictors, 1)
plot(wrld_simpl, add=TRUE)
# with the points function, "add" is implicit
points(bradypus, col='blue')

#or

plot(predictors, 1)
plot(wrld_simpl, add=TRUE)
# with the points function, "add" is implicit
points(acgeo$lon, acgeo$lat, col='blue')

#extracting data from the model
presval <- raster::extract(predictors, bradypus)

set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- raster::extract(predictors, backgr)
pb <- c(rep(1, nrow(presval)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presval, absvals)))
sdmdata[, "biome"] = as.factor(sdmdata[, "biome"])
head(sdmdata)


pairs(sdmdata[,2:5], cex = 0.1) #covariance?

#save
saveRDS(sdmdata, "sdm.Rds")
saveRDS(presval, "pvals.Rds")


#----- modelling
m1 = glm(pb ~bio1 + bio5 + bio12, data = sdmdata)
class(m1)


#### bioclim modelling now

bc <- bioclim(presval[, c('bio1', 'bio5', 'bio12')])
class(bc)


### predictive modelling
#arbitrary scenarios
bio1 = c(40, 150, 200)
bio5 = c(60, 115, 290)
bio12 = c(600, 1600, 1700)
pd = data.frame(cbind(bio1, bio5, bio12))
predict(m1, pd)
response(bc)

#making a predictive map
predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern = 'grds', full.names = TRUE))
names(predictors)
p <- predict(predictors, m1)
plot(p)

