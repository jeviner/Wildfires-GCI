
#install devtools first.

#Hydrography data layers won't load, need specialized unzipping software.  
#Something to work on later.  Seems like a good Wesley Ho project.

#install devtools first to pull R code from Git
#library(nhdR)  #... might need to restart R for this to work.
#damn thing won't open 7-sip files... program below should allow 
#devtools::install_github("jimhester/archive")
#throws error code because needs installation via terminal or X code.

***************************************************
  ***************************************************
  ***************************************************
  ***************************************************
#Adding vegetation layers
  


******************************************
  #load packages
  pkgs <- c("tidyverse","rgdal","ggmap","sf","cowplot","here","ggplot2",
            "knitr","ggrepel","magrittr","lubridate","sp","geosphere","ggalt",
            "rgeos","FedData","raster","mapview","dbscan","SDMtools","grid","gtable",
            "gridextra","maptools","ncf","pgirmess","proj4","rnaturalearth","rnaturalearthdata","shadowtext","leaflet",
            "leaflet.extras","htmltools","RColorBrewer","leaflet")
  
# Install and load all CRAN packages provided from a character vector
load_pkgs = function(pkgs) {
  new_pkgs = pkgs[!(pkgs %in% installed.packages()[ ,'Package'])]
  if (length(new_pkgs) > 0) install.packages(new_pkgs,repos = "http://cran.cnr.berkeley.edu/")
  invisible(lapply(pkgs,function(x)
    suppressPackageStartupMessages(library(x,character.only = T))))
}
# Load packages
load_pkgs(pkgs)
  
library(tidyverse)
library(rgdal)
library(sf)
library(ggrepel)
library(ggplot2)
library(ggmap)
library(knitr)
library(magrittr)
library(lubridate)
library(sp)
library(geosphere)
library(ggalt) 
library(dbscan)
library(SDMTools)
library(rgeos)
library(grid)
library(gtable)
library(gridExtra)
library(maptools)
library(ncf)
library(pgirmess)
library(proj4)
library(RColorBrewer)
library(leaflet)


#get data at: https://data.fs.usda.gov/geodata/edw/datasets.php
#for this, I downloaded the shape file for: Ecological Subsection: Potential Natural Vegetation,
#and put data in the folder I made below on my desktop

#set our directory
setwd("~/Downloads/Wildrefires/USDA_veg")
Veg_data <- st_read("S_USA.PNV_2000Sections.zip")

#okay, let's look and see what's in here
head(Veg_data, n=4)

#Review geometry type
st_geometry_type(Veg_data)

#what CRS is it in?
st_crs(Veg_data)

#What are the boundaries of this shape file?
st_bbox(Veg_data)

#ok let's plot this crap
ggplot()+geom_sf(data=Veg_data, size=0.01)+theme_bw()

#ok let's get down to a better CA view
#For the moment I don't know how to select by CA boundaires only, 
#so I'm just going to make a let-long box.  For the future, one 
#of us can figure out how to pull CA boundaries only.

CA.bbox <- data.frame( long=c(-122.084, -115.997, -115.997, -122.084, -122.084),
                       lat =c( 32.822, 32.822, 37.677, 37.677, 32.822))


crsLongLat <- "+proj=longlat +datum=WGS84 +no_defs"
crsLAEA <- "+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
CA.bbox.sf <- st_sfc(st_polygon(list(as.matrix(CA.bbox[,c("long","lat")]))), crs=crsLongLat)

laeabb <- st_transform(CA.bbox.sf, crs = crsLAEA)
b <- st_bbox(laeabb)
b

#look at layers of interest
ggplot()+geom_sf(data=Veg_data, size=0.01, aes(fill=WATER_PCT))+theme_bw()+
coord_sf(crs = crsLAEA, xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]))


ggplot()+geom_sf(data=Veg_data, size=0.01, aes(fill=Water_PCT))+theme_bw()+
coord_sf(crs = crsLAEA, xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]))










