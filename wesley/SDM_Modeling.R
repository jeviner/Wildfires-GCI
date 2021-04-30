
#install devtools first.

#Hydrography data layers won't load, need specialized unzipping software.  
#Something to work on later.  Seems like a good Wesley Ho project.

#install devtools first to pull R code from Git
#devtools::install_github("jsta/nhdR")
#library(nhdR)  #... might need to restart R for this to work.
#damn thing won't open 7-sip files... program below should allow 
#devtools::install_github("jimhester/archive")
#throws error code because needs installation via terminal or X code.
  #Adding vegetation layers

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
  #for this, I downloaded the shapefile for: Ecological Subsection: Potential Natural Vegetation,
  #and put data in the folder I made below on my desktop
  
  #set our directory
  setwd("E:/SynologyDrive/Documents/School/Chapman/Spring 2021/SCI 200/S_USA.PNV_2000Sections")
  #import the shape file
  Veg_data <- st_read("S_USA.PNV_2000Sections.shp")
  
  print(length(Veg_data))
  CA.bbox <- data.frame( long=c(-122.084, -115.997, -115.997, -122.084, -122.084),
                         lat =c( 32.822, 32.822, 37.677, 37.677, 32.822))
  crsLongLat <- "+proj=longlat +datum=WGS84 +no_defs"
  crsLAEA <- "+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  CA.bbox.sf <- st_sfc(st_polygon(list(as.matrix(CA.bbox[,c("long","lat")]))), crs=crsLongLat)
  
  new_bbox=c(-122.084,32.822,-115.997,37.677)
  names(new_bbox) = c("xmin", "ymin", "xmax", "ymax")
  attr(new_bbox, "class") = "bbox"
  attr(st_geometry(Veg_data), "bbox") = new_bbox
  head(Veg_data)
  print(length(Veg_data))
  
  laeabb <- st_transform(CA.bbox.sf, crs = crsLAEA)
  b <- st_bbox(laeabb)
  b
  
  tree_list_all = strsplit("PINE_FORES PINE_FOR_1 GREAT_BASI GREAT_BA_1 PINE_DOUG_ PINE_DOUG1 DOUG_FIR_A DOUG_FIR_P MIXED_CONI MIXED_CO_1 SILVER_FIR SILVER_F_1 GRAND_FIR_ GRAND_FIR1 RED_FIR_CA RED_FIR__1 SPRUCE_FIR SPRUCE_F_1 SW_MIXED_C SW_MIXED_1 REDWOOD_CA REDWOOD__1 CEDAR_HEML CEDAR_HE_1 CEDAR_HE_2 CEDAR_HE_3 SPRUCE_CED SPRUCE_C_1 FIR_HEMLOC FIR_HEML_1 WESTERN_SP WESTERN__1 LODGEPOLE_ LODGEPOLE1 CA_MIXED_E CA_MIXED_1 OAKWOODS_C OAKWOODS_1 MSAIC_CDR_ MSAIC_CDR1 ALDER_ASH_ ALDER_ASH1 JUNIPER_PI JUNIPER__1 JUNIPER_ST JUNIPER__2 MEQUITE_BO MEQUITE__1 SAGEBRUSH_ SAGEBRUSH1 CHAPARRAL_ CHAPARRAL1 SOUTHWEST_ SOUTHWEST1 DESERT_SHR DESERT_S_1 SHINNERY_A SHINNERY_P ANNUAL_GRA ANNUAL_G_1 MOUNTAIN_G MOUNTAIN_1 PLAINS_GRA PLAINS_G_1 PRAIRIE_AC PRAIRIE_PC DESERT_GRA DESERT_G_1 TEXAS_SAVA TEXAS_SA_1 WET_GRASSL WET_GRAS_1 ALPINE_MEA ALPINE_M_1 OAK_SAVANN OAK_SAVA_1 MSAIC_BLUE MSAIC_BL_1 CROSS_TIMB CROSS_TI_1 CONIFER_BO CONIFER__1 GREAT_LAKE GREAT_LA_1 EASTERN_SP EASTERN__1 MAPLE_BASS MAPLE_BA_1 OAK_HICKOR OAK_HICK_1 ELM_ASH_FO ELM_ASH__1 MAPLE_BEEC MAPLE_BE_1 MIXED_MESO MIXED_ME_1 APPALACHIA APPALACH_1 TRANS_APPL TRANS_AP_1 NORTHERN_H NORTHERN_1 NORTHERN_2 NORTHERN_3 NORTHERN_4 NORTHERN_5 NORTHEASTE NORTHEAS_1 OAK_HICK_2 OAK_HICK_3 SOUTHERN_M SOUTHERN_1 LOBLOLLY_S LOBLOLLY_1 BLACKBELT_ BLACKBELT1 OAK_GUM_CY OAK_GUM__1 NORTHERN_F NORTHERN_6 SOUTHERN_F SOUTHERN_2", " ")[[1]]
  tree_list = list()
  index = 0
  for(i in 1:length(tree_list_all)) {
    if(i %% 2 != 0) {
      index = index + 1
      tree_list[[index]] <- tree_list_all[i]
    }
  }
  tree_list
  
  #ok, let's look and see what's in here
  head(Veg_data, n=4)
  Veg_data$geometry
  
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
  
  # TODO: Find California polygon
  # Find polygon files for California in R or shapefile, and combine layers
  
  
  for(str in tree_list) {
    print(str)
    print(sum(Veg_data[str][[1]]))
  }
  
  print(Veg_data$SOUTHERN_F)
  
  
  
  #look at layers of interest
  ggplot()+geom_sf(data=Veg_data, size=0.01, aes(fill=WATER_PCT))+theme_bw()+
    coord_sf(crs = crsLAEA, xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]))
  
  ggplot()+geom_sf(data=Veg_data, size=0.01, aes(fill=PINE_FORES))+theme_bw()+
    coord_sf(crs = crsLAEA, xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]))
  
  ggplot()+geom_sf(data=Veg_data, size=0.01, aes(fill=PINE_DOUG_))+theme_bw()+
    coord_sf(crs = crsLAEA, xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]))
  
  
  
  
  
  
  
  
  
  
  