library(rmsfuns)

#batch install packages needed
pkgs <- c("dbplyr","dplyr","ggmap","sf","cowplot","here","devtools",
          "rgeos","raster","mapview","leaflet",
          "leaflet.extras","htmltools","RSQLite","purr","ggplot2","ggthemes",
          "xts","ggfortify","maps","mapdata","datasets")

# Load packages
load_pkg(pkgs)

#looks like RSQLite will need to be added manually

install.packages("RSQLite")
#on my R, I had to update or install DBI package too, otherwise RSQLite wouldn't work
install.packages("DBI")

#load the libraries we need for this actual task
library(RSQLite)
library(dbplyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(xts)
library(ggfortify)
library(ggthemes)
library(maps)
library(mapdata)
library(leaflet)
library(datasets)
library(reshape2)

setwd("E:/SynologyDrive/Documents/School/Chapman/Spring 2021/SCI 200")
fires<-read.csv("Wildfires(2).csv")
CA_fires<- filter(fires,region == "california")

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

CA_fires_G <- filter(CA_fires,FIRE_SIZE_CLASS=='G')
CA_fires_G <- CA_fires_G[, c("FIRE_YEAR","FIRE_SIZE")]
head(CA_fires_G)
maxFireSize <- max(CA_fires_G$FIRE_SIZE)
ggplot(CA_fires_G, aes(x=FIRE_YEAR, y=FIRE_SIZE/1000)) +
  geom_point() + 
  stat_smooth(method="glm", formula=y~x, se=FALSE) +
  theme_minimal() +
  labs(x="Year",y="Fire Size (1000 acres)",title="Large Fire Sizes by Year")

# I think G = at least 10000
# 20000 seems good, shows trend + still not feeling like a lot of data is missing
CA_fires_over_X_acres <- filter(CA_fires_G, FIRE_SIZE>20000)
ggplot(CA_fires_over_X_acres, aes(x=FIRE_YEAR, y=FIRE_SIZE/1000)) +
  geom_point() + 
  stat_smooth(method="glm", formula=y~x, se=FALSE) +
  theme_minimal() +
  labs(x="Year",y="Fire Size (1000 acres)",title="Fire Sizes by Year")

get_fires_year <- function(sizes, reverse = TRUE) {
  if(reverse) {
    sizes = rev(sizes)
  }
  
  CA_fires_by_year<-CA_fires %>% 
    group_by(FIRE_YEAR) %>%
    summarize(n_fires = n()) 
  
  CA_fires_size<-CA_fires %>% 
    group_by(FIRE_SIZE_CLASS) %>%
    summarize(n = n()) 

  for(i in 1:length(sizes)) {
    size_class <- sizes[i]
    for(j in 1:nrow(CA_fires_by_year)) {
      row <- CA_fires_by_year[j,]
      year <- row[["FIRE_YEAR"]]
      matching_fires <- filter(CA_fires, FIRE_SIZE_CLASS == size_class, FIRE_YEAR == year)
      CA_fires_by_year[j,paste("size_", size_class, sep="")] <- nrow(matching_fires)
    }
  }
  # NOTE: Requires library reshape2
  CA_fires_by_year_melt = melt(CA_fires_by_year,id.vars=c("FIRE_YEAR","n_fires"))
  
  ggplot(CA_fires_by_year_melt, aes(x=FIRE_YEAR, y=value/1000,fill=variable)) +
    geom_bar(stat='identity') + 
    labs(x="Year",y="Number of wildfires (thousands)", title = "CA Wildfires Per Size By Year")
}

get_fires_proportion <- function(sizes, reverse = TRUE) {
  if(reverse) {
    sizes = rev(sizes)
  }
  CA_fires_by_proportion<-CA_fires %>% 
    group_by(FIRE_YEAR) %>%
    summarize(n_fires = n()) 
  #CA_fires_by_year
  
  CA_fires_size<-CA_fires %>% 
    group_by(FIRE_SIZE_CLASS) %>%
    summarize(n = n()) 
  #CA_fires_size
  
  #CA_fires
  bar_length <- 100
  
  for(i in nrow(CA_fires_by_proportion)) {
    row <- CA_fires_by_proportion[i,]
    year <- row[["FIRE_YEAR"]]
    num_fires <- row[["n_fires"]]
    sum <- 0
    matching <- numeric(length(sizes))
    for(j in 1:length(sizes)) {
      size_class <- sizes[j]
      matching_fires <- filter(CA_fires, FIRE_SIZE_CLASS == size_class, FIRE_YEAR == year)
      sum <- sum + nrow(matching_fires)
      matching[j] = nrow(matching_fires)
    }
    print(matching)
    for(j in 1:length(sizes)) {
      num_matching = matching[j]
      CA_fires_by_proportion[j,paste("perc_", size_class, sep="")] <- bar_length * (num_matching / sum)
    }
    
  }
  head(CA_fires_by_proportion)
  CA_fires_by_year_melt = melt(CA_fires_by_proportion,id.vars=c("FIRE_YEAR","n_fires"))
  #head(CA_fires_by_year_melt)
  
  ggplot(CA_fires_by_year_melt, aes(x=FIRE_YEAR, y=value,fill=variable)) +
    geom_bar(stat='identity') + 
  labs(x="Year",y="Percentage (%)", title = "Proprortions of Wildfire Sizes by Year")
    
}

sizes = c('A', 'B', 'C', 'D', 'E', 'F', 'G')
get_fires_year(sizes, FALSE);

sizes = c('C', 'D', 'E', 'F', 'G')
get_fires_year(sizes);

sizes = c('D', 'E', 'F', 'G')
get_fires_year(sizes);

sizes = c('A', 'B', 'C', 'D', 'E', 'F', 'G')
get_fires_proportion(sizes);

sizes = c('D', 'E', 'F', 'G')
get_fires_proportion(sizes);


# Wildfire Counts by Year
ggplot(CA_fires_by_year, aes(x = FIRE_YEAR, y = n_fires/1000)) + 
  geom_bar(stat = 'identity', fill = 'orange') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', size = 0.4, color = 'red') + 
  labs(x = '', y = 'Number of wildfires (thousands)', title = 'CA Wildfires by Year')

  

CA_fires_year_size <- filter(CA_fires, FIRE_SIZE_CLASS == 'B')
CA_fires_year_size<-CA_fires_year_size %>% 
  group_by(FIRE_YEAR) %>%
  summarize(n_fires = n()) 

CA_fires_year_size %>% ggplot(aes(x = FIRE_YEAR, y = n_fires/1000)) + 
  geom_bar(stat = 'identity', fill = 'orange') +
  theme_minimal() +
  geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', size = 0.4, color = 'red') + 
  labs(x = '', y = 'Number of wildfires (thousands)', title = 'CA Wildfires by Year')