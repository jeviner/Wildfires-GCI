
#Feb 18 2021, start of wildfire analyses in R for GCI wildfire team
#Brian Hoover initial code to get us started.
#Idea is to start with a burn history of previous CA fires, and start to use that
#to predict where the trouble spots are.  Then, we can match that with a future 
#land use data set.

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
library(rmsfuns)
library(mapdata)
library(leaflet) #interactive maps 
library(datasets)


##this step is for connecting to a database and pulling just one aspect from it, then disconnecting.
#Brian will send the fire data he pulls so there is no need to do this unless you want to get some
#practice pulling data, etc. 

#conn <- dbConnect(SQLite(), 'desktop/FPA_FOD_20170508.sqlite')
#fires <- tbl(conn, "Fires") %>% collect()

#print(object.size(fires), units = 'Gb')
#dbDisconnect(conn)

#loading the data yourself
setwd("~/Desktop/Wildfires-GCI")
fires<-read.csv("Wildfires.csv")

#Let's start looking at the data
glimpse (fires)
head(fires)


# Let's pick CA fires only.  We do this using the command in the dpyr package.
#Dplyr is the way you sort, filter, select, and summarize data in R. 
#Definitely worth your time doing a tutorial
#    https://rpubs.com/coleeagland/dplyrtutorialforjohnandjenn

CA_fires<- filter(fires,region == "california")
head(CA_fires)

#How many fires occurred within each year.  Is there an increasing trend?

CA_fires_by_year<-CA_fires %>% 
  group_by(FIRE_YEAR) %>%
  summarize(n_fires = n()) 
CA_fires_by_year



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
}

#Is there a trend in this data?  We can plot it using the package ggplot2, which is
#all about data visualization

ggplot(data=CA_fires_by_year, aes(x = FIRE_YEAR, y = n_fires/1000)) + 
  geom_bar(stat = 'identity', fill = 'orange') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank()) +
  geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', size = 0.4, color = 'red') + 
  labs(x = '', y = 'Number of wildfires (thousands)', title = 'CA Wildfires by Year')

#many ways to plot this
ggplot(data=CA_fires_by_year, aes(x = FIRE_YEAR, y = n_fires/1000)) + 
  geom_line() + geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank()) + 
  geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', size = 0.4, color = 'red') + 
  labs(x = '', y = 'Number of wildfires (thousands)', title = 'CA Wildfires by Year')

#what about the size of the fires??
CA_fires_size <- CA_fires %>% 
  group_by(FIRE_SIZE_CLASS) %>%
  summarize(n = n()) 
CA_fires_size

#I looked up what the letters correspond to in terms of surface area burned.  
#We can turn it into size classes

size_classes <- c('A' = '0-0.25', 'B' = '0.26-9.9', 'C' = '10.0-99.9', 'D' = '100-299', 'E' = '300-999',
                  'F' = '1000-4999', 'G' = '5000+')
size_classes

#we can plot this too
CA_fires_size %>% mutate(FIRE_SIZE_CLASS = size_classes[FIRE_SIZE_CLASS]) %>%
  ggplot(aes(x = FIRE_SIZE_CLASS, y= n)) +
  geom_bar(stat = 'identity', fill = 'Orange') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank()) +
  labs(x = 'Fire size (acres)', y = 'Number of fires', title = 'Number of Wildfires by Size Class')

#____________________________________________________________________________________________________________________________
#we can make some loose maps

state.abb <- append(state.abb, c("DC", "PR"))
state.name <- append(state.name, c("District of Columbia", "Puerto Rico"))


#fires$region <- map_chr(fires$STATE, function(x) { tolower(state.name[grep(x, state.abb)]) })

#historical issues
#make map by year

# Get the us state map data
counties <- map_data('county')
county_map <- map_data('county', 'california')


fires %>%
  filter(region == 'california') %>%
  group_by(region, subregion = tolower(FIPS_NAME)) %>%
  summarize(n_fires = n())  %>%
  right_join(county_map, by = c('region', 'subregion'))  %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n_fires)) + 
  geom_polygon() + 
  geom_path(color = 'white', size = 0.1) + 
  scale_fill_continuous(low = "orange", 
                        high = "darkred",
                        name = 'number of fires') + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("Average Burn Time of CA Wildfires by County 1992-2015") + 
  theme(plot.title = element_text(hjust = 0.5))
  
#iris package
