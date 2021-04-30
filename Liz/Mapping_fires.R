library(leaflet)
#library(RColorBrewer)
#display.brewer.all()

CA_fires$Fire_Duration <- CA_fires$CONT_DATE - CA_fires$DISCOVERY_DATE
 #add color palette
pal <- colorNumeric(
  palette = "RdBu",
  domain = CA_fires$Fire_Duration
)

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

# Make a leaflet map!
m <- leaflet() %>% addTiles() %>% 
  #setView(lng = -120.8, lat = 39, zoom = 8) %>%  if you want to preset the view/zoom default
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  

  
  # add scale bar
  addMeasure(position = "topright",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters",
             activeColor = "#3D535D",
             completedColor = "#7D4479") %>%
  
  
  # CDEC SNOW STATIONS
  addCircleMarkers(data=CA_fires, group="Fire_Duration",
                   lng= ~LONGITUDE, lat= ~LATITUDE, 
                   stroke=TRUE, weight=0.6,radius=2,
                   fillOpacity = 0.85, color="black",
                   fillColor= ~pal(Fire_Duration)) %>% # mapping to the color palette 
  
  # add controls for base maps and data
  addLayersControl(
    baseGroups = c("ESRI Aerial", "Topo"),
    overlayGroups = c("Fire_Duration"),
    options = layersControlOptions(collapsed = T))

m    #see if it worked!!!
get_fires_year


