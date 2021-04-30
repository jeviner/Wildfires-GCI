library(leaflet)

CA_fires["Fire_Duration"] <- CA_fires$CONT_DATE - CA_fires$DISCOVERY_DATE
# add color palette
pal <- colorNumeric(
  palette = "RdBu",
  #palette = colorRamp(c("#000000", "#FFFFFF"), interpolate = "spline"),
  domain = CA_fires$Fire_Duration
)

CA_fires_yr <- filter(CA_fires, FIRE_YEAR == 2010, Fire_Duration > 0, STAT_CAUSE_DESCR != "Arson")
head(CA_fires)
head(CA_fires_yr)

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
  addCircleMarkers(data=CA_fires_yr, group="Fire_Duration",
                   lng= ~LONGITUDE, lat= ~LATITUDE, 
                   stroke=TRUE, weight=0.6,radius=8,
                   fillOpacity = 0.85, color="black",
                   fillColor= ~pal(Fire_Duration)) %>% # mapping to the color palette 
  
  # add controls for basemaps and data
  addLayersControl(
    baseGroups = c("ESRI Aerial", "Topo"),
    overlayGroups = c("Fire_Duration"),
    options = layersControlOptions(collapsed = T))

m    #see if it worked!!!

