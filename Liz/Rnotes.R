library(datasets) #loads a package 

#importing data from a spesadsheet (.csv or excel)
install.packages("pacman")
#load contributing packages w pacman 
pacman::p_load(pacman, tidyverse)

#df<-import("Wildrefires/Wildfires.R") %>%
#  as_tibble() %>%

#######load data and make a bar plot and colors 
x = c(24,17,90,22,56)
barplot(x)
colors() #will output in console all the colors alphabetically
barplot(x, col = "red3") #makes red --- col = color
barplot(x, col = "green") #makes green
barplot(x, col = "643") #to use the index code
###to make the different columns different colors
barplot(x, col = 1:6)
barplot(x, col = rainbow(6)) ##makes it rainbow
barplot(x, col = terrain.colors(6)) ###for terrain
barplot(x, col = cm.colors(6)) ##cute blue and pink colors j fun

####barchart visualization 
diamonds ## dataset included w R
plot(diamonds$cut) ##fastest easiest plot 

##^^ same thing w pipe
diamonds %>%
  select(color) %>%
  plot()

###create a table and do it!
diamonds %>%
  select(clarity) %>%
  table() %>% #format's the data correclty for the barplot 
  barplot()

##sort the data in decreasing order
diamonds %>%
  select(clarity) %>%
  table() %>%
  sort(decreasing = T) %>% #sorts table in decreasing order
  barplot()

###add options like title and axis 
diamonds %>%
  select(clarity) %>%
  table() %>%
  barplot(
    main = "Clarity of Diamonds",
    sub = "(Source: ggplot2::diamonds",
    horiz = T, #draws  horizontal bar 
    ylab = "Clarity of Diamonds",
    xlab = "Frequency",
    xlim = c(0, 15000),
    border = NA, #no border
    col = "red" #sets color 
  )

##step 1) create a stacked bar 
df <- diamonds %>%
  select(color, clarity) %>%
  table() %>%
  print() ##shows the table in console 

df %>%
  barplot(legend = rownames(.)) ##draws the plot w legend ##dot means to take the df info and 
  

###########HISTAGRAMS###################
pacman::p_load(pacmen, tidyverse)

diamonds
hist(diamonds$price) ##hist of diamonds w the price variable 

hist(diamonds$price,
     breaks = 7, #number of breaks 
     main = "Histagram of Price of Diamonds",
     sub = "(Source: ggplot2::diamonds)",
     ylab = "Frequency",
     xlab = "Price",
     border = T,     ##gives a border 
     col = "slateblue"
)
##########################################

##################BOXPLOT##################
pacman::p_load(pacmen, tidyverse)

diamonds
plot

boxplot(diamonds$price) #gives a default boxplot

##does same thing as above 
diamonds%>%
  select(price)%>%
  boxplot()

##more options
diamonds%>%
  select(price)%>% #numeric 
  boxplot(
    horizontal = T,  #horizontal 
    notch = T,       #confidence interval of median 
    main = "Boxplot of Price of Diamonds",
    sub = "(Source ggplot2::diamonds)",
    xlab = "Price of Diamonds",
    col = "orchid"   ##color 
)

##compare groups or variables on the same scale
diamonds%>%
  select(color, price)%>%  ##price increases as color chnages 
  plot()

##another option 
diamonds%>%
  select(color, price)%>%  ##price increases as color changes 
  boxplot(
    price~color, #tilde indicates a formula 
    data = .,
    col = "purple"
  )
###############################
##########SCATTER PLOT#########
pacman::p_load(pacmen, rio, tidyverse)
#load data into a df
setwd("/Users/elizabethlyon/Downloads/Wildrefires/")
df <- read.csv("Wildfires.csv")%>%
  as_tibble() %>%
  select(FIRE_NAME,
         FIRE_YEAR,
         FIRE_SIZE,
         STATE)
print()

setwd("/Users/elizabethlyon/Downloads/Wildrefires/")
df <- read.csv("Wildfires.csv")%>%
  
df %>%
  select(DISCOVERY_DOY:DISCOVERY_TIME)%>%
  plot()
    # main = "Scatterplot of fires by lat & long",
    # xlab = "DISCOVERY_DOY",
    # ylab = "DISCOVERY_TIME",
    # col = "green",
    # pch = 20, #plots a circle character filled in green 
  
  
##add regression line 
df %>%
  lm(LATITUDE~LONGITUDE, data = .)%>% #latitude is a formula 
  abline() #specifies the line you add to something; puts it ontop 

####LINE CHARTS#################
library(datasets) #loads a package 
#load contributing packages w pacman 
pacman::p_load(pacman, tidyverse)

setwd("/Users/elizabethlyon/Downloads/Wildrefires/")
df <- read.csv("Wildfires.csv")%>%
df %>%
  as_tibble()%>%
  select(FIRE_YEAR)%>%
  plot()
  

#########CLEAN UP###############

#iris data
iris #description of iris flowers 

UCBAdmissions #another data set that loadsw r ab UCBA

#titanic -- who survived the titanic sink
Titanic 

#swiss -- historic data
swiss

#x <- c(1,2,3,5,9) #concatenates and creates a list 
#x

#sequential data 
# 0:10      #0 - 10

#math
#sqrt(64)
#log(100)   #natural log
#log10(100) #log base 10