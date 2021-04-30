install.packages("shiny")
library(shiny)
library(dplyr)
library(ggplot2)

setwd("~/Downloads/Wildrefires")
fires<-read.csv("Wildfires.csv")
CA_fires<- filter(fires, region == "california")

CA_fires_size<-CA_fires %>% 
  group_by(FIRE_SIZE_CLASS) %>%
  summarize(n = n())
CA_fires_size

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("California's Wildfire Class Sizes"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput("selectedclass", h4("Fire Size Class"), choices
                  =list("A", "B", "C", "D", "E", "F", "G"), selected = "A")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot", width = "500px",
                 height = "300px")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x<- filter(CA_fires, FIRE_SIZE_CLASS == input$fires)
    ggplot(x, aes(FIRE_SIZE_CLASS)) + geom_histogram()
    #hist(x, breaks = bins, col = "#75AADB", border = "white",
    #     xlab = "Waiting time to next eruption (in mins)",
    #     main = "Histogram of waiting times")
    
  })
}
shinyApp(ui, server)