install.packages("shiny")

#load the libraries we need for this actual task
library(ggplot2)
library(shiny)
library(dbplyr)
library(dplyr)

setwd("E:/SynologyDrive/Documents/School/Chapman/Spring 2021/SCI 200")
fires<-read.csv("Wildfires(2).csv")
CA_fires<- filter(fires, region == "california")
CA_fires<-filter(fires,FIRE_SIZE_CLASS > 'D')

CA_fires_by_year<-CA_fires %>% 
  group_by(FIRE_YEAR) %>%
  summarize(n_fires = n()) 
CA_fires_by_year

ui <- fluidPage(
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="year",
                  label="Year:",
                  min=1992,
                  max=2014,
                  value=1992)
    ),
    mainPanel(
      plotOutput(outputId="plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    data <- filter(CA_fires, FIRE_YEAR == input$year)
    ggplot(data, aes(FIRE_SIZE)) + geom_histogram()
  })
}

shinyApp(ui = ui, server = server)