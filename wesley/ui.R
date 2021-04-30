#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#install.packages("shinythemes")
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cosmo"),

    # Application title
    titlePanel("Southern California Property Evaluation"),
    
    navbarPage("Menu",
              # tabPanel("Welcome"),
               tabPanel("Summary", 
                        # sidebarLayout(
                        #     sidebarPanel(
                        #         # sliderInput("bins",
                        #         #             "Number of bins:",
                        #         #             min = 1,
                        #         #             max = 50,
                        #         #             value = 30),
                        # 
                        #         
                        #     ),
                            mainPanel(
                                #plotOutput("evalDist"), 
                                verbatimTextOutput("summary"))),
               navbarMenu("County",
                          tabPanel("Orange County",
                                   titlePanel("Orange County Linear Regression Model"),
                                   sidebarLayout(
                                       sidebarPanel(
                                   titlePanel("Select desired choice for each variable"),
                                   numericInput(inputId = "O_airconditioningtypeid",
                                                "Air Conditioning", value =0),
                                   numericInput(inputId  = "O_bathroomcnt",
                                                "Bathroom Count", value =1),
                                   numericInput(inputId = "O_bedroomcnt",
                                                "Bedroom Count", value = 1),
                                   numericInput(inputId = "O_calculatedfinishedsquarefeet",
                                                "Square Feet", value = 0),
                                   numericInput(inputId = "O_garagecarcnt",
                                                "Carage Car Capacity", value = 0),
                                   numericInput(inputId = "O_yearbuilt",
                                                "Year Built", min = 1900, max = 2017, value = 2015),
                                   numericInput(inputId = "O_numberofstories",
                                                "Number of Stories", min = 1, max = 2, value = 1),
                                   numericInput(inputId = "O_cluster",
                                                "Region", min = 1, max = 4, value = 1),
                                   
                                   textOutput(outputId = "O_taxvaluedollarcnt")),
                                   
                                   mainPanel(
                                       # textOutput("orangeOut"),
                                       # verbatimTextOutput("O_summary")
                                   ))
                          ),
                          tabPanel("Ventura County",
                                   titlePanel("Ventura County Linear Regression Model"),
                                   
                             sidebarLayout(
                                   sidebarPanel(
                                   titlePanel("Select desired choice for each variable"),
                                   sliderInput(inputId = "V_basementsqft",
                                               "Basement Square Feet",min = 0, max = 4122,  value = 0),
                                   numericInput(inputId  = "V_bathroomcnt",
                                                "Bathroom Count",value = 1),
                                   numericInput( inputId = "V_bedroomcnt",
                                                 "Bedroom Count", value = 1),
                                   sliderInput(inputId = "V_calculatedfinishedsquarefeet",
                                               "Square Feet", min = 102, max = 19061, value = 102),
                                   numericInput(inputId = "V_fireplacecnt",
                                                "Fireplace count ", value = 0),
                                   numericInput(inputId = "V_garagecarcnt",
                                                "Garage Car Capacity", value = 0),
                                   sliderInput(inputId = "V_garagetotalsqft",
                                               "Garage Sqaure Feet", min = 0, max = 7749, value = 0),
                                   sliderInput(inputId = "V_lotsizesquarefeet",
                                               "Lot Size", min = 0, max = 1000000, value = 0),
                                   selectInput(inputId = "V_poolcnt",
                                               "Pool", choices = list(0, 1), selected = 0),
                                   selectInput(inputId = "V_yardbuildingsqft17",
                                               "Patio in Yard", choices = list(0, 1), selected = 0),
                                   numericInput(inputId = "V_yearbuilt",
                                                "Year Built", min = 1880, max = 2016, value = 1880),
                                   radioButtons(inputId = "V_numberofstories",
                                                "Number of Stories", choices = list(1, 2, 3), selected = 1),
                                   radioButtons(inputId = "V_assessmentyear",
                                                "Assessment Year", choices = list(2015, 2016), selected = 2015),
                                   selectInput(inputId = "V_cluster",
                                               "Region", choices = list(1, 2, 3, 4), selected = 1),
                                   
                                   textOutput(outputId = "V_taxvaluedollarcnt")
                                   
                                   ),
                                   
                                   mainPanel(
                                       #verbatimTextOutput("V_summary")
                                   )
                          )),
                          tabPanel("Los Angeles County",
                                   titlePanel("Los Angeles County Linear Regression Model"),
                                   
                                   sidebarLayout(
                                   sidebarPanel("Select desired choice for each variable",
                                   numericInput(inputId = "LA_bedroomcnt",
                                                "Bedroom Count", value =0),
                                   numericInput(inputId  = "LA_bathroomcnt",
                                                "Bathroom Count", value =1),
                                   numericInput(inputId = "LA_calculatedfinishedsquarefeet",
                                                "Square feet", value = 1000),
                                   numericInput(inputId = "LA_garagecarcnt",
                                                "Garage Vehicle Capacity", value = 1),
                                   textOutput(outputId = "LA_taxvaluedollarcnt")),
                                   mainPanel(
                                       # titlePanel("Your Estimated Property Value:"),
                                       # #textOutput(outputId = "LA_taxvaluedollarcnt"),
                                       # textOutput(outputId = "LA_taxvaluedollarcnt"),
                                       # verbatimTextOutput("LA_summary")
                                   )
                          )))
              

    
    
)))
