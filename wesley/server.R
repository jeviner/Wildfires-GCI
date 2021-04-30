#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic 
shinyServer(function(input, output) {

    
    output$summary <- renderText(summary(properties))
    
    # output$evalDist <- renderPlot({
    #     x <- properties$taxvaluedollarcnt
    #     bins <- seq(min(x), 2*median(x), length.out = input$bins + 1)
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
        
    #Orange model---
    output$O_summary <- renderText(summary(orange))
    output$O_taxvaluedollarcnt <- renderText({
        userDataOrange <- data.frame(airconditioningtypeid = input$O_airconditioningtypeid,
                               bathroomcnt = input$O_bathroomcnt,
                               bedroomcnt = input$O_bedroomcnt,
                               calculatedfinishedsquarefeet = input$O_calculatedfinishedsquarefeet,
                               garagecarcnt = input$O_garagecarcnt,
                               yearbuilt = input$O_yearbuilt,
                               numberofstories = input$O_numberofstories,
                               cluster = input$O_cluster)
        orange_predict <- predict(orange_final_model, userDataOrange)    
        
        
        
    })
    
    
    #Ventura model---
    output$V_summary <- renderText(summary(Ventura))
    output$V_taxvaluedollarcnt <- renderText({
        userDataVentura <- data.frame(basementsqft = input$V_basementsqft,
                               bathroomcnt = input$V_bathroomcnt,
                               bedroomcnt = input$V_bedroomcnt,
                               calculatedfinishedsquarefeet = input$V_calculatedfinishedsquarefeet,
                               fireplacecnt = input$V_fireplacecnt,
                               garagecarcnt = input$V_garagecarcnt,
                               garagetotalsqft = input$V_garagetotalsqft,
                               lotsizesquarefeet = input$V_lotsizesquarefeet,
                               poolcnt = as.numeric(input$V_poolcnt),
                               yardbuildingsqft17 = as.numeric(input$V_yardbuildingsqft17),
                               yearbuilt = input$V_yearbuilt,
                               numberofstories = as.numeric(input$V_numberofstories),
                               assessmentyear = as.numeric(input$V_assessmentyear),
                               cluster = as.numeric(input$V_cluster))
        ventura_predict <- predict(ventura_final_model, userDataVentura)    
        
    })
    output$V_bath <- renderPlot(V_bathBarPlot)
    
    
    #LA model-----
    output$LA_summary <- renderText(summary(la1))
    output$LA_taxvaluedollarcnt <- renderText({
        userDataLA <- data.frame(bathroomcnt = input$LA_bathroomcnt,
                               bedroomcnt = input$LA_bedroomcnt,
                               calculatedfinishedsquarefeet = input$LA_calculatedfinishedsquarefeet,
                               garagecarcnt = input$LA_garagecarcnt)
        LA_predict <- predict(la_final_model, userDataLA)    
    }
    )
    
    
    
    
    # output$preImage <- renderImage({
    #     # When input$n is 3, filename is ./images/image3.jpeg
    #     filename <- "OrangeCountyClusterPlot.png"
    #     
    #     # Return a list containing the filename and alt text
    #     list(src = filename, contentType = "png")
    #     
    # }, deleteFile = FALSE)
#process page ----
    
    

})
