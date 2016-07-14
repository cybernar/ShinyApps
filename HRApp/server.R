#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#

library(shiny)
library(leaflet)

# Define server logic required to render a dynamic map
shinyServer(function(input, output) {
  
  data <- reactive({
    inFile <- input$fichier1
    validate(need(!is.null(input$fichier1), "Please choose a file"))
    df <- read.table(inFile$datapath, header=T, sep="\t")
    #validate(identical(c('LON','LAT') %in% colnames(df), c(T,T)), "Input file must have LON and LAT columns")
    as.matrix(df[,c("LON","LAT")])
  })
   
  output$carte <- renderLeaflet({
    m <- data()
    leaflet(data=m) %>%
      addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
  })
  
})
