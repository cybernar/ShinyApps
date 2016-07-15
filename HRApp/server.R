#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#

library(shiny)
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(leaflet)

# Define server logic required to render a dynamic map
shinyServer(function(input, output) {
  
  # create empty matrix
  rv <- reactiveValues(m = matrix(nrow = 0,ncol=2),l = NULL)
    
  # init session object matcoords <- NULL
  observeEvent(input$fichier1, {
    inFile <- input$fichier1
    #validate(need(!is.null(inFile), "Please choose a file"))
    df <- read.table(inFile$datapath, header=T, sep="\t")
    #validate(identical(c('LON','LAT') %in% colnames(df), c(T,T)), "Input file must have LON and LAT columns")
    rv$m <- as.matrix(df[,c("LON","LAT")])
  })
  
  observeEvent(input$go, {
    relocs_wgs84 <- SpatialPoints(rv$m,proj4string=CRS("+init=EPSG:4326"))
    withProgress(message = 'Home Range computing', value = 0, {
      relocs <- spTransform(relocs_wgs84, CRS("+init=EPSG:32631"))
      mg <- makegrid(relocs, cellsize=100)
      coordinates(mg) <- ~x1+x2
      spixg <- SpatialPixels(mg)
      KUD <- kernelUD(relocs, grid=spixg, h="href")
      VUD <- getvolumeUD(KUD)
      Raster_VUD <- raster(VUD)
      vIsolignes <- seq(50, 95, 5)
      Shapefile_Isolignes <- rasterToContour(Raster_VUD,levels=vIsolignes)
      proj4string(Shapefile_Isolignes) <- CRS("+init=EPSG:32631")
      Shapefile_Isolignes2 <- spTransform(Shapefile_Isolignes, CRS("+init=EPSG:4326"))
      setProgress(1)
    })
    
    proxy <- leafletProxy("carte")
    proxy %>% addPolylines(data=Shapefile_Isolignes2)
    
  })
   
  output$carte <- renderLeaflet({
    validate(need(dim(rv$m)[1] > 0, "Please choose a file"))
    leaflet(data=rv$m) %>%
      addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
  })
  
  output$nbpoints <- reactive({dim(rv$m)[1]})
  
})
