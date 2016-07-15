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

UTM_zone <- function(m) {
  meanLON <- mean(m[,'LON']) + 180
  meanLAT <- mean(m[,'LAT'])
  n_zone <- as.character(ceiling(meanLON / 6))
  hemi <- ifelse(meanLAT < 0,'S','N')
  c(n_zone, hemi)
}

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
    longlat_coords <- rv$m
    crs_longlat <- CRS("+proj=longlat +datum=WGS84 +no_defs")
    utmz <- UTM_zone(longlat_coords)
    crs_utm <- paste0('+proj=utm +zone=', utmz[1], 
                      ifelse(utmz[2]=='S', ' +south', ''),
                      ' +datum=WGS84 +units=m +no_defs')
    # transform points coordinates to UTM zone (derived from central location of the set)
    relocs_wgs84 <- SpatialPoints(longlat_coords, proj4string=crs_longlat)
    withProgress(message = 'Transforming coordinates to UTM', value=0, {
      relocs <- spTransform(relocs_wgs84, crs_utm)
      mg <- makegrid(relocs, cellsize=100)
      coordinates(mg) <- ~x1+x2
      spixg <- SpatialPixels(mg)
      setProgress(value=0.2, message='Estimating UD')
      KUD <- kernelUD(relocs, grid=spixg, h="href")
      #VUD <- getvolumeUD(KUD)
      #Raster_VUD <- raster(VUD)
      setProgress(value=0.8, message='Estimating 95% home range from UD')
      #vIsolignes <- seq(50, 95, 5)
      #Shapefile_Isolignes <- rasterToContour(Raster_VUD,levels=vIsolignes)
      hrplyg <- getverticeshr(KUD)
      proj4string(hrplyg) <- crs_utm
      hrplyg2 <- spTransform(hrplyg, crs_longlat)
      setProgress(1)
    })
    
    proxy <- leafletProxy("carte")
    proxy %>% addPolygons(data=hrplyg2)
    
  })
   
  output$carte <- renderLeaflet({
    validate(need(dim(rv$m)[1] > 0, "Please choose a file"))
    leaflet(data=rv$m) %>%
      addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
  })
  
  output$nbpoints <- reactive({dim(rv$m)[1]})
  
})
