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
  
  # reactive values :
  #   m = coordinates matrix
  #   rkud = UD raster
  # init with NULL
  rv <- reactiveValues(m = NULL, kud = NULL)
  observeEvent(input$fichier1, {
    in_file <- input$fichier1
    rv$m <- try({
      df <- read.table(in_file$datapath, header=T, sep="\t")
      m <- as.matrix(df[,c("LON","LAT")])
      mfilter <- (-180 <= m[,'LON'] & m[,'LON'] <= 180 & -90 <= m[,'LAT'] & m[,'LAT'] <= 90)
      m[mfilter,]
      })
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
      rv$kud <- kernelUD(relocs, grid=spixg, h="href")
      setProgress(value=0.8, message='Estimating 95% home range from UD')
      hrplyg <- getverticeshr(rv$kud)
      proj4string(hrplyg) <- crs_utm
      hrplyg2 <- spTransform(hrplyg, crs_longlat)
      setProgress(1)
    })
    
    proxy <- leafletProxy("carte")
    proxy %>% addPolygons(data=hrplyg2)
    
  })
   
  output$carte <- renderLeaflet({
    validate(need(!inherits(rv$m, "try-error"), "Parsing error. Please check input file."),
             need(!is.null(rv$m), "Empty data. Please select input file.")
             )
    leaflet(data=rv$m) %>%
      addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
  })
  
  output$nbpoints <- renderText({
    validate(need(!inherits(rv$m, "try-error"), "Empty"),
             need(!is.null(rv$m), "Empty")
    )
    dim(rv$m)[1]
    })

  output$href <- renderText({
    validate(need(!inherits(rv$m, "try-error"), "Empty"),
             need(!is.null(rv$m), "Empty")
    )
    if (dim(rv$m)[1] > 0) {
      "href = dummy"
    } else {
      "href = NA"
    }
  })
  
})
