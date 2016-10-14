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
shinyServer(function(input, output, clientData, session) {
  
  # reactive values :
  #   m = coordinates (class matrix)
  #   relocs = relocs in UTM (class SpatialPoints)
  # init with NULL
  rv <- reactiveValues(m = NULL, relocs = NULL, hrplyg = NULL)
  
  crs_longlat <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  crs_utm <- NULL
  kud <- NULL

  observeEvent(input$fichier1, {
    crs_utm <<- NULL
    kud <<- NULL
    in_file <- input$fichier1
    rv$m <- try({
      df <- read.table(in_file$datapath, header=T, sep="\t")
      m <- as.matrix(df[,c("LON","LAT")])
      mfilter <- (-180 <= m[,'LON'] & m[,'LON'] <= 180 & -90 <= m[,'LAT'] & m[,'LAT'] <= 90)
      m[mfilter,]
      })
    rv$relocs <- try({
      relocs <- NULL
      if (dim(rv$m)[1] > 0) {
        # transform points coordinates to UTM zone (derived from central location of the set)
        utmz <- UTM_zone(rv$m)
        withProgress(message = 'Transforming coords to UTM', value=0, {
          crs_utm <<- paste0('+proj=utm +zone=', utmz[1], 
                             ifelse(utmz[2]=='S', ' +south', ''),
                             ' +datum=WGS84 +units=m +no_defs')
          relocs_wgs84 <- SpatialPoints(rv$m, proj4string=crs_longlat)
          relocs <- spTransform(relocs_wgs84, crs_utm)
          setProgress(value=1)
        })
      }
      relocs
    })
    rv$hrplyg <- NULL
  })

  observeEvent(input$go, {
    withProgress(message = 'Estimating UD', value=0, {
      kud <<- kernelUD(rv$relocs, grid=200, h=input$h)
      setProgress(value=0.8, message=paste0('Getting HR from ',input$pud,'% UD'))
      hrplyg <- getverticeshr(kud, percent=input$pud, unout='km2')
      proj4string(hrplyg) <- crs_utm
      hrplyg2 <- spTransform(hrplyg, crs_longlat)
      setProgress(1)
    })
    rv$hrplyg <- hrplyg
    proxy <- leafletProxy("carte")
    proxy %>% addPolygons(layerId="HR", data=hrplyg2, color="#303", opacity=0.8, weight=2, fillColor="#303")
  })
   
  output$carte <- renderLeaflet({
    validate(need(!inherits(rv$m, "try-error"), "Parsing error. Please check input file."),
             need(try(!is.null(rv$m) && dim(rv$m)[1] > 0), "Empty data. Please select input file.")
             )
    leaflet(data=rv$m) %>%
      addTiles() %>% addCircleMarkers(radius=2, stroke=F, fillOpacity=0.5, fillColor="#909")
  })
  
  output$nbpoints <- renderText({
    validate(need(!inherits(rv$m, "try-error"), "NA"),
             need(!is.null(rv$m), "NA")
    )
    dim(rv$m)[1]
    })

  output$href <- renderText({
    validate(need(!inherits(rv$relocs, "try-error"), "NA"),
             need(!is.null(rv$relocs), "NA")
    )
    coords_UTM <- slot(rv$relocs,"coords")
    n <- dim(coords_UTM)[1]
    if (n > 0) {
      sigma <- sqrt(0.5 * (var(coords_UTM[,1]) + var(coords_UTM[,2])))
      href <- sigma * n ^ (-1/6)
      updateNumericInput(session, 'h', label='h = ', value=round(href,0))
      paste("href", "=", round(href,1))
    } else {
      updateNumericInput(session, 'h', label='h = ', value=NA)
      "href = NA"
    }
  })
  
  output$size <- renderText({
    validate(need(!inherits(rv$hrplyg, "try-error"), "NA"),
             need(!is.null(rv$hrplyg), "NA")
    )
    hrplyg <- rv$hrplyg
    hrplyg$area
  })

  observeEvent (input$pud, {
    if (!is.null(kud)) {
      withProgress(value=0, message=paste0('Getting HR from ',input$pud,'% UD'), {
        hrplyg <- getverticeshr(kud, percent=input$pud, unout='km2')
        proj4string(hrplyg) <- crs_utm
        hrplyg2 <- spTransform(hrplyg, crs_longlat)
        setProgress(1)
      })
      rv$hrplyg <- hrplyg
      proxy <- leafletProxy("carte")
      proxy %>% addPolygons(layerId="HR", data=hrplyg2, color="#303", opacity=0.8, weight=2, fillColor="#303")
    }
  })
  
  output$downloadSHP <- downloadHandler(
    filename = function() { 
      paste('export_shiny_UD',input$pud,'.zip', sep='') 
    },
    content = function(fname) {
      tmpdir <- tempdir()
      shpbasename <- paste0('export_shiny_UD',input$pud)
      setwd(tempdir())
      writeOGR(rv$hrplyg, tmpdir, shpbasename,'ESRI Shapefile',overwrite_layer=T)
      zip(zipfile=fname, files=list.files(tmpdir,pattern=paste0("^",shpbasename,"\\.")))
    },
    contentType = "application/zip"
  )
  
  
})
