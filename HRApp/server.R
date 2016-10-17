#
# This is the server logic of a Shiny web application. 
#

library(shiny)
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(leaflet)

# determine UTM zone from mean lon & lat
UTM_zone <- function(m) {
  meanLON <- mean(m[,'LON']) + 180
  meanLAT <- mean(m[,'LAT'])
  n_zone <- as.character(ceiling(meanLON / 6))
  hemi <- ifelse(meanLAT < 0,'S','N')
  c(n_zone, hemi)
}

# define server logic 
shinyServer(function(input, output, clientData, session) {
  
  # reactive values :
  #   m = coordinates (class matrix)
  #   relocs = relocs in UTM (class SpatialPoints)
  #   hrplyg = HR polygon (class SpatialPolygon)
  # init with NULL
  rv <- reactiveValues(m = NULL, relocs = NULL, hrplyg = NULL)
  
  # session variables :
  #   crs_longlat = WGS84 CRS
  #   crs_utm = relocs UTM CRS
  #   kud = UD raster from relocs with h (class kernelUD)
  crs_longlat <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  crs_utm <- NULL
  kud <- NULL

  # sequence 1 : txt input file is provided
  # reinit reactive values (m, relocs, hrplyg)
  observeEvent(input$fichier1, {
    crs_utm <<- NULL
    kud <<- NULL
    in_file <- input$fichier1
    # input file 2 latlon matrix
    rv$m <- try({
      df <- read.table(in_file$datapath, header=T, sep="\t")
      m <- as.matrix(df[,c("LON","LAT")])
      mfilter <- (-180 <= m[,'LON'] & m[,'LON'] <= 180 & -90 <= m[,'LAT'] & m[,'LAT'] <= 90)
      m[mfilter,]
      })
    # latlon matrix to UTM SpatialPoints
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
    # (re)init HR polygon
    rv$hrplyg <- NULL
  })

  # sequence 1 bis : leaflet output rendering is reactive to input matrix
  # check if input matrix is valid and not null
  output$carte <- renderLeaflet({
    validate(need(!inherits(rv$m, "try-error"), "Parsing error. Please check input file."),
             need(try(!is.null(rv$m) && dim(rv$m)[1] > 0), "Empty data. Please select input file.")
    )
    leaflet(data=rv$m) %>%
      addTiles() %>% addCircleMarkers(radius=2, stroke=F, fillOpacity=0.5, fillColor="#909")
  })
  
  # sequence 1 ter : nbpoints + href rendering and h input filling is reactive to input matrix
  # check if input matrix is valid and not null
  # [nbpoints rendering make 'Estimate' button visible]
  output$nbpoints <- renderText({
    validate(need(!inherits(rv$m, "try-error"), "NA"),
             need(!is.null(rv$m), "NA")
    )
    dim(rv$m)[1]
  })
  
  # sequence 1 ter ... end
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

  # sequence 2 : KUD raster + HR polygon + leaflet output updating
  #   is reactive to 'Estimate' button clicking. 
  # we assume that relocs SpatialPoints is not null
  # (since 'Estimate' button is visible)
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
   
  
  # sequence 3 : HR polygon + leaflet output updating
  #   is reactive to 'Estimate' button clicking
  #   ...ONLY IF kud is not null !!
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
  
  # sequence 2/3 bis : HR area rendering
  #   is reactive to HR polygon computing
  # ['size' rendering make 'Download' button visible]
  output$size <- renderText({
    validate(need(!inherits(rv$hrplyg, "try-error"), "NA"),
             need(!is.null(rv$hrplyg), "NA")
    )
    hrplyg <- rv$hrplyg
    hrplyg$area
  })
  
  # sequence 4 : shapefile creation & transfer 
  #   is reactive to 'Download' button clicking
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
