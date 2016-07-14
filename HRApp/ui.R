#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

library(shiny)
library(leaflet)

# define UI for application that render dynamic map
shinyUI(fluidPage(
  
  # application title
  titlePanel("Home Range App"),
  
  # sidebar with file input 
  sidebarLayout(
    sidebarPanel(
      p('The calculation made in this app is based on the ', 
        strong('adehabitatHR'), ' package.'),
      p('Input text file must be tab separated.', br(), 
        'It must contain 2 columns ', em('LON'), ' and ', em('LAT'), 
        ' with WGS84 coordinates in decimal degrees.'),
      p('Find an example of input file ', a(href='test_IT117.txt', 'here'),'.'),
      fileInput('fichier1', label='Choose a text file :', accept=c('text/plain','text/tab-separated-values'))
    ),
    # leaflet map
    mainPanel(
      leafletOutput('carte')
    )
  )
))
