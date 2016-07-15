#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

library(shiny)
library(leaflet)

# define UI for application that render dynamic map
shinyUI(fluidPage(
  
  # application title
  titlePanel("Home Range App v1"),
  
  # sidebar with file input 
  sidebarLayout(
    sidebarPanel(
      fileInput(
        'fichier1', 
        label='Select input text file :', 
        accept=c('text/plain','text/tab-separated-values')
      ),
      textOutput('nbpoints'),
      conditionalPanel(condition="output.nbpoints", 
                       actionButton(inputId = "go", label = "Compute"))
    ),
    # leaflet map
    mainPanel(
      leafletOutput('carte')
    )
  ),
  h3('About'),
  p('The calculation made in this app is based on the ', 
    strong('adehabitatHR'), ' package.'),
  p('Input text file must be tab separated.', 
    'It must contain 2 columns ', em('LON'), ' and ', em('LAT'), 
    ' with WGS84 coordinates in decimal degrees.'),
  p('Find an example of input file ', a(href='test_IT117.txt', 'here'),'.')
  
))
