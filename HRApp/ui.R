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
      div('N points = ', textOutput('nbpoints',inline=TRUE)),
      conditionalPanel(condition="output.nbpoints > 0", 
                       textOutput('href'), 
                       numericInput('h', label='h = ', value=0),
                       actionButton(inputId = "go", label = "Estimate HR"))
    ),
    # leaflet map
    mainPanel(
      leafletOutput('carte', height=500),
      conditionalPanel(condition="output.nbpoints > 0", 
                       sliderInput(inputId="pud", label="% of UD", min=50, max=95, value=90,step=5))
    )
  ),
  h3('About'),
  p('The calculation made in this app is based on the ', 
    strong('adehabitatHR'), ' package.'),
  h3('How to use'),
  p('Input text file must be tab separated.', 
    'It must contain 2 columns ', em('LON'), ' and ', em('LAT'), 
    ' with WGS84 coordinates in decimal degrees.'),
  p('Find an example of input file ', a(href='test_IT117.txt', 'here'),'.')
  
))
