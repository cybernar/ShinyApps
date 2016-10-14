#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

library(shiny)
library(leaflet)

# define UI for application that render dynamic map
shinyUI(fluidPage(
  
  # application title
  titlePanel("Home Range App 1.1"),
  
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
                       hr(),
                       numericInput('h', label='h = ', value=0, step=10),
                       actionButton(inputId = "go", label = "Estimate HR"),
                       hr(),
                       sliderInput(inputId="pud", label="% of UD in HR", min=50, max=95, value=90,step=5),
                       div('HR Size (km2) = ', textOutput('size',inline=TRUE))
                       )
    ),
    # leaflet map
    mainPanel(
      leafletOutput('carte', height=500),
      conditionalPanel(condition="output.size > 0", 
                       downloadButton('downloadSHP', 'Download'))
    )
  ),
  h3('About'),
  p('App created by Cyril Bernard, SIE, CEFE-CNRS.'),
  p('The data processing made in this app is based on the ', 
    strong(a(href='https://cran.r-project.org/web/packages/adehabitatHR/index.html','adehabitatHR')), 'package.',
    br(), 'Calenge, C. (2006) The package adehabitat for the R software:',
    'a tool for the analysis of space and habitat use by animals.',
    'Ecological Modelling, 197, 516-519'),
  h3('How to use'),
  p('Input text file must be tab separated.', 
    'It must contain 2 columns ', em('LON'), ' and ', em('LAT'), 
    ' with WGS84 coordinates in decimal degrees (with \'.\' as decimal sepator).',
    'You can find an example of input file here : ', a(href='test_IT117.txt', 'test_IT117.txt')),
  p('Set',strong('h parameter'),'(default href), and click ',strong('Estimate'), 'for UD and HR estimation.'),
  p('You can change the',strong('%UD of the home range'), '(default 90) with the slider, before or after clicking', strong('Estimate.')),
  p('The',strong('download'),'button allows you to save the HR polygon as a shapefile.')
))
