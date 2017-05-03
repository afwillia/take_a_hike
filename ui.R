# ui.R
# 11/25/2016
# create ui for hiking shiny app
# 
# have a bar to choose date, origin lon and lat
# show sunset time for chosen date
# show hikes doable within that time
#
# ------------------------------------
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Take a hike!"),
  
  # Choose a date
  fluidRow(
    column(3, 
           dateInput("date", 
                     label = h5("What day are you hiking?"), 
                     value = Sys.Date())
     ),
    column(3, 
           textInput("time_leaving", 
                     label = h5("What time are you leaving (in 24-hr format)"), 
                     value = '13:00')
     ),
    column(3,
           textInput("lat",
                     label= h5("Start Latitude"),
                     value=47.64)
     ),
    column(3,
           textInput("lon",
                     label= h5("Start Longitude"),
                     value=-122.322)
     )
  ),
  
  fluidRow(
    column(8,
          leafletOutput("map", height = 400)
          )
  ),
  
  mainPanel(
    textOutput('sunset'),
    textOutput('time')
    
  )
  
))