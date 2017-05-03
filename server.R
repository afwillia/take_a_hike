# server.R
# 11/25/2016
# Shiny server side for plotting hikes on map with information about hike length
#
#
# TO DO:
#   Add sliders/menu for choosing sort and display options
#   Add option to select how long you want to "hang out" at hike
# -------------------------------------------------

library(shiny)
library(leaflet)
library(lubridate)

load("data/hikes_times.Rda") # for hikes_times for map


# Define server logic required to return the sunset at a particular day
shinyServer(function(input, output) {
  
    # take day and retrieve time of sunset
    output$sunset <- renderText({
      this <- maptools::sunriset(matrix(c(as.numeric(input$lon), as.numeric(input$lat)),nrow=1), 
                                   as.POSIXct(input$date), POSIXct.out=TRUE,direction='sunset')$time
      time <- strftime(this, tz=Sys.timezone(),format='%H:%M:%S')
      paste0("The sun will set at ", as.POSIXlt(this)$hour,':',
             as.POSIXlt(this)$min, " that day.")
    
    })
    
    output$time<- renderText({
      leave <- as.POSIXct(input$time_leaving, format='%H:%M')
      leave <- as.POSIXlt(leave)$hour + as.POSIXlt(leave)$min/60
      
      this <- maptools::sunriset(matrix(c(as.numeric(input$lon), as.numeric(input$lat)),nrow=1), 
                                 as.POSIXct(input$date), POSIXct.out=TRUE,direction='sunset')$time
      this <- as.POSIXlt(this)$hour + as.POSIXlt(this)$min/60
      
      paste0('You have ', this - leave , ' hours before sunset.')
      
    })
  
    # plot hikes on map
    output$map <- renderLeaflet({
      leave <- as.POSIXct(input$time_leaving, format='%H:%M')
      leave <- as.POSIXlt(leave)$hour + as.POSIXlt(leave)$min/60
      # fix this
      this <- maptools::sunriset(matrix(c(as.numeric(input$lon), as.numeric(input$lat)),nrow=1), 
                                 as.POSIXct(input$date), POSIXct.out=TRUE,direction='sunset')$time
      this <- as.POSIXlt(this)$hour + as.POSIXlt(this)$min/60
      
      hike_in_time <- subset(hikes_times, total_time + leave < this)
      hike_in_time <- hike_in_time[order(hike_in_time$time_naismith, decreasing=TRUE),]
      five_hikes <- head(hike_in_time,10)
    # just using leaflet's usage example
    # center map on origin and add markers containing hike name and time to drive and complete hike
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng=input$lon, lat=input$lat, zoom=7) %>%
      addMarkers(lng=five_hikes$cord_long, lat=five_hikes$cord_lat, 
                 popup=paste0(five_hikes$name," : ", five_hikes$total_time, " hours total")) %>%
      addCircleMarkers(lng=as.numeric(input$lon), lat=as.numeric(input$lat), popup="Origin", radius=10)
    m  # Print the map
  
  })
})
