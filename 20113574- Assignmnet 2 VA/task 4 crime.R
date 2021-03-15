# *******************************************************
# SHINY APP TEMPLATE
# *******************************************************
#install.packages("sf")

# *******************************************************
# Load the required packages
# *******************************************************

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggmap)

# *******************************************************
# Load the required data
# *******************************************************
crime <- read.csv('crime_data.csv')

crime_fil <- filter(crime, offense!= "theft") %>%
  select(time,date, offense, address, lon, lat)
crime_fil$date<-mdy(crime_fil$date)


str(crime_fil)
# *******************************************************
# Create the user interface (ui)
# *******************************************************


ui <- fluidPage(
  # Give the page a title
  titlePanel("Houston Crime rate over time"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      
      checkboxGroupInput(inputId = "offense",
                         label = "Select the offence type"
                         ,choices = unique(crime_fil$offense)
                         ,selected = "robbery"),
      #hr(),
      helpText("choose different offense type to compare their rate over time "),
      
      #DAte range
      
      dateRangeInput("daterange", "Select the date range:",
                     start = as.Date(min(crime_fil$date), format = "%m/%d/%Y"),
                     end = as.Date(max(crime_fil$date), format = "%m/%d/%Y"),
                     min = as.Date(min(crime_fil$date), format = "%m/%d/%Y"),
                     max = as.Date(max(crime_fil$date), format = "%m/%d/%Y")),
      
    ),
      mainPanel(
        plotOutput("mapPlot",width=650, height=600),
      
    )))

server <- function(input, output)
{
  data_plot <- reactive({
    crime_fil %>%
      filter( date >= input$daterange[1],
              date <= input$daterange[2],
              offense %in% input$offense) })
  
  
  output$mapPlot <- renderPlot({
    bbox = c(left=-95.8, bottom=29.4, right=-95.0, top=30.0)
    map <- get_stamenmap(bbox, zoom = 10, source="stamen")
    ggmap(map) + stat_density2d(data = data_plot(),
                                aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon') +
      scale_fill_gradient(low = "green", high = "red") +
      scale_alpha_continuous(range = c(0, 0.8)) +
      geom_point(data = data_plot(),
                 aes(x = lon, y = lat,color = offense), size = 1) +scale_color_brewer(palette="Dark2")+
      guides(fill = F, alpha = FALSE) +
      ggtitle('Crime in Houston TX',
              subtitle = 'Murder concentration in the city')
  })
  
  
  }



# *******************************************************
# Run the App
# *******************************************************
shinyApp(ui, server)
