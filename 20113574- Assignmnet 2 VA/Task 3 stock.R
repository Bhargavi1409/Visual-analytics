# *******************************************************
# SHINY APP TEMPLATE
# *******************************************************


# *******************************************************
# Load the required packages
# *******************************************************

library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)

# *******************************************************
# Load the required data
# *******************************************************
stock <- read.csv('big_stock_data.csv')
stock$date<-dmy(stock$date)

str(stock)
# *******************************************************
# Create the user interface (ui)
# ************************* ******************************


ui <- fluidPage(
  # Give the page a title
  titlePanel("Stock Market Share change over time"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      
      checkboxGroupInput(inputId = "company",
                         label = "Share Companies"
                         ,choices = unique(stock$company)
                         ,selected = "Apple"
      ),
      #hr(),
      helpText("choose different companies to compare the change over time "),
      
      # Radio buttons
      selectInput(inputId = "Metric", label = "Category", 
                  choices = colnames(stock[3:4]), selected = colnames(stock[3:4])[1]),
      helpText("choose one of the metrics compare the change over time "),
      #Date range
      
      dateRangeInput("daterange", "Select the date range:",
                     start = as.Date(min(stock$date), format = "%d/%m/%Y"),
                     end = as.Date(max(stock$date), format = "%d/%m/%Y"),
                     min = as.Date(min(stock$date), format = "%d/%m/%Y"),
                     max = as.Date(max(stock$date), format = "%d/%m/%Y")),
      
      # Submit button
      submitButton("Submit")
    ),
    # Create a spot for the plot
    mainPanel(
      plotOutput("stockPlot"),
      br(), br(),
      dataTableOutput(outputId = "table")
      #verbatimTextOutput("stats")
    )
  )
)

# *******************************************************
# Create the server function
# *******************************************************
server <- function(input, output)
{
  data_plot<-reactive({
    stock %>%
      filter( date >= input$daterange[1],
              date <= input$daterange[2],
              company %in% input$company) })
  
  
  
  
  output$stockPlot <- renderPlot({
    
    y_axis<-input$Metric
    ggplot(data = data_plot(), aes_string(x = 'date', y =y_axis, colour = 'company')) + geom_line() + theme_bw() + xlab('Date') + ylab(y_axis)
  })
  
  output$table<-renderDataTable({
    data_plot()
    })
      
 
    

  
}

# *******************************************************
# Run the App
# *******************************************************
shinyApp(ui, server)