# Model Selector Shiny App

library(shiny)
library(googlesheets)
library(DT)
library(dplyr)
library(datasets)
library(condformat)
suppressMessages(library(dplyr))

#################################################
# Google Sheet Preparation

# authenticate browser and check google sheets
gs_ls()

# get google sheet
outputsheet <- gs_title("output")

# list all worksheets in google sheet
print(gs_ws_ls(outputsheet))

#################################################
# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Model Selector"),
  
  # Sidebar with a dropdown for selecting the region 
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Region:", choices=gs_ws_ls(outputsheet)),
      hr(),
      helpText("Select a region to evaluate its models.")
      ######
    ),
    
    # Show a table of statistical values
    mainPanel(
      dataTableOutput("x1Table")
      #cat(file=stderr(), "main panel successful", "\n")
    )
  )
)

#################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
  #  outputsheet <- gs_title("output")
  worksheet <- reactive({
    input$region
  })
  
  output$x1Table <- renderDataTable({
    region_data <- gs_read(outputsheet, ws = worksheet())
    datatable(t(region_data))
  })
  
  #cat(file=stderr(), "entered server", "\n")
  #selectedRegion <- reactive(input$region)
  #  regionalData <- reactive({
  #    gs_read(outputsheet, ws = input$region)
  #  })
  #cat(file=stderr(), "data read", "\n")
  #  output$x1Table <- renderDataTable({
  #cat(file=stderr(), "entered render", "\n")
  #    datatable(regionalData)
  #cat(file=stderr(), "successful datatable render", "\n")
  #datatable(gs_read_csv(outputsheet, ws = input$region))
  #  })
  
}

#################################################
# Run the application 
shinyApp(ui = ui, server = server)