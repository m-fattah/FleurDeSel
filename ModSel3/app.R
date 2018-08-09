# Model Selector Shiny App

library(shiny)
library(googlesheets)
library(DT)
library(dplyr)
library(datasets)
library(gplots)
library(plotly)
library(heatmaply)
library(shinyHeatmaply)
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
      dataTableOutput("x1Table"),
      #plotlyOutput("heatmap")
      plotlyOutput("heatmapyo")
      # {{{{{{{{{{{{{}}}}}}}}}}}}}      
      #condformatOutput("formattedTable")
      #cat(file=stderr(), "main panel successful", "\n")
      #      condformatOutput("x2HeatMap")
      #plotlyOutput("heat")
      
      #heatmaply(region_data)
      # {{{{{{{{{{{{{}}}}}}}}}}}}}   
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
    #output$formattedTable <- renderCondformat({
    region_data <- gs_read(outputsheet, ws = worksheet())
    datatable(t(region_data))
    #datatable(condformat(t(region_data)) +
    #       rule_fill_gradient(pcc)
    #        )
    #renderCondformat(condformat(datatable(t(region_data))))
  })
  
  #  output$heatmap <- renderPlotly({
  #   region_data <- gs_read(outputsheet, ws = worksheet())
  #    heatmaply(as.matrix(t(region_data)))
  # })
  
#  output$heatmapyo <- plot_ly(z=region_data, type = "heatmap")
  
#  heatmap(region_data)
 
  as.matrix(region_data)
   
  #output$heat <- renderPlotly({
  # region_data <- gs_read(outputsheet, ws = worksheet())
  #plot_ly(x = region_data, y = region_data)
  #})
  
  #  output$x2HeatMap <- renderCondformat(condformat()
  
  #  )
  
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
  
  #plotlyOutput("heatmap")
  
  
  
}

#################################################
# Run the application 
shinyApp(ui = ui, server = server)