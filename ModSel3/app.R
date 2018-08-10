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
library(markdown)
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
      helpText("Select a region to evaluate its models."),
      plotOutput("plot", height = "800px")
    ),
    
    # Show a table of statistical values
    mainPanel(
      dataTableOutput("x1Table"),
      #plotlyOutput("heatmap")
      #plotlyOutput("heatmapyo")
      #      plotOutput("heatmapPlot", height = "800px"),

      print("plotOutput passed")
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
  #as.matrix(region_data)
    })
  
  #  output$x2Heatmap <- renderPlot({
  #    print("entered heatmap generator")
  #      mtscaled <- as.matrix(type.convert(testdata))
  #      heatmap(mtscaled)
  #    print("completed heatmap generator")
  #  })
  
    output$plot <- renderPlot({
      region_data <- as.data.frame(gs_read(outputsheet, ws = worksheet()))
      print("current row names:")
      print(rownames(region_data))
      print("Want the row names to be:")
      print(region_data[,1])
      print("current column names:")
      print(colnames(region_data))
          print("Want the column names to be:")
          print(as.character(region_data[1,-1]))
      rownames(region_data) <- region_data[,1]
          colnames(region_data) <- region_data[1,]
      print("new row names:")
      print(rownames(region_data))
      print("new col names:")
      print(colnames(region_data))
      region_data <- region_data[-1,-1]
      print("final table for processing:")
      print(region_data)
      mtscaled <- as.matrix(type.convert(region_data))
      print(mtscaled)
      mtscaled[2,] <- mtscaled[2,] * (-1)
      mtscaled[3,] <- mtscaled[3,] * (-1)
  #    print("mtscaled row 2")
 #     print(mtscaled[2])
         print(mtscaled)
      heatmap(mtscaled, Colv=NA, Rowv=NA)
    })
  
  output$plot2 <- renderPlot({
    plot(head(cars, input$n), main="Foo")
  }, bg = "#F5F5F5")
  
  #  output$x3heatmap <- 
  
  ### output$heatmapyo <- plot_ly(z=as.matrix(type.convert(testdata)), type = "heatmap")
  
  #  heatmap(region_data)
  
  
  
}

#################################################
# Run the application 
shinyApp(ui = ui, server = server)