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
library(RColorBrewer)
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
      dataTableOutput("x1Table")
    )
  )
)

#################################################
# Define server logic required to generate table and heatmap
server <- function(input, output) {
  worksheet <- reactive({
    input$region
  })
  
  output$x1Table <- renderDataTable({
    region_data <- gs_read(outputsheet, ws = worksheet())
    datatable(t(region_data))
  })
  
  output$plot <- renderPlot({
    region_data <- as.data.frame(gs_read(outputsheet, ws = worksheet()))
    rownames(region_data) <- region_data[,1]
    colnames(region_data) <- region_data[1,]
    region_data <- region_data[-1,-1]
    mtscaled <- as.matrix(type.convert(region_data))
    mtscaled[2,] <- mtscaled[2,] * (-1)
    mtscaled[3,] <- mtscaled[3,] * (-1)
    palette <- colorRampPalette(brewer.pal(11,"RdYlGn"))(100)
    heatmap(mtscaled, Colv=NA, Rowv=NA, col = palette)
  })
  
  output$plot2 <- renderPlot({
    plot(head(cars, input$n), main="Foo")
  }, bg = "#F5F5F5")
  
}

#################################################
# Run the application 
shinyApp(ui = ui, server = server)