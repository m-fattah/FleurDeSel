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
library(lubridate)
library(RColorBrewer)
library(markdown)
suppressMessages(library(dplyr))

MONTHS_PRIOR <- 12
ARRIVALS_PERCENT_ACCURACY_LOWER_BOUND <- .75
ARRIVALS_PERCENT_ACCURACY_UPPER_BOUND <- 1.1

#################################################
# Google Sheet Preparation

# authenticate browser and check google sheets
gs_ls()

# get google sheet
outputsheet <- gs_title("output")
correlationSheet <- gs_title("correlations")
print("Correlations Sheet:")
baySheet <- gs_read(correlationSheet, ws = "Bay")

# list all worksheets in google sheet
print("All output worksheets:")
print(gs_ws_ls(outputsheet))

# test that one column can be searched as a vector
print("Bay:")
print(baySheet)
print(baySheet$Date)
#startDate <- Sys.Date()
#month(startDate) <- month(startDate) - 12
#day(startDate) <- 1
#print("Start Date:")
#print(startDate)
print("Reformatted Dates:")
print(mdy(baySheet$Date))
print("Matching Date Index:")
print(which(mdy(baySheet$Date) == startDate))


#################################################
# Functions for pulling from Correlations sheet

# Method: findStartDate ??? Returns the desired date from a certain number of months ago
findStartDate <- function(monthsPrior) {
  startDate <- Sys.Date()
  month(startDate) <- month(startDate) - monthsPrior
  day(startDate) <- 1
  return(startDate)
}

# Method: findIndex ??? Returns the index of a value within a given data frame
findIndex <- function(dfInput, valueInput) {
  index <- which(dfInput == valueInput)
  return(index)
}

# Method: getDataFrame - Returns a subset of a given model's data, limited to the given dates
getDataFrame <- function(modelDataColumn, startRowIndex) {
  return(modelDataColumn[startRowIndex, startRowIndex + (MONTHS_PRIOR - 1)])
}

# Method: getModelSheetName - Returns the name of the model formatted like the Google Sheets column names
getModelSheetName <- function(modelName) {
  return(cat("modelarrivals_", modelName, "arrivals"))
}

# Method: addArrivalsDiffPercent ??? Adds a column for the arrivals difference and a column for the arrivals percentage accuracy
addArrivalsDiffPercent <- function(dateActualArrModelArr) {
  arrivalsDiff <- dateActualArrModelArr$modelArrivals - dateActualArrModelArr$actualArrivals
  arrivalsPercentage <- dateActualArrModelArr$modelArrivals / dateActualArrModelArr$actualArrivals
  return(data.frame(dateActualArrModelArr, arrivalsDiff, arrivalsPercentage))
}

# Method: 

# Method: addOtherCorrelationsStats - 
addOtherCorrelationsStats <- function(dateActualArrModelArr) {
  dfDateActualModelDiffPercent <- addArrivalsDiffPercent(dateActualArrModelArr)
  
  # add a column to indicate whether the actual arrivals percentage accuracy lies between the lower and upper 
  #  bounds (constants at the top). 1 if yes, 0 if no.
  percentIsWithinRange <- NULL
  for (i in 1:length(dfDateActualModelDiffPercent$arrivalsPercentage)) {
    if ((ARRIVALS_PERCENT_ACCURACY_LOWER_BOUND < dfDateActualModelDiffPercent$arrivalsPercentage[i]) & (dfDateActualModelDiffPercent$arrivalsPercentage[i] < ARRIVALS_PERCENT_ACCURACY_UPPER_BOUND)) {
      percentIsWithinRange[i] <- 1
    }
    else {
      percentIsWithinRange[i] <- 0
    }
  }
  dfDateActualModelDiffPercentScore <- data.frame(dfDateActualModelDiffPercent, percentIsWithinRange)
### TODO: consider 2 points vs. 1 point in the scoring above  
  
  # add a column for last year % accuracy
  yearPercentAcc <- mean(dfDateActualModelDiffPercentScore$arrivalsPercentage)
  dfDateActualModelDiffPercentScoreYearly <- data.frame(dfDateActualModelDiffPercentScore, yearPercentAcc)
  
  # add a column for PCC values
  PCC <- sum(percentIsWithinRange/length(percentIsWithinRange))
  dfDateActualModelDiffPercentScoreYearlyPCC <- data.frame(dfDateActualModelDiffPercentScoreYearly, PCC)
   
  return(dfDateActualModelDiffPercentScoreYearlyPCC)
}


checkTest <- 
  for (i in 1:length(longTest)) {
  if ((.75 < longTest[i]) & (longTest[i] < 1.1)) {
    blah[i] <- 1
  }
    else {
      blah[i] <- 0
    }
}







#################################################
# Function for outputing a variable number of data tables

#find_index <- function(modelName){
#  index <- which(correlations.long$Date == modelName)
#  return(index)
#}

#getDataFrame <- function(modelName) {
#  
#}

#tableize <- function(selectedModels) {

#  tables <- list() # create a list to hold all tables

#  for (selectedModel in selectedModels) { # go through all possible values of variables
#    table <- getDataFrame(selectedModel, arg2, ...)
#    tables[[as.character(selectedModel)]] <- 
#      # save table into slot in created list
#      # print table as HTML with additional formatting options
#      print(xtable(table, caption=paste("Selected Model:", selectedModel)),
#            type="html",
#            html.table.attributes='class="data table table-bordered table-condensed"',
#            caption.placement="top")
#  }
#  return(lapply(tables, paste)) # return HTML tables pasted together
#}

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
    
    #    fluidRow(
    #
    #    )
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
  
  #  output$correlationsDataTables <- renderUI({
  
  #### must go into the tableize function because modelName is a variable from a vector
  #
  currentSheet <- gs_read(correlationSheet, ws = worksheet())
  startDate <- findStartDate(MONTHS_PRIOR)
  startDateRowIndex <- findIndex(mdy(currentSheet$Date), startDate)
  dates <- getDataFrame(currentSheet$Date, startDateRowIndex)
  actualArrivals <- getDataFrame(currentSheet$actualarrivals, startDateRowIndex)
  modelAlternateName <- getModelSheetName(modelName)
  modelArrivals <- getDataFrame(currentSheet$modelAlternateName, startDateRowIndex)
  dateActualArrModelArr <- data.frame(dates, actualArrivals, modelArrivals)
  dataFrameWithStats <- addOtherCorrelationsStats(dateActualArrModelArr)
  
  #
  ####
  
  
  #    tableize(rownames(input$x1Table_rows_selected))
  #  })
  
}

#################################################
# Run the application 
shinyApp(ui = ui, server = server)