#Developed by Fred Davey
#Custom Stock Data Browser -- Lean UI file
#This project is intended to be used for custom indicator definitions and exploration of stocks utilizing statistical methods not available in common charting software.
#Note-- there is no warranty provided with this software.  Any user who actively trades with this software is encouraged to do their own validations.

#This file is for a lean UI to pair with a separate R instance that queries the Yahoo API, runs calculations, and populates a SQL Server Table


#Imports
library(shiny)
library(plotly)
library(tidyverse)
library(tidyquant)
library(quantreg)
library(broom)
library(RODBC)

#####
#Functions
fnPullSQLStockData<- function(aConString, aSymbol){
  fQuery = paste0("SELECT [Symbol]
      ,[TradeTime]
      ,[Last]
      ,[Change]
      ,[pctChange]
      ,[Open]
      ,[High]
      ,[Low]
      ,[Volume]
      ,[appendTime]
      ,[Slope]
      ,case when [SloLoCI] < -50000 then NULL else SloLoCI end as SloLoCI
	    ,case when SloUpCI > 50000 then NULL else SloUpCI end as SloUpCI
	  ,Volume - lag(Volume,1) over(order by appendTime asc) as IntervalVolume
  FROM [Stocks].[dbo].[YahooQuotesAndLADSlope]
Where Symbol like '", 'SPY', "' and appendTime > cast(getdate() as Date)
order by appendTime asc")
  return(sqlQuery(connection, fQuery, 100))
}

#####
#Global Variables
connection = odbcConnect(dsn = 'Stock')

#####
ui <- fluidPage(
  
  # Application title
  titlePanel(h1('Stock Scanner and Custom Chart Indicators'),
      ),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      p('LastUpdated: ', textOutput('Title')),
      #Text Input Box to Pull a Ticker
      textInput('TickerSearch', label = 'Specify Ticker', value = "SPY")
      
      #List of everything pulled currently
      #dataTableOutput("IndicatorList"),
      
      #Filters toggles for RS / RW / Volume
      
      #Toggle for auto refresh with timescale (seconds)
      #Button for manual refresh
      
    ),
    
    # Plots
    mainPanel(
      
      fluidRow( column(6, 
      plotOutput("Chart")),
      column(3, 
      # Not available with historical setup at the moment (requires a lag reference)
      plotOutput("VolumeProfile"))), 
      fluidRow(column(6, 
      plotOutput("IndicatorVsTime")))
      
    )
  )
)

#####
server <- function(input, output) {
  #Pull Day's data for our ticker
  
  output$Title<- renderText(as.character(Sys.time()))
  
  WorkingData <- fnPullSQLStockData(connection, input$TickerSearch)
  
  #Plot Ticker
  output$Chart <- renderPlot({

        ggplot(data = WorkingData, aes(x = appendTime, y= Last)) +
          geom_line() + geom_point() + labs(title = input$TickerSearch)
      
  })
# 
#   #Plot Volume Profile
#   output$VolumeProfile <- renderPlot({
#      ggplot(data = WorkingData[], aes(x = appendTime, y= Last))+
#          geom_violin(aes(weight = IntervalVolume))
#    })
# 
#   #Plot Indicators
#   output$IndicatorVsTime <- renderPlot({
#     #We get warnings for the 'null' data for the beginning points prior to indicator calculation
# 
#         ggplot(data = WorkingData[seq(1, nrow(WorkingData), 6), ], aes(x = appendTime, y= Slope)) +
#           geom_point() + geom_ribbon(aes(ymin = SloLoCI, ymax = SloUpCI), alpha = .1) +
#           labs(title = 'Robust Regression Slope vs Time') + geom_hline(aes(yintercept = 0))
#   })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
