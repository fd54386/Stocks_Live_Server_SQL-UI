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
fnPullSQLTickerData<- function(aConString, aSymbol){
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
      ,[SloLoCI]
	    ,SloUpCI
	  ,IntervalVolume
  FROM [Stocks].[dbo].[YahooQuotesAndLADSlope]
Where Symbol like '", aSymbol, "' and appendTime > cast(getdate() as Date)
order by appendTime asc")
  return(sqlQuery(connection, fQuery, 100))
}
fnPullSectorSnapshot <- function(aConnection, aNSectors ){
  fQuerySector = paste0("SELECT top(",aNSectors, ") [Sector]
	  ,round([Last],2) as Last
      ,round([Change], 2) as Change
      ,round([pctChange], 2) as pctChange
      ,[Open]
      ,[High]
      ,[Low]
      ,[Volume] * Last as DollarVolume
	  ,appendTime
  FROM [Stocks].[dbo].[SectorLookup] inner join [Stocks].[dbo].[YahooQuotesAndLADSlope] on YahooTickerforSP500Sector = Symbol
  where appendTime > convert(date, getdate())

order by appendTime desc")
  
fSectorData <- as.tibble(sqlQuery(aConnection, fQuerySector))

  fQuerySPY = "Select top (1) 'SP500' as [Sector]
	  ,[Last]
      ,round([Change], 2) as Change
      ,round([pctChange], 2) as pctChange
      ,[Open]
      ,[High]
      ,[Low]
      ,[Volume] * Last as DollarVolume
	  ,appendTime

	From [Stocks].[dbo].[YahooQuotesAndLADSlope]
	where appendTime > convert(date, getdate()) and Symbol like 'SPY'
order by appendTime desc
  "
fSPYData <- as.tibble(sqlQuery(aConnection, fQuerySPY))

  return(bind_rows(fSectorData, fSPYData) %>% arrange (desc(pctChange)))
}

fnPullSectorsDaily <- function (aConnection){
  fQuery = "SELECT Sector, Symbol, pctChange, appendTime

  FROM [Stocks].[dbo].[SectorLookup] sec inner join [Stocks].[dbo].[YahooQuotesAndLADSlope] quote on sec.YahooTickerforSP500Sector = Quote.Symbol
  where appendTime > convert(date, getdate())

  union all
  
  Select 'SPY' as Sector, Symbol, pctChange, appendTime
  from [Stocks].[dbo].[YahooQuotesAndLADSlope]
  where appendTime > convert(date, getdate()) and symbol like 'SPY'"
  
  return(sqlQuery(aConnection, fQuery))
  
}
fnPullWithinSectorSnapshot <- function(aConnection, aSector){
#Should be manageable either way with only a few hundred tickers, but probably want to determine if joining sector data is more efficient than using a 'where in () ' query with 100 tickers
  fQuerySymbols = paste0("SELECT [Symbol]
FROM [Stocks].[dbo].[TickerLookup] where Sector like '", aSector, "'")

  #Pull sector tickers
  fSectorTickers <- sqlQuery(aConnection, fQuerySymbols)

  #Trying to include a join statement to get ticker name
  # #Prep the quote query -- tickers with a where in statement and most recent 1 datapoint
  # #Using a wildcard for column names for indicator scalability
  # fInStatement <- paste0("'", fSectorTickers$Symbol, "'", collapse = ", ")
  # fCount <- nrow(fSectorTickers)
  # 
  # fQueryLast = paste0("Select top (", as.character(fCount), ") 
  #                       FROM [Stocks].[dbo].[YahooQuotesAndLADSlope]
  #                       where symbol in (", fInStatement,") 
  #                       order by appendTime desc, pctChange asc")
  
  fCount <- nrow(fSectorTickers)
  fQueryLast = paste0("
  Select top (", as.character(fCount), ") 
      quote.* 
      ,left([Name], 10) as ShortName
      ,[Industry]
      ,[MarketCap]
      ,[MarketCapDate]
	    ,[Sector]
	    ,IntervalVolume * Last as DollarIntVol
	    ,Volume * Last / MarketCap * 100 as DailyVolumeVsCap
	  FROM [Stocks].[dbo].[TickerLookup] tick inner join [Stocks].[dbo].[YahooQuotesAndLADSlope] quote on tick.Symbol = quote.Symbol
    where sector like '", aSector,"' 
    order by quote.appendTime desc, quote.pctChange asc")
#Using wildcard in query requires reformatting but allows us to pull all future indicators
  #relocate will move to front by default
  fResult <- as_tibble(sqlQuery(aConnection, fQueryLast)) 
  fResult <- fResult %>% relocate(Symbol, ShortName, pctChange, Last, Open, High, Low, Industry, MarketCap, MarketCapDate, Sector, appendTime)
  return(fResult)
}
fnPullSectorList <- function(aConnection){
  fQuery = "SELECT [Sector]
  FROM [Stocks].[dbo].[SectorLookup]"
  
  return(tibble(sqlQuery(aConnection, fQuery)))
}

#####
#Global Variables
connection = odbcConnect(dsn = 'Stock')
sectorList <- fnPullSectorList(connection)$Sector

#####
ui <- fluidPage(
  
  # Application title
  titlePanel(h1('Stock Scanner and Custom Chart Indicators'),
      ),
  
  # Sidebar
  navbarPage("Views",
    tabPanel("Sector Performance", plotOutput('SectorsChart'), dataTableOutput("SectorPerformance")
             ), 
    tabPanel("Within Sector Performance", 
        selectInput('SectorSearch', label = 'Specify Sector to Screen', choices = sectorList),
        dataTableOutput("StockPerformanceWithinSector")
        ),
    tabPanel("Ticker Performance",
      sidebarLayout(
        sidebarPanel(
          p('LastUpdated: ', textOutput('Title')),
          #Text Input Box to Pull a Ticker
          textInput('TickerSearch', label = 'Specify Ticker', value = "SPY")
          #,
        ),
      
      # Plots
      mainPanel(
        fluidRow( column(6, 
                         plotOutput("Chart")),
                  column(3, 
                         #Commented for 6-22 only while interval volume meaningfully populates
                         #plotOutput("VolumeProfile")
                  )), 
        fluidRow(column(6, 
                        plotOutput("IndicatorVsTime"))),
        #May want to hide this on a separate tab since the auto refresh moves the screen focus.

      )
    ))
  )

)

#####
server <- function(input, output) {
  #Charts for ticker of interest, only updates when ticker entered in
  #Pull today's data for our ticker
  #Reactive function only evaluates when the TickerSearch input is updated (verified with a quick printline in the fn call)
  pullTicker <- reactive({
    fnPullSQLTickerData(connection, input$TickerSearch)
  })
  
  #Auto updating tables for sector snapshots
  #Set a timer, update every 60 seconds
  autoInvalidate1min <- reactiveTimer(60000)
  
  #Plot Ticker
  output$Chart <- renderPlot({
    output$Title<- renderText(as.character(Sys.time()))
        ggplot(data = pullTicker(), aes(x = appendTime, y= Last)) +
          geom_line() + geom_point() + labs(title = input$TickerSearch)
      
  })

  #Plot Volume Profile
  output$VolumeProfile <- renderPlot({
     ggplot(data = pullTicker(), aes(x = appendTime, y= Last))+
         geom_violin(aes(weight = IntervalVolume))
   })

  #Plot Indicators
  output$IndicatorVsTime <- renderPlot({
    
        ggplot(data = pullTicker()[seq(1, nrow(pullTicker()), 3), ], aes(x = appendTime, y= Slope)) +
          geom_line() + geom_ribbon(aes(ymin = SloLoCI, ymax = SloUpCI), alpha = .1) +
          labs(title = 'Robust Regression Slope vs Time') + geom_hline(aes(yintercept = 0))
  })
  
  output$SectorPerformance<- renderDataTable({
    autoInvalidate1min()
    formattedSecData <- fnPullSectorSnapshot(connection, length(sectorList))
    formattedSecData$DollarVolume <- formatC(formattedSecData$DollarVolume, format = 'e', digits = 2)
    formattedSecData
  })
  
  output$SectorsChart <- renderPlot({
    autoInvalidate1min()
    ggplot(data = fnPullSectorsDaily(connection), aes(x = appendTime, y= pctChange, group = Sector, color = Sector)) + geom_point() 
  })
  
  output$StockPerformanceWithinSector <- renderDataTable({
    autoInvalidate1min()
    formattedSecStocks <- fnPullWithinSectorSnapshot(connection, input$SectorSearch)
    formattedSecStocks$pctChange <- formatC(formattedSecStocks$pctChange, format = 'f', digits = 2)
    formattedSecStocks$Last <- formatC(formattedSecStocks$Last, format = 'f', digits = 2)
    formattedSecStocks$Open <- formatC(formattedSecStocks$Open, format = 'f', digits = 2)
    formattedSecStocks$High <- formatC(formattedSecStocks$High, format = 'f', digits = 2)
    formattedSecStocks$Low <- formatC(formattedSecStocks$Low, format = 'f', digits = 2)
    formattedSecStocks$MarketCap <- formatC(formattedSecStocks$MarketCap, format = 'e', digits = 2)
    formattedSecStocks$Volume <- formatC(formattedSecStocks$Volume, format = 'e', digits = 2)
    formattedSecStocks$IntervalVolume <- formatC(formattedSecStocks$IntervalVolume, format = 'e', digits = 2)
    formattedSecStocks$DollarIntVol <- formatC(formattedSecStocks$DollarIntVol, format = 'e', digits = 2)
    formattedSecStocks$DailyVolumeVsCap <- formatC(formattedSecStocks$DailyVolumeVsCap, format = 'f', digits = 2)
    formattedSecStocks
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
