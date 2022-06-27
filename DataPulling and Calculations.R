#Developed by Fred Davey
#This script is intended to run on a standalone R instance throughout the trading Day
#It handles pulling real time quotes from Yahoo, and calculating indicators on them.
#
library(tidyverse)
library(tidyquant)
library(quantreg)
library(broom)
library(RODBC)


#####
#Functions
#1.  Pulls the most recent trade data from Yahoo for the tickers passed in.
#Input -- Ticker List, table with the most recent trades for passing into the indicator function.
#Action -- Query  Yahoo
#Output -- New Lines for tickers of interest  
fnPullQuoteData_singleQuote<- function(aTickerList, aLogger){
  #We will build query table throughout the subloop.  This frontruns Yahoo's maximum query size
  fQueryTibble = NULL
  if(aLogger){
  p1 <- Sys.time()
  print(paste0('PreYahooTime is: ',p1))
  }
  #Error after 521 queries between 9:35:20 and 11:02:41 broke the loop.  
  # Error in open.connection(con, "rb") : 
  #   Timeout was reached: [query1.finance.yahoo.com] Operation timed out after 10008 milliseconds with 0 out of 0 bytes received
  #Will need some sort of trycatch to fail gracefully.
  
  for (j in 0:(ceiling(length(aTickerList) / 200) - 1))
  {
    fTickerShort <- aTickerList[(j*200+1):min(length(aTickerList),((1+j)*200)) ]
    recentQuote<- getQuote(fTickerShort)
    recentQuote<- rownames_to_column(recentQuote, var = 'Symbol')
    recentQuote <- recentQuote %>% rename(pctChange = `% Change`)
    fQueryTibble <- bind_rows(fQueryTibble, recentQuote)
  }
  
  fQueryTibble<- fQueryTibble %>% mutate(appendTime = Sys.time())
  
  if(aLogger){
  p2 <- Sys.time()
  print(paste0('YahooTime is: ',difftime(p2, p1, units = 'secs')))
  }
  
  return(fQueryTibble)
}

fnAddIntradayIndicatorCols <- function(aRecentDatapoints, aMasterDataFrame, aPeriodCount, aPeriod_sec, aLogger = FALSE){
  #We are building the output tib row by row to avoid excessive joining logic & column management
  fOutputTib = NULL
  if(aLogger){
    p1 <- Sys.time()
    print(paste0('PreIndicatorTime is: ',p1))
  }
  
  #Subset Out Most Recent number of points based on indicator requested aPeriodCount and datacollection aPeriod_sec
  if(!is.null(aMasterDataFrame)){
    fOldRows <- aMasterDataFrame %>% arrange(Symbol, appendTime)
    
  }
  
  #Loop through the calculations for all tickers
  for (i in 1:nrow(aRecentDatapoints)){
    #Generate tibble with only the points we need on this iteration
    if(!is.null(aMasterDataFrame)){
      fCalcTib = bind_rows(fOldRows[fOldRows$Symbol == aRecentDatapoints$Symbol[i],], aRecentDatapoints[i,])
    }
    else{
      fCalcTib = aRecentDatapoints[i,]
    }
    #Bind existing row data with calculated data.
    #This would be a reasonable place to add additional indicator functions
    fIndicatorRowResult <- bind_cols(aRecentDatapoints[i, ], fnPriceSlope(fCalcTib, aPeriodCount), fnIntVol(fCalcTib))
    
    #Concatenate tibble with all tickers for output
    fOutputTib <- bind_rows(fOutputTib, fIndicatorRowResult)
  }
  if(aLogger){
    p2 <- Sys.time()
    print(paste0('Post Indicator Time is: ',difftime(p2, p1, units = 'secs')))
  }
  
  return(fOutputTib)
}

fnPriceSlope<- function(aDataFrame, aLookbackLength){
  if(nrow(aDataFrame) < aLookbackLength){
    return(tibble(Slope = NA, SloLoCI = NA, SloUpCI = NA))
  }
  
  #For a demo, we are going to run a robust regresion (IE -- has less weight on outliers than LeastSquares Regression) and report all parameters and CIs
  #rq function from the quantreg package -- LAD regression
  #times %%86400 -> times go from absolute dates to hours since midnight.  This helps with precision & helps the function solve.  With full date, the regression was computationally singular.
  
  #Note, we also have issues when there isn't a trade executed over the timeframe specified
  #we'll run against appendtime instead for now (flat is useful!), may go for tryCatch if more shenanigans pop up.
  
  #Non-unique solution warnings are suppressed -- we're running enough fits that noisy solutions are ok.
  tryCatch(
    expr ={
  suppressWarnings(fTrendFit<- rq(formula = Last~(as.numeric(appendTime)%% 10000), data = aDataFrame))
  fParams <- tidy(fTrendFit)
  #CIs sometimes evaluated to +/- infinity, so we will recode those to null prior to data entry
  fLower = ifelse(as.numeric(fParams[2,3]) < -50000, as.numeric(NULL), as.numeric(fParams[2,3]))
  fUpper = ifelse(as.numeric(fParams[2,4]) > 50000, as.numeric(NULL), as.numeric(fParams[2,4]))
  fReturn = tibble(Slope = as.numeric(fParams[2,2]), SloLoCI = fLower, SloUpCI = fUpper)
  return(fReturn) 
    }
  ,error = function(e){
    print(paste0('fnPriceSlope regression error.  Symbol: ', aDataFrame$Symbol[1], ' Time: ', as.character(Sys.time()), ' Error: ', e))
    
    fReturn = tibble (Slope = as.numeric(NULL), SloLoCI = as.numeric(NULL), SloUpCI = as.numeric(NULL))
    return(fReturn) 
    }
  )
}


fnIntVol <- function(aDataFrame){
  if(nrow(aDataFrame) == 1){
    return(tibble(IntervalVolume =0))
  }
  
  fVolDiff = aDataFrame[nrow(aDataFrame), 'Volume'] - aDataFrame[nrow(aDataFrame)-1,'Volume']
  #Appear to have occasional API data artifacts where volume decreases, recode these to 0 and keep going
  fVolDiff = ifelse(fVolDiff<0, 0, fVolDiff)
  return(tibble(IntervalVolume = fVolDiff))
}

#####
#Global Variables
logYahooTimes = FALSE
logIndicatorCalcTimes = FALSE
connection = odbcConnect(dsn = 'Stock')
stockTickers<- sqlFetch(connection, 'TickerLookup')
sectorTickers<- sqlFetch(connection, 'SectorLookup')
# n queries used for indicators, at most
maxLookbackPeriod = 18
#Length between queries, in seconds
periodLength = 10

#####
#Begin our main processing
#Initialize so we have something to bind on
WorkingDataset = NULL

while (as.numeric(Sys.time()) %% (3600*24) < 72000)
{

  p1 <- Sys.time()
#Generate our most recent set of quotes + indicator
OneFinishedRowset <- fnPullQuoteData_singleQuote(stockTickers$Symbol, logYahooTimes) %>% 
  fnAddIntradayIndicatorCols(WorkingDataset, aPeriodCount = maxLookbackPeriod, aPeriod_sec = periodLength )

#Upload SQL and reclose connection
sqlSave(connection, OneFinishedRowset, tablename = 'YahooQuotesAndSlope', rownames = FALSE, append = TRUE)

#Manage persistent tibbles for future calcs
WorkingDataset = bind_rows(WorkingDataset, OneFinishedRowset)

if(nrow(WorkingDataset) > maxLookbackPeriod * nrow(stockTickers)){
  #Can potentially skip arrangement if most recent rows are always appended to the bottom
  WorkingDataset= WorkingDataset %>% arrange(appendTime, Symbol)
  #Grab all rows after the earliest rowset retained
  WorkingDataset = WorkingDataset[(nrow(stockTickers)+1):nrow(WorkingDataset),]
}

print(paste0('Loop Time is: ', difftime(Sys.time(), p1, units = 'secs')))
theDelay <- periodLength - as.numeric(difftime(Sys.time(),p1,unit="secs"))

Sys.sleep(max(0, theDelay))
}
