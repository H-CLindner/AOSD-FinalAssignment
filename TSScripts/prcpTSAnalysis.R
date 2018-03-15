#script for the analysis of the precipitation time series

library("readxl")
library("xts")
library("maps")
library("maptools")
library("mapdata")
library("plyr")
library("forecast")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

createPrcpIndexPrcp = function(rawData){
  rawDataIndex = rawData$DATE
  rawDataIndex.date = as.Date(rawDataIndex)
  return(rawDataIndex.date)
}

fillMissingDatesPrcp = function(dates, dataFrame){
  dateRange = seq(min(dates), max(dates), by = 1)
  allDates = data.frame(dateRange)
  allDates = rename(allDates, replace = c("dateRange" = "DATE"))
  dataFrame = merge(dataFrame, allDates, all=TRUE)
  dataFrame = dataFrame[!duplicated(dataFrame$DATE),]
  return(dataFrame)
}

deleteNAsPrcp = function(frame){
  if(sum(is.na(frame)) > 0){
    frame = na.approx(frame)
  } else {
    print("no NA's")
  }
}

tsTrendPrcp = function(start, dataFrame){
  PrcpTS = ts(dataFrame$PRCP, start=start, frequency=365.25)
  PrcpTS = na.approx(PrcpTS)
  stlPrcpTS = stl(PrcpTS, s.window = "periodic")
  trendPrcpTS = stlPrcpTS$time.series[,2]
  return(trendPrcpTS)
}

forecastPrcp = function(xtsPrcp){
  arimaPrcp = auto.arima(xtsPrcp)
  fcastPrcp = forecast(arimaPrcp, h=100)
  return(fcastPrcp)
}

statisticsPrcp = function(xtsPrcp){
  mean = mean(xtsPrcp)
  sd = sd(xtsPrcp)
  min = min(xtsPrcp)
  max = max(xtsPrcp)
  var = var(xtsPrcp)
  median = median(xtsPrcp)
  firstQuartile = quantile(xtsPrcp, 0.25)
  thirdQuartile = quantile(xtsPrcp, 0.75)
  statistics = data.frame(mean, sd, min, max, var, median, firstQuartile, thirdQuartile)
  return(statistics)
}

# Angleton, Texas in the South East near Houston
# Precipitation timeseries for Angleton in the SouthEast of Texas
angletonPrcp = read_xlsx("timeseries/Angleton, SE/Angleton, Prcp.xlsx")
angletonPrcpIndex = createPrcpIndexPrcp(angletonPrcp)

#fill missing dates
angletonPrcp = fillMissingDatesPrcp(angletonPrcpIndex, angletonPrcp)

#create an xts object
angletonPrcpIndex = createPrcpIndexPrcp(angletonPrcp)
PrcpAngleton = xts(angletonPrcp$PRCP, angletonPrcpIndex)

#delete NA's
PrcpAngleton = deleteNAsPrcp(PrcpAngleton)

#plot
plot(PrcpAngleton)

#plot acf and pacf of the timeseries
acfAngletonPrcp = acf(PrcpAngleton, lag.max = 365)
pacfAngletonPrcp = pacf(PrcpAngleton)

#create trend
start = as.Date("1950-01-01")  
trendPrcpAngleton = tsTrendPrcp(start, angletonPrcp)

#forecast
fcastPrcpAngleton = forecastPrcp(PrcpAngleton)

#statistics
statisticsAngletonPrcp = statisticsPrcp(PrcpAngleton)


# Benbrook Dam, Texas in the North East near Dallas
# Precipitation timeseries for Benbrook in the NorthEast of Texas
BenbrookPrcp = read_xlsx("timeseries/Benbrook, NE/Benbrook, Prcp.xlsx")
BenbrookPrcpIndex = createPrcpIndexPrcp(BenbrookPrcp)

#fill missing dates
BenbrookPrcp = fillMissingDatesPrcp(BenbrookPrcpIndex, BenbrookPrcp)

#create an xts object
BenbrookPrcpIndex = createPrcpIndexPrcp(BenbrookPrcp)
PrcpBenbrook = xts(BenbrookPrcp$PRCP, BenbrookPrcpIndex)

#delete NA's
PrcpBenbrook = deleteNAsPrcp(PrcpBenbrook)

#plot
plot(PrcpBenbrook)

#plot acf and pacf of the timeseries
acfBenbrookPrcp = acf(PrcpBenbrook, lag.max = 365)
pacfBenbrookPrcp = pacf(PrcpBenbrook)

#create trend
start = as.Date("1950-01-01")  
trendPrcpBenbrook = tsTrendPrcp(start, BenbrookPrcp)

#forecast
fcastPrcpBenbrook = forecastPrcp(PrcpBenbrook)

#statistics
statisticsBenbrookPrcp = statisticsPrcp(PrcpBenbrook)



# Fort Stockton, Texas in the North West near the border to New Mexico
# Precipitation timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonPrcp = read_xlsx("timeseries/fortStockton, NW/fortStockton, Prcp.xlsx")
fortStocktonPrcpIndex = createPrcpIndexPrcp(fortStocktonPrcp)

#fill missing dates
fortStocktonPrcp = fillMissingDatesPrcp(fortStocktonPrcpIndex, fortStocktonPrcp)

#create an xts object
fortStocktonPrcpIndex = createPrcpIndexPrcp(fortStocktonPrcp)
PrcpfortStockton = xts(fortStocktonPrcp$PRCP, fortStocktonPrcpIndex)

#delete NA's
PrcpfortStockton = deleteNAsPrcp(PrcpfortStockton)

#plot
plot(PrcpfortStockton)

#plot acf and pacf of the timeseries
acffortStocktonPrcp = acf(PrcpfortStockton, lag.max = 365)
pacffortStocktonPrcp = pacf(PrcpfortStockton)

#create trend
start = as.Date("1950-01-01")  
trendPrcpfortStockton = tsTrendPrcp(start, fortStocktonPrcp)

#forecast
fcastPrcpfortStockton = forecastPrcp(PrcpfortStockton)

#statistics
statisticsfortStocktonPrcp = statisticsPrcp(PrcpfortStockton)



# Mccook, Texas in the South West near the coast of the Golf of Mexico and the Mexican border
# Precipitation timeseries for Mccook in the South West of Texas
MccookPrcp = read_xlsx("timeseries/Mccook, SW/Mccook, Prcp.xlsx")
MccookPrcpIndex = createPrcpIndexPrcp(MccookPrcp)

#fill missing dates
MccookPrcp = fillMissingDatesPrcp(MccookPrcpIndex, MccookPrcp)

#create an xts object
MccookPrcpIndex = createPrcpIndexPrcp(MccookPrcp)
PrcpMccook = xts(MccookPrcp$PRCP, MccookPrcpIndex)

#delete NA's
PrcpMccook = deleteNAsPrcp(PrcpMccook)

#plot
plot(PrcpMccook)

#plot acf and pacf of the timeseries
acfMccookPrcp = acf(PrcpMccook, lag.max = 365)
pacfMccookPrcp = pacf(PrcpMccook)

#create trend
start = as.Date("1950-01-01")  
trendPrcpMccook = tsTrendPrcp(start, MccookPrcp)

#forecast
fcastPrcpMccook = forecastPrcp(PrcpMccook)

#statistics
statisticsMccookPrcp = statisticsPrcp(PrcpMccook)


#boxplot of precipitation timeseries
combinePrcpTS = merge(PrcpAngleton, PrcpBenbrook, PrcpfortStockton, PrcpMccook)
combinePrcpTS = rename(combinePrcpTS, replace=c("PrcpAngleton" = "Angleton", "PrcpBenbrook" = "Benbrook", "PrcpfortStockton" = "Fort Stockton", "PrcpMccook" = "Mccook"))
boxplot(coredata(combinePrcpTS))

#linear models of the time series
linTrendAngletonPrcp = lm(trendPrcpAngleton~time(trendPrcpAngleton))
linTrendBenbrookPrcp = lm(trendPrcpBenbrook~time(trendPrcpBenbrook))
linTrendfortStocktonPrcp = lm(trendPrcpfortStockton~time(trendPrcpfortStockton))
linTrendMccookPrcp = lm(trendPrcpMccook~time(trendPrcpMccook))

#plot trend of time series and compare linear regression with slope
par(mfrow=c(1,1))
plot(trendPrcpAngleton)
abline(linTrendAngletonPrcp, col="red")
slopeAngletonPrcp = linTrendAngletonPrcp$coefficients[2]
plot(trendPrcpBenbrook)
abline(linTrendBenbrookPrcp, col="red")
slopeBenbrookPrcp = linTrendBenbrookPrcp$coefficients[2]
plot(trendPrcpfortStockton)
abline(linTrendfortStocktonPrcp, col="red")
slopefortStocktonPrcp = linTrendfortStocktonPrcp$coefficients[2]
plot(trendPrcpMccook)
abline(linTrendMccookPrcp, col="red")
slopeMccookPrcp = linTrendMccookPrcp$coefficients[2]
slopePrcp = data.frame(slopeAngletonPrcp, slopeBenbrookPrcp, slopefortStocktonPrcp, slopeMccookPrcp)
