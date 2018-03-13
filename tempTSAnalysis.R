#script for the analysis of temperature time series

library("readxl")
library("xts")
library("maps")
library("maptools")
library("mapdata")
library("plyr")
library("forecast")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

createTempIndexTemp = function(rawData){
  rawDataIndex = rawData$DATE
  rawDataIndex.date = as.Date(rawDataIndex)
  return(rawDataIndex.date)
}

fillMissingDatesTemp = function(dates, dataFrame){
  dateRange = seq(min(dates), max(dates), by = 1)
  allDates = data.frame(dateRange)
  allDates = rename(allDates, replace = c("dateRange" = "DATE"))
  dataFrame = merge(dataFrame, allDates, all=TRUE)
  dataFrame = dataFrame[!duplicated(dataFrame$DATE),]
  return(dataFrame)
}

deleteNAsTemp = function(frame){
  if(sum(is.na(frame)) > 0){
    frame = na.approx(frame)
  } else {
    print("no NA's")
  }
}

tsTrendTemp = function(start, dataFrame){
  tempTS = ts(dataFrame$TMAX, start=start, frequency=365.25)
  tempTS = na.approx(tempTS)
  stlTempTS = stl(tempTS, s.window = "periodic")
  trendTempTS = stlTempTS$time.series[,2]
  return(trendTempTS)
}

forecastTemp = function(xtsTemp){
  arimaTemp = auto.arima(xtsTemp)
  fcastTemp = forecast(arimaTemp, h=100)
  return(fcastTemp)
}

statisticsTemp = function(xtsTemp){
  mean = mean(xtsTemp)
  sd = sd(xtsTemp)
  min = min(xtsTemp)
  max = max(xtsTemp)
  var = var(xtsTemp)
  median = median(xtsTemp)
  firstQuartile = quantile(xtsTemp, 0.25)
  thirdQuartile = quantile(xtsTemp, 0.75)
  statistics = data.frame(mean, sd, min, max, var, median, firstQuartile, thirdQuartile)
  return(statistics)
}

# Angleton, Texas in the South East near Houston
# Temperature timeseries for Angleton in the SouthEast of Texas
angletonTemp = read_xlsx("timeseries/Angleton, SE/Angleton, TMax.xlsx")
angletonTempIndex = createTempIndexTemp(angletonTemp)

#fill missing dates
angletonTemp = fillMissingDatesTemp(angletonTempIndex, angletonTemp)

#create xts object
angletonTempIndex = createTempIndexTemp(angletonTemp)
tmpMaxAngleton = xts(angletonTemp$TMAX, angletonTempIndex)

#delete NA's
tmpMaxAngleton = deleteNAsTemp(tmpMaxAngleton)

#plot
plot(tmpMaxAngleton)

#plot acf and pacf of the timeseries
acfAngletonTemp = acf(tmpMaxAngleton, lag.max = 365)
pacfAngletonTemp = pacf(tmpMaxAngleton)

#create trend
start = as.Date("1950-01-01") 
trendTmpAngleton = tsTrendTemp(start, angletonTemp)

#forecast
fcastTempAngleton = forecastTemp(tmpMaxAngleton)

#create statistics
statisticsAngletonTemp = statisticsTemp(tmpMaxAngleton)


# Benbrook Dam, Texas in the North East near Dallas
# Temperature timeseries for Benbrook in the NorthEast of Texas
BenbrookTemp = read_xlsx("timeseries/Benbrook, NE/Benbrook, TMax.xlsx")
BenbrookTempIndex = createTempIndexTemp(BenbrookTemp)


#fill missing dates
BenbrookTemp = fillMissingDatesTemp(BenbrookTempIndex, BenbrookTemp)

#create xts object
BenbrookTempIndex = createTempIndexTemp(BenbrookTemp)
tmpMaxBenbrook = xts(BenbrookTemp$TMAX, BenbrookTempIndex)

#delete NA's
tmpMaxBenbrook = deleteNAsTemp(tmpMaxBenbrook)

#plot
plot(tmpMaxBenbrook)

#plot acf and pacf of the timeseries
acfBenbrookTemp = acf(tmpMaxBenbrook, lag.max = 365)
pacfBenbrookTemp = pacf(tmpMaxBenbrook)

#create trend
start = as.Date("1950-07-03") 
trendTmpBenbrook = tsTrendTemp(start, BenbrookTemp)

#forecast
fcastTempBenbrook = forecastTemp(tmpMaxBenbrook)

#create statistics
statisticsBenbrookTemp = statisticsTemp(tmpMaxBenbrook)

# Fort Stockton, Texas in the North West near the border to New Mexico
# Temperature timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonTemp = read_xlsx("timeseries/FortStockton, NW/FortStockton, TMax.xlsx")
fortStocktonTempIndex = createTempIndexTemp(fortStocktonTemp)


#fill missing dates
fortStocktonTemp = fillMissingDatesTemp(fortStocktonTempIndex, fortStocktonTemp)

#create xts object
fortStocktonTempIndex = createTempIndexTemp(fortStocktonTemp)
tmpMaxfortStockton = xts(fortStocktonTemp$TMAX, fortStocktonTempIndex)

#delete NA's
tmpMaxfortStockton = deleteNAsTemp(tmpMaxfortStockton)

#plot
plot(tmpMaxfortStockton)

#plot acf and pacf of the timeseries
acffortStocktonTemp = acf(tmpMaxfortStockton, lag.max = 365)
pacffortStocktonTemp = pacf(tmpMaxfortStockton)

#create trend
start = as.Date("1950-01-01") 
trendTmpfortStockton = tsTrendTemp(start, fortStocktonTemp)

#forecast
fcastTempfortStockton = forecastTemp(tmpMaxfortStockton)

#create statistics
statisticsfortStocktonTemp = statisticsTemp(tmpMaxfortStockton)

# Mccook, Texas in the South West near the coast of the Golf of Mexico and the Mexican border
# Temperature timeseries for Mccook in the South West of Texas
MccookTemp = read_xlsx("timeseries/Mccook, SW/Mccook, TMax.xlsx")
MccookTempIndex = createTempIndexTemp(MccookTemp)


#fill missing dates
MccookTemp = fillMissingDatesTemp(MccookTempIndex, MccookTemp)

#create xts object
MccookTempIndex = createTempIndexTemp(MccookTemp)
tmpMaxMccook = xts(MccookTemp$TMAX, MccookTempIndex)

#delete NA's
tmpMaxMccook = deleteNAsTemp(tmpMaxMccook)

#plot
plot(tmpMaxMccook)

#plot acf and pacf of the timeseries
acfMccookTemp = acf(tmpMaxMccook, lag.max = 365)
pacfMccookTemp = pacf(tmpMaxMccook)

#create trend
start = as.Date("1950-01-01") 
trendTmpMccook = tsTrendTemp(start, MccookTemp)

#forecast
fcastTempMccook = forecastTemp(tmpMaxMccook)

#create statistics
statisticsMccookTemp = statisticsTemp(tmpMaxMccook)

#boxplot of temperature timeseries
combineTmpTS = merge(tmpMaxAngleton, tmpMaxBenbrook, tmpMaxfortStockton, tmpMaxMccook)
combineTmpTS = rename(combineTmpTS, replace=c("tmpMaxAngleton" = "Angleton", "tmpMaxBenbrook" = "Benbrook", "tmpMaxfortStockton" = "Fort Stockton", "tmpMaxMccook" = "Mccook"))
boxplot(coredata(combineTmpTS))

#linear models of the time series
linTrendAngletonTemp = lm(trendTmpAngleton~time(trendTmpAngleton))
linTrendBenbrookTemp = lm(trendTmpBenbrook~time(trendTmpBenbrook))
linTrendfortStocktonTemp = lm(trendTmpfortStockton~time(trendTmpfortStockton))
linTrendMccookTemp = lm(trendTmpMccook~time(trendTmpMccook))

#plot trend of time series and compare linear regression with slope
par(mfrow=c(1,1))
plot(trendTmpAngleton)
abline(linTrendAngletonTemp, col="red")
slopeAngletonTemp = linTrendAngletonTemp$coefficients[2]
plot(trendTmpBenbrook)
abline(linTrendBenbrookTemp, col="red")
slopeBenbrookTemp = linTrendBenbrookTemp$coefficients[2]
plot(trendTmpfortStockton)
abline(linTrendfortStocktonTemp, col="red")
slopefortStocktonTemp = linTrendfortStocktonTemp$coefficients[2]
plot(trendTmpMccook)
abline(linTrendMccookTemp, col="red")
slopeMccookTemp = linTrendMccookTemp$coefficients[2]
slopeTemp = data.frame(slopeAngletonTemp, slopeBenbrookTemp, slopefortStocktonTemp, slopeMccookTemp)
