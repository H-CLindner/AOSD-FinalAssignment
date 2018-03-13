# script for the analysis of the evaporation time series

library("readxl")
library("xts")
library("maps")
library("maptools")
library("mapdata")
library("plyr")
library("forecast")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

createEvapIndexEvap = function(rawData){
  rawDataIndex = rawData$DATE
  rawDataIndex.date = as.Date(rawDataIndex)
  return(rawDataIndex.date)
}

fillMissingDatesEvap = function(dates, dataFrame){
  dateRange = seq(min(dates), max(dates), by = 1)
  allDates = data.frame(dateRange)
  allDates = rename(allDates, replace = c("dateRange" = "DATE"))
  dataFrame = merge(dataFrame, allDates, all=TRUE)
  dataFrame = dataFrame[!duplicated(dataFrame$DATE),]
  return(dataFrame)
}

deleteNAsEvap = function(frame){
  if(sum(is.na(frame)) > 0){
    frame = na.approx(frame)
  } else {
    print("no NA's")
  }
}

tsTrendEvap = function(start, dataFrame){
  EvapTS = ts(dataFrame$EVAP, start=start, frequency=365.25)
  EvapTS = na.approx(EvapTS)
  stlEvapTS = stl(EvapTS, s.window = "periodic")
  trendEvapTS = stlEvapTS$time.series[,2]
  return(trendEvapTS)
}

forecastEvap = function(xtsEvap){
  arimaEvap = auto.arima(xtsEvap)
  fcastEvap = forecast(arimaEvap, h=100)
  return(fcastEvap)
}

statisticsEvap = function(xtsEvap){
  mean = mean(xtsEvap)
  sd = sd(xtsEvap)
  min = min(xtsEvap)
  max = max(xtsEvap)
  var = var(xtsEvap)
  median = median(xtsEvap)
  firstQuartile = quantile(xtsEvap, 0.25)
  thirdQuartile = quantile(xtsEvap, 0.75)
  statistics = data.frame(mean, sd, min, max, var, median, firstQuartile, thirdQuartile)
  return(statistics)
}

# Angleton, Texas in the South East near Houston
# Evaporation timeseries for Angleton in the SouthEast of Texas
angletonEvap = read_xlsx("timeseries/Angleton, SE/Angleton, Evap.xlsx")
angletonEvapIndex = createEvapIndexEvap(angletonEvap)

#fill missing dates
angletonEvap = fillMissingDatesEvap(angletonEvapIndex, angletonEvap)

#create an xts object
angletonEvapIndex = createEvapIndexEvap(angletonEvap)
EvapAngleton = xts(angletonEvap$EVAP, angletonEvapIndex)

#delete NA's
EvapAngleton = deleteNAsEvap(EvapAngleton)

#plot
plot(EvapAngleton)

#plot acf and pacf of the timeseries
acfAngletonEvap = acf(EvapAngleton, lag.max = 365)
pacfAngletonEvap = pacf(EvapAngleton)

#create trend
start = as.Date("1953-01-01")
trendEvapAngleton = tsTrendEvap(start, angletonEvap)

#forecast
fcastEvapAngleton = forecastEvap(EvapAngleton)

#statistics
statisticsAngletonEvap = statisticsEvap(EvapAngleton)


# Benbrook Dam, Texas in the North East near Dallas
# Evaporation timeseries for Benbrook in the NorthEast of Texas
BenbrookEvap = read_xlsx("timeseries/Benbrook, NE/Benbrook, Evap.xlsx")
BenbrookEvapIndex = createEvapIndexEvap(BenbrookEvap)

#fill missing dates
BenbrookEvap = fillMissingDatesEvap(BenbrookEvapIndex, BenbrookEvap)

#create an xts object
BenbrookEvapIndex = createEvapIndexEvap(BenbrookEvap)
EvapBenbrook = xts(BenbrookEvap$EVAP, BenbrookEvapIndex)

#delete NA's
EvapBenbrook = deleteNAsEvap(EvapBenbrook)

#plot
plot(EvapBenbrook)

#plot acf and pacf of the timeseries
acfBenbrookEvap = acf(EvapBenbrook, lag.max = 365)
pacfBenbrookEvap = pacf(EvapBenbrook)

#create trend
start = as.Date("1953-01-01")
trendEvapBenbrook = tsTrendEvap(start, BenbrookEvap)

#forecast
fcastEvapBenbrook = forecastEvap(EvapBenbrook)

#statistics
statisticsBenbrookEvap = statisticsEvap(EvapBenbrook)



# Fort Stockton, Texas in the North West near the border to New Mexico
# Evaporation timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonEvap = read_xlsx("timeseries/fortStockton, NW/fortStockton, Evap.xlsx")
fortStocktonEvapIndex = createEvapIndexEvap(fortStocktonEvap)

#fill missing dates
fortStocktonEvap = fillMissingDatesEvap(fortStocktonEvapIndex, fortStocktonEvap)

#create an xts object
fortStocktonEvapIndex = createEvapIndexEvap(fortStocktonEvap)
EvapfortStockton = xts(fortStocktonEvap$EVAP, fortStocktonEvapIndex)

#delete NA's
EvapfortStockton = deleteNAsEvap(EvapfortStockton)

#plot
plot(EvapfortStockton)

#plot acf and pacf of the timeseries
acffortStocktonEvap = acf(EvapfortStockton, lag.max = 365)
pacffortStocktonEvap = pacf(EvapfortStockton)

#create trend
start = as.Date("1953-01-01")
trendEvapfortStockton = tsTrendEvap(start, fortStocktonEvap)

#forecast
fcastEvapfortStockton = forecastEvap(EvapfortStockton)

#statistics
statisticsfortStocktonEvap = statisticsEvap(EvapfortStockton)



# Mccook, Texas in the South West near the coast of the Golf of Mexico and the Mexican border
# Evaporation timeseries for Mccook in the South West of Texas
MccookEvap = read_xlsx("timeseries/Mccook, SW/Mccook, Evap.xlsx")
MccookEvapIndex = createEvapIndexEvap(MccookEvap)

#fill missing dates
MccookEvap = fillMissingDatesEvap(MccookEvapIndex, MccookEvap)

#create an xts object
MccookEvapIndex = createEvapIndexEvap(MccookEvap)
EvapMccook = xts(MccookEvap$EVAP, MccookEvapIndex)

#delete NA's
EvapMccook = deleteNAsEvap(EvapMccook)

#plot
plot(EvapMccook)

#plot acf and pacf of the timeseries
acfMccookEvap = acf(EvapMccook, lag.max = 365)
pacfMccookEvap = pacf(EvapMccook)

#create trend
start = as.Date("1953-01-01")
trendEvapMccook = tsTrendEvap(start, MccookEvap)

#forecast
fcastEvapMccook = forecastEvap(EvapMccook)

#statistics
statisticsMccookEvap = statisticsEvap(EvapMccook)


#boxplot of evaporation timeseries
combineEvapTS = merge(EvapAngleton, EvapBenbrook, EvapfortStockton, EvapMccook)
combineEvapTS = rename(combineEvapTS, replace=c("EvapAngleton" = "Angleton", "EvapBenbrook" = "Benbrook", "EvapfortStockton" = "Fort Stockton", "EvapMccook" = "Mccook"))
boxplot(coredata(combineEvapTS))

#linear models of the time series
linTrendAngletonEvap = lm(trendEvapAngleton~time(trendEvapAngleton))
linTrendBenbrookEvap = lm(trendEvapBenbrook~time(trendEvapBenbrook))
linTrendfortStocktonEvap = lm(trendEvapfortStockton~time(trendEvapfortStockton))
linTrendMccookEvap = lm(trendEvapMccook~time(trendEvapMccook))

#plot trend of time series and compare linear regression with slope
par(mfrow=c(1,1))
plot(trendEvapAngleton)
abline(linTrendAngletonEvap, col="red")
slopeAngletonEvap = linTrendAngletonEvap$coefficients[2]
plot(trendEvapBenbrook)
abline(linTrendBenbrookEvap, col="red")
slopeBenbrookEvap = linTrendBenbrookEvap$coefficients[2]
plot(trendEvapfortStockton)
abline(linTrendfortStocktonEvap, col="red")
slopefortStocktonEvap = linTrendfortStocktonEvap$coefficients[2]
plot(trendEvapMccook)
abline(linTrendMccookEvap, col="red")
slopeMccookEvap = linTrendMccookEvap$coefficients[2]
slopeEvap = data.frame(slopeAngletonEvap, slopeBenbrookEvap, slopefortStocktonEvap, slopeMccookEvap)
