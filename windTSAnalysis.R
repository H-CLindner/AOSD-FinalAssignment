#script for the analysis of wind time series

library("readxl")
library("xts")
library("maps")
library("maptools")
library("mapdata")
library("plyr")
library("forecast")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

createWindIndexWind = function(rawData){
  rawDataIndex = rawData$DATE
  rawDataIndex.date = as.Date(rawDataIndex)
  return(rawDataIndex.date)
}

fillMissingDatesWind = function(dates, dataFrame){
  dateRange = seq(min(dates), max(dates), by = 1)
  allDates = data.frame(dateRange)
  allDates = rename(allDates, replace = c("dateRange" = "DATE"))
  dataFrame = merge(dataFrame, allDates, all=TRUE)
  dataFrame = dataFrame[!duplicated(dataFrame$DATE),]
  return(dataFrame)
}

deleteNAsWind = function(frame){
  if(sum(is.na(frame)) > 0){
    frame = na.approx(frame)
  } else {
    print("no NA's")
  }
}

tsTrendWind = function(start, dataFrame){
  WindTS = ts(dataFrame$WDMV, start=start, frequency=365.25)
  WindTS = na.approx(WindTS)
  stlWindTS = stl(WindTS, s.window = "periodic")
  trendWindTS = stlWindTS$time.series[,2]
  return(trendWindTS)
}

forecastWind = function(xtsWind){
  arimaWind = auto.arima(xtsWind)
  fcastWind = forecast(arimaWind, h=100)
  return(fcastWind)
}

statisticsWind = function(xtsWind){
  mean = mean(xtsWind)
  sd = sd(xtsWind)
  min = min(xtsWind)
  max = max(xtsWind)
  var = var(xtsWind)
  median = median(xtsWind)
  firstQuartile = quantile(xtsWind, 0.25)
  thirdQuartile = quantile(xtsWind, 0.75)
  statistics = data.frame(mean, sd, min, max, var, median, firstQuartile, thirdQuartile)
  return(statistics)
}

# Angleton, Texas in the South East near Houston
# Wind timeseries for Angleton in the SouthEast of Texas
angletonWind = read_xlsx("timeseries/Angleton, SE/Angleton, Wind.xlsx")
angletonWindIndex = createWindIndexWind(angletonWind)

#fill missing dates
angletonWind = fillMissingDatesWind(angletonWindIndex, angletonWind)

#create an xts object
angletonWindIndex = createWindIndexWind(angletonWind)
WindAngleton = xts(angletonWind$WDMV, angletonWindIndex)

#delete NA's
WindAngleton = deleteNAsWind(WindAngleton)

#plot
plot(WindAngleton)

#plot acf and pacf of the timeseries
acfAngletonWind = acf(WindAngleton, lag.max = 365)
pacfAngletonWind = pacf(WindAngleton)

#create trend
start = as.Date("1953-01-01") 
trendWindAngleton = tsTrendWind(start, angletonWind)

#forecast
fcastWindAngleton = forecastWind(WindAngleton)

#statistics
statisticsAngletonWind = statisticsWind(WindAngleton)




# Benbrook Dam, Texas in the North East near Dallas
# Precipitation timeseries for Benbrook in the NorthEast of Texas
BenbrookWind = read_xlsx("timeseries/Benbrook, NE/Benbrook, Wind.xlsx")
BenbrookWindIndex = createWindIndexWind(BenbrookWind)

#fill missing dates
BenbrookWind = fillMissingDatesWind(BenbrookWindIndex, BenbrookWind)

#create an xts object
BenbrookWindIndex = createWindIndexWind(BenbrookWind)
WindBenbrook = xts(BenbrookWind$WDMV, BenbrookWindIndex)

#delete NA's
WindBenbrook = deleteNAsWind(WindBenbrook)

#plot
plot(WindBenbrook)

#plot acf and pacf of the timeseries
acfBenbrookWind = acf(WindBenbrook, lag.max = 365)
pacfBenbrookWind = pacf(WindBenbrook)

#create trend
start = as.Date("1979-01-01") 
trendWindBenbrook = tsTrendWind(start, BenbrookWind)

#forecast
fcastWindBenbrook = forecastWind(WindBenbrook)

#statistics
statisticsBenbrookWind = statisticsWind(WindBenbrook)



# Fort Stockton, Texas in the North West near the border to New Mexico
# Precipitation timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonWind = read_xlsx("timeseries/fortStockton, NW/fortStockton, Wind.xlsx")
fortStocktonWindIndex = createWindIndexWind(fortStocktonWind)

#fill missing dates
fortStocktonWind = fillMissingDatesWind(fortStocktonWindIndex, fortStocktonWind)

#create an xts object
fortStocktonWindIndex = createWindIndexWind(fortStocktonWind)
WindfortStockton = xts(fortStocktonWind$WDMV, fortStocktonWindIndex)

#delete NA's
WindfortStockton = deleteNAsWind(WindfortStockton)

#plot
plot(WindfortStockton)

#plot acf and pacf of the timeseries
acffortStocktonWind = acf(WindfortStockton, lag.max = 365)
pacffortStocktonWind = pacf(WindfortStockton)

#create trend
start = as.Date("1950-01-01")
trendWindfortStockton = tsTrendWind(start, fortStocktonWind)

#forecast
fcastWindfortStockton = forecastWind(WindfortStockton)

#statistics
statisticsfortStocktonWind = statisticsWind(WindfortStockton)



# Mccook, Texas in the South West near the coast of the Golf of Mexico and the Mexican border
# Precipitation timeseries for Mccook in the South West of Texas
MccookWind = read_xlsx("timeseries/Mccook, SW/Mccook, Wind.xlsx")
MccookWindIndex = createWindIndexWind(MccookWind)

#fill missing dates
MccookWind = fillMissingDatesWind(MccookWindIndex, MccookWind)

#create an xts object
MccookWindIndex = createWindIndexWind(MccookWind)
WindMccook = xts(MccookWind$WDMV, MccookWindIndex)

#delete NA's
WindMccook = deleteNAsWind(WindMccook)

#plot
plot(WindMccook)

#plot acf and pacf of the timeseries
acfMccookWind = acf(WindMccook, lag.max = 365)
pacfMccookWind = pacf(WindMccook)

#create trend
start = as.Date("1963-01-01")
trendWindMccook = tsTrendWind(start, MccookWind)

#forecast
fcastWindMccook = forecastWind(WindMccook)

#statistics
statisticsMccookWind = statisticsWind(WindMccook)

#boxplot of precipitation timeseries
combineWindTS = merge(WindAngleton, WindBenbrook, WindfortStockton, WindMccook)
combineWindTS = rename(combineWindTS, replace=c("WindAngleton" = "Angleton", "WindBenbrook" = "Benbrook", "WindfortStockton" = "Fort Stockton", "WindMccook" = "Mccook"))
boxplot(coredata(combineWindTS))

#linear models of the time series
linTrendAngletonWind = lm(trendWindAngleton~time(trendWindAngleton))
linTrendBenbrookWind = lm(trendWindBenbrook~time(trendWindBenbrook))
linTrendfortStocktonWind = lm(trendWindfortStockton~time(trendWindfortStockton))
linTrendMccookWind = lm(trendWindMccook~time(trendWindMccook))

#plot trend of time series and compare linear regression with slope
par(mfrow=c(1,1))
plot(trendWindAngleton)
abline(linTrendAngletonWind, col="red")
slopeAngletonWind = linTrendAngletonWind$coefficients[2]
plot(trendWindBenbrook)
abline(linTrendBenbrookWind, col="red")
slopeBenbrookWind = linTrendBenbrookWind$coefficients[2]
plot(trendWindfortStockton)
abline(linTrendfortStocktonWind, col="red")
slopefortStocktonWind = linTrendfortStocktonWind$coefficients[2]
plot(trendWindMccook)
abline(linTrendMccookWind, col="red")
slopeMccookWind = linTrendMccookWind$coefficients[2]
slopeWind = data.frame(slopeAngletonWind, slopeBenbrookWind, slopefortStocktonWind, slopeMccookWind)
