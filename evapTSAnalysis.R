# script for the analysis of the evaporation time series

library("readxl")
library("xts")
library("maps")
library("maptools")
library("mapdata")
library("plyr")
library("forecast")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

# Angleton, Texas in the South East near Houston
# Evaporation timeseries for Angleton in the SouthEast of Texas
angletonEvap = read_xlsx("timeseries/Angleton, SE/Angleton, Evap.xlsx")

angletonEvapIndex = (1953 + angletonEvap$DATE)
angletonEvapIndex.date = as.Date(angletonEvapIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeAngletonEvap = seq(min(angletonEvapIndex.date), max(angletonEvapIndex.date), by = 1)
allDatesAngletonEvap = data.frame(DateRangeAngletonEvap)
allDatesAngletonEvap = rename(allDatesAngletonEvap, replace = c("DateRangeAngletonEvap" = "DATE"))
angletonEvap = merge(angletonEvap, allDatesAngletonEvap, all=TRUE)
angletonEvap = angletonEvap[!duplicated(angletonEvap$DATE),]

#create an xts object out of the data frame for temperature
angletonEvapIndex = (1953 + angletonEvap$DATE)
angletonEvapIndex.date = as.Date(angletonEvapIndex)
evapAngleton = xts(angletonEvap$EVAP, angletonEvapIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(evapAngleton)) > 0){
  evapAngleton = na.approx(evapAngleton)
} else {
  print("no NA's")
}

plot(evapAngleton)

#plot acf and pacf of the timeseries
acfAngletonEvap = acf(evapAngleton, lag.max = 365)
pacfAngletonEvap = pacf(evapAngleton)

#make a ts object out of the xts object
start = as.Date("1953-01-01") 
evapAngleton2 = ts(angletonEvap$EVAP, start=start, frequency = 365.25)
evapAngleton2 = na.approx(evapAngleton2)
stlAngletonEvap = stl(evapAngleton2, s.window="periodic")
trendEvapAngleton = stlAngletonEvap$time.series[,2]

#forecast
arimaEvapAngleton = auto.arima(evapAngleton)
fcastEvapAngleton = forecast(arimaEvapAngleton, h=100)
plot(fcastEvapAngleton, include=500)

meanEvapAngleton = mean(evapAngleton)
sdEvapAngleton = sd(evapAngleton)
minEvapAngleton = min(evapAngleton)
maxEvapAngleton = max(evapAngleton)
varEvapAngleton = var(evapAngleton)




# Benbrook Dam, Texas in the North East near Dallas
# Evaporation timeseries for Benbrook in the NorthEast of Texas
benbrookEvap = read_xlsx("timeseries/Benbrook, NE/Benbrook, Evap.xlsx")

benbrookEvapIndex = (1979 + benbrookEvap$DATE)
benbrookEvapIndex.date = as.Date(benbrookEvapIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeBenbrookEvap = seq(min(benbrookEvapIndex.date), max(benbrookEvapIndex.date), by = 1)
allDatesBenbrookEvap = data.frame(DateRangeBenbrookEvap)
allDatesBenbrookEvap = rename(allDatesBenbrookEvap, replace = c("DateRangeBenbrookEvap" = "DATE"))
benbrookEvap = merge(benbrookEvap, allDatesBenbrookEvap, all=TRUE)
benbrookEvap = benbrookEvap[!duplicated(benbrookEvap$DATE),]

#create an xts object out of the data frame for temperature
benbrookEvapIndex = (1979 + benbrookEvap$DATE)
benbrookEvapIndex.date = as.Date(benbrookEvapIndex)
evapBenbrook = xts(benbrookEvap$EVAP, benbrookEvapIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(evapBenbrook)) > 0){
  evapBenbrook = na.approx(evapBenbrook)
} else {
  print("no NA's")
}

plot(evapBenbrook)

#plot acf and pacf of the timeseries
acfBenbrookEvap = acf(evapBenbrook, lag.max = 365)
pacfBenbrookEvap = pacf(evapBenbrook)

#make a ts object out of the xts object
start = as.Date("1979-01-18") 
evapBenbrook2 = ts(benbrookEvap$EVAP, start=start, frequency = 365.25)
evapBenbrook2 = na.approx(evapBenbrook2)
stlBenbrookEvap = stl(evapBenbrook2, s.window="periodic")
trendEvapBenbrook = stlBenbrookEvap$time.series[,2]

#forecast
arimaEvapBenbrook = auto.arima(evapBenbrook)
fcastEvapBenbrook = forecast(arimaEvapBenbrook, h=100)
plot(fcastEvapBenbrook, include=500)

meanEvapBenbrook = mean(evapBenbrook)
sdEvapBenbrook = sd(evapBenbrook)
minEvapBenbrook = min(evapBenbrook)
maxEvapBenbrook = max(evapBenbrook)
varEvapBenbrook = var(evapBenbrook)



# Fort Stockton, Texas in the North West near the border to New Mexico
# Evaporation timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonEvap = read_xlsx("timeseries/FortStockton, NW/FortStockton, Evap.xlsx")

fortStocktonEvapIndex = (1950 + fortStocktonEvap$DATE)
fortStocktonEvapIndex.date = as.Date(fortStocktonEvapIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangefortStocktonEvap = seq(min(fortStocktonEvapIndex.date), max(fortStocktonEvapIndex.date), by = 1)
allDatesfortStocktonEvap = data.frame(DateRangefortStocktonEvap)
allDatesfortStocktonEvap = rename(allDatesfortStocktonEvap, replace = c("DateRangefortStocktonEvap" = "DATE"))
fortStocktonEvap = merge(fortStocktonEvap, allDatesfortStocktonEvap, all=TRUE)
fortStocktonEvap = fortStocktonEvap[!duplicated(fortStocktonEvap$DATE),]

#create an xts object out of the data frame for temperature
fortStocktonEvapIndex = (1950 + fortStocktonEvap$DATE)
fortStocktonEvapIndex.date = as.Date(fortStocktonEvapIndex)
evapfortStockton = xts(fortStocktonEvap$EVAP, fortStocktonEvapIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(evapfortStockton)) > 0){
  evapfortStockton = na.approx(evapfortStockton)
} else {
  print("no NA's")
}

plot(evapfortStockton)

#plot acf and pacf of the timeseries
acffortStocktonEvap = acf(evapfortStockton, lag.max = 365)
pacffortStocktonEvap = pacf(evapfortStockton)

#make a ts object out of the xts object
start = as.Date("1950-01-01") 
evapfortStockton2 = ts(fortStocktonEvap$EVAP, start=start, frequency = 365.25)
evapfortStockton2 = na.approx(evapfortStockton2)
stlfortStocktonEvap = stl(evapfortStockton2, s.window="periodic")
trendEvapfortStockton = stlfortStocktonEvap$time.series[,2]

#forecast
arimaEvapfortStockton = auto.arima(evapfortStockton)
fcastEvapfortStockton = forecast(arimaEvapfortStockton, h=100)
plot(fcastEvapfortStockton, include=500)

meanEvapfortStockton = mean(evapfortStockton)
sdEvapfortStockton = sd(evapfortStockton)
minEvapfortStockton = min(evapfortStockton)
maxEvapfortStockton = max(evapfortStockton)
varEvapfortStockton = var(evapfortStockton)



# Mccook, Texas in the South West near the coast of the Golf of Mexico and the Mexican border
# Evaporation timeseries for Mccook in the South West of Texas
mccookEvap = read_xlsx("timeseries/Mccook, SW/Mccook, Evap.xlsx")

mccookEvapIndex = (1963 + mccookEvap$DATE)
mccookEvapIndex.date = as.Date(mccookEvapIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeMccookEvap = seq(min(mccookEvapIndex.date), max(mccookEvapIndex.date), by = 1)
allDatesMccookEvap = data.frame(DateRangeMccookEvap)
allDatesMccookEvap = rename(allDatesMccookEvap, replace = c("DateRangeMccookEvap" = "DATE"))
mccookEvap = merge(mccookEvap, allDatesMccookEvap, all=TRUE)
mccookEvap = mccookEvap[!duplicated(mccookEvap$DATE),]

#create an xts object out of the data frame for temperature
mccookEvapIndex = (1963 + mccookEvap$DATE)
mccookEvapIndex.date = as.Date(mccookEvapIndex)
evapMccook = xts(mccookEvap$EVAP, mccookEvapIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(evapMccook)) > 0){
  evapMccook = na.approx(evapMccook)
} else {
  print("no NA's")
}

plot(evapMccook)

#plot acf and pacf of the timeseries
acfMccookEvap = acf(evapMccook, lag.max = 365)
pacfMccookEvap = pacf(evapMccook)

#make a ts object out of the xts object
start = as.Date("1963-09-01") 
evapMccook2 = ts(mccookEvap$EVAP, start=start, frequency = 365.25)
evapMccook2 = na.approx(evapMccook2)
stlMccookEvap = stl(evapMccook2, s.window="periodic")
trendEvapMccook = stlMccookEvap$time.series[,2]

#forecast
arimaEvapMccook = auto.arima(evapMccook)
fcastEvapMccook = forecast(arimaEvapMccook, h=100)
plot(fcastEvapMccook, include=500)

meanEvapMccook = mean(evapMccook)
sdEvapMccook = sd(evapMccook)
minEvapMccook = min(evapMccook)
maxEvapMccook = max(evapMccook)
varEvapMccook = var(evapMccook)


#boxplot of temperature timeseries
combineEvapTS = merge(evapAngleton, evapBenbrook, evapfortStockton, evapMccook)
combineEvapTS = rename(combineEvapTS, replace=c("evapAngleton" = "Angleton", "evapBenbrook" = "Benbrook", "evapfortStockton" = "Fort Stockton", "evapMccook" = "Mccook"))
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
linTrendAngletonEvap$coefficients[2]
plot(trendEvapBenbrook)
abline(linTrendBenbrookEvap, col="red")
linTrendBenbrookEvap$coefficients[2]
plot(trendEvapfortStockton)
abline(linTrendfortStocktonEvap, col="red")
linTrendfortStocktonEvap$coefficients[2]
plot(trendEvapMccook)
abline(linTrendMccookEvap, col="red")
linTrendMccookEvap$coefficients[2]
