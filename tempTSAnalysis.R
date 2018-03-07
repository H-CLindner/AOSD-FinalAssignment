#script for the analysis of temperature time series

library("readxl")
library("xts")
library("maps")
library("maptools")
library("mapdata")
library("plyr")
library("forecast")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

# Angleton, Texas in the South East near Houston
# Temperature timeseries for Angleton in the SouthEast of Texas
angletonTemp = read_xlsx("timeseries/Angleton, SE/Angleton, TMax.xlsx")

angletonTempIndex = (1950 + angletonTemp$DATE)
angletonTempIndex.date = as.Date(angletonTempIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeAngletonTemp = seq(min(angletonTempIndex.date), max(angletonTempIndex.date), by = 1)
allDatesAngletonTemp = data.frame(DateRangeAngletonTemp)
allDatesAngletonTemp = rename(allDatesAngletonTemp, replace = c("DateRangeAngletonTemp" = "DATE"))
angletonTemp = merge(angletonTemp, allDatesAngletonTemp, all=TRUE)
angletonTemp = angletonTemp[!duplicated(angletonTemp$DATE),]

#create an xts object out of the data frame for temperature
angletonTempIndex = (1950 + angletonTemp$DATE)
angletonTempIndex.date = as.Date(angletonTempIndex)
tmpMaxAngleton = xts(angletonTemp$TMAX, angletonTempIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(tmpMaxAngleton)) > 0){
  tmpMaxAngleton = na.approx(tmpMaxAngleton)
} else {
  print("no NA's")
}

plot(tmpMaxAngleton)

#plot acf and pacf of the timeseries
acfAngletonTemp = acf(tmpMaxAngleton, lag.max = 365)
pacfAngletonTemp = pacf(tmpMaxAngleton)

#Holt-Winters of the time series
#hw = HoltWinters(tmpMaxAngleton, gamma=FALSE)

#dtw for comparison and maybe simple statistical values and leave forecast out because it makes no sense

#make a ts object out of the xts object
start = as.Date("1950-01-01") 
tmpMaxAngleton2 = ts(angletonTemp$TMAX, start=start, frequency = 365.25)
tmpMaxAngleton2 = na.approx(tmpMaxAngleton2)
stlAngletonTemp = stl(tmpMaxAngleton2, s.window="periodic")
trendTmpAngleton = stlAngletonTemp$time.series[,2]

#forecast
arimaTempAngleton = auto.arima(tmpMaxAngleton)
fcastTempAngleton = forecast(arimaTempAngleton, h=100)
plot(fcastTempAngleton, include=500)

meanTempAngleton = mean(tmpMaxAngleton)
sdTempAngleton = sd(tmpMaxAngleton)
minTempAngleton = min(tmpMaxAngleton)
maxTempAngleton = max(tmpMaxAngleton)
varTempAngleton = var(tmpMaxAngleton)

# Benbrook Dam, Texas in the North East near Dallas
# Temperature timeseries for Benbrook in the NorthEast of Texas
benbrookTemp = read_xlsx("timeseries/Benbrook, NE/Benbrook, TMax.xlsx")

benbrookTempIndex = (1950 + benbrookTemp$DATE)
benbrookTempIndex.date = as.Date(benbrookTempIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeBenbrookTemp = seq(min(benbrookTempIndex.date), max(benbrookTempIndex.date), by = 1)
allDatesBenbrookTemp = data.frame(DateRangeBenbrookTemp)
allDatesBenbrookTemp = rename(allDatesBenbrookTemp, replace = c("DateRangeBenbrookTemp" = "DATE"))
benbrookTemp = merge(benbrookTemp, allDatesBenbrookTemp, all=TRUE)
benbrookTemp = benbrookTemp[!duplicated(benbrookTemp$DATE),]

benbrookTempIndex = (1950 + benbrookTemp$DATE)
benbrookTempIndex.date = as.Date(benbrookTempIndex)
tmpMaxBenbrook = xts(benbrookTemp$TMAX, benbrookTempIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(tmpMaxBenbrook)) > 0){
  tmpMaxBenbrook = na.approx(tmpMaxBenbrook)
} else {
  print("no NA's")
}

plot(tmpMaxBenbrook)

start = as.Date("1950-07-03")
tmpMaxBenbrook2 = ts(benbrookTemp$TMAX, start=start, frequency = 365.25)
tmpMaxBenbrook2 = na.approx(tmpMaxBenbrook2)
stlBenbrookTemp = stl(tmpMaxBenbrook2, s.window="periodic")
trendTmpBenbrook = stlBenbrookTemp$time.series[,2]

#forecast
arimaTempBenbrook = auto.arima(tmpMaxBenbrook)
fcastTempBenbrook = forecast(arimaTempBenbrook, h=100)
plot(fcastTempBenbrook, include=500)

meanTempBenbrook = mean(tmpMaxBenbrook)
sdTempBenbrook = sd(tmpMaxBenbrook)
minTempBenbrook = min(tmpMaxBenbrook)
maxTempBenbrook = max(tmpMaxBenbrook)
varTempBenbrook = var(tmpMaxBenbrook)

# Fort Stockton, Texas in the North West near the border to New Mexico
# Temperature timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonTemp = read_xlsx("timeseries/FortStockton, NW/FortStockton, TMax.xlsx")

fortStocktonTempIndex = (1950 + fortStocktonTemp$DATE)
fortStocktonTempIndex.date = as.Date(fortStocktonTempIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangefortStocktonTemp = seq(min(fortStocktonTempIndex.date), max(fortStocktonTempIndex.date), by = 1)
allDatesfortStocktonTemp = data.frame(DateRangefortStocktonTemp)
allDatesfortStocktonTemp = rename(allDatesfortStocktonTemp, replace = c("DateRangefortStocktonTemp" = "DATE"))
fortStocktonTemp = merge(fortStocktonTemp, allDatesfortStocktonTemp, all=TRUE)
fortStocktonTemp = fortStocktonTemp[!duplicated(fortStocktonTemp$DATE),]

fortStocktonTempIndex = (1950 + fortStocktonTemp$DATE)
fortStocktonTempIndex.date = as.Date(fortStocktonTempIndex)
tmpMaxfortStockton = xts(fortStocktonTemp$TMAX, fortStocktonTempIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(tmpMaxfortStockton)) > 0){
  tmpMaxfortStockton = na.approx(tmpMaxfortStockton)
} else {
  print("no NA's")
}

plot(tmpMaxfortStockton)

start = as.Date("1950-01-01")
tmpMaxfortStockton2 = ts(fortStocktonTemp$TMAX, start=start, frequency = 365.25)
tmpMaxfortStockton2 = na.approx(tmpMaxfortStockton2)
stlfortStocktonTemp = stl(tmpMaxfortStockton2, s.window="periodic")
trendTmpfortStockton = stlfortStocktonTemp$time.series[,2]

#forecast
arimaTempfortStockton = auto.arima(tmpMaxfortStockton)
fcastTempfortStockton = forecast(arimaTempfortStockton, h=100)
plot(fcastTempfortStockton, include=500)

meanTempfortStockton = mean(tmpMaxfortStockton)
sdTempfortStockton = sd(tmpMaxfortStockton)
minTempfortStockton = min(tmpMaxfortStockton)
maxTempfortStockton = max(tmpMaxfortStockton)
varTempfortStockton = var(tmpMaxfortStockton)

# Mccook, Texas in the South West near the coast of the Golf of Mexico and the Mexican border
# Temperature timeseries for Mccook in the South West of Texas
MccookTemp = read_xlsx("timeseries/Mccook, SW/Mccook, TMax.xlsx")

MccookTempIndex = (1950 + MccookTemp$DATE)
MccookTempIndex.date = as.Date(MccookTempIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeMccookTemp = seq(min(MccookTempIndex.date), max(MccookTempIndex.date), by = 1)
allDatesMccookTemp = data.frame(DateRangeMccookTemp)
allDatesMccookTemp = rename(allDatesMccookTemp, replace = c("DateRangeMccookTemp" = "DATE"))
MccookTemp = merge(MccookTemp, allDatesMccookTemp, all=TRUE)
MccookTemp = MccookTemp[!duplicated(MccookTemp$DATE),]

MccookTempIndex = (1950 + MccookTemp$DATE)
MccookTempIndex.date = as.Date(MccookTempIndex)
tmpMaxMccook = xts(MccookTemp$TMAX, MccookTempIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(tmpMaxMccook)) > 0){
  tmpMaxMccook = na.approx(tmpMaxMccook)
} else {
  print("no NA's")
}

plot(tmpMaxMccook)

start = as.Date("1950-01-01")
tmpMaxMccook2 = ts(MccookTemp$TMAX, start=start, frequency = 365.25)
tmpMaxMccook2 = na.approx(tmpMaxMccook2)
stlMccookTemp = stl(tmpMaxMccook2, s.window="periodic")
trendTmpMccook = stlMccookTemp$time.series[,2]

#forecast
arimaTempMccook = auto.arima(tmpMaxMccook)
fcastTempMccook = forecast(arimaTempMccook, h=100)
plot(fcastTempMccook, include=500)

meanTempMccook = mean(tmpMaxMccook)
sdTempMccook = sd(tmpMaxMccook)
minTempMccook = min(tmpMaxMccook)
maxTempMccook = max(tmpMaxMccook)
varTempMccook = var(tmpMaxMccook)

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
linTrendAngletonTemp$coefficients[2]
plot(trendTmpBenbrook)
abline(linTrendBenbrookTemp, col="red")
linTrendBenbrookTemp$coefficients[2]
plot(trendTmpfortStockton)
abline(linTrendfortStocktonTemp, col="red")
linTrendfortStocktonTemp$coefficients[2]
plot(trendTmpMccook)
abline(linTrendMccookTemp, col="red")
linTrendMccookTemp$coefficients[2]
