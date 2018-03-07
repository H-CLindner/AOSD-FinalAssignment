#script for the analysis of the precipitation time series

library("readxl")
library("xts")
library("maps")
library("maptools")
library("mapdata")
library("plyr")
library("forecast")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

# Angleton, Texas in the South East near Houston
# Precipitation timeseries for Angleton in the SouthEast of Texas
angletonPrcp = read_xlsx("timeseries/Angleton, SE/Angleton, Prcp.xlsx")

angletonPrcpIndex = (1950 + angletonPrcp$DATE)
angletonPrcpIndex.date = as.Date(angletonPrcpIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeAngletonPrcp = seq(min(angletonPrcpIndex.date), max(angletonPrcpIndex.date), by = 1)
allDatesAngletonPrcp = data.frame(DateRangeAngletonPrcp)
allDatesAngletonPrcp = rename(allDatesAngletonPrcp, replace = c("DateRangeAngletonPrcp" = "DATE"))
angletonPrcp = merge(angletonPrcp, allDatesAngletonPrcp, all=TRUE)
angletonPrcp = angletonPrcp[!duplicated(angletonPrcp$DATE),]

#create an xts object out of the data frame for Prcperature
angletonPrcpIndex = (1950 + angletonPrcp$DATE)
angletonPrcpIndex.date = as.Date(angletonPrcpIndex)
prcpAngleton = xts(angletonPrcp$PRCP, angletonPrcpIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(prcpAngleton)) > 0){
  prcpAngleton = na.approx(prcpAngleton)
} else {
  print("no NA's")
}

plot(prcpAngleton)

#plot acf and pacf of the timeseries
acfAngletonPrcp = acf(prcpAngleton, lag.max = 365)
pacfAngletonPrcp = pacf(prcpAngleton)

#Holt-Winters of the time series
#hw = HoltWinters(tmpMaxAngleton, gamma=FALSE)

#dtw for comparison and maybe simple statistical values and leave forecast out because it makes no sense

#make a ts object out of the xts object
start = as.Date("1950-01-01") 
prcpAngleton2 = ts(angletonPrcp$PRCP, start=start, frequency = 365.25)
prcpAngleton2 = na.approx(prcpAngleton2)
stlAngletonPrcp = stl(prcpAngleton2, s.window="periodic")
trendPrcpAngleton = stlAngletonPrcp$time.series[,2]

#forecast
arimaPrcpAngleton = auto.arima(prcpAngleton)
fcastPrcpAngleton = forecast(arimaPrcpAngleton, h=100)
plot(fcastPrcpAngleton, include=500)

meanPrcpAngleton = mean(prcpAngleton)
sdPrcpAngleton = sd(prcpAngleton)
minPrcpAngleton = min(prcpAngleton)
maxPrcpAngleton = max(prcpAngleton)
varPrcpAngleton = var(prcpAngleton)



# Benbrook Dam, Texas in the North East near Dallas
# Precipitation timeseries for Benbrook in the NorthEast of Texas
benbrookPrcp = read_xlsx("timeseries/Benbrook, NE/Benbrook, Prcp.xlsx")

benbrookPrcpIndex = (1950 + benbrookPrcp$DATE)
benbrookPrcpIndex.date = as.Date(benbrookPrcpIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeBenbrookPrcp = seq(min(benbrookPrcpIndex.date), max(benbrookPrcpIndex.date), by = 1)
allDatesBenbrookPrcp = data.frame(DateRangeBenbrookPrcp)
allDatesBenbrookPrcp = rename(allDatesBenbrookPrcp, replace = c("DateRangeBenbrookPrcp" = "DATE"))
benbrookPrcp = merge(benbrookPrcp, allDatesBenbrookPrcp, all=TRUE)
benbrookPrcp = benbrookPrcp[!duplicated(benbrookPrcp$DATE),]

benbrookPrcpIndex = (1950 + benbrookPrcp$DATE)
benbrookPrcpIndex.date = as.Date(benbrookPrcpIndex)
prcpBenbrook = xts(benbrookPrcp$PRCP, benbrookPrcpIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(prcpBenbrook)) > 0){
  prcpBenbrook = na.approx(prcpBenbrook)
} else {
  print("no NA's")
}

plot(prcpBenbrook)

start = as.Date("1950-01-01")
prcpBenbrook2 = ts(benbrookPrcp$PRCP, start=start, frequency = 365.25)
prcpBenbrook2 = na.approx(prcpBenbrook2)
stlBenbrookPrcp = stl(prcpBenbrook2, s.window="periodic")
trendPrcpBenbrook = stlBenbrookPrcp$time.series[,2]

#forecast
arimaPrcpBenbrook = auto.arima(prcpBenbrook)
fcastPrcpBenbrook = forecast(arimaPrcpBenbrook, h=100)
plot(fcastPrcpBenbrook, include=500)

meanPrcpBenbrook = mean(prcpBenbrook)
sdPrcpBenbrook = sd(prcpBenbrook)
minPrcpBenbrook = min(prcpBenbrook)
maxPrcpBenbrook = max(prcpBenbrook)
varPrcpBenbrook = var(prcpBenbrook)



# Fort Stockton, Texas in the North West near the border to New Mexico
# Precipitation timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonPrcp = read_xlsx("timeseries/FortStockton, NW/FortStockton, Prcp.xlsx")

fortStocktonPrcpIndex = (1950 + fortStocktonPrcp$DATE)
fortStocktonPrcpIndex.date = as.Date(fortStocktonPrcpIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangefortStocktonPrcp = seq(min(fortStocktonPrcpIndex.date), max(fortStocktonPrcpIndex.date), by = 1)
allDatesfortStocktonPrcp = data.frame(DateRangefortStocktonPrcp)
allDatesfortStocktonPrcp = rename(allDatesfortStocktonPrcp, replace = c("DateRangefortStocktonPrcp" = "DATE"))
fortStocktonPrcp = merge(fortStocktonPrcp, allDatesfortStocktonPrcp, all=TRUE)
fortStocktonPrcp = fortStocktonPrcp[!duplicated(fortStocktonPrcp$DATE),]

fortStocktonPrcpIndex = (1950 + fortStocktonPrcp$DATE)
fortStocktonPrcpIndex.date = as.Date(fortStocktonPrcpIndex)
prcpfortStockton = xts(fortStocktonPrcp$PRCP, fortStocktonPrcpIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(prcpfortStockton)) > 0){
  prcpfortStockton = na.approx(prcpfortStockton)
} else {
  print("no NA's")
}

plot(prcpfortStockton)

start = as.Date("1950-01-01")
prcpfortStockton2 = ts(fortStocktonPrcp$PRCP, start=start, frequency = 365.25)
prcpfortStockton2 = na.approx(prcpfortStockton2)
stlfortStocktonPrcp = stl(prcpfortStockton2, s.window="periodic")
trendPrcpfortStockton = stlfortStocktonPrcp$time.series[,2]

#forecast
arimaPrcpfortStockton = auto.arima(prcpfortStockton)
fcastPrcpfortStockton = forecast(arimaPrcpfortStockton, h=100)
plot(fcastPrcpfortStockton, include=500)

meanPrcpfortStockton = mean(prcpfortStockton)
sdPrcpfortStockton = sd(prcpfortStockton)
minPrcpfortStockton = min(prcpfortStockton)
maxPrcpfortStockton = max(prcpfortStockton)
varPrcpfortStockton = var(prcpfortStockton)



# Mccook, Texas in the South West near the coast of the Golf of Mexico and the Mexican border
# Precipitation timeseries for Mccook in the South West of Texas
MccookPrcp = read_xlsx("timeseries/Mccook, SW/Mccook, Prcp.xlsx")

MccookPrcpIndex = (1950 + MccookPrcp$DATE)
MccookPrcpIndex.date = as.Date(MccookPrcpIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeMccookPrcp = seq(min(MccookPrcpIndex.date), max(MccookPrcpIndex.date), by = 1)
allDatesMccookPrcp = data.frame(DateRangeMccookPrcp)
allDatesMccookPrcp = rename(allDatesMccookPrcp, replace = c("DateRangeMccookPrcp" = "DATE"))
MccookPrcp = merge(MccookPrcp, allDatesMccookPrcp, all=TRUE)
MccookPrcp = MccookPrcp[!duplicated(MccookPrcp$DATE),]

MccookPrcpIndex = (1950 + MccookPrcp$DATE)
MccookPrcpIndex.date = as.Date(MccookPrcpIndex)
prcpMccook = xts(MccookPrcp$PRCP, MccookPrcpIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(prcpMccook)) > 0){
  prcpMccook = na.approx(prcpMccook)
} else {
  print("no NA's")
}

plot(prcpMccook)

start = as.Date("1950-01-01")
prcpMccook2 = ts(MccookPrcp$PRCP, start=start, frequency = 365.25)
prcpMccook2 = na.approx(prcpMccook2)
stlMccookPrcp = stl(prcpMccook2, s.window="periodic")
trendPrcpMccook = stlMccookPrcp$time.series[,2]

#forecast
arimaPrcpMccook = auto.arima(prcpMccook)
fcastPrcpMccook = forecast(arimaPrcpMccook, h=100)
plot(fcastPrcpMccook, include=500)

meanPrcpMccook = mean(prcpMccook)
sdPrcpMccook = sd(prcpMccook)
minPrcpMccook = min(prcpMccook)
maxPrcpMccook = max(prcpMccook)
varPrcpMccook = var(prcpMccook)


#boxplot of precipitation timeseries
combinePrcpTS = merge(prcpAngleton, prcpBenbrook, prcpfortStockton, prcpMccook)
combinePrcpTS = rename(combinePrcpTS, replace=c("prcpAngleton" = "Angleton", "prcpBenbrook" = "Benbrook", "prcpfortStockton" = "Fort Stockton", "prcpMccook" = "Mccook"))
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
linTrendAngletonPrcp$coefficients[2]
plot(trendPrcpBenbrook)
abline(linTrendBenbrookPrcp, col="red")
linTrendBenbrookPrcp$coefficients[2]
plot(trendPrcpfortStockton)
abline(linTrendfortStocktonPrcp, col="red")
linTrendfortStocktonPrcp$coefficients[2]
plot(trendPrcpMccook)
abline(linTrendMccookPrcp, col="red")
linTrendMccookPrcp$coefficients[2]
