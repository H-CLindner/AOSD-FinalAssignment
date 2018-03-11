#script for the analysis of wind time series

library("readxl")
library("xts")
library("maps")
library("maptools")
library("mapdata")
library("plyr")
library("forecast")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

# Angleton, Texas in the South East near Houston
# Wind timeseries for Angleton in the SouthEast of Texas
angletonWind = read_xlsx("timeseries/Angleton, SE/Angleton, Wind.xlsx")

angletonWindIndex = (1953 + angletonWind$DATE)
angletonWindIndex.date = as.Date(angletonWindIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeAngletonWind = seq(min(angletonWindIndex.date), max(angletonWindIndex.date), by = 1)
allDatesAngletonWind = data.frame(DateRangeAngletonWind)
allDatesAngletonWind = rename(allDatesAngletonWind, replace = c("DateRangeAngletonWind" = "DATE"))
angletonWind = merge(angletonWind, allDatesAngletonWind, all=TRUE)
angletonWind = angletonWind[!duplicated(angletonWind$DATE),]

#create an xts object out of the data frame for Winderature
angletonWindIndex = (1953 + angletonWind$DATE)
angletonWindIndex.date = as.Date(angletonWindIndex)
WindAngleton = xts(angletonWind$WDMV, angletonWindIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(WindAngleton)) > 0){
  WindAngleton = na.approx(WindAngleton)
} else {
  print("no NA's")
}

plot(WindAngleton)

#plot acf and pacf of the timeseries
acfAngletonWind = acf(WindAngleton, lag.max = 365)
pacfAngletonWind = pacf(WindAngleton)

#Holt-Winters of the time series
#hw = HoltWinters(tmpMaxAngleton, gamma=FALSE)

#dtw for comparison and maybe simple statistical values and leave forecast out because it makes no sense

#make a ts object out of the xts object
start = as.Date("1953-01-01") 
WindAngleton2 = ts(angletonWind$WDMV, start=start, frequency = 365.25)
WindAngleton2 = na.approx(WindAngleton2)
stlAngletonWind = stl(WindAngleton2, s.window="periodic")
trendWindAngleton = stlAngletonWind$time.series[,2]

#forecast
arimaWindAngleton = auto.arima(WindAngleton)
fcastWindAngleton = forecast(arimaWindAngleton, h=100)
plot(fcastWindAngleton, include=500)

meanWindAngleton = mean(WindAngleton)
sdWindAngleton = sd(WindAngleton)
minWindAngleton = min(WindAngleton)
maxWindAngleton = max(WindAngleton)
varWindAngleton = var(WindAngleton)




# Benbrook Dam, Texas in the North East near Dallas
# Precipitation timeseries for Benbrook in the NorthEast of Texas
benbrookWind = read_xlsx("timeseries/Benbrook, NE/Benbrook, Wind.xlsx")

benbrookWindIndex = (1979 + benbrookWind$DATE)
benbrookWindIndex.date = as.Date(benbrookWindIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeBenbrookWind = seq(min(benbrookWindIndex.date), max(benbrookWindIndex.date), by = 1)
allDatesBenbrookWind = data.frame(DateRangeBenbrookWind)
allDatesBenbrookWind = rename(allDatesBenbrookWind, replace = c("DateRangeBenbrookWind" = "DATE"))
benbrookWind = merge(benbrookWind, allDatesBenbrookWind, all=TRUE)
benbrookWind = benbrookWind[!duplicated(benbrookWind$DATE),]

benbrookWindIndex = (1979 + benbrookWind$DATE)
benbrookWindIndex.date = as.Date(benbrookWindIndex)
WindBenbrook = xts(benbrookWind$WDMV, benbrookWindIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(WindBenbrook)) > 0){
  WindBenbrook = na.approx(WindBenbrook)
} else {
  print("no NA's")
}

plot(WindBenbrook)

start = as.Date("1979-01-01")
WindBenbrook2 = ts(benbrookWind$WDMV, start=start, frequency = 365.25)
WindBenbrook2 = na.approx(WindBenbrook2)
stlBenbrookWind = stl(WindBenbrook2, s.window="periodic")
trendWindBenbrook = stlBenbrookWind$time.series[,2]

#forecast
arimaWindBenbrook = auto.arima(WindBenbrook)
fcastWindBenbrook = forecast(arimaWindBenbrook, h=100)
plot(fcastWindBenbrook, include=500)

meanWindBenbrook = mean(WindBenbrook)
sdWindBenbrook = sd(WindBenbrook)
minWindBenbrook = min(WindBenbrook)
maxWindBenbrook = max(WindBenbrook)
varWindBenbrook = var(WindBenbrook)



# Fort Stockton, Texas in the North West near the border to New Mexico
# Precipitation timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonWind = read_xlsx("timeseries/FortStockton, NW/FortStockton, Wind.xlsx")

fortStocktonWindIndex = (1950 + fortStocktonWind$DATE)
fortStocktonWindIndex.date = as.Date(fortStocktonWindIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangefortStocktonWind = seq(min(fortStocktonWindIndex.date), max(fortStocktonWindIndex.date), by = 1)
allDatesfortStocktonWind = data.frame(DateRangefortStocktonWind)
allDatesfortStocktonWind = rename(allDatesfortStocktonWind, replace = c("DateRangefortStocktonWind" = "DATE"))
fortStocktonWind = merge(fortStocktonWind, allDatesfortStocktonWind, all=TRUE)
fortStocktonWind = fortStocktonWind[!duplicated(fortStocktonWind$DATE),]

fortStocktonWindIndex = (1950 + fortStocktonWind$DATE)
fortStocktonWindIndex.date = as.Date(fortStocktonWindIndex)
WindfortStockton = xts(fortStocktonWind$WDMV, fortStocktonWindIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(WindfortStockton)) > 0){
  WindfortStockton = na.approx(WindfortStockton)
} else {
  print("no NA's")
}

plot(WindfortStockton)

start = as.Date("1950-01-01")
WindfortStockton2 = ts(fortStocktonWind$WDMV, start=start, frequency = 365.25)
WindfortStockton2 = na.approx(WindfortStockton2)
stlfortStocktonWind = stl(WindfortStockton2, s.window="periodic")
trendWindfortStockton = stlfortStocktonWind$time.series[,2]

#forecast
arimaWindfortStockton = auto.arima(WindfortStockton)
fcastWindfortStockton = forecast(arimaWindfortStockton, h=100)
plot(fcastWindfortStockton, include=500)

meanWindfortStockton = mean(WindfortStockton)
sdWindfortStockton = sd(WindfortStockton)
minWindfortStockton = min(WindfortStockton)
maxWindfortStockton = max(WindfortStockton)
varWindfortStockton = var(WindfortStockton)



# Mccook, Texas in the South West near the coast of the Golf of Mexico and the Mexican border
# Precipitation timeseries for Mccook in the South West of Texas
MccookWind = read_xlsx("timeseries/Mccook, SW/Mccook, Wind.xlsx")

MccookWindIndex = (1963 + MccookWind$DATE)
MccookWindIndex.date = as.Date(MccookWindIndex)

#to be sure that there are no missing dates in the data frame, take the first and last entry and merge the data frames; fill missing dates with NA and interpolate later
DateRangeMccookWind = seq(min(MccookWindIndex.date), max(MccookWindIndex.date), by = 1)
allDatesMccookWind = data.frame(DateRangeMccookWind)
allDatesMccookWind = rename(allDatesMccookWind, replace = c("DateRangeMccookWind" = "DATE"))
MccookWind = merge(MccookWind, allDatesMccookWind, all=TRUE)
MccookWind = MccookWind[!duplicated(MccookWind$DATE),]

MccookWindIndex = (1963 + MccookWind$DATE)
MccookWindIndex.date = as.Date(MccookWindIndex)
WindMccook = xts(MccookWind$WDMV, MccookWindIndex.date)

#if there are NA's, interpolate their value
if(sum(is.na(WindMccook)) > 0){
  WindMccook = na.approx(WindMccook)
} else {
  print("no NA's")
}

plot(WindMccook)

start = as.Date("1963-01-01")
WindMccook2 = ts(MccookWind$WDMV, start=start, frequency = 365.25)
WindMccook2 = na.approx(WindMccook2)
stlMccookWind = stl(WindMccook2, s.window="periodic")
trendWindMccook = stlMccookWind$time.series[,2]

#forecast
arimaWindMccook = auto.arima(WindMccook)
fcastWindMccook = forecast(arimaWindMccook, h=100)
plot(fcastWindMccook, include=500)

meanWindMccook = mean(WindMccook)
sdWindMccook = sd(WindMccook)
minWindMccook = min(WindMccook)
maxWindMccook = max(WindMccook)
varWindMccook = var(WindMccook)

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
linTrendAngletonWind$coefficients[2]
plot(trendWindBenbrook)
abline(linTrendBenbrookWind, col="red")
linTrendBenbrookWind$coefficients[2]
plot(trendWindfortStockton)
abline(linTrendfortStocktonWind, col="red")
linTrendfortStocktonWind$coefficients[2]
plot(trendWindMccook)
abline(linTrendMccookWind, col="red")
linTrendMccookWind$coefficients[2]
