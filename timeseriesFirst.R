library("readxl")
library("xts")
library("maps")
library("maptools")
library("mapdata")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

# Angleton, Texas in the South East near Houston
# Temperature timeseries for Angleton in the SouthEast of Texas
angletonTemp = read_xlsx("timeseries/Angleton, SE/Angleton, TMax.xlsx")

angletonTempIndex = (1950 + angletonTemp$DATE)
angletonTempIndex.date = as.Date(angletonTempIndex)
tmpMaxAngleton = xts(angletonTemp$TMAX, angletonTempIndex.date)

plot(tmpMaxAngleton)

# Evaporation timeseries for Angleton in the SouthEast of Texas
angletonEvap = read_xlsx("timeseries/Angleton, SE/Angleton, Evap.xlsx")

angletonEvapIndex = (1953 + angletonEvap$DATE)
angletonEvapIndex.date = as.Date(angletonEvapIndex)
evapAngleton = xts(angletonEvap$EVAP, angletonEvapIndex.date)

plot(evapAngleton)

# Precipitation timeseries for Angleton in the SouthEast of Texas
angletonPrcp = read_xlsx("timeseries/Angleton, SE/Angleton, Prcp.xlsx")

angletonPrcpIndex = (1953 + angletonPrcp$DATE)
angletonPrcpIndex.date = as.Date(angletonPrcpIndex)
prcpAngleton = xts(angletonPrcp$PRCP, angletonPrcpIndex.date)

plot(prcpAngleton)

angletonLocations = angletonEvap[c(2,3,4)]
angletonLocations = angletonLocations[!duplicated(angletonLocations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(angletonLocations) = angletonLocations[c(3,2)]

# Benbrook Dam, Texas in the North East near Dallas
# Temperature timeseries for Benbrook in the NorthEast of Texas
benbrookTemp = read_xlsx("timeseries/Benbrook, NE/Benbrook, TMax.xlsx")

benbrookTempIndex = (1950 + benbrookTemp$DATE)
benbrookTempIndex.date = as.Date(benbrookTempIndex)
tmpMaxBenbrook = xts(benbrookTemp$TMAX, benbrookTempIndex.date)

plot(tmpMaxBenbrook)

# Evaporation timeseries for Benbrook in the NorthEast of Texas
benbrookEvap = read_xlsx("timeseries/Benbrook, NE/Benbrook, Evap.xlsx")

benbrookEvapIndex = (1950 + benbrookEvap$DATE)
benbrookEvapIndex.date = as.Date(benbrookEvapIndex)
evapBenbrook = xts(benbrookEvap$EVAP, benbrookEvapIndex.date)

plot(evapBenbrook)

# Precipitation timeseries for Benbrook in the NorthEast of Texas
benbrookPrcp = read_xlsx("timeseries/Benbrook, NE/Benbrook, Prcp.xlsx")

benbrookPrcpIndex = (1950 + benbrookPrcp$DATE)
benbrookPrcpIndex.date = as.Date(benbrookPrcpIndex)
PrcpBenbrook = xts(benbrookPrcp$PRCP, benbrookPrcpIndex.date)

plot(PrcpBenbrook)

benbrookLocations = benbrookEvap[c(2,3,4)]
benbrookLocations = benbrookLocations[!duplicated(benbrookLocations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(benbrookLocations) = benbrookLocations[c(3,2)]

# Fort Stockton, Texas in the North West near the border to New Mexico
# Temperature timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonTemp = read_xlsx("timeseries/FortStockton, NW/FortStockton, TMax.xlsx")

fortStocktonTempIndex = (1950 + fortStocktonTemp$DATE)
fortStocktonTempIndex.date = as.Date(fortStocktonTempIndex)
tmpMaxFortStockton = xts(fortStocktonTemp$TMAX, fortStocktonTempIndex.date)

plot(tmpMaxFortStockton)

# Evaporation timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonEvap = read_xlsx("timeseries/FortStockton, NW/FortStockton, Evap.xlsx")

fortStocktonEvapIndex = (1950 + fortStocktonEvap$DATE)
fortStocktonEvapIndex.date = as.Date(fortStocktonEvapIndex)
evapFortStockton = xts(fortStocktonEvap$EVAP, fortStocktonEvapIndex.date)

plot(evapFortStockton)

# Precipitation timeseries for Fort Stockton in the NorthWest of Texas
fortStocktonPrcp = read_xlsx("timeseries/FortStockton, NW/FortStockton, Prcp.xlsx")

fortStocktonPrcpIndex = (1950 + fortStocktonPrcp$DATE)
fortStocktonPrcpIndex.date = as.Date(fortStocktonPrcpIndex)
PrcpFortStockton = xts(fortStocktonPrcp$PRCP, fortStocktonPrcpIndex.date)

plot(PrcpFortStockton)

fortStocktonLocations = fortStocktonEvap[c(2,3,4)]
fortStocktonLocations = fortStocktonLocations[!duplicated(fortStocktonLocations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(fortStocktonLocations) = fortStocktonLocations[c(3,2)]

# Mccook, Texas in the South West near the coast of the Golf of Mexico and the Mexican border
# Temperature timeseries for Mccook in the NorthWest of Texas
mccookTemp = read_xlsx("timeseries/Mccook, SW/Mccook, TMax.xlsx")

mccookTempIndex = (1950 + mccookTemp$DATE)
mccookTempIndex.date = as.Date(mccookTempIndex)
tmpMaxMccook = xts(mccookTemp$TMAX, mccookTempIndex.date)

plot(tmpMaxMccook)

# Evaporation timeseries for Mccook in the NorthWest of Texas
mccookEvap = read_xlsx("timeseries/Mccook, SW/Mccook, Evap.xlsx")

mccookEvapIndex = (1950 + mccookEvap$DATE)
mccookEvapIndex.date = as.Date(mccookEvapIndex)
evapMccook = xts(mccookEvap$EVAP, mccookEvapIndex.date)

plot(evapMccook)

# Precipitation timeseries for Mccook in the NorthWest of Texas
mccookPrcp = read_xlsx("timeseries/Mccook, SW/Mccook, Prcp.xlsx")

mccookPrcpIndex = (1950 + mccookPrcp$DATE)
mccookPrcpIndex.date = as.Date(mccookPrcpIndex)
PrcpMccook = xts(mccookPrcp$PRCP, mccookPrcpIndex.date)

plot(PrcpMccook)

mccookLocations = mccookEvap[c(2,3,4)]
mccookLocations = mccookLocations[!duplicated(mccookLocations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(mccookLocations) = mccookLocations[c(3,2)]

map('state', 'texas')
plot(angletonLocations, add=TRUE, pch=15, col="blue")
plot(benbrookLocations, add=TRUE, pch=15, col="blue")
plot(fortStocktonLocations, add=TRUE, pch=15, col="blue")
plot(mccookLocations, add=TRUE, pch=15, col="blue")