# script for the analysis of the wind values of all stations in texas

library("readxl")
library("maps")
library("maptools")
library("mapdata")
library("xts")
library("plyr")
library("forecast")
library("gstat")
library("sp")
library("rgdal")
library("dplyr")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

computeWindAverage = function(dataFrame){
  dataFrameSubset = mutate(dataFrame, month = format(DATE, "%m"))
  dataFrameMean = group_by(dataFrameSubset, STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, month) %>% summarise(meanWind = mean(AWND, na.rm = T))
  dataFrameMean$meanWind = round(dataFrameMean$meanWind, digits=4)
  return(dataFrameMean)
}

coordinateTransformWind = function(dataSet){
  coordinates(dataSet) = dataSet[c(4,3)]
  proj4string(dataSet) = CRS("+init=epsg:4326")
  dataSet = spTransform(dataSet, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  return(dataSet)
}

readDataWind = function(path){
  rawData = read_xlsx(path)
  rawData$LATITUDE = as.numeric(rawData$LATITUDE)
  rawData$LONGITUDE = as.numeric(rawData$LONGITUDE)
  return(rawData)
}

texas = map('state', 'texas', fill=TRUE)
texas2 = data.frame(LONGITUDE = texas$x, LATITUDE = texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]
proj4string(texas2) = CRS("+init=epsg:4326")
texas2 = spTransform(texas2, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

texasGrid = spsample(texas2, type="regular")
tx = as.data.frame(coordinates(texasGrid))
names(tx) = c("LONGITUDE", "LATITUDE")
coordinates(tx) = tx[c(1,2)]
texasGrid = tx
proj4string(texasGrid) = CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

texasMap = map2SpatialPolygons(texas, ID=texas$names)
proj4string(texasMap) = CRS("+init=epsg:4326")
texasMap = spTransform(texasMap, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

                                      # JANUARY 2015 #

# Average wind speed values for January 2015 in Texas
JanWind2015 = readDataWind("2015/Jan/2015-Jan-Wind.xlsx")

#data frame with data for one day
JanWind2015Subset = computeWindAverage(JanWind2015)
JanWind2015Subset = coordinateTransformWind(JanWind2015Subset)

# variogram for the one day
vJanWind2015 = variogram(meanWind ~ LATITUDE+LONGITUDE, JanWind2015Subset)
plot(vJanWind2015)

#fitting variogram
vJanWind2015.fit = fit.variogram(vJanWind2015, vgm(1, 'Lin', 0))
plot(vJanWind2015 ,vJanWind2015.fit)

#kriging
JanWind2015krige1 = krige(meanWind ~ 1, JanWind2015Subset, texasGrid, vJanWind2015.fit)
spplot(JanWind2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







                                            # AUGUST 2015 #

# Average wind speed values for Auguary 2015 in Texas
AugWind2015 = readDataWind("2015/Aug/2015-Aug-Wind.xlsx")

#data frame with data for one day
AugWind2015Subset = computeWindAverage(AugWind2015)
AugWind2015Subset = coordinateTransformWind(AugWind2015Subset)

# variogram for the one day
vAugWind2015 = variogram(meanWind ~ LATITUDE+LONGITUDE, AugWind2015Subset)
plot(vAugWind2015)

#fitting variogram
vAugWind2015.fit = fit.variogram(vAugWind2015, vgm(1, 'Lin', 0))
plot(vAugWind2015 ,vAugWind2015.fit)

#kriging
AugWind2015krige1 = krige(meanWind ~ 1, AugWind2015Subset, texasGrid, vAugWind2015.fit)
spplot(AugWind2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # JANUARY 2005 #

# Average wind speed values for January 2005 in Texas
JanWind2005 = readDataWind("2005/Jan/2005-Jan-Wind.xlsx")

#data frame with data for one day
JanWind2005Subset = computeWindAverage(JanWind2005)
JanWind2005Subset = coordinateTransformWind(JanWind2005Subset)

# variogram for the one day
vJanWind2005 = variogram(meanWind ~ LATITUDE+LONGITUDE, JanWind2005Subset)
plot(vJanWind2005)

#fitting variogram
vJanWind2005.fit = fit.variogram(vJanWind2005, vgm(1, 'Lin', 0))
plot(vJanWind2005 ,vJanWind2005.fit)

#kriging
JanWind2005krige1 = krige(meanWind ~ 1, JanWind2005Subset, texasGrid, vJanWind2005.fit)
spplot(JanWind2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







# AUGUST 2005 #

# Average wind speed values for Auguary 2005 in Texas
AugWind2005 = readDataWind("2005/Aug/2005-Aug-Wind.xlsx")

#data frame with data for one day
AugWind2005Subset = computeWindAverage(AugWind2005)
AugWind2005Subset = coordinateTransformWind(AugWind2005Subset)

# variogram for the one day
vAugWind2005 = variogram(meanWind ~ LATITUDE+LONGITUDE, AugWind2005Subset)
plot(vAugWind2005)

#fitting variogram
vAugWind2005.fit = fit.variogram(vAugWind2005, vgm(1, 'Lin', 0))
plot(vAugWind2005 ,vAugWind2005.fit)

#kriging
AugWind2005krige1 = krige(meanWind ~ 1, AugWind2005Subset, texasGrid, vAugWind2005.fit)
spplot(AugWind2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))








                                                # JANUARY 1995 #

# Average wind speed values for January 1995 in Texas
JanWind1995 = readDataWind("1995/Jan/1995-Jan-Wind.xlsx")

#data frame with data for one day
JanWind1995Subset = computeWindAverage(JanWind1995)
JanWind1995Subset = coordinateTransformWind(JanWind1995Subset)

# variogram for the one day
vJanWind1995 = variogram(meanWind ~ LATITUDE+LONGITUDE, JanWind1995Subset)
plot(vJanWind1995)

#fitting variogram
vJanWind1995.fit = fit.variogram(vJanWind1995, vgm(1, 'Lin', 0))
plot(vJanWind1995 ,vJanWind1995.fit)

#kriging
JanWind1995krige1 = krige(meanWind ~ 1, JanWind1995Subset, texasGrid, vJanWind1995.fit)
spplot(JanWind1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







# AUGUST 1995 #

# Average wind speed values for Auguary 1995 in Texas
AugWind1995 = readDataWind("1995/Aug/1995-Aug-Wind.xlsx")

#data frame with data for one day
AugWind1995Subset = computeWindAverage(AugWind1995)
AugWind1995Subset = coordinateTransformWind(AugWind1995Subset)

# variogram for the one day
vAugWind1995 = variogram(meanWind ~ LATITUDE+LONGITUDE, AugWind1995Subset)
plot(vAugWind1995)

#fitting variogram
vAugWind1995.fit = fit.variogram(vAugWind1995, vgm(1, 'Lin', 0))
plot(vAugWind1995 ,vAugWind1995.fit)

#kriging
AugWind1995krige1 = krige(meanWind ~ 1, AugWind1995Subset, texasGrid, vAugWind1995.fit)
spplot(AugWind1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                      # JANUARY 1985 #

# Average wind speed values for January 1985 in Texas
JanWind1985 = readDataWind("1985/Jan/1985-Jan-Wind.xlsx")

#data frame with data for one day
JanWind1985Subset = computeWindAverage(JanWind1985)
JanWind1985Subset = coordinateTransformWind(JanWind1985Subset)

# variogram for the one day
vJanWind1985 = variogram(meanWind ~ LATITUDE+LONGITUDE, JanWind1985Subset)
plot(vJanWind1985)

#fitting variogram
vJanWind1985.fit = fit.variogram(vJanWind1985, vgm(1, 'Lin', 0))
plot(vJanWind1985 ,vJanWind1985.fit)

#kriging
JanWind1985krige1 = krige(meanWind ~ 1, JanWind1985Subset, texasGrid, vJanWind1985.fit)
spplot(JanWind1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







# AUGUST 1985 #

# Average wind speed values for Auguary 1985 in Texas
AugWind1985 = readDataWind("1985/Aug/1985-Aug-Wind.xlsx")

#data frame with data for one day
AugWind1985Subset = computeWindAverage(AugWind1985)
AugWind1985Subset = coordinateTransformWind(AugWind1985Subset)

# variogram for the one day
vAugWind1985 = variogram(meanWind ~ LATITUDE+LONGITUDE, AugWind1985Subset)
plot(vAugWind1985)

#fitting variogram
vAugWind1985.fit = fit.variogram(vAugWind1985, vgm(1, 'Lin', 0))
plot(vAugWind1985 ,vAugWind1985.fit)

#kriging
AugWind1985krige1 = krige(meanWind ~ 1, AugWind1985Subset, texasGrid, vAugWind1985.fit)
spplot(AugWind1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))