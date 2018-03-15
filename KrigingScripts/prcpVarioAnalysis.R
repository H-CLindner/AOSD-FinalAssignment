# script for the analysis of the precipitation values of all stations in texas

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

computePrcpAverage = function(dataFrame){
  dataFrameSubset = mutate(dataFrame, month = format(DATE, "%m"))
  dataFrameMean = group_by(dataFrameSubset, STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, month) %>% summarise(meanPrcp = mean(PRCP, na.rm = T))
  dataFrameMean$meanPrcp = round(dataFrameMean$meanPrcp, digits=4)
  dataFrameMean = dataFrameMean[!rowSums(dataFrameMean[7]>15),]
  return(dataFrameMean)
}

coordinateTransformPrcp = function(dataSet){
  coordinates(dataSet) = dataSet[c(4,3)]
  proj4string(dataSet) = CRS("+init=epsg:4326")
  dataSet = spTransform(dataSet, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  return(dataSet)
}

readDataPrcp = function(path){
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

# Precipitation values for January 2015 in Texas
JanPrcp2015 = readDataPrcp("2015/Jan/2015-Jan-Prcp.xlsx")

#data frame with data for one day
JanPrcp2015Subset = computePrcpAverage(JanPrcp2015)
JanPrcp2015Subset = coordinateTransformPrcp(JanPrcp2015Subset)

# variogram for the one day
vJanPrcp2015 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp2015Subset)
plot(vJanPrcp2015)

#fitting variogram
vJanPrcp2015.fit = fit.variogram(vJanPrcp2015, vgm('Exp'))
plot(vJanPrcp2015 ,vJanPrcp2015.fit)

#kriging
JanPrcp2015krige1 = krige(meanPrcp ~ 1, JanPrcp2015Subset, texasGrid, vJanPrcp2015.fit)
spplot(JanPrcp2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





  
                                  # AUGUST 2015 #

# Precipitation values for Auguary 2015 in Texas
AugPrcp2015 = readDataPrcp("2015/Aug/2015-Aug-Prcp.xlsx")

#data frame with data for one day
AugPrcp2015Subset = computePrcpAverage(AugPrcp2015)
AugPrcp2015Subset = coordinateTransformPrcp(AugPrcp2015Subset)

# variogram for the one day
vAugPrcp2015 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp2015Subset)
plot(vAugPrcp2015)

#fitting variogram
vAugPrcp2015.fit = fit.variogram(vAugPrcp2015, vgm('Exp'))
plot(vAugPrcp2015 ,vAugPrcp2015.fit)

#kriging
AugPrcp2015krige1 = krige(meanPrcp ~ 1, AugPrcp2015Subset, texasGrid, vAugPrcp2015.fit)
spplot(AugPrcp2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                      # JANUARY 2005 #

# Precipitation values for January 2005 in Texas
JanPrcp2005 = readDataPrcp("2005/Jan/2005-Jan-Prcp.xlsx")

#data frame with data for one day
JanPrcp2005Subset = computePrcpAverage(JanPrcp2005)
JanPrcp2005Subset = coordinateTransformPrcp(JanPrcp2005Subset)

# variogram for the one day
vJanPrcp2005 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp2005Subset)
plot(vJanPrcp2005)

#fitting variogram
vJanPrcp2005.fit = fit.variogram(vJanPrcp2005, vgm('Exp'))
plot(vJanPrcp2005 ,vJanPrcp2005.fit)

#kriging
JanPrcp2005krige1 = krige(meanPrcp ~ 1, JanPrcp2005Subset, texasGrid, vJanPrcp2005.fit)
spplot(JanPrcp2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






# AUGUST 2005 #

# Precipitation values for Auguary 2005 in Texas
AugPrcp2005 = readDataPrcp("2005/Aug/2005-Aug-Prcp.xlsx")

#data frame with data for one day
AugPrcp2005Subset = computePrcpAverage(AugPrcp2005)
AugPrcp2005Subset = coordinateTransformPrcp(AugPrcp2005Subset)

# variogram for the one day
vAugPrcp2005 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp2005Subset)
plot(vAugPrcp2005)

#fitting variogram
vAugPrcp2005.fit = fit.variogram(vAugPrcp2005, vgm('Exp'))
plot(vAugPrcp2005 ,vAugPrcp2005.fit)

#kriging
AugPrcp2005krige1 = krige(meanPrcp ~ 1, AugPrcp2005Subset, texasGrid, vAugPrcp2005.fit)
spplot(AugPrcp2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                        # JANUARY 1995 #

# Precipitation values for January 1995 in Texas
JanPrcp1995 = readDataPrcp("1995/Jan/1995-Jan-Prcp.xlsx")

#data frame with data for one day
JanPrcp1995Subset = computePrcpAverage(JanPrcp1995)
JanPrcp1995Subset = coordinateTransformPrcp(JanPrcp1995Subset)

# variogram for the one day
vJanPrcp1995 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp1995Subset)
plot(vJanPrcp1995)

#fitting variogram
vJanPrcp1995.fit = fit.variogram(vJanPrcp1995, vgm('Exp'))
plot(vJanPrcp1995 ,vJanPrcp1995.fit)

#kriging
JanPrcp1995krige1 = krige(meanPrcp ~ 1, JanPrcp1995Subset, texasGrid, vJanPrcp1995.fit)
spplot(JanPrcp1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






# AUGUST 1995 #

# Precipitation values for Auguary 1995 in Texas
AugPrcp1995 = readDataPrcp("1995/Aug/1995-Aug-Prcp.xlsx")

#data frame with data for one day
AugPrcp1995Subset = computePrcpAverage(AugPrcp1995)
AugPrcp1995Subset = coordinateTransformPrcp(AugPrcp1995Subset)

# variogram for the one day
vAugPrcp1995 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp1995Subset)
plot(vAugPrcp1995)

#fitting variogram
vAugPrcp1995.fit = fit.variogram(vAugPrcp1995, vgm('Exp'))
plot(vAugPrcp1995 ,vAugPrcp1995.fit)

#kriging
AugPrcp1995krige1 = krige(meanPrcp ~ 1, AugPrcp1995Subset, texasGrid, vAugPrcp1995.fit)
spplot(AugPrcp1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                          # JANUARY 1985 #

# Precipitation values for January 1985 in Texas
JanPrcp1985 = readDataPrcp("1985/Jan/1985-Jan-Prcp.xlsx")

#data frame with data for one day
JanPrcp1985Subset = computePrcpAverage(JanPrcp1985)
JanPrcp1985Subset = coordinateTransformPrcp(JanPrcp1985Subset)

# variogram for the one day
vJanPrcp1985 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp1985Subset)
plot(vJanPrcp1985)

#fitting variogram
vJanPrcp1985.fit = fit.variogram(vJanPrcp1985, vgm('Exp'))
plot(vJanPrcp1985 ,vJanPrcp1985.fit)

#kriging
JanPrcp1985krige1 = krige(meanPrcp ~ 1, JanPrcp1985Subset, texasGrid, vJanPrcp1985.fit)
spplot(JanPrcp1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






# AUGUST 1985 #

# Precipitation values for Auguary 1985 in Texas
AugPrcp1985 = readDataPrcp("1985/Aug/1985-Aug-Prcp.xlsx")

#data frame with data for one day
AugPrcp1985Subset = computePrcpAverage(AugPrcp1985)
AugPrcp1985Subset = coordinateTransformPrcp(AugPrcp1985Subset)

# variogram for the one day
vAugPrcp1985 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp1985Subset)
plot(vAugPrcp1985)

#fitting variogram
vAugPrcp1985.fit = fit.variogram(vAugPrcp1985, vgm('Exp'))
plot(vAugPrcp1985 ,vAugPrcp1985.fit)

#kriging
AugPrcp1985krige1 = krige(meanPrcp ~ 1, AugPrcp1985Subset, texasGrid, vAugPrcp1985.fit)
spplot(AugPrcp1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                                  # JANUARY 1975 #

# Precipitation values for January 1975 in Texas
JanPrcp1975 = readDataPrcp("1975/Jan/1975-Jan-Prcp.xlsx")

#data frame with data for one day
JanPrcp1975Subset = computePrcpAverage(JanPrcp1975)
JanPrcp1975Subset = coordinateTransformPrcp(JanPrcp1975Subset)

# variogram for the one day
vJanPrcp1975 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp1975Subset)
plot(vJanPrcp1975)

#fitting variogram
vJanPrcp1975.fit = fit.variogram(vJanPrcp1975, vgm('Exp'))
plot(vJanPrcp1975 ,vJanPrcp1975.fit)

#kriging
JanPrcp1975krige1 = krige(meanPrcp ~ 1, JanPrcp1975Subset, texasGrid, vJanPrcp1975.fit)
spplot(JanPrcp1975krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






# AUGUST 1975 #

# Precipitation values for Auguary 1975 in Texas
AugPrcp1975 = readDataPrcp("1975/Aug/1975-Aug-Prcp.xlsx")

#data frame with data for one day
AugPrcp1975Subset = computePrcpAverage(AugPrcp1975)
AugPrcp1975Subset = coordinateTransformPrcp(AugPrcp1975Subset)

# variogram for the one day
vAugPrcp1975 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp1975Subset)
plot(vAugPrcp1975)

#fitting variogram
vAugPrcp1975.fit = fit.variogram(vAugPrcp1975, vgm('Exp'))
plot(vAugPrcp1975 ,vAugPrcp1975.fit)

#kriging
AugPrcp1975krige1 = krige(meanPrcp ~ 1, AugPrcp1975Subset, texasGrid, vAugPrcp1975.fit)
spplot(AugPrcp1975krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                    # JANUARY 1965 #

# Precipitation values for January 1965 in Texas
JanPrcp1965 = readDataPrcp("1965/Jan/1965-Jan-Prcp.xlsx")

#data frame with data for one day
JanPrcp1965Subset = computePrcpAverage(JanPrcp1965)
JanPrcp1965Subset = coordinateTransformPrcp(JanPrcp1965Subset)

# variogram for the one day
vJanPrcp1965 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp1965Subset)
plot(vJanPrcp1965)

#fitting variogram
vJanPrcp1965.fit = fit.variogram(vJanPrcp1965, vgm('Exp'))
plot(vJanPrcp1965 ,vJanPrcp1965.fit)

#kriging
JanPrcp1965krige1 = krige(meanPrcp ~ 1, JanPrcp1965Subset, texasGrid, vJanPrcp1965.fit)
spplot(JanPrcp1965krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






# AUGUST 1965 #

# Precipitation values for Auguary 1965 in Texas
AugPrcp1965 = readDataPrcp("1965/Aug/1965-Aug-Prcp.xlsx")

#data frame with data for one day
AugPrcp1965Subset = computePrcpAverage(AugPrcp1965)
AugPrcp1965Subset = coordinateTransformPrcp(AugPrcp1965Subset)

# variogram for the one day
vAugPrcp1965 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp1965Subset)
plot(vAugPrcp1965)

#fitting variogram
vAugPrcp1965.fit = fit.variogram(vAugPrcp1965, vgm('Exp'))
plot(vAugPrcp1965 ,vAugPrcp1965.fit)

#kriging
AugPrcp1965krige1 = krige(meanPrcp ~ 1, AugPrcp1965Subset, texasGrid, vAugPrcp1965.fit)
spplot(AugPrcp1965krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                    # JANUARY 1955 #

# Precipitation values for January 1955 in Texas
JanPrcp1955 = readDataPrcp("1955/Jan/1955-Jan-Prcp.xlsx")

#data frame with data for one day
JanPrcp1955Subset = computePrcpAverage(JanPrcp1955)
JanPrcp1955Subset = coordinateTransformPrcp(JanPrcp1955Subset)

# variogram for the one day
vJanPrcp1955 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp1955Subset)
plot(vJanPrcp1955)

#fitting variogram
vJanPrcp1955.fit = fit.variogram(vJanPrcp1955, vgm('Exp'))
plot(vJanPrcp1955 ,vJanPrcp1955.fit)

#kriging
JanPrcp1955krige1 = krige(meanPrcp ~ 1, JanPrcp1955Subset, texasGrid, vJanPrcp1955.fit)
spplot(JanPrcp1955krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






# AUGUST 1955 #

# Precipitation values for Auguary 1955 in Texas
AugPrcp1955 = readDataPrcp("1955/Aug/1955-Aug-Prcp.xlsx")

#data frame with data for one day
AugPrcp1955Subset = computePrcpAverage(AugPrcp1955)
AugPrcp1955Subset = coordinateTransformPrcp(AugPrcp1955Subset)

# variogram for the one day
vAugPrcp1955 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp1955Subset)
plot(vAugPrcp1955)

#fitting variogram
vAugPrcp1955.fit = fit.variogram(vAugPrcp1955, vgm('Exp'))
plot(vAugPrcp1955 ,vAugPrcp1955.fit)

#kriging
AugPrcp1955krige1 = krige(meanPrcp ~ 1, AugPrcp1955Subset, texasGrid, vAugPrcp1955.fit)
spplot(AugPrcp1955krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))