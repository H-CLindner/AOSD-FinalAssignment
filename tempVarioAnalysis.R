# script for the analysis of the temperature values of all stations in texas

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


computeTempAverage = function(dataFrame){
  dataFrameSubset = mutate(dataFrame, month = format(DATE, "%m"))
  dataFrameMean = group_by(dataFrameSubset, STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, month) %>% summarise(meanTemp = mean(TMAX, na.rm = T))
  dataFrameMean$meanTemp = round(dataFrameMean$meanTemp, digits=4)
  return(dataFrameMean)
}

coordinateTransformTemp = function(dataSet){
  coordinates(dataSet) = dataSet[c(4,3)]
  proj4string(dataSet) = CRS("+init=epsg:4326")
  dataSet = spTransform(dataSet, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  return(dataSet)
}

readDataTemp = function(path){
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


#Temperature values for January 2015 in Texas
JanTmpMax2015 = readDataTemp("2015/Jan/2015-Jan-TMax.xlsx")

#data frame with data for one day
JanTmpMax2015Subset = computeTempAverage(JanTmpMax2015)
JanTmpMax2015Subset = coordinateTransformTemp(JanTmpMax2015Subset)

# variogram for the one day
vJanTmpMax2015 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax2015Subset)
plot(vJanTmpMax2015)

#fitting variogram
vJanTmpMax2015.fit = fit.variogram(vJanTmpMax2015, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax2015 ,vJanTmpMax2015.fit)

#kriging
JanTmpMax2015krige1 = krige(meanTemp ~ 1, JanTmpMax2015Subset, texasGrid, vJanTmpMax2015.fit)
spplot(JanTmpMax2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                            # AUGUST 2015 #



#Temperature values for Auguary 2015 in Texas
AugTmpMax2015 = readDataTemp("2015/Aug/2015-Aug-TMax.xlsx")

#data frame with data for one day
AugTmpMax2015Subset = computeTempAverage(AugTmpMax2015)
AugTmpMax2015Subset = coordinateTransformTemp(AugTmpMax2015Subset)

# variogram for the one day
vAugTmpMax2015 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax2015Subset)
plot(vAugTmpMax2015)

#fitting variogram
vAugTmpMax2015.fit = fit.variogram(vAugTmpMax2015, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax2015 ,vAugTmpMax2015.fit)

#kriging
AugTmpMax2015krige1 = krige(meanTemp ~ 1, AugTmpMax2015Subset, texasGrid, vAugTmpMax2015.fit)
spplot(AugTmpMax2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))




                                  # JANUARY 2005 #


#Temperature values for January 2005 in Texas
JanTmpMax2005 = readDataTemp("2005/Jan/2005-Jan-TMax.xlsx")

#data frame with data for one day
JanTmpMax2005Subset = computeTempAverage(JanTmpMax2005)
JanTmpMax2005Subset = coordinateTransformTemp(JanTmpMax2005Subset)

# variogram for the one day
vJanTmpMax2005 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax2005Subset)
plot(vJanTmpMax2005)

#fitting variogram
vJanTmpMax2005.fit = fit.variogram(vJanTmpMax2005, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax2005 ,vJanTmpMax2005.fit)

#kriging
JanTmpMax2005krige1 = krige(meanTemp ~ 1, JanTmpMax2005Subset, texasGrid, vJanTmpMax2005.fit)
spplot(JanTmpMax2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





# AUGUST 2005 #



#Temperature values for Auguary 2005 in Texas
AugTmpMax2005 = readDataTemp("2005/Aug/2005-Aug-TMax.xlsx")

#data frame with data for one day
AugTmpMax2005Subset = computeTempAverage(AugTmpMax2005)
AugTmpMax2005Subset = coordinateTransformTemp(AugTmpMax2005Subset)

# variogram for the one day
vAugTmpMax2005 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax2005Subset)
plot(vAugTmpMax2005)

#fitting variogram
vAugTmpMax2005.fit = fit.variogram(vAugTmpMax2005, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax2005 ,vAugTmpMax2005.fit)

#kriging
AugTmpMax2005krige1 = krige(meanTemp ~ 1, AugTmpMax2005Subset, texasGrid, vAugTmpMax2005.fit)
spplot(AugTmpMax2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # JANUARY 1995 #


#Temperature values for January 1995 in Texas
JanTmpMax1995 = readDataTemp("1995/Jan/1995-Jan-TMax.xlsx")

#data frame with data for one day
JanTmpMax1995Subset = computeTempAverage(JanTmpMax1995)
JanTmpMax1995Subset = coordinateTransformTemp(JanTmpMax1995Subset)

# variogram for the one day
vJanTmpMax1995 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax1995Subset)
plot(vJanTmpMax1995)

#fitting variogram
vJanTmpMax1995.fit = fit.variogram(vJanTmpMax1995, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax1995 ,vJanTmpMax1995.fit)

#kriging
JanTmpMax1995krige1 = krige(meanTemp ~ 1, JanTmpMax1995Subset, texasGrid, vJanTmpMax1995.fit)
spplot(JanTmpMax1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





# AUGUST 1995 #



#Temperature values for Auguary 1995 in Texas
AugTmpMax1995 = readDataTemp("1995/Aug/1995-Aug-TMax.xlsx")

#data frame with data for one day
AugTmpMax1995Subset = computeTempAverage(AugTmpMax1995)
AugTmpMax1995Subset = coordinateTransformTemp(AugTmpMax1995Subset)

# variogram for the one day
vAugTmpMax1995 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax1995Subset)
plot(vAugTmpMax1995)

#fitting variogram
vAugTmpMax1995.fit = fit.variogram(vAugTmpMax1995, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax1995 ,vAugTmpMax1995.fit)

#kriging
AugTmpMax1995krige1 = krige(meanTemp ~ 1, AugTmpMax1995Subset, texasGrid, vAugTmpMax1995.fit)
spplot(AugTmpMax1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # JANUARY 1985 #


#Temperature values for January 1985 in Texas
JanTmpMax1985 = readDataTemp("1985/Jan/1985-Jan-TMax.xlsx")

#data frame with data for one day
JanTmpMax1985Subset = computeTempAverage(JanTmpMax1985)
JanTmpMax1985Subset = coordinateTransformTemp(JanTmpMax1985Subset)

# variogram for the one day
vJanTmpMax1985 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax1985Subset)
plot(vJanTmpMax1985)

#fitting variogram
vJanTmpMax1985.fit = fit.variogram(vJanTmpMax1985, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax1985 ,vJanTmpMax1985.fit)

#kriging
JanTmpMax1985krige1 = krige(meanTemp ~ 1, JanTmpMax1985Subset, texasGrid, vJanTmpMax1985.fit)
spplot(JanTmpMax1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





# AUGUST 1985 #



#Temperature values for Auguary 1985 in Texas
AugTmpMax1985 = readDataTemp("1985/Aug/1985-Aug-TMax.xlsx")

#data frame with data for one day
AugTmpMax1985Subset = computeTempAverage(AugTmpMax1985)
AugTmpMax1985Subset = coordinateTransformTemp(AugTmpMax1985Subset)

# variogram for the one day
vAugTmpMax1985 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax1985Subset)
plot(vAugTmpMax1985)

#fitting variogram
vAugTmpMax1985.fit = fit.variogram(vAugTmpMax1985, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax1985 ,vAugTmpMax1985.fit)

#kriging
AugTmpMax1985krige1 = krige(meanTemp ~ 1, AugTmpMax1985Subset, texasGrid, vAugTmpMax1985.fit)
spplot(AugTmpMax1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                        # JANUARY 1975 #


#Temperature values for January 1975 in Texas
JanTmpMax1975 = readDataTemp("1975/Jan/1975-Jan-TMax.xlsx")

#data frame with data for one day
JanTmpMax1975Subset = computeTempAverage(JanTmpMax1975)
JanTmpMax1975Subset = coordinateTransformTemp(JanTmpMax1975Subset)

# variogram for the one day
vJanTmpMax1975 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax1975Subset)
plot(vJanTmpMax1975)

#fitting variogram
vJanTmpMax1975.fit = fit.variogram(vJanTmpMax1975, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax1975 ,vJanTmpMax1975.fit)

#kriging
JanTmpMax1975krige1 = krige(meanTemp ~ 1, JanTmpMax1975Subset, texasGrid, vJanTmpMax1975.fit)
spplot(JanTmpMax1975krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





# AUGUST 1975 #



#Temperature values for Auguary 1975 in Texas
AugTmpMax1975 = readDataTemp("1975/Aug/1975-Aug-TMax.xlsx")

#data frame with data for one day
AugTmpMax1975Subset = computeTempAverage(AugTmpMax1975)
AugTmpMax1975Subset = coordinateTransformTemp(AugTmpMax1975Subset)

# variogram for the one day
vAugTmpMax1975 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax1975Subset)
plot(vAugTmpMax1975)

#fitting variogram
vAugTmpMax1975.fit = fit.variogram(vAugTmpMax1975, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax1975 ,vAugTmpMax1975.fit)

#kriging
AugTmpMax1975krige1 = krige(meanTemp ~ 1, AugTmpMax1975Subset, texasGrid, vAugTmpMax1975.fit)
spplot(AugTmpMax1975krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                      # JANUARY 1965 #


#Temperature values for January 1965 in Texas
JanTmpMax1965 = readDataTemp("1965/Jan/1965-Jan-TMax.xlsx")

#data frame with data for one day
JanTmpMax1965Subset = computeTempAverage(JanTmpMax1965)
JanTmpMax1965Subset = coordinateTransformTemp(JanTmpMax1965Subset)

# variogram for the one day
vJanTmpMax1965 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax1965Subset)
plot(vJanTmpMax1965)

#fitting variogram
vJanTmpMax1965.fit = fit.variogram(vJanTmpMax1965, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax1965 ,vJanTmpMax1965.fit)

#kriging
JanTmpMax1965krige1 = krige(meanTemp ~ 1, JanTmpMax1965Subset, texasGrid, vJanTmpMax1965.fit)
spplot(JanTmpMax1965krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





# AUGUST 1965 #



#Temperature values for Auguary 1965 in Texas
AugTmpMax1965 = readDataTemp("1965/Aug/1965-Aug-TMax.xlsx")

#data frame with data for one day
AugTmpMax1965Subset = computeTempAverage(AugTmpMax1965)
AugTmpMax1965Subset = coordinateTransformTemp(AugTmpMax1965Subset)

# variogram for the one day
vAugTmpMax1965 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax1965Subset)
plot(vAugTmpMax1965)

#fitting variogram
vAugTmpMax1965.fit = fit.variogram(vAugTmpMax1965, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax1965 ,vAugTmpMax1965.fit)

#kriging
AugTmpMax1965krige1 = krige(meanTemp ~ 1, AugTmpMax1965Subset, texasGrid, vAugTmpMax1965.fit)
spplot(AugTmpMax1965krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                    # JANUARY 1955 #


#Temperature values for January 1955 in Texas
JanTmpMax1955 = readDataTemp("1955/Jan/1955-Jan-TMax.xlsx")

#data frame with data for one day
JanTmpMax1955Subset = computeTempAverage(JanTmpMax1955)
JanTmpMax1955Subset = coordinateTransformTemp(JanTmpMax1955Subset)

# variogram for the one day
vJanTmpMax1955 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax1955Subset)
plot(vJanTmpMax1955)

#fitting variogram
vJanTmpMax1955.fit = fit.variogram(vJanTmpMax1955, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax1955 ,vJanTmpMax1955.fit)

#kriging
JanTmpMax1955krige1 = krige(meanTemp ~ 1, JanTmpMax1955Subset, texasGrid, vJanTmpMax1955.fit)
spplot(JanTmpMax1955krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





# AUGUST 1955 #



#Temperature values for Auguary 1955 in Texas
AugTmpMax1955 = readDataTemp("1955/Aug/1955-Aug-TMax.xlsx")

#data frame with data for one day
AugTmpMax1955Subset = computeTempAverage(AugTmpMax1955)
AugTmpMax1955Subset = coordinateTransformTemp(AugTmpMax1955Subset)

# variogram for the one day
vAugTmpMax1955 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax1955Subset)
plot(vAugTmpMax1955)

#fitting variogram
vAugTmpMax1955.fit = fit.variogram(vAugTmpMax1955, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax1955 ,vAugTmpMax1955.fit)

#kriging
AugTmpMax1955krige1 = krige(meanTemp ~ 1, AugTmpMax1955Subset, texasGrid, vAugTmpMax1955.fit)
spplot(AugTmpMax1955krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))