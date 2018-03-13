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

                                      # JANUARY 2015 #

# Average wind speed values for January 2015 in Texas
JanWind2015 = read_xlsx("2015/Jan/2015-Jan-Wind.xlsx")
JanWind2015$LATITUDE = as.numeric(JanWind2015$LATITUDE)
JanWind2015$LONGITUDE = as.numeric(JanWind2015$LONGITUDE)

#data frame with data for one day
JanWind2015Subset = computeWindAverage(JanWind2015)
coordinates(JanWind2015Subset) = JanWind2015Subset[c(4,3)]
proj4string(JanWind2015Subset) = CRS("+init=epsg:4326")
JanWind2015Subset = spTransform(JanWind2015Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanWind2015 = variogram(meanWind ~ LATITUDE+LONGITUDE, JanWind2015Subset)
plot(vJanWind2015)

vJanWind2015.fit = fit.variogram(vJanWind2015, vgm(1, 'Lin', 0))
plot(vJanWind2015 ,vJanWind2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanWind2015Subset, add=TRUE, pch=20, col="red")
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

JanWind2015krige1 = krige(meanWind ~ 1, JanWind2015Subset, texasGrid, vJanWind2015.fit)
spplot(JanWind2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







                                            # AUGUST 2015 #

# Average wind speed values for Auguary 2015 in Texas
AugWind2015 = read_xlsx("2015/Aug/2015-Aug-Wind.xlsx")
AugWind2015$LATITUDE = as.numeric(AugWind2015$LATITUDE)
AugWind2015$LONGITUDE = as.numeric(AugWind2015$LONGITUDE)

#data frame with data for one day
AugWind2015Subset = computeWindAverage(AugWind2015)
coordinates(AugWind2015Subset) = AugWind2015Subset[c(4,3)]
proj4string(AugWind2015Subset) = CRS("+init=epsg:4326")
AugWind2015Subset = spTransform(AugWind2015Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugWind2015 = variogram(meanWind ~ LATITUDE+LONGITUDE, AugWind2015Subset)
plot(vAugWind2015)

vAugWind2015.fit = fit.variogram(vAugWind2015, vgm(1, 'Lin', 0))
plot(vAugWind2015 ,vAugWind2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugWind2015Subset, add=TRUE, pch=20, col="red")
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

AugWind2015krige1 = krige(meanWind ~ 1, AugWind2015Subset, texasGrid, vAugWind2015.fit)
spplot(AugWind2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # JANUARY 2005 #

# Average wind speed values for January 2005 in Texas
JanWind2005 = read_xlsx("2005/Jan/2005-Jan-Wind.xlsx")
JanWind2005$LATITUDE = as.numeric(JanWind2005$LATITUDE)
JanWind2005$LONGITUDE = as.numeric(JanWind2005$LONGITUDE)

#data frame with data for one day
JanWind2005Subset = computeWindAverage(JanWind2005)
coordinates(JanWind2005Subset) = JanWind2005Subset[c(4,3)]
proj4string(JanWind2005Subset) = CRS("+init=epsg:4326")
JanWind2005Subset = spTransform(JanWind2005Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanWind2005 = variogram(meanWind ~ LATITUDE+LONGITUDE, JanWind2005Subset)
plot(vJanWind2005)

vJanWind2005.fit = fit.variogram(vJanWind2005, vgm(1, 'Lin', 0))
plot(vJanWind2005 ,vJanWind2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanWind2005Subset, add=TRUE, pch=20, col="red")
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

JanWind2005krige1 = krige(meanWind ~ 1, JanWind2005Subset, texasGrid, vJanWind2005.fit)
spplot(JanWind2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







                                        # AUGUST 2005 #

# Average wind speed values for Auguary 2005 in Texas
AugWind2005 = read_xlsx("2005/Aug/2005-Aug-Wind.xlsx")
AugWind2005$LATITUDE = as.numeric(AugWind2005$LATITUDE)
AugWind2005$LONGITUDE = as.numeric(AugWind2005$LONGITUDE)

#data frame with data for one day
AugWind2005Subset = computeWindAverage(AugWind2005)
coordinates(AugWind2005Subset) = AugWind2005Subset[c(4,3)]
proj4string(AugWind2005Subset) = CRS("+init=epsg:4326")
AugWind2005Subset = spTransform(AugWind2005Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugWind2005 = variogram(meanWind ~ LATITUDE+LONGITUDE, AugWind2005Subset)
plot(vAugWind2005)

vAugWind2005.fit = fit.variogram(vAugWind2005, vgm(1, 'Lin', 0))
plot(vAugWind2005 ,vAugWind2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugWind2005Subset, add=TRUE, pch=20, col="red")
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

AugWind2005krige1 = krige(meanWind ~ 1, AugWind2005Subset, texasGrid, vAugWind2005.fit)
spplot(AugWind2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))








                                                # JANUARY 1995 #

# Average wind speed values for January 1995 in Texas
JanWind1995 = read_xlsx("1995/Jan/1995-Jan-Wind.xlsx")
JanWind1995$LATITUDE = as.numeric(JanWind1995$LATITUDE)
JanWind1995$LONGITUDE = as.numeric(JanWind1995$LONGITUDE)

#data frame with data for one day
JanWind1995Subset = computeWindAverage(JanWind1995)
coordinates(JanWind1995Subset) = JanWind1995Subset[c(4,3)]
proj4string(JanWind1995Subset) = CRS("+init=epsg:4326")
JanWind1995Subset = spTransform(JanWind1995Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanWind1995 = variogram(meanWind ~ LATITUDE+LONGITUDE, JanWind1995Subset)
plot(vJanWind1995)

vJanWind1995.fit = fit.variogram(vJanWind1995, vgm(1, 'Lin', 0))
plot(vJanWind1995 ,vJanWind1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanWind1995Subset, add=TRUE, pch=20, col="red")
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

JanWind1995krige1 = krige(meanWind ~ 1, JanWind1995Subset, texasGrid, vJanWind1995.fit)
spplot(JanWind1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







                                          # AUGUST 1995 #

# Average wind speed values for Auguary 1995 in Texas
AugWind1995 = read_xlsx("1995/Aug/1995-Aug-Wind.xlsx")
AugWind1995$LATITUDE = as.numeric(AugWind1995$LATITUDE)
AugWind1995$LONGITUDE = as.numeric(AugWind1995$LONGITUDE)

#data frame with data for one day
AugWind1995Subset = computeWindAverage(AugWind1995)
coordinates(AugWind1995Subset) = AugWind1995Subset[c(4,3)]
proj4string(AugWind1995Subset) = CRS("+init=epsg:4326")
AugWind1995Subset = spTransform(AugWind1995Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugWind1995 = variogram(meanWind ~ LATITUDE+LONGITUDE, AugWind1995Subset)
plot(vAugWind1995)

vAugWind1995.fit = fit.variogram(vAugWind1995, vgm(1, 'Lin', 0))
plot(vAugWind1995 ,vAugWind1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugWind1995Subset, add=TRUE, pch=20, col="red")
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

AugWind1995krige1 = krige(meanWind ~ 1, AugWind1995Subset, texasGrid, vAugWind1995.fit)
spplot(AugWind1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                      # JANUARY 1985 #

# Average wind speed values for January 1985 in Texas
JanWind1985 = read_xlsx("1985/Jan/1985-Jan-Wind.xlsx")
JanWind1985$LATITUDE = as.numeric(JanWind1985$LATITUDE)
JanWind1985$LONGITUDE = as.numeric(JanWind1985$LONGITUDE)

#data frame with data for one day
JanWind1985Subset = computeWindAverage(JanWind1985)
coordinates(JanWind1985Subset) = JanWind1985Subset[c(4,3)]
proj4string(JanWind1985Subset) = CRS("+init=epsg:4326")
JanWind1985Subset = spTransform(JanWind1985Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanWind1985 = variogram(meanWind ~ LATITUDE+LONGITUDE, JanWind1985Subset)
plot(vJanWind1985)

vJanWind1985.fit = fit.variogram(vJanWind1985, vgm(1, 'Lin', 0))
plot(vJanWind1985 ,vJanWind1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanWind1985Subset, add=TRUE, pch=20, col="red")
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

JanWind1985krige1 = krige(meanWind ~ 1, JanWind1985Subset, texasGrid, vJanWind1985.fit)
spplot(JanWind1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







                                                # AUGUST 1985 #

# Average wind speed values for Auguary 1985 in Texas
AugWind1985 = read_xlsx("1985/Aug/1985-Aug-Wind.xlsx")
AugWind1985$LATITUDE = as.numeric(AugWind1985$LATITUDE)
AugWind1985$LONGITUDE = as.numeric(AugWind1985$LONGITUDE)

#data frame with data for one day
AugWind1985Subset = computeWindAverage(AugWind1985)
coordinates(AugWind1985Subset) = AugWind1985Subset[c(4,3)]
proj4string(AugWind1985Subset) = CRS("+init=epsg:4326")
AugWind1985Subset = spTransform(AugWind1985Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugWind1985 = variogram(meanWind ~ LATITUDE+LONGITUDE, AugWind1985Subset)
plot(vAugWind1985)

vAugWind1985.fit = fit.variogram(vAugWind1985, vgm(1, 'Lin', 0))
plot(vAugWind1985 ,vAugWind1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugWind1985Subset, add=TRUE, pch=20, col="red")
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

AugWind1985krige1 = krige(meanWind ~ 1, AugWind1985Subset, texasGrid, vAugWind1985.fit)
spplot(AugWind1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))