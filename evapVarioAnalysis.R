# script for the analysis of the evaporation values of all stations in texas

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

computeEvapAverage = function(dataFrame){
  dataFrameSubset = mutate(dataFrame, month = format(DATE, "%m"))
  dataFrameMean = group_by(dataFrameSubset, STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, month) %>% summarise(meanEvap = mean(EVAP, na.rm = T))
  dataFrameMean$meanEvap = round(dataFrameMean$meanEvap, digits=4)
  dataFrameMean = dataFrameMean[!rowSums(dataFrameMean[7]>30),]
  return(dataFrameMean)
}

                                      # JANUARY 2015 #

# Evaporation values for January 2015 in Texas
JanEvap2015 = read_xlsx("2015/Jan/2015-Jan-Evap.xlsx")
JanEvap2015$LATITUDE = as.numeric(JanEvap2015$LATITUDE)
JanEvap2015$LONGITUDE = as.numeric(JanEvap2015$LONGITUDE)

#data frame with data for one day
JanEvap2015Subset = computeEvapAverage(JanEvap2015)
coordinates(JanEvap2015Subset) = JanEvap2015Subset[c(4,3)]
proj4string(JanEvap2015Subset) = CRS("+init=epsg:4326")
JanEvap2015Subset = spTransform(JanEvap2015Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanEvap2015 = variogram(meanEvap ~ LATITUDE+LONGITUDE, JanEvap2015Subset)
plot(vJanEvap2015)

vJanEvap2015.fit = fit.variogram(vJanEvap2015, vgm(1.5, 'Lin', 0))
plot(vJanEvap2015 ,vJanEvap2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap2015Subset, add=TRUE, pch=20, col="red")
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

JanEvap2015krige1 = krige(meanEvap ~ 1, JanEvap2015Subset, texasGrid, vJanEvap2015.fit)
spplot(JanEvap2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                            # AUGUST 2015 #

# Evaporation values for Auguary 2015 in Texas
AugEvap2015 = read_xlsx("2015/Aug/2015-Aug-Evap.xlsx")
AugEvap2015$LATITUDE = as.numeric(AugEvap2015$LATITUDE)
AugEvap2015$LONGITUDE = as.numeric(AugEvap2015$LONGITUDE)

#data frame with data for one day
AugEvap2015Subset = computeEvapAverage(AugEvap2015)
coordinates(AugEvap2015Subset) = AugEvap2015Subset[c(4,3)]
proj4string(AugEvap2015Subset) = CRS("+init=epsg:4326")
AugEvap2015Subset = spTransform(AugEvap2015Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugEvap2015 = variogram(meanEvap ~ LATITUDE+LONGITUDE, AugEvap2015Subset)
plot(vAugEvap2015)

vAugEvap2015.fit = fit.variogram(vAugEvap2015, vgm(1.5, 'Lin', 0))
plot(vAugEvap2015 ,vAugEvap2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap2015Subset, add=TRUE, pch=20, col="red")
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

AugEvap2015krige1 = krige(meanEvap ~ 1, AugEvap2015Subset, texasGrid, vAugEvap2015.fit)
spplot(AugEvap2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))








                                          # JANUARY 2005 #

# Evaporation values for January 2005 in Texas
JanEvap2005 = read_xlsx("2005/Jan/2005-Jan-Evap.xlsx")
JanEvap2005$LATITUDE = as.numeric(JanEvap2005$LATITUDE)
JanEvap2005$LONGITUDE = as.numeric(JanEvap2005$LONGITUDE)

#data frame with data for one day
JanEvap2005Subset = computeEvapAverage(JanEvap2005)
coordinates(JanEvap2005Subset) = JanEvap2005Subset[c(4,3)]
proj4string(JanEvap2005Subset) = CRS("+init=epsg:4326")
JanEvap2005Subset = spTransform(JanEvap2005Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanEvap2005 = variogram(meanEvap ~ LATITUDE+LONGITUDE, JanEvap2005Subset)
plot(vJanEvap2005)

vJanEvap2005.fit = fit.variogram(vJanEvap2005, vgm(0.5, 'Lin', 0))
plot(vJanEvap2005 ,vJanEvap2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap2005Subset, add=TRUE, pch=20, col="red")
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

JanEvap2005krige1 = krige(meanEvap ~ 1, JanEvap2005Subset, texasGrid, vJanEvap2005.fit)
spplot(JanEvap2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                                # AUGUST 2005 #

# Evaporation values for Auguary 2005 in Texas
AugEvap2005 = read_xlsx("2005/Aug/2005-Aug-Evap.xlsx")
AugEvap2005$LATITUDE = as.numeric(AugEvap2005$LATITUDE)
AugEvap2005$LONGITUDE = as.numeric(AugEvap2005$LONGITUDE)

#data frame with data for one day
AugEvap2005Subset = computeEvapAverage(AugEvap2005)
coordinates(AugEvap2005Subset) = AugEvap2005Subset[c(4,3)]
proj4string(AugEvap2005Subset) = CRS("+init=epsg:4326")
AugEvap2005Subset = spTransform(AugEvap2005Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugEvap2005 = variogram(meanEvap ~ LATITUDE+LONGITUDE, AugEvap2005Subset)
plot(vAugEvap2005)

vAugEvap2005.fit = fit.variogram(vAugEvap2005, vgm(1.5, 'Lin', 0))
plot(vAugEvap2005 ,vAugEvap2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap2005Subset, add=TRUE, pch=20, col="red")
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

AugEvap2005krige1 = krige(meanEvap ~ 1, AugEvap2005Subset, texasGrid, vAugEvap2005.fit)
spplot(AugEvap2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                            # JANUARY 1995 #

# Evaporation values for January 1995 in Texas
JanEvap1995 = read_xlsx("1995/Jan/1995-Jan-Evap.xlsx")
JanEvap1995$LATITUDE = as.numeric(JanEvap1995$LATITUDE)
JanEvap1995$LONGITUDE = as.numeric(JanEvap1995$LONGITUDE)

#data frame with data for one day
JanEvap1995Subset = computeEvapAverage(JanEvap1995)
coordinates(JanEvap1995Subset) = JanEvap1995Subset[c(4,3)]
proj4string(JanEvap1995Subset) = CRS("+init=epsg:4326")
JanEvap1995Subset = spTransform(JanEvap1995Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanEvap1995 = variogram(meanEvap ~ LATITUDE+LONGITUDE, JanEvap1995Subset)
plot(vJanEvap1995)

vJanEvap1995.fit = fit.variogram(vJanEvap1995, vgm(0.5, 'Lin', 0))
plot(vJanEvap1995 ,vJanEvap1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap1995Subset, add=TRUE, pch=20, col="red")
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

JanEvap1995krige1 = krige(meanEvap ~ 1, JanEvap1995Subset, texasGrid, vJanEvap1995.fit)
spplot(JanEvap1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                            # AUGUST 1995 #

# Evaporation values for Auguary 1995 in Texas
AugEvap1995 = read_xlsx("1995/Aug/1995-Aug-Evap.xlsx")
AugEvap1995$LATITUDE = as.numeric(AugEvap1995$LATITUDE)
AugEvap1995$LONGITUDE = as.numeric(AugEvap1995$LONGITUDE)

#data frame with data for one day
AugEvap1995Subset = computeEvapAverage(AugEvap1995)
coordinates(AugEvap1995Subset) = AugEvap1995Subset[c(4,3)]
proj4string(AugEvap1995Subset) = CRS("+init=epsg:4326")
AugEvap1995Subset = spTransform(AugEvap1995Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugEvap1995 = variogram(meanEvap ~ LATITUDE+LONGITUDE, AugEvap1995Subset)
plot(vAugEvap1995)

vAugEvap1995.fit = fit.variogram(vAugEvap1995, vgm(1.5, 'Lin', 0))
plot(vAugEvap1995 ,vAugEvap1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap1995Subset, add=TRUE, pch=20, col="red")
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

AugEvap1995krige1 = krige(meanEvap ~ 1, AugEvap1995Subset, texasGrid, vAugEvap1995.fit)
spplot(AugEvap1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))








                                          # JANUARY 1985 #

# Evaporation values for January 1985 in Texas
JanEvap1985 = read_xlsx("1985/Jan/1985-Jan-Evap.xlsx")
JanEvap1985$LATITUDE = as.numeric(JanEvap1985$LATITUDE)
JanEvap1985$LONGITUDE = as.numeric(JanEvap1985$LONGITUDE)

#data frame with data for one day
JanEvap1985Subset = computeEvapAverage(JanEvap1985)
coordinates(JanEvap1985Subset) = JanEvap1985Subset[c(4,3)]
proj4string(JanEvap1985Subset) = CRS("+init=epsg:4326")
JanEvap1985Subset = spTransform(JanEvap1985Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanEvap1985 = variogram(meanEvap ~ LATITUDE+LONGITUDE, JanEvap1985Subset)
plot(vJanEvap1985)

vJanEvap1985.fit = fit.variogram(vJanEvap1985, vgm(0.5, 'Lin', 0))
plot(vJanEvap1985 ,vJanEvap1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap1985Subset, add=TRUE, pch=20, col="red")
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

JanEvap1985krige1 = krige(meanEvap ~ 1, JanEvap1985Subset, texasGrid, vJanEvap1985.fit)
spplot(JanEvap1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                        # AUGUST 1985 #

# Evaporation values for Auguary 1985 in Texas
AugEvap1985 = read_xlsx("1985/Aug/1985-Aug-Evap.xlsx")
AugEvap1985$LATITUDE = as.numeric(AugEvap1985$LATITUDE)
AugEvap1985$LONGITUDE = as.numeric(AugEvap1985$LONGITUDE)

#data frame with data for one day
AugEvap1985Subset = computeEvapAverage(AugEvap1985)
coordinates(AugEvap1985Subset) = AugEvap1985Subset[c(4,3)]
proj4string(AugEvap1985Subset) = CRS("+init=epsg:4326")
AugEvap1985Subset = spTransform(AugEvap1985Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugEvap1985 = variogram(meanEvap ~ LATITUDE+LONGITUDE, AugEvap1985Subset)
plot(vAugEvap1985)

vAugEvap1985.fit = fit.variogram(vAugEvap1985, vgm(1.5, 'Lin', 0))
plot(vAugEvap1985 ,vAugEvap1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap1985Subset, add=TRUE, pch=20, col="red")
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

AugEvap1985krige1 = krige(meanEvap ~ 1, AugEvap1985Subset, texasGrid, vAugEvap1985.fit)
spplot(AugEvap1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







                                          # JANUARY 1975 #

# Evaporation values for January 1975 in Texas
JanEvap1975 = read_xlsx("1975/Jan/1975-Jan-Evap.xlsx")
JanEvap1975$LATITUDE = as.numeric(JanEvap1975$LATITUDE)
JanEvap1975$LONGITUDE = as.numeric(JanEvap1975$LONGITUDE)

#data frame with data for one day
JanEvap1975Subset = computeEvapAverage(JanEvap1975)
coordinates(JanEvap1975Subset) = JanEvap1975Subset[c(4,3)]
proj4string(JanEvap1975Subset) = CRS("+init=epsg:4326")
JanEvap1975Subset = spTransform(JanEvap1975Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanEvap1975 = variogram(meanEvap ~ LATITUDE+LONGITUDE, JanEvap1975Subset)
plot(vJanEvap1975)

vJanEvap1975.fit = fit.variogram(vJanEvap1975, vgm(0.5, 'Lin', 0))
plot(vJanEvap1975 ,vJanEvap1975.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap1975Subset, add=TRUE, pch=20, col="red")
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

JanEvap1975krige1 = krige(meanEvap ~ 1, JanEvap1975Subset, texasGrid, vJanEvap1975.fit)
spplot(JanEvap1975krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                              # AUGUST 1975 #

# Evaporation values for Auguary 1975 in Texas
AugEvap1975 = read_xlsx("1975/Aug/1975-Aug-Evap.xlsx")
AugEvap1975$LATITUDE = as.numeric(AugEvap1975$LATITUDE)
AugEvap1975$LONGITUDE = as.numeric(AugEvap1975$LONGITUDE)

#data frame with data for one day
AugEvap1975Subset = computeEvapAverage(AugEvap1975)
coordinates(AugEvap1975Subset) = AugEvap1975Subset[c(4,3)]
proj4string(AugEvap1975Subset) = CRS("+init=epsg:4326")
AugEvap1975Subset = spTransform(AugEvap1975Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugEvap1975 = variogram(meanEvap ~ LATITUDE+LONGITUDE, AugEvap1975Subset)
plot(vAugEvap1975)

vAugEvap1975.fit = fit.variogram(vAugEvap1975, vgm(1.5, 'Lin', 0))
plot(vAugEvap1975 ,vAugEvap1975.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap1975Subset, add=TRUE, pch=20, col="red")
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

AugEvap1975krige1 = krige(meanEvap ~ 1, AugEvap1975Subset, texasGrid, vAugEvap1975.fit)
spplot(AugEvap1975krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                          # JANUARY 1965 #

# Evaporation values for January 1965 in Texas
JanEvap1965 = read_xlsx("1965/Jan/1965-Jan-Evap.xlsx")
JanEvap1965$LATITUDE = as.numeric(JanEvap1965$LATITUDE)
JanEvap1965$LONGITUDE = as.numeric(JanEvap1965$LONGITUDE)

#data frame with data for one day
JanEvap1965Subset = computeEvapAverage(JanEvap1965)
coordinates(JanEvap1965Subset) = JanEvap1965Subset[c(4,3)]
proj4string(JanEvap1965Subset) = CRS("+init=epsg:4326")
JanEvap1965Subset = spTransform(JanEvap1965Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanEvap1965 = variogram(meanEvap ~ LATITUDE+LONGITUDE, JanEvap1965Subset)
plot(vJanEvap1965)

vJanEvap1965.fit = fit.variogram(vJanEvap1965, vgm(0.5, 'Lin', 0))
plot(vJanEvap1965 ,vJanEvap1965.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap1965Subset, add=TRUE, pch=20, col="red")
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

JanEvap1965krige1 = krige(meanEvap ~ 1, JanEvap1965Subset, texasGrid, vJanEvap1965.fit)
spplot(JanEvap1965krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                              # AUGUST 1965 #

# Evaporation values for Auguary 1965 in Texas
AugEvap1965 = read_xlsx("1965/Aug/1965-Aug-Evap.xlsx")
AugEvap1965$LATITUDE = as.numeric(AugEvap1965$LATITUDE)
AugEvap1965$LONGITUDE = as.numeric(AugEvap1965$LONGITUDE)

#data frame with data for one day
AugEvap1965Subset = computeEvapAverage(AugEvap1965)
coordinates(AugEvap1965Subset) = AugEvap1965Subset[c(4,3)]
proj4string(AugEvap1965Subset) = CRS("+init=epsg:4326")
AugEvap1965Subset = spTransform(AugEvap1965Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugEvap1965 = variogram(meanEvap ~ LATITUDE+LONGITUDE, AugEvap1965Subset)
plot(vAugEvap1965)

vAugEvap1965.fit = fit.variogram(vAugEvap1965, vgm(1.5, 'Lin', 0))
plot(vAugEvap1965 ,vAugEvap1965.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap1965Subset, add=TRUE, pch=20, col="red")
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

AugEvap1965krige1 = krige(meanEvap ~ 1, AugEvap1965Subset, texasGrid, vAugEvap1965.fit)
spplot(AugEvap1965krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







                                          # JANUARY 1955 #

# Evaporation values for January 1955 in Texas
JanEvap1955 = read_xlsx("1955/Jan/1955-Jan-Evap.xlsx")
JanEvap1955$LATITUDE = as.numeric(JanEvap1955$LATITUDE)
JanEvap1955$LONGITUDE = as.numeric(JanEvap1955$LONGITUDE)

#data frame with data for one day
JanEvap1955Subset = computeEvapAverage(JanEvap1955)
coordinates(JanEvap1955Subset) = JanEvap1955Subset[c(4,3)]
proj4string(JanEvap1955Subset) = CRS("+init=epsg:4326")
JanEvap1955Subset = spTransform(JanEvap1955Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanEvap1955 = variogram(meanEvap ~ LATITUDE+LONGITUDE, JanEvap1955Subset)
plot(vJanEvap1955)

vJanEvap1955.fit = fit.variogram(vJanEvap1955, vgm(0.5, 'Lin', 0))
plot(vJanEvap1955 ,vJanEvap1955.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap1955Subset, add=TRUE, pch=20, col="red")
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

JanEvap1955krige1 = krige(meanEvap ~ 1, JanEvap1955Subset, texasGrid, vJanEvap1955.fit)
spplot(JanEvap1955krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                              # AUGUST 1955 #

# Evaporation values for Auguary 1955 in Texas
AugEvap1955 = read_xlsx("1955/Aug/1955-Aug-Evap.xlsx")
AugEvap1955$LATITUDE = as.numeric(AugEvap1955$LATITUDE)
AugEvap1955$LONGITUDE = as.numeric(AugEvap1955$LONGITUDE)

#data frame with data for one day
AugEvap1955Subset = computeEvapAverage(AugEvap1955)
coordinates(AugEvap1955Subset) = AugEvap1955Subset[c(4,3)]
proj4string(AugEvap1955Subset) = CRS("+init=epsg:4326")
AugEvap1955Subset = spTransform(AugEvap1955Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugEvap1955 = variogram(meanEvap ~ LATITUDE+LONGITUDE, AugEvap1955Subset)
plot(vAugEvap1955)

vAugEvap1955.fit = fit.variogram(vAugEvap1955, vgm(1.5, 'Lin', 0))
plot(vAugEvap1955 ,vAugEvap1955.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap1955Subset, add=TRUE, pch=20, col="red")
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

AugEvap1955krige1 = krige(meanEvap ~ 1, AugEvap1955Subset, texasGrid, vAugEvap1955.fit)
spplot(AugEvap1955krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))


# Not enough stations for 1945