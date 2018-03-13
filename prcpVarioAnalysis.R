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


                                # JANUARY 2015 #

# Precipitation values for January 2015 in Texas
JanPrcp2015 = read_xlsx("2015/Jan/2015-Jan-Prcp.xlsx")
JanPrcp2015$LATITUDE = as.numeric(JanPrcp2015$LATITUDE)
JanPrcp2015$LONGITUDE = as.numeric(JanPrcp2015$LONGITUDE)

#data frame with data for one day
JanPrcp2015Subset = computePrcpAverage(JanPrcp2015)
coordinates(JanPrcp2015Subset) = JanPrcp2015Subset[c(4,3)]
proj4string(JanPrcp2015Subset) = CRS("+init=epsg:4326")
JanPrcp2015Subset = spTransform(JanPrcp2015Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#JanPrcp2015Zero = zerodist(JanPrcp2015Subset, zero=1000)
#JanPrcp2015Subset = JanPrcp2015Subset[-c(JanPrcp2015Zero[,1]),]

# variogram for the one day
vJanPrcp2015 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp2015Subset)
plot(vJanPrcp2015)

#vJanPrcp2015.fit = fit.variogram(vJanPrcp2015, vgm(42, 'Lin', 0))
vJanPrcp2015.fit = fit.variogram(vJanPrcp2015, vgm('Exp'))
plot(vJanPrcp2015 ,vJanPrcp2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanPrcp2015Subset, add=TRUE, pch=20, col="red")
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

JanPrcp2015krige1 = krige(meanPrcp ~ 1, JanPrcp2015Subset, texasGrid, vJanPrcp2015.fit)
spplot(JanPrcp2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





  
                                  # AUGUST 2015 #

# Precipitation values for Auguary 2015 in Texas
AugPrcp2015 = read_xlsx("2015/Aug/2015-Aug-Prcp.xlsx")
AugPrcp2015$LATITUDE = as.numeric(AugPrcp2015$LATITUDE)
AugPrcp2015$LONGITUDE = as.numeric(AugPrcp2015$LONGITUDE)

#data frame with data for one day
AugPrcp2015Subset = computePrcpAverage(AugPrcp2015)
coordinates(AugPrcp2015Subset) = AugPrcp2015Subset[c(4,3)]
proj4string(AugPrcp2015Subset) = CRS("+init=epsg:4326")
AugPrcp2015Subset = spTransform(AugPrcp2015Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugPrcp2015 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp2015Subset)
plot(vAugPrcp2015)

#vAugPrcp2015.fit = fit.variogram(vAugPrcp2015, vgm(42, 'Lin', 0, 32))
vAugPrcp2015.fit = fit.variogram(vAugPrcp2015, vgm('Exp'))
plot(vAugPrcp2015 ,vAugPrcp2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugPrcp2015Subset, add=TRUE, pch=20, col="red")
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

AugPrcp2015krige1 = krige(meanPrcp ~ 1, AugPrcp2015Subset, texasGrid, vAugPrcp2015.fit)
spplot(AugPrcp2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                      # JANUARY 2005 #

# Precipitation values for January 2005 in Texas
JanPrcp2005 = read_xlsx("2005/Jan/2005-Jan-Prcp.xlsx")
JanPrcp2005$LATITUDE = as.numeric(JanPrcp2005$LATITUDE)
JanPrcp2005$LONGITUDE = as.numeric(JanPrcp2005$LONGITUDE)

#data frame with data for one day
JanPrcp2005Subset = computePrcpAverage(JanPrcp2005)
coordinates(JanPrcp2005Subset) = JanPrcp2005Subset[c(4,3)]
proj4string(JanPrcp2005Subset) = CRS("+init=epsg:4326")
JanPrcp2005Subset = spTransform(JanPrcp2005Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#JanPrcp2005Zero = zerodist(JanPrcp2005Subset, zero=1000)
#JanPrcp2005Subset = JanPrcp2005Subset[-c(JanPrcp2005Zero[,1]),]

# variogram for the one day
vJanPrcp2005 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp2005Subset)
plot(vJanPrcp2005)

#vJanPrcp2005.fit = fit.variogram(vJanPrcp2005, vgm(42, 'Lin', 0))
vJanPrcp2005.fit = fit.variogram(vJanPrcp2005, vgm('Exp'))
plot(vJanPrcp2005 ,vJanPrcp2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanPrcp2005Subset, add=TRUE, pch=20, col="red")
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

JanPrcp2005krige1 = krige(meanPrcp ~ 1, JanPrcp2005Subset, texasGrid, vJanPrcp2005.fit)
spplot(JanPrcp2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                      # AUGUST 2005 #

# Precipitation values for Auguary 2005 in Texas
AugPrcp2005 = read_xlsx("2005/Aug/2005-Aug-Prcp.xlsx")
AugPrcp2005$LATITUDE = as.numeric(AugPrcp2005$LATITUDE)
AugPrcp2005$LONGITUDE = as.numeric(AugPrcp2005$LONGITUDE)

#data frame with data for one day
AugPrcp2005Subset = computePrcpAverage(AugPrcp2005)
coordinates(AugPrcp2005Subset) = AugPrcp2005Subset[c(4,3)]
proj4string(AugPrcp2005Subset) = CRS("+init=epsg:4326")
AugPrcp2005Subset = spTransform(AugPrcp2005Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugPrcp2005 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp2005Subset)
plot(vAugPrcp2005)

#vAugPrcp2005.fit = fit.variogram(vAugPrcp2005, vgm(42, 'Lin', 0, 32))
vAugPrcp2005.fit = fit.variogram(vAugPrcp2005, vgm('Exp'))
plot(vAugPrcp2005 ,vAugPrcp2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugPrcp2005Subset, add=TRUE, pch=20, col="red")
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

AugPrcp2005krige1 = krige(meanPrcp ~ 1, AugPrcp2005Subset, texasGrid, vAugPrcp2005.fit)
spplot(AugPrcp2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                        # JANUARY 1995 #

# Precipitation values for January 1995 in Texas
JanPrcp1995 = read_xlsx("1995/Jan/1995-Jan-Prcp.xlsx")
JanPrcp1995$LATITUDE = as.numeric(JanPrcp1995$LATITUDE)
JanPrcp1995$LONGITUDE = as.numeric(JanPrcp1995$LONGITUDE)

#data frame with data for one day
JanPrcp1995Subset = computePrcpAverage(JanPrcp1995)
coordinates(JanPrcp1995Subset) = JanPrcp1995Subset[c(4,3)]
proj4string(JanPrcp1995Subset) = CRS("+init=epsg:4326")
JanPrcp1995Subset = spTransform(JanPrcp1995Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#JanPrcp1995Zero = zerodist(JanPrcp1995Subset, zero=1000)
#JanPrcp1995Subset = JanPrcp1995Subset[-c(JanPrcp1995Zero[,1]),]

# variogram for the one day
vJanPrcp1995 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp1995Subset)
plot(vJanPrcp1995)

#vJanPrcp1995.fit = fit.variogram(vJanPrcp1995, vgm(42, 'Lin', 0))
vJanPrcp1995.fit = fit.variogram(vJanPrcp1995, vgm('Exp'))
plot(vJanPrcp1995 ,vJanPrcp1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanPrcp1995Subset, add=TRUE, pch=20, col="red")
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

JanPrcp1995krige1 = krige(meanPrcp ~ 1, JanPrcp1995Subset, texasGrid, vJanPrcp1995.fit)
spplot(JanPrcp1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                          # AUGUST 1995 #

# Precipitation values for Auguary 1995 in Texas
AugPrcp1995 = read_xlsx("1995/Aug/1995-Aug-Prcp.xlsx")
AugPrcp1995$LATITUDE = as.numeric(AugPrcp1995$LATITUDE)
AugPrcp1995$LONGITUDE = as.numeric(AugPrcp1995$LONGITUDE)

#data frame with data for one day
AugPrcp1995Subset = computePrcpAverage(AugPrcp1995)
coordinates(AugPrcp1995Subset) = AugPrcp1995Subset[c(4,3)]
proj4string(AugPrcp1995Subset) = CRS("+init=epsg:4326")
AugPrcp1995Subset = spTransform(AugPrcp1995Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugPrcp1995 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp1995Subset)
plot(vAugPrcp1995)

#vAugPrcp1995.fit = fit.variogram(vAugPrcp1995, vgm(42, 'Lin', 0, 32))
vAugPrcp1995.fit = fit.variogram(vAugPrcp1995, vgm('Exp'))
plot(vAugPrcp1995 ,vAugPrcp1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugPrcp1995Subset, add=TRUE, pch=20, col="red")
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

AugPrcp1995krige1 = krige(meanPrcp ~ 1, AugPrcp1995Subset, texasGrid, vAugPrcp1995.fit)
spplot(AugPrcp1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                          # JANUARY 1985 #

# Precipitation values for January 1985 in Texas
JanPrcp1985 = read_xlsx("1985/Jan/1985-Jan-Prcp.xlsx")
JanPrcp1985$LATITUDE = as.numeric(JanPrcp1985$LATITUDE)
JanPrcp1985$LONGITUDE = as.numeric(JanPrcp1985$LONGITUDE)

#data frame with data for one day
JanPrcp1985Subset = computePrcpAverage(JanPrcp1985)
coordinates(JanPrcp1985Subset) = JanPrcp1985Subset[c(4,3)]
proj4string(JanPrcp1985Subset) = CRS("+init=epsg:4326")
JanPrcp1985Subset = spTransform(JanPrcp1985Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#JanPrcp1985Zero = zerodist(JanPrcp1985Subset, zero=1000)
#JanPrcp1985Subset = JanPrcp1985Subset[-c(JanPrcp1985Zero[,1]),]

# variogram for the one day
vJanPrcp1985 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp1985Subset)
plot(vJanPrcp1985)

#vJanPrcp1985.fit = fit.variogram(vJanPrcp1985, vgm(42, 'Lin', 0))
vJanPrcp1985.fit = fit.variogram(vJanPrcp1985, vgm('Exp'))
plot(vJanPrcp1985 ,vJanPrcp1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanPrcp1985Subset, add=TRUE, pch=20, col="red")
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

JanPrcp1985krige1 = krige(meanPrcp ~ 1, JanPrcp1985Subset, texasGrid, vJanPrcp1985.fit)
spplot(JanPrcp1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                          # AUGUST 1985 #

# Precipitation values for Auguary 1985 in Texas
AugPrcp1985 = read_xlsx("1985/Aug/1985-Aug-Prcp.xlsx")
AugPrcp1985$LATITUDE = as.numeric(AugPrcp1985$LATITUDE)
AugPrcp1985$LONGITUDE = as.numeric(AugPrcp1985$LONGITUDE)

#data frame with data for one day
AugPrcp1985Subset = computePrcpAverage(AugPrcp1985)
coordinates(AugPrcp1985Subset) = AugPrcp1985Subset[c(4,3)]
proj4string(AugPrcp1985Subset) = CRS("+init=epsg:4326")
AugPrcp1985Subset = spTransform(AugPrcp1985Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugPrcp1985 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp1985Subset)
plot(vAugPrcp1985)

#vAugPrcp1985.fit = fit.variogram(vAugPrcp1985, vgm(42, 'Lin', 0, 32))
vAugPrcp1985.fit = fit.variogram(vAugPrcp1985, vgm('Exp'))
plot(vAugPrcp1985 ,vAugPrcp1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugPrcp1985Subset, add=TRUE, pch=20, col="red")
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

AugPrcp1985krige1 = krige(meanPrcp ~ 1, AugPrcp1985Subset, texasGrid, vAugPrcp1985.fit)
spplot(AugPrcp1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                                  # JANUARY 1975 #

# Precipitation values for January 1975 in Texas
JanPrcp1975 = read_xlsx("1975/Jan/1975-Jan-Prcp.xlsx")
JanPrcp1975$LATITUDE = as.numeric(JanPrcp1975$LATITUDE)
JanPrcp1975$LONGITUDE = as.numeric(JanPrcp1975$LONGITUDE)

#data frame with data for one day
JanPrcp1975Subset = computePrcpAverage(JanPrcp1975)
coordinates(JanPrcp1975Subset) = JanPrcp1975Subset[c(4,3)]
proj4string(JanPrcp1975Subset) = CRS("+init=epsg:4326")
JanPrcp1975Subset = spTransform(JanPrcp1975Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#JanPrcp1975Zero = zerodist(JanPrcp1975Subset, zero=1000)
#JanPrcp1975Subset = JanPrcp1975Subset[-c(JanPrcp1975Zero[,1]),]

# variogram for the one day
vJanPrcp1975 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp1975Subset)
plot(vJanPrcp1975)

#vJanPrcp1975.fit = fit.variogram(vJanPrcp1975, vgm(42, 'Lin', 0))
vJanPrcp1975.fit = fit.variogram(vJanPrcp1975, vgm('Exp'))
plot(vJanPrcp1975 ,vJanPrcp1975.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanPrcp1975Subset, add=TRUE, pch=20, col="red")
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

JanPrcp1975krige1 = krige(meanPrcp ~ 1, JanPrcp1975Subset, texasGrid, vJanPrcp1975.fit)
spplot(JanPrcp1975krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                          # AUGUST 1975 #

# Precipitation values for Auguary 1975 in Texas
AugPrcp1975 = read_xlsx("1975/Aug/1975-Aug-Prcp.xlsx")
AugPrcp1975$LATITUDE = as.numeric(AugPrcp1975$LATITUDE)
AugPrcp1975$LONGITUDE = as.numeric(AugPrcp1975$LONGITUDE)

#data frame with data for one day
AugPrcp1975Subset = computePrcpAverage(AugPrcp1975)
coordinates(AugPrcp1975Subset) = AugPrcp1975Subset[c(4,3)]
proj4string(AugPrcp1975Subset) = CRS("+init=epsg:4326")
AugPrcp1975Subset = spTransform(AugPrcp1975Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugPrcp1975 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp1975Subset)
plot(vAugPrcp1975)

#vAugPrcp1975.fit = fit.variogram(vAugPrcp1975, vgm(42, 'Lin', 0, 32))
vAugPrcp1975.fit = fit.variogram(vAugPrcp1975, vgm('Exp'))
plot(vAugPrcp1975 ,vAugPrcp1975.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugPrcp1975Subset, add=TRUE, pch=20, col="red")
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

AugPrcp1975krige1 = krige(meanPrcp ~ 1, AugPrcp1975Subset, texasGrid, vAugPrcp1975.fit)
spplot(AugPrcp1975krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                    # JANUARY 1965 #

# Precipitation values for January 1965 in Texas
JanPrcp1965 = read_xlsx("1965/Jan/1965-Jan-Prcp.xlsx")
JanPrcp1965$LATITUDE = as.numeric(JanPrcp1965$LATITUDE)
JanPrcp1965$LONGITUDE = as.numeric(JanPrcp1965$LONGITUDE)

#data frame with data for one day
JanPrcp1965Subset = computePrcpAverage(JanPrcp1965)
coordinates(JanPrcp1965Subset) = JanPrcp1965Subset[c(4,3)]
proj4string(JanPrcp1965Subset) = CRS("+init=epsg:4326")
JanPrcp1965Subset = spTransform(JanPrcp1965Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#JanPrcp1965Zero = zerodist(JanPrcp1965Subset, zero=1000)
#JanPrcp1965Subset = JanPrcp1965Subset[-c(JanPrcp1965Zero[,1]),]

# variogram for the one day
vJanPrcp1965 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp1965Subset)
plot(vJanPrcp1965)

#vJanPrcp1965.fit = fit.variogram(vJanPrcp1965, vgm(42, 'Lin', 0))
vJanPrcp1965.fit = fit.variogram(vJanPrcp1965, vgm('Exp'))
plot(vJanPrcp1965 ,vJanPrcp1965.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanPrcp1965Subset, add=TRUE, pch=20, col="red")
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

JanPrcp1965krige1 = krige(meanPrcp ~ 1, JanPrcp1965Subset, texasGrid, vJanPrcp1965.fit)
spplot(JanPrcp1965krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                        # AUGUST 1965 #

# Precipitation values for Auguary 1965 in Texas
AugPrcp1965 = read_xlsx("1965/Aug/1965-Aug-Prcp.xlsx")
AugPrcp1965$LATITUDE = as.numeric(AugPrcp1965$LATITUDE)
AugPrcp1965$LONGITUDE = as.numeric(AugPrcp1965$LONGITUDE)

#data frame with data for one day
AugPrcp1965Subset = computePrcpAverage(AugPrcp1965)
coordinates(AugPrcp1965Subset) = AugPrcp1965Subset[c(4,3)]
proj4string(AugPrcp1965Subset) = CRS("+init=epsg:4326")
AugPrcp1965Subset = spTransform(AugPrcp1965Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugPrcp1965 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp1965Subset)
plot(vAugPrcp1965)

#vAugPrcp1965.fit = fit.variogram(vAugPrcp1965, vgm(42, 'Lin', 0, 32))
vAugPrcp1965.fit = fit.variogram(vAugPrcp1965, vgm('Exp'))
plot(vAugPrcp1965 ,vAugPrcp1965.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugPrcp1965Subset, add=TRUE, pch=20, col="red")
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

AugPrcp1965krige1 = krige(meanPrcp ~ 1, AugPrcp1965Subset, texasGrid, vAugPrcp1965.fit)
spplot(AugPrcp1965krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                    # JANUARY 1955 #

# Precipitation values for January 1955 in Texas
JanPrcp1955 = read_xlsx("1955/Jan/1955-Jan-Prcp.xlsx")
JanPrcp1955$LATITUDE = as.numeric(JanPrcp1955$LATITUDE)
JanPrcp1955$LONGITUDE = as.numeric(JanPrcp1955$LONGITUDE)

#data frame with data for one day
JanPrcp1955Subset = computePrcpAverage(JanPrcp1955)
coordinates(JanPrcp1955Subset) = JanPrcp1955Subset[c(4,3)]
proj4string(JanPrcp1955Subset) = CRS("+init=epsg:4326")
JanPrcp1955Subset = spTransform(JanPrcp1955Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#JanPrcp1955Zero = zerodist(JanPrcp1955Subset, zero=1000)
#JanPrcp1955Subset = JanPrcp1955Subset[-c(JanPrcp1955Zero[,1]),]

# variogram for the one day
vJanPrcp1955 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, JanPrcp1955Subset)
plot(vJanPrcp1955)

#vJanPrcp1955.fit = fit.variogram(vJanPrcp1955, vgm(42, 'Lin', 0))
vJanPrcp1955.fit = fit.variogram(vJanPrcp1955, vgm('Exp'))
plot(vJanPrcp1955 ,vJanPrcp1955.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanPrcp1955Subset, add=TRUE, pch=20, col="red")
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

JanPrcp1955krige1 = krige(meanPrcp ~ 1, JanPrcp1955Subset, texasGrid, vJanPrcp1955.fit)
spplot(JanPrcp1955krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                      # AUGUST 1955 #

# Precipitation values for Auguary 1955 in Texas
AugPrcp1955 = read_xlsx("1955/Aug/1955-Aug-Prcp.xlsx")
AugPrcp1955$LATITUDE = as.numeric(AugPrcp1955$LATITUDE)
AugPrcp1955$LONGITUDE = as.numeric(AugPrcp1955$LONGITUDE)

#data frame with data for one day
AugPrcp1955Subset = computePrcpAverage(AugPrcp1955)
coordinates(AugPrcp1955Subset) = AugPrcp1955Subset[c(4,3)]
proj4string(AugPrcp1955Subset) = CRS("+init=epsg:4326")
AugPrcp1955Subset = spTransform(AugPrcp1955Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugPrcp1955 = variogram(meanPrcp ~ LATITUDE+LONGITUDE, AugPrcp1955Subset)
plot(vAugPrcp1955)

#vAugPrcp1955.fit = fit.variogram(vAugPrcp1955, vgm(42, 'Lin', 0, 32))
vAugPrcp1955.fit = fit.variogram(vAugPrcp1955, vgm('Exp'))
plot(vAugPrcp1955 ,vAugPrcp1955.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugPrcp1955Subset, add=TRUE, pch=20, col="red")
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

AugPrcp1955krige1 = krige(meanPrcp ~ 1, AugPrcp1955Subset, texasGrid, vAugPrcp1955.fit)
spplot(AugPrcp1955krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))