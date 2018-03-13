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

# !!!vgm() without input returns the models names!!!
#1    Nug                              Nug (nugget)
#2    Exp                         Exp (exponential)
#3    Sph                           Sph (spherical)
#4    Gau                            Gau (gaussian)   1
#5    Exc        Exclass (Exponential class/stable)
#6    Mat                              Mat (Matern)
#7    Ste Mat (Matern, M. Stein's parameterization)
#8    Cir                            Cir (circular)
#9    Lin                              Lin (linear)
#10   Bes                              Bes (bessel)   2
#11   Pen                      Pen (pentaspherical)
#12   Per                            Per (periodic)
#13   Wav                                Wav (wave)
#14   Hol                                Hol (hole)
#15   Log                         Log (logarithmic)
#16   Pow                               Pow (power)
#17   Spl                              Spl (spline)
#18   Leg                            Leg (Legendre)
#19   Err                   Err (Measurement error)
#20   Int                           Int (Intercept)
#

computeTempAverage = function(dataFrame){
  dataFrameSubset = mutate(dataFrame, month = format(DATE, "%m"))
  dataFrameMean = group_by(dataFrameSubset, STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, month) %>% summarise(meanTemp = mean(TMAX, na.rm = T))
  dataFrameMean$meanTemp = round(dataFrameMean$meanTemp, digits=4)
  return(dataFrameMean)
}


                                # JANUARY 2015 #


#define coordinates
# Temperature values for January 2015 in Texas
JanTmpMax2015 = read_xlsx("2015/Jan/2015-Jan-TMax.xlsx")
JanTmpMax2015$LATITUDE = as.numeric(JanTmpMax2015$LATITUDE)
JanTmpMax2015$LONGITUDE = as.numeric(JanTmpMax2015$LONGITUDE)

#data frame with data for one day
JanTmpMax2015Subset = computeTempAverage(JanTmpMax2015)
coordinates(JanTmpMax2015Subset) = JanTmpMax2015Subset[c(4,3)]
proj4string(JanTmpMax2015Subset) = CRS("+init=epsg:4326")
JanTmpMax2015Subset = spTransform(JanTmpMax2015Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanTmpMax2015 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax2015Subset)
plot(vJanTmpMax2015)

vJanTmpMax2015.fit = fit.variogram(vJanTmpMax2015, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax2015 ,vJanTmpMax2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax2015Subset, add=TRUE, pch=20, col="red")
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

JanTmpMax2015krige1 = krige(meanTemp ~ 1, JanTmpMax2015Subset, texasGrid, vJanTmpMax2015.fit)
spplot(JanTmpMax2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                            # AUGUST 2015 #



#define coordinates
# Temperature values for August 2015 in Texas
AugTmpMax2015 = read_xlsx("2015/Aug/2015-Aug-TMax.xlsx")
AugTmpMax2015$LATITUDE = as.numeric(AugTmpMax2015$LATITUDE)
AugTmpMax2015$LONGITUDE = as.numeric(AugTmpMax2015$LONGITUDE)

#data frame with data for one day
AugTmpMax2015Subset = computeTempAverage(AugTmpMax2015)
coordinates(AugTmpMax2015Subset) = AugTmpMax2015Subset[c(4,3)]
proj4string(AugTmpMax2015Subset) = CRS("+init=epsg:4326")
AugTmpMax2015Subset = spTransform(AugTmpMax2015Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugTmpMax2015 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax2015Subset)
plot(vAugTmpMax2015)

vAugTmpMax2015.fit = fit.variogram(vAugTmpMax2015, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax2015 ,vAugTmpMax2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax2015Subset, add=TRUE, pch=20, col="red")
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

AugTmpMax2015krige1 = krige(meanTemp ~ 1, AugTmpMax2015Subset, texasGrid, vAugTmpMax2015.fit)
spplot(AugTmpMax2015krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                  # JANUARY 2005 #


#define coordinates
# Temperature values for January 2005 in Texas
JanTmpMax2005 = read_xlsx("2005/Jan/2005-Jan-TMax.xlsx")
JanTmpMax2005$LATITUDE = as.numeric(JanTmpMax2005$LATITUDE)
JanTmpMax2005$LONGITUDE = as.numeric(JanTmpMax2005$LONGITUDE)

#data frame with data for one day
JanTmpMax2005Subset = computeTempAverage(JanTmpMax2005)
coordinates(JanTmpMax2005Subset) = JanTmpMax2005Subset[c(4,3)]
proj4string(JanTmpMax2005Subset) = CRS("+init=epsg:4326")
JanTmpMax2005Subset = spTransform(JanTmpMax2005Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanTmpMax2005 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax2005Subset)
plot(vJanTmpMax2005)

vJanTmpMax2005.fit = fit.variogram(vJanTmpMax2005, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax2005 ,vJanTmpMax2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax2005Subset, add=TRUE, pch=20, col="red")
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

JanTmpMax2005krige1 = krige(meanTemp ~ 1, JanTmpMax2005Subset, texasGrid, vJanTmpMax2005.fit)
spplot(JanTmpMax2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                  # AUGUST 2005 #



#define coordinates
# Temperature values for August 2005 in Texas
AugTmpMax2005 = read_xlsx("2005/Aug/2005-Aug-TMax.xlsx")
AugTmpMax2005$LATITUDE = as.numeric(AugTmpMax2005$LATITUDE)
AugTmpMax2005$LONGITUDE = as.numeric(AugTmpMax2005$LONGITUDE)

#data frame with data for one day
AugTmpMax2005Subset = computeTempAverage(AugTmpMax2005)
coordinates(AugTmpMax2005Subset) = AugTmpMax2005Subset[c(4,3)]
proj4string(AugTmpMax2005Subset) = CRS("+init=epsg:4326")
AugTmpMax2005Subset = spTransform(AugTmpMax2005Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugTmpMax2005 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax2005Subset)
plot(vAugTmpMax2005)

vAugTmpMax2005.fit = fit.variogram(vAugTmpMax2005, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax2005 ,vAugTmpMax2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax2005Subset, add=TRUE, pch=20, col="red")
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

AugTmpMax2005krige1 = krige(meanTemp ~ 1, AugTmpMax2005Subset, texasGrid, vAugTmpMax2005.fit)
spplot(AugTmpMax2005krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # JANUARY 1995 #


#define coordinates
# Temperature values for January 1995 in Texas
JanTmpMax1995 = read_xlsx("1995/Jan/1995-Jan-TMax.xlsx")
JanTmpMax1995$LATITUDE = as.numeric(JanTmpMax1995$LATITUDE)
JanTmpMax1995$LONGITUDE = as.numeric(JanTmpMax1995$LONGITUDE)

#data frame with data for one day
JanTmpMax1995Subset = computeTempAverage(JanTmpMax1995)
coordinates(JanTmpMax1995Subset) = JanTmpMax1995Subset[c(4,3)]
proj4string(JanTmpMax1995Subset) = CRS("+init=epsg:4326")
JanTmpMax1995Subset = spTransform(JanTmpMax1995Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanTmpMax1995 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax1995Subset)
plot(vJanTmpMax1995)

vJanTmpMax1995.fit = fit.variogram(vJanTmpMax1995, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax1995 ,vJanTmpMax1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1995Subset, add=TRUE, pch=20, col="red")
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

JanTmpMax1995krige1 = krige(meanTemp ~ 1, JanTmpMax1995Subset, texasGrid, vJanTmpMax1995.fit)
spplot(JanTmpMax1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # AUGUST 1995 #



#define coordinates
# Temperature values for August 1995 in Texas
AugTmpMax1995 = read_xlsx("1995/Aug/1995-Aug-TMax.xlsx")
AugTmpMax1995$LATITUDE = as.numeric(AugTmpMax1995$LATITUDE)
AugTmpMax1995$LONGITUDE = as.numeric(AugTmpMax1995$LONGITUDE)

#data frame with data for one day
AugTmpMax1995Subset = computeTempAverage(AugTmpMax1995)
coordinates(AugTmpMax1995Subset) = AugTmpMax1995Subset[c(4,3)]
proj4string(AugTmpMax1995Subset) = CRS("+init=epsg:4326")
AugTmpMax1995Subset = spTransform(AugTmpMax1995Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugTmpMax1995 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax1995Subset)
plot(vAugTmpMax1995)

vAugTmpMax1995.fit = fit.variogram(vAugTmpMax1995, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax1995 ,vAugTmpMax1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1995Subset, add=TRUE, pch=20, col="red")
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

AugTmpMax1995krige1 = krige(meanTemp ~ 1, AugTmpMax1995Subset, texasGrid, vAugTmpMax1995.fit)
spplot(AugTmpMax1995krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # JANUARY 1985 #


#define coordinates
# Temperature values for January 1985 in Texas
JanTmpMax1985 = read_xlsx("1985/Jan/1985-Jan-TMax.xlsx")
JanTmpMax1985$LATITUDE = as.numeric(JanTmpMax1985$LATITUDE)
JanTmpMax1985$LONGITUDE = as.numeric(JanTmpMax1985$LONGITUDE)

#data frame with data for one day
JanTmpMax1985Subset = computeTempAverage(JanTmpMax1985)
coordinates(JanTmpMax1985Subset) = JanTmpMax1985Subset[c(4,3)]
proj4string(JanTmpMax1985Subset) = CRS("+init=epsg:4326")
JanTmpMax1985Subset = spTransform(JanTmpMax1985Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanTmpMax1985 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax1985Subset)
plot(vJanTmpMax1985)

vJanTmpMax1985.fit = fit.variogram(vJanTmpMax1985, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax1985 ,vJanTmpMax1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1985Subset, add=TRUE, pch=20, col="red")
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

JanTmpMax1985krige1 = krige(meanTemp ~ 1, JanTmpMax1985Subset, texasGrid, vJanTmpMax1985.fit)
spplot(JanTmpMax1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                          # AUGUST 1985 #



#define coordinates
# Temperature values for August 1985 in Texas
AugTmpMax1985 = read_xlsx("1985/Aug/1985-Aug-TMax.xlsx")
AugTmpMax1985$LATITUDE = as.numeric(AugTmpMax1985$LATITUDE)
AugTmpMax1985$LONGITUDE = as.numeric(AugTmpMax1985$LONGITUDE)

#data frame with data for one day
AugTmpMax1985Subset = computeTempAverage(AugTmpMax1985)
coordinates(AugTmpMax1985Subset) = AugTmpMax1985Subset[c(4,3)]
proj4string(AugTmpMax1985Subset) = CRS("+init=epsg:4326")
AugTmpMax1985Subset = spTransform(AugTmpMax1985Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugTmpMax1985 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax1985Subset)
plot(vAugTmpMax1985)

vAugTmpMax1985.fit = fit.variogram(vAugTmpMax1985, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax1985 ,vAugTmpMax1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1985Subset, add=TRUE, pch=20, col="red")
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

AugTmpMax1985krige1 = krige(meanTemp ~ 1, AugTmpMax1985Subset, texasGrid, vAugTmpMax1985.fit)
spplot(AugTmpMax1985krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                        # JANUARY 1975 #


#define coordinates
# Temperature values for January 1975 in Texas
JanTmpMax1975 = read_xlsx("1975/Jan/1975-Jan-TMax.xlsx")
JanTmpMax1975$LATITUDE = as.numeric(JanTmpMax1975$LATITUDE)
JanTmpMax1975$LONGITUDE = as.numeric(JanTmpMax1975$LONGITUDE)

#data frame with data for one day
JanTmpMax1975Subset = computeTempAverage(JanTmpMax1975)
coordinates(JanTmpMax1975Subset) = JanTmpMax1975Subset[c(4,3)]
proj4string(JanTmpMax1975Subset) = CRS("+init=epsg:4326")
JanTmpMax1975Subset = spTransform(JanTmpMax1975Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanTmpMax1975 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax1975Subset)
plot(vJanTmpMax1975)

vJanTmpMax1975.fit = fit.variogram(vJanTmpMax1975, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax1975 ,vJanTmpMax1975.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1975Subset, add=TRUE, pch=20, col="red")
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

JanTmpMax1975krige1 = krige(meanTemp ~ 1, JanTmpMax1975Subset, texasGrid, vJanTmpMax1975.fit)
spplot(JanTmpMax1975krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # AUGUST 1975 #



#define coordinates
# Temperature values for August 1975 in Texas
AugTmpMax1975 = read_xlsx("1975/Aug/1975-Aug-TMax.xlsx")
AugTmpMax1975$LATITUDE = as.numeric(AugTmpMax1975$LATITUDE)
AugTmpMax1975$LONGITUDE = as.numeric(AugTmpMax1975$LONGITUDE)

#data frame with data for one day
AugTmpMax1975Subset = computeTempAverage(AugTmpMax1975)
coordinates(AugTmpMax1975Subset) = AugTmpMax1975Subset[c(4,3)]
proj4string(AugTmpMax1975Subset) = CRS("+init=epsg:4326")
AugTmpMax1975Subset = spTransform(AugTmpMax1975Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugTmpMax1975 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax1975Subset)
plot(vAugTmpMax1975)

vAugTmpMax1975.fit = fit.variogram(vAugTmpMax1975, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax1975 ,vAugTmpMax1975.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1975Subset, add=TRUE, pch=20, col="red")
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

AugTmpMax1975krige1 = krige(meanTemp ~ 1, AugTmpMax1975Subset, texasGrid, vAugTmpMax1975.fit)
spplot(AugTmpMax1975krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                      # JANUARY 1965 #


#define coordinates
# Temperature values for January 1965 in Texas
JanTmpMax1965 = read_xlsx("1965/Jan/1965-Jan-TMax.xlsx")
JanTmpMax1965$LATITUDE = as.numeric(JanTmpMax1965$LATITUDE)
JanTmpMax1965$LONGITUDE = as.numeric(JanTmpMax1965$LONGITUDE)

#data frame with data for one day
JanTmpMax1965Subset = computeTempAverage(JanTmpMax1965)
coordinates(JanTmpMax1965Subset) = JanTmpMax1965Subset[c(4,3)]
proj4string(JanTmpMax1965Subset) = CRS("+init=epsg:4326")
JanTmpMax1965Subset = spTransform(JanTmpMax1965Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanTmpMax1965 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax1965Subset)
plot(vJanTmpMax1965)

vJanTmpMax1965.fit = fit.variogram(vJanTmpMax1965, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax1965 ,vJanTmpMax1965.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1965Subset, add=TRUE, pch=20, col="red")
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

JanTmpMax1965krige1 = krige(meanTemp ~ 1, JanTmpMax1965Subset, texasGrid, vJanTmpMax1965.fit)
spplot(JanTmpMax1965krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # AUGUST 1965 #



#define coordinates
# Temperature values for August 1965 in Texas
AugTmpMax1965 = read_xlsx("1965/Aug/1965-Aug-TMax.xlsx")
AugTmpMax1965$LATITUDE = as.numeric(AugTmpMax1965$LATITUDE)
AugTmpMax1965$LONGITUDE = as.numeric(AugTmpMax1965$LONGITUDE)

#data frame with data for one day
AugTmpMax1965Subset = computeTempAverage(AugTmpMax1965)
coordinates(AugTmpMax1965Subset) = AugTmpMax1965Subset[c(4,3)]
proj4string(AugTmpMax1965Subset) = CRS("+init=epsg:4326")
AugTmpMax1965Subset = spTransform(AugTmpMax1965Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugTmpMax1965 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax1965Subset)
plot(vAugTmpMax1965)

vAugTmpMax1965.fit = fit.variogram(vAugTmpMax1965, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax1965 ,vAugTmpMax1965.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1965Subset, add=TRUE, pch=20, col="red")
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

AugTmpMax1965krige1 = krige(meanTemp ~ 1, AugTmpMax1965Subset, texasGrid, vAugTmpMax1965.fit)
spplot(AugTmpMax1965krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                    # JANUARY 1955 #


#define coordinates
# Temperature values for January 1955 in Texas
JanTmpMax1955 = read_xlsx("1955/Jan/1955-Jan-TMax.xlsx")
JanTmpMax1955$LATITUDE = as.numeric(JanTmpMax1955$LATITUDE)
JanTmpMax1955$LONGITUDE = as.numeric(JanTmpMax1955$LONGITUDE)

#data frame with data for one day
JanTmpMax1955Subset = computeTempAverage(JanTmpMax1955)
coordinates(JanTmpMax1955Subset) = JanTmpMax1955Subset[c(4,3)]
proj4string(JanTmpMax1955Subset) = CRS("+init=epsg:4326")
JanTmpMax1955Subset = spTransform(JanTmpMax1955Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vJanTmpMax1955 = variogram(meanTemp ~ LATITUDE+LONGITUDE, JanTmpMax1955Subset)
plot(vJanTmpMax1955)

vJanTmpMax1955.fit = fit.variogram(vJanTmpMax1955, vgm(2, 'Lin', 0, 1))
plot(vJanTmpMax1955 ,vJanTmpMax1955.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1955Subset, add=TRUE, pch=20, col="red")
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

JanTmpMax1955krige1 = krige(meanTemp ~ 1, JanTmpMax1955Subset, texasGrid, vJanTmpMax1955.fit)
spplot(JanTmpMax1955krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                                    # AUGUST 1955 #



#define coordinates
# Temperature values for August 1955 in Texas
AugTmpMax1955 = read_xlsx("1955/Aug/1955-Aug-TMax.xlsx")
AugTmpMax1955$LATITUDE = as.numeric(AugTmpMax1955$LATITUDE)
AugTmpMax1955$LONGITUDE = as.numeric(AugTmpMax1955$LONGITUDE)

#data frame with data for one day
AugTmpMax1955Subset = computeTempAverage(AugTmpMax1955)
coordinates(AugTmpMax1955Subset) = AugTmpMax1955Subset[c(4,3)]
proj4string(AugTmpMax1955Subset) = CRS("+init=epsg:4326")
AugTmpMax1955Subset = spTransform(AugTmpMax1955Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# variogram for the one day
vAugTmpMax1955 = variogram(meanTemp ~ LATITUDE+LONGITUDE, AugTmpMax1955Subset)
plot(vAugTmpMax1955)

vAugTmpMax1955.fit = fit.variogram(vAugTmpMax1955, vgm(2, 'Lin', 0, 1))
plot(vAugTmpMax1955 ,vAugTmpMax1955.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1955Subset, add=TRUE, pch=20, col="red")
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

AugTmpMax1955krige1 = krige(meanTemp ~ 1, AugTmpMax1955Subset, texasGrid, vAugTmpMax1955.fit)
spplot(AugTmpMax1955krige1[1], sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))
