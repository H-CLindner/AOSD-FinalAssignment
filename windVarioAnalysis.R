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
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

                                      # JANUARY 2015 #

# Average wind speed values for January 2015 in Texas
JanWind2015 = read_xlsx("2015/Jan/2015-Jan-Wind.xlsx")
JanWind2015$LATITUDE = as.numeric(JanWind2015$LATITUDE)
JanWind2015$LONGITUDE = as.numeric(JanWind2015$LONGITUDE)

#data frame with data for one day
JanWind2015Subset = JanWind2015
JanWind2015Subset$DATE = as.character(JanWind2015Subset$DATE)
JanWind2015Subset = subset(JanWind2015Subset, JanWind2015Subset$DATE == "2015-01-01")
JanWind2015Subset = na.approx(JanWind2015Subset)

JanWind2015Locations = JanWind2015[c(2,3,4)]
JanWind2015Locations = JanWind2015Locations[!duplicated(JanWind2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanWind2015Locations) = JanWind2015Locations[c(3,2)]

coordinates(JanWind2015Subset) = JanWind2015Subset[c(4,3)]

# variogram for the one day
vJanWind2015 = variogram(JanWind2015Subset$AWND ~ 1, JanWind2015Subset, cutoff=12)
plot(vJanWind2015)

vJanWind2015.fit = fit.variogram(vJanWind2015, vgm(4, 'Sph', 9, 0.8))
plot(vJanWind2015 ,vJanWind2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanWind2015Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanWind2015Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanWind2015krige1 = krige(JanWind2015Subset$AWND ~ 1, JanWind2015Subset, texasGrid, vJanWind2015.fit) #ordinary kriging
spplot(JanWind2015krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanWind2015krige2 = krige(JanWind2015Subset$AWND ~ 1, JanWind2015Subset, texasGrid, vJanWind2015.fit, beta=10.0) #same with simple kriging
spplot(JanWind2015krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanWind2015krige3 = krige(JanWind2015Subset$AWND ~ 1, JanWind2015Subset, texasGrid, vJanWind2015.fit, nmax=20) #ordinary kriging
spplot(JanWind2015krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                            # AUGUST 2015 #

# Average wind speed values for Auguary 2015 in Texas
AugWind2015 = read_xlsx("2015/Aug/2015-Aug-Wind.xlsx")
AugWind2015$LATITUDE = as.numeric(AugWind2015$LATITUDE)
AugWind2015$LONGITUDE = as.numeric(AugWind2015$LONGITUDE)

#data frame with data for one day
AugWind2015Subset = AugWind2015
AugWind2015Subset$DATE = as.character(AugWind2015Subset$DATE)
AugWind2015Subset = subset(AugWind2015Subset, AugWind2015Subset$DATE == "2015-08-01")
AugWind2015Subset = na.approx(AugWind2015Subset)

AugWind2015Locations = AugWind2015[c(2,3,4)]
AugWind2015Locations = AugWind2015Locations[!duplicated(AugWind2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugWind2015Locations) = AugWind2015Locations[c(3,2)]

coordinates(AugWind2015Subset) = AugWind2015Subset[c(4,3)]

# variogram for the one day
vAugWind2015 = variogram(AugWind2015Subset$AWND ~ 1, AugWind2015Subset, cutoff=12)
plot(vAugWind2015)

vAugWind2015.fit = fit.variogram(vAugWind2015, vgm(4, 'Sph', 9, 0.8))
plot(vAugWind2015 ,vAugWind2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugWind2015Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugWind2015Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugWind2015krige1 = krige(AugWind2015Subset$AWND ~ 1, AugWind2015Subset, texasGrid, vAugWind2015.fit) #ordinary kriging
spplot(AugWind2015krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugWind2015krige2 = krige(AugWind2015Subset$AWND ~ 1, AugWind2015Subset, texasGrid, vAugWind2015.fit, beta=10.0) #same with simple kriging
spplot(AugWind2015krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugWind2015krige3 = krige(AugWind2015Subset$AWND ~ 1, AugWind2015Subset, texasGrid, vAugWind2015.fit, nmax=20) #ordinary kriging
spplot(AugWind2015krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # JANUARY 2005 #

# Average wind speed values for January 2005 in Texas
JanWind2005 = read_xlsx("2005/Jan/2005-Jan-Wind.xlsx")
JanWind2005$LATITUDE = as.numeric(JanWind2005$LATITUDE)
JanWind2005$LONGITUDE = as.numeric(JanWind2005$LONGITUDE)

#data frame with data for one day
JanWind2005Subset = JanWind2005
JanWind2005Subset$DATE = as.character(JanWind2005Subset$DATE)
JanWind2005Subset = subset(JanWind2005Subset, JanWind2005Subset$DATE == "2005-01-01")
JanWind2005Subset = na.approx(JanWind2005Subset)

JanWind2005Locations = JanWind2005[c(2,3,4)]
JanWind2005Locations = JanWind2005Locations[!duplicated(JanWind2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanWind2005Locations) = JanWind2005Locations[c(3,2)]

coordinates(JanWind2005Subset) = JanWind2005Subset[c(4,3)]

# variogram for the one day
vJanWind2005 = variogram(JanWind2005Subset$AWND ~ 1, JanWind2005Subset, cutoff=12)
plot(vJanWind2005)

vJanWind2005.fit = fit.variogram(vJanWind2005, vgm(4, 'Sph', 9, 0.8))
plot(vJanWind2005 ,vJanWind2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanWind2005Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanWind2005Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanWind2005krige1 = krige(JanWind2005Subset$AWND ~ 1, JanWind2005Subset, texasGrid, vJanWind2005.fit) #ordinary kriging
spplot(JanWind2005krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanWind2005krige2 = krige(JanWind2005Subset$AWND ~ 1, JanWind2005Subset, texasGrid, vJanWind2005.fit, beta=10.0) #same with simple kriging
spplot(JanWind2005krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanWind2005krige3 = krige(JanWind2005Subset$AWND ~ 1, JanWind2005Subset, texasGrid, vJanWind2005.fit, nmax=20) #ordinary kriging
spplot(JanWind2005krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                              # AUGUST 2005 #

# Average wind speed values for Auguary 2005 in Texas
AugWind2005 = read_xlsx("2005/Aug/2005-Aug-Wind.xlsx")
AugWind2005$LATITUDE = as.numeric(AugWind2005$LATITUDE)
AugWind2005$LONGITUDE = as.numeric(AugWind2005$LONGITUDE)

#data frame with data for one day
AugWind2005Subset = AugWind2005
AugWind2005Subset$DATE = as.character(AugWind2005Subset$DATE)
AugWind2005Subset = subset(AugWind2005Subset, AugWind2005Subset$DATE == "2005-08-01")
AugWind2005Subset = na.approx(AugWind2005Subset)

AugWind2005Locations = AugWind2005[c(2,3,4)]
AugWind2005Locations = AugWind2005Locations[!duplicated(AugWind2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugWind2005Locations) = AugWind2005Locations[c(3,2)]

coordinates(AugWind2005Subset) = AugWind2005Subset[c(4,3)]

# variogram for the one day
vAugWind2005 = variogram(AugWind2005Subset$AWND ~ 1, AugWind2005Subset, cutoff=12)
plot(vAugWind2005)

vAugWind2005.fit = fit.variogram(vAugWind2005, vgm(4, 'Sph', 9, 0.8))
plot(vAugWind2005 ,vAugWind2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugWind2005Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugWind2005Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugWind2005krige1 = krige(AugWind2005Subset$AWND ~ 1, AugWind2005Subset, texasGrid, vAugWind2005.fit) #ordinary kriging
spplot(AugWind2005krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugWind2005krige2 = krige(AugWind2005Subset$AWND ~ 1, AugWind2005Subset, texasGrid, vAugWind2005.fit, beta=10.0) #same with simple kriging
spplot(AugWind2005krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugWind2005krige3 = krige(AugWind2005Subset$AWND ~ 1, AugWind2005Subset, texasGrid, vAugWind2005.fit, nmax=20) #ordinary kriging
spplot(AugWind2005krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))








                                                # JANUARY 1995 #

# Average wind speed values for January 1995 in Texas
JanWind1995 = read_xlsx("1995/Jan/1995-Jan-Wind.xlsx")
JanWind1995$LATITUDE = as.numeric(JanWind1995$LATITUDE)
JanWind1995$LONGITUDE = as.numeric(JanWind1995$LONGITUDE)

#data frame with data for one day
JanWind1995Subset = JanWind1995
JanWind1995Subset$DATE = as.character(JanWind1995Subset$DATE)
JanWind1995Subset = subset(JanWind1995Subset, JanWind1995Subset$DATE == "1995-01-01")
JanWind1995Subset = na.approx(JanWind1995Subset)

JanWind1995Locations = JanWind1995[c(2,3,4)]
JanWind1995Locations = JanWind1995Locations[!duplicated(JanWind1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanWind1995Locations) = JanWind1995Locations[c(3,2)]

coordinates(JanWind1995Subset) = JanWind1995Subset[c(4,3)]

# variogram for the one day
vJanWind1995 = variogram(JanWind1995Subset$AWND ~ 1, JanWind1995Subset, cutoff=12)
plot(vJanWind1995)

vJanWind1995.fit = fit.variogram(vJanWind1995, vgm(4, 'Sph', 9, 0.8))
plot(vJanWind1995 ,vJanWind1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanWind1995Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanWind1995Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanWind1995krige1 = krige(JanWind1995Subset$AWND ~ 1, JanWind1995Subset, texasGrid, vJanWind1995.fit) #ordinary kriging
spplot(JanWind1995krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanWind1995krige2 = krige(JanWind1995Subset$AWND ~ 1, JanWind1995Subset, texasGrid, vJanWind1995.fit, beta=10.0) #same with simple kriging
spplot(JanWind1995krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanWind1995krige3 = krige(JanWind1995Subset$AWND ~ 1, JanWind1995Subset, texasGrid, vJanWind1995.fit, nmax=20) #ordinary kriging
spplot(JanWind1995krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                                # AUGUST 1995 #

# Average wind speed values for Auguary 1995 in Texas
AugWind1995 = read_xlsx("1995/Aug/1995-Aug-Wind.xlsx")
AugWind1995$LATITUDE = as.numeric(AugWind1995$LATITUDE)
AugWind1995$LONGITUDE = as.numeric(AugWind1995$LONGITUDE)

#data frame with data for one day
AugWind1995Subset = AugWind1995
AugWind1995Subset$DATE = as.character(AugWind1995Subset$DATE)
AugWind1995Subset = subset(AugWind1995Subset, AugWind1995Subset$DATE == "1995-08-01")
AugWind1995Subset = na.approx(AugWind1995Subset)

AugWind1995Locations = AugWind1995[c(2,3,4)]
AugWind1995Locations = AugWind1995Locations[!duplicated(AugWind1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugWind1995Locations) = AugWind1995Locations[c(3,2)]

coordinates(AugWind1995Subset) = AugWind1995Subset[c(4,3)]

# variogram for the one day
vAugWind1995 = variogram(AugWind1995Subset$AWND ~ 1, AugWind1995Subset, cutoff=12)
plot(vAugWind1995)

vAugWind1995.fit = fit.variogram(vAugWind1995, vgm(4, 'Sph', 9, 0.8))
plot(vAugWind1995 ,vAugWind1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugWind1995Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugWind1995Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugWind1995krige1 = krige(AugWind1995Subset$AWND ~ 1, AugWind1995Subset, texasGrid, vAugWind1995.fit) #ordinary kriging
spplot(AugWind1995krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugWind1995krige2 = krige(AugWind1995Subset$AWND ~ 1, AugWind1995Subset, texasGrid, vAugWind1995.fit, beta=10.0) #same with simple kriging
spplot(AugWind1995krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugWind1995krige3 = krige(AugWind1995Subset$AWND ~ 1, AugWind1995Subset, texasGrid, vAugWind1995.fit, nmax=20) #ordinary kriging
spplot(AugWind1995krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                      # JANUARY 1985 #

# Average wind speed values for January 1985 in Texas
JanWind1985 = read_xlsx("1985/Jan/1985-Jan-Wind.xlsx")
JanWind1985$LATITUDE = as.numeric(JanWind1985$LATITUDE)
JanWind1985$LONGITUDE = as.numeric(JanWind1985$LONGITUDE)

#data frame with data for one day
JanWind1985Subset = JanWind1985
JanWind1985Subset$DATE = as.character(JanWind1985Subset$DATE)
JanWind1985Subset = subset(JanWind1985Subset, JanWind1985Subset$DATE == "1985-01-01")
JanWind1985Subset = na.approx(JanWind1985Subset)

JanWind1985Locations = JanWind1985[c(2,3,4)]
JanWind1985Locations = JanWind1985Locations[!duplicated(JanWind1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanWind1985Locations) = JanWind1985Locations[c(3,2)]

coordinates(JanWind1985Subset) = JanWind1985Subset[c(4,3)]

# variogram for the one day
vJanWind1985 = variogram(JanWind1985Subset$AWND ~ 1, JanWind1985Subset, cutoff=12)
plot(vJanWind1985)

vJanWind1985.fit = fit.variogram(vJanWind1985, vgm(4, 'Sph', 9, 0.8))
plot(vJanWind1985 ,vJanWind1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanWind1985Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanWind1985Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanWind1985krige1 = krige(JanWind1985Subset$AWND ~ 1, JanWind1985Subset, texasGrid, vJanWind1985.fit) #ordinary kriging
spplot(JanWind1985krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanWind1985krige2 = krige(JanWind1985Subset$AWND ~ 1, JanWind1985Subset, texasGrid, vJanWind1985.fit, beta=10.0) #same with simple kriging
spplot(JanWind1985krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanWind1985krige3 = krige(JanWind1985Subset$AWND ~ 1, JanWind1985Subset, texasGrid, vJanWind1985.fit, nmax=20) #ordinary kriging
spplot(JanWind1985krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                      # AUGUST 1985 #

# Average wind speed values for Auguary 1985 in Texas
AugWind1985 = read_xlsx("1985/Aug/1985-Aug-Wind.xlsx")
AugWind1985$LATITUDE = as.numeric(AugWind1985$LATITUDE)
AugWind1985$LONGITUDE = as.numeric(AugWind1985$LONGITUDE)

#data frame with data for one day
AugWind1985Subset = AugWind1985
AugWind1985Subset$DATE = as.character(AugWind1985Subset$DATE)
AugWind1985Subset = subset(AugWind1985Subset, AugWind1985Subset$DATE == "1985-08-01")
AugWind1985Subset = na.approx(AugWind1985Subset)

AugWind1985Locations = AugWind1985[c(2,3,4)]
AugWind1985Locations = AugWind1985Locations[!duplicated(AugWind1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugWind1985Locations) = AugWind1985Locations[c(3,2)]

coordinates(AugWind1985Subset) = AugWind1985Subset[c(4,3)]

# variogram for the one day
vAugWind1985 = variogram(AugWind1985Subset$AWND ~ 1, AugWind1985Subset, cutoff=12)
plot(vAugWind1985)

vAugWind1985.fit = fit.variogram(vAugWind1985, vgm(4, 'Sph', 9, 0.8))
plot(vAugWind1985 ,vAugWind1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugWind1985Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugWind1985Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugWind1985krige1 = krige(AugWind1985Subset$AWND ~ 1, AugWind1985Subset, texasGrid, vAugWind1985.fit) #ordinary kriging
spplot(AugWind1985krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugWind1985krige2 = krige(AugWind1985Subset$AWND ~ 1, AugWind1985Subset, texasGrid, vAugWind1985.fit, beta=10.0) #same with simple kriging
spplot(AugWind1985krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugWind1985krige3 = krige(AugWind1985Subset$AWND ~ 1, AugWind1985Subset, texasGrid, vAugWind1985.fit, nmax=20) #ordinary kriging
spplot(AugWind1985krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))