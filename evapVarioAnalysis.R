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
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

                                      # JANUARY 2015 #

# Evaporation values for January 2015 in Texas
JanEvap2015 = read_xlsx("2015/Jan/2015-Jan-Evap.xlsx")
JanEvap2015$LATITUDE = as.numeric(JanEvap2015$LATITUDE)
JanEvap2015$LONGITUDE = as.numeric(JanEvap2015$LONGITUDE)

#data frame with data for one day
JanEvap2015Subset = JanEvap2015
JanEvap2015Subset$DATE = as.character(JanEvap2015Subset$DATE)
JanEvap2015Subset = subset(JanEvap2015Subset, JanEvap2015Subset$DATE == "2015-01-01")
JanEvap2015Subset = na.approx(JanEvap2015Subset)

JanEvap2015Locations = JanEvap2015[c(2,3,4)]
JanEvap2015Locations = JanEvap2015Locations[!duplicated(JanEvap2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap2015Locations) = JanEvap2015Locations[c(3,2)]

coordinates(JanEvap2015Subset) = JanEvap2015Subset[c(4,3)]

# variogram for the one day
vJanEvap2015 = variogram(JanEvap2015Subset$EVAP ~ 1, JanEvap2015Subset, cutoff=0.75)
plot(vJanEvap2015)

vJanEvap2015.fit = fit.variogram(vJanEvap2015, vgm(310, 'Lin', 0.8, 0))
plot(vJanEvap2015 ,vJanEvap2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap2015Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanEvap2015Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanEvap2015krige1 = krige(JanEvap2015Subset$EVAP ~ 1, JanEvap2015Subset, texasGrid, vJanEvap2015.fit) #ordinary kriging
spplot(JanEvap2015krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanEvap2015krige2 = krige(JanEvap2015Subset$EVAP ~ 1, JanEvap2015Subset, texasGrid, vJanEvap2015.fit, beta=10.0) #same with simple kriging
spplot(JanEvap2015krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanEvap2015krige3 = krige(JanEvap2015Subset$EVAP ~ 1, JanEvap2015Subset, texasGrid, vJanEvap2015.fit, nmax=20) #ordinary kriging
spplot(JanEvap2015krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                            # AUGUST 2015 #

# Evaporation values for Auguary 2015 in Texas
AugEvap2015 = read_xlsx("2015/Aug/2015-Aug-Evap.xlsx")
AugEvap2015$LATITUDE = as.numeric(AugEvap2015$LATITUDE)
AugEvap2015$LONGITUDE = as.numeric(AugEvap2015$LONGITUDE)

#data frame with data for one day
AugEvap2015Subset = AugEvap2015
AugEvap2015Subset$DATE = as.character(AugEvap2015Subset$DATE)
AugEvap2015Subset = subset(AugEvap2015Subset, AugEvap2015Subset$DATE == "2015-08-01")
AugEvap2015Subset = na.approx(AugEvap2015Subset)

AugEvap2015Locations = AugEvap2015[c(2,3,4)]
AugEvap2015Locations = AugEvap2015Locations[!duplicated(AugEvap2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap2015Locations) = AugEvap2015Locations[c(3,2)]

coordinates(AugEvap2015Subset) = AugEvap2015Subset[c(4,3)]

# variogram for the one day
vAugEvap2015 = variogram(AugEvap2015Subset$EVAP ~ 1, AugEvap2015Subset, cutoff=0.75)
plot(vAugEvap2015)

vAugEvap2015.fit = fit.variogram(vAugEvap2015, vgm(310, 'Lin', 0.8, 0))
plot(vAugEvap2015 ,vAugEvap2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap2015Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugEvap2015Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugEvap2015krige1 = krige(AugEvap2015Subset$EVAP ~ 1, AugEvap2015Subset, texasGrid, vAugEvap2015.fit) #ordinary kriging
spplot(AugEvap2015krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugEvap2015krige2 = krige(AugEvap2015Subset$EVAP ~ 1, AugEvap2015Subset, texasGrid, vAugEvap2015.fit, beta=10.0) #same with simple kriging
spplot(AugEvap2015krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugEvap2015krige3 = krige(AugEvap2015Subset$EVAP ~ 1, AugEvap2015Subset, texasGrid, vAugEvap2015.fit, nmax=20) #ordinary kriging
spplot(AugEvap2015krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))








                                          # JANUARY 2005 #

# Evaporation values for January 2005 in Texas
JanEvap2005 = read_xlsx("2005/Jan/2005-Jan-Evap.xlsx")
JanEvap2005$LATITUDE = as.numeric(JanEvap2005$LATITUDE)
JanEvap2005$LONGITUDE = as.numeric(JanEvap2005$LONGITUDE)

#data frame with data for one day
JanEvap2005Subset = JanEvap2005
JanEvap2005Subset$DATE = as.character(JanEvap2005Subset$DATE)
JanEvap2005Subset = subset(JanEvap2005Subset, JanEvap2005Subset$DATE == "2005-01-01")
JanEvap2005Subset = na.approx(JanEvap2005Subset)

JanEvap2005Locations = JanEvap2005[c(2,3,4)]
JanEvap2005Locations = JanEvap2005Locations[!duplicated(JanEvap2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap2005Locations) = JanEvap2005Locations[c(3,2)]

coordinates(JanEvap2005Subset) = JanEvap2005Subset[c(4,3)]

# variogram for the one day
vJanEvap2005 = variogram(JanEvap2005Subset$EVAP ~ 1, JanEvap2005Subset, cutoff=0.75)
plot(vJanEvap2005)

vJanEvap2005.fit = fit.variogram(vJanEvap2005, vgm(310, 'Lin', 0.8, 0))
plot(vJanEvap2005 ,vJanEvap2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap2005Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanEvap2005Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanEvap2005krige1 = krige(JanEvap2005Subset$EVAP ~ 1, JanEvap2005Subset, texasGrid, vJanEvap2005.fit) #ordinary kriging
spplot(JanEvap2005krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanEvap2005krige2 = krige(JanEvap2005Subset$EVAP ~ 1, JanEvap2005Subset, texasGrid, vJanEvap2005.fit, beta=10.0) #same with simple kriging
spplot(JanEvap2005krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanEvap2005krige3 = krige(JanEvap2005Subset$EVAP ~ 1, JanEvap2005Subset, texasGrid, vJanEvap2005.fit, nmax=20) #ordinary kriging
spplot(JanEvap2005krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                            # AUGUST 2005 #

# Evaporation values for Auguary 2005 in Texas
AugEvap2005 = read_xlsx("2005/Aug/2005-Aug-Evap.xlsx")
AugEvap2005$LATITUDE = as.numeric(AugEvap2005$LATITUDE)
AugEvap2005$LONGITUDE = as.numeric(AugEvap2005$LONGITUDE)

#data frame with data for one day
AugEvap2005Subset = AugEvap2005
AugEvap2005Subset$DATE = as.character(AugEvap2005Subset$DATE)
AugEvap2005Subset = subset(AugEvap2005Subset, AugEvap2005Subset$DATE == "2005-08-01")
AugEvap2005Subset = na.approx(AugEvap2005Subset)

AugEvap2005Locations = AugEvap2005[c(2,3,4)]
AugEvap2005Locations = AugEvap2005Locations[!duplicated(AugEvap2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap2005Locations) = AugEvap2005Locations[c(3,2)]

coordinates(AugEvap2005Subset) = AugEvap2005Subset[c(4,3)]

# variogram for the one day
vAugEvap2005 = variogram(AugEvap2005Subset$EVAP ~ 1, AugEvap2005Subset, cutoff=0.75)
plot(vAugEvap2005)

vAugEvap2005.fit = fit.variogram(vAugEvap2005, vgm(310, 'Lin', 0.8, 0))
plot(vAugEvap2005 ,vAugEvap2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap2005Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugEvap2005Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugEvap2005krige1 = krige(AugEvap2005Subset$EVAP ~ 1, AugEvap2005Subset, texasGrid, vAugEvap2005.fit) #ordinary kriging
spplot(AugEvap2005krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugEvap2005krige2 = krige(AugEvap2005Subset$EVAP ~ 1, AugEvap2005Subset, texasGrid, vAugEvap2005.fit, beta=10.0) #same with simple kriging
spplot(AugEvap2005krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugEvap2005krige3 = krige(AugEvap2005Subset$EVAP ~ 1, AugEvap2005Subset, texasGrid, vAugEvap2005.fit, nmax=20) #ordinary kriging
spplot(AugEvap2005krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                            # JANUARY 1995 #

# Evaporation values for January 1995 in Texas
JanEvap1995 = read_xlsx("1995/Jan/1995-Jan-Evap.xlsx")
JanEvap1995$LATITUDE = as.numeric(JanEvap1995$LATITUDE)
JanEvap1995$LONGITUDE = as.numeric(JanEvap1995$LONGITUDE)

#data frame with data for one day
JanEvap1995Subset = JanEvap1995
JanEvap1995Subset$DATE = as.character(JanEvap1995Subset$DATE)
JanEvap1995Subset = subset(JanEvap1995Subset, JanEvap1995Subset$DATE == "1995-01-01")
JanEvap1995Subset = na.approx(JanEvap1995Subset)

JanEvap1995Locations = JanEvap1995[c(2,3,4)]
JanEvap1995Locations = JanEvap1995Locations[!duplicated(JanEvap1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1995Locations) = JanEvap1995Locations[c(3,2)]

coordinates(JanEvap1995Subset) = JanEvap1995Subset[c(4,3)]

# variogram for the one day
vJanEvap1995 = variogram(JanEvap1995Subset$EVAP ~ 1, JanEvap1995Subset, cutoff=0.75)
plot(vJanEvap1995)

vJanEvap1995.fit = fit.variogram(vJanEvap1995, vgm(310, 'Lin', 0.8, 0))
plot(vJanEvap1995 ,vJanEvap1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap1995Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanEvap1995Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanEvap1995krige1 = krige(JanEvap1995Subset$EVAP ~ 1, JanEvap1995Subset, texasGrid, vJanEvap1995.fit) #ordinary kriging
spplot(JanEvap1995krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanEvap1995krige2 = krige(JanEvap1995Subset$EVAP ~ 1, JanEvap1995Subset, texasGrid, vJanEvap1995.fit, beta=10.0) #same with simple kriging
spplot(JanEvap1995krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanEvap1995krige3 = krige(JanEvap1995Subset$EVAP ~ 1, JanEvap1995Subset, texasGrid, vJanEvap1995.fit, nmax=20) #ordinary kriging
spplot(JanEvap1995krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                              # AUGUST 1995 #

# Evaporation values for Auguary 1995 in Texas
AugEvap1995 = read_xlsx("1995/Aug/1995-Aug-Evap.xlsx")
AugEvap1995$LATITUDE = as.numeric(AugEvap1995$LATITUDE)
AugEvap1995$LONGITUDE = as.numeric(AugEvap1995$LONGITUDE)

#data frame with data for one day
AugEvap1995Subset = AugEvap1995
AugEvap1995Subset$DATE = as.character(AugEvap1995Subset$DATE)
AugEvap1995Subset = subset(AugEvap1995Subset, AugEvap1995Subset$DATE == "1995-08-01")
AugEvap1995Subset = na.approx(AugEvap1995Subset)

AugEvap1995Locations = AugEvap1995[c(2,3,4)]
AugEvap1995Locations = AugEvap1995Locations[!duplicated(AugEvap1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1995Locations) = AugEvap1995Locations[c(3,2)]

coordinates(AugEvap1995Subset) = AugEvap1995Subset[c(4,3)]

# variogram for the one day
vAugEvap1995 = variogram(AugEvap1995Subset$EVAP ~ 1, AugEvap1995Subset, cutoff=0.75)
plot(vAugEvap1995)

vAugEvap1995.fit = fit.variogram(vAugEvap1995, vgm(310, 'Lin', 0.8, 0))
plot(vAugEvap1995 ,vAugEvap1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap1995Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugEvap1995Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugEvap1995krige1 = krige(AugEvap1995Subset$EVAP ~ 1, AugEvap1995Subset, texasGrid, vAugEvap1995.fit) #ordinary kriging
spplot(AugEvap1995krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugEvap1995krige2 = krige(AugEvap1995Subset$EVAP ~ 1, AugEvap1995Subset, texasGrid, vAugEvap1995.fit, beta=10.0) #same with simple kriging
spplot(AugEvap1995krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugEvap1995krige3 = krige(AugEvap1995Subset$EVAP ~ 1, AugEvap1995Subset, texasGrid, vAugEvap1995.fit, nmax=20) #ordinary kriging
spplot(AugEvap1995krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))








                                          # JANUARY 1985 #

# Evaporation values for January 1985 in Texas
JanEvap1985 = read_xlsx("1985/Jan/1985-Jan-Evap.xlsx")
JanEvap1985$LATITUDE = as.numeric(JanEvap1985$LATITUDE)
JanEvap1985$LONGITUDE = as.numeric(JanEvap1985$LONGITUDE)

#data frame with data for one day
JanEvap1985Subset = JanEvap1985
JanEvap1985Subset$DATE = as.character(JanEvap1985Subset$DATE)
JanEvap1985Subset = subset(JanEvap1985Subset, JanEvap1985Subset$DATE == "1985-01-01")
JanEvap1985Subset = na.approx(JanEvap1985Subset)

JanEvap1985Locations = JanEvap1985[c(2,3,4)]
JanEvap1985Locations = JanEvap1985Locations[!duplicated(JanEvap1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1985Locations) = JanEvap1985Locations[c(3,2)]

coordinates(JanEvap1985Subset) = JanEvap1985Subset[c(4,3)]

# variogram for the one day
vJanEvap1985 = variogram(JanEvap1985Subset$EVAP ~ 1, JanEvap1985Subset, cutoff=0.75)
plot(vJanEvap1985)

vJanEvap1985.fit = fit.variogram(vJanEvap1985, vgm(310, 'Lin', 0.8, 0))
plot(vJanEvap1985 ,vJanEvap1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap1985Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanEvap1985Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanEvap1985krige1 = krige(JanEvap1985Subset$EVAP ~ 1, JanEvap1985Subset, texasGrid, vJanEvap1985.fit) #ordinary kriging
spplot(JanEvap1985krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanEvap1985krige2 = krige(JanEvap1985Subset$EVAP ~ 1, JanEvap1985Subset, texasGrid, vJanEvap1985.fit, beta=10.0) #same with simple kriging
spplot(JanEvap1985krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanEvap1985krige3 = krige(JanEvap1985Subset$EVAP ~ 1, JanEvap1985Subset, texasGrid, vJanEvap1985.fit, nmax=20) #ordinary kriging
spplot(JanEvap1985krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                            # AUGUST 1985 #

# Evaporation values for Auguary 1985 in Texas
AugEvap1985 = read_xlsx("1985/Aug/1985-Aug-Evap.xlsx")
AugEvap1985$LATITUDE = as.numeric(AugEvap1985$LATITUDE)
AugEvap1985$LONGITUDE = as.numeric(AugEvap1985$LONGITUDE)

#data frame with data for one day
AugEvap1985Subset = AugEvap1985
AugEvap1985Subset$DATE = as.character(AugEvap1985Subset$DATE)
AugEvap1985Subset = subset(AugEvap1985Subset, AugEvap1985Subset$DATE == "1985-08-01")
AugEvap1985Subset = na.approx(AugEvap1985Subset)

AugEvap1985Locations = AugEvap1985[c(2,3,4)]
AugEvap1985Locations = AugEvap1985Locations[!duplicated(AugEvap1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1985Locations) = AugEvap1985Locations[c(3,2)]

coordinates(AugEvap1985Subset) = AugEvap1985Subset[c(4,3)]

# variogram for the one day
vAugEvap1985 = variogram(AugEvap1985Subset$EVAP ~ 1, AugEvap1985Subset, cutoff=0.75)
plot(vAugEvap1985)

vAugEvap1985.fit = fit.variogram(vAugEvap1985, vgm(310, 'Lin', 0.8, 0))
plot(vAugEvap1985 ,vAugEvap1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap1985Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugEvap1985Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugEvap1985krige1 = krige(AugEvap1985Subset$EVAP ~ 1, AugEvap1985Subset, texasGrid, vAugEvap1985.fit) #ordinary kriging
spplot(AugEvap1985krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugEvap1985krige2 = krige(AugEvap1985Subset$EVAP ~ 1, AugEvap1985Subset, texasGrid, vAugEvap1985.fit, beta=10.0) #same with simple kriging
spplot(AugEvap1985krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugEvap1985krige3 = krige(AugEvap1985Subset$EVAP ~ 1, AugEvap1985Subset, texasGrid, vAugEvap1985.fit, nmax=20) #ordinary kriging
spplot(AugEvap1985krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







                                          # JANUARY 1975 #

# Evaporation values for January 1975 in Texas
JanEvap1975 = read_xlsx("1975/Jan/1975-Jan-Evap.xlsx")
JanEvap1975$LATITUDE = as.numeric(JanEvap1975$LATITUDE)
JanEvap1975$LONGITUDE = as.numeric(JanEvap1975$LONGITUDE)

#data frame with data for one day
JanEvap1975Subset = JanEvap1975
JanEvap1975Subset$DATE = as.character(JanEvap1975Subset$DATE)
JanEvap1975Subset = subset(JanEvap1975Subset, JanEvap1975Subset$DATE == "1975-01-01")
JanEvap1975Subset = na.approx(JanEvap1975Subset)

JanEvap1975Locations = JanEvap1975[c(2,3,4)]
JanEvap1975Locations = JanEvap1975Locations[!duplicated(JanEvap1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1975Locations) = JanEvap1975Locations[c(3,2)]

coordinates(JanEvap1975Subset) = JanEvap1975Subset[c(4,3)]

# variogram for the one day
vJanEvap1975 = variogram(JanEvap1975Subset$EVAP ~ 1, JanEvap1975Subset, cutoff=0.75)
plot(vJanEvap1975)

vJanEvap1975.fit = fit.variogram(vJanEvap1975, vgm(310, 'Lin', 0.8, 0))
plot(vJanEvap1975 ,vJanEvap1975.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap1975Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanEvap1975Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanEvap1975krige1 = krige(JanEvap1975Subset$EVAP ~ 1, JanEvap1975Subset, texasGrid, vJanEvap1975.fit) #ordinary kriging
spplot(JanEvap1975krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanEvap1975krige2 = krige(JanEvap1975Subset$EVAP ~ 1, JanEvap1975Subset, texasGrid, vJanEvap1975.fit, beta=10.0) #same with simple kriging
spplot(JanEvap1975krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanEvap1975krige3 = krige(JanEvap1975Subset$EVAP ~ 1, JanEvap1975Subset, texasGrid, vJanEvap1975.fit, nmax=20) #ordinary kriging
spplot(JanEvap1975krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                              # AUGUST 1975 #

# Evaporation values for Auguary 1975 in Texas
AugEvap1975 = read_xlsx("1975/Aug/1975-Aug-Evap.xlsx")
AugEvap1975$LATITUDE = as.numeric(AugEvap1975$LATITUDE)
AugEvap1975$LONGITUDE = as.numeric(AugEvap1975$LONGITUDE)

#data frame with data for one day
AugEvap1975Subset = AugEvap1975
AugEvap1975Subset$DATE = as.character(AugEvap1975Subset$DATE)
AugEvap1975Subset = subset(AugEvap1975Subset, AugEvap1975Subset$DATE == "1975-08-01")
AugEvap1975Subset = na.approx(AugEvap1975Subset)

AugEvap1975Locations = AugEvap1975[c(2,3,4)]
AugEvap1975Locations = AugEvap1975Locations[!duplicated(AugEvap1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1975Locations) = AugEvap1975Locations[c(3,2)]

coordinates(AugEvap1975Subset) = AugEvap1975Subset[c(4,3)]

# variogram for the one day
vAugEvap1975 = variogram(AugEvap1975Subset$EVAP ~ 1, AugEvap1975Subset, cutoff=0.75)
plot(vAugEvap1975)

vAugEvap1975.fit = fit.variogram(vAugEvap1975, vgm(310, 'Lin', 0.8, 0))
plot(vAugEvap1975 ,vAugEvap1975.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap1975Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugEvap1975Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugEvap1975krige1 = krige(AugEvap1975Subset$EVAP ~ 1, AugEvap1975Subset, texasGrid, vAugEvap1975.fit) #ordinary kriging
spplot(AugEvap1975krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugEvap1975krige2 = krige(AugEvap1975Subset$EVAP ~ 1, AugEvap1975Subset, texasGrid, vAugEvap1975.fit, beta=10.0) #same with simple kriging
spplot(AugEvap1975krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugEvap1975krige3 = krige(AugEvap1975Subset$EVAP ~ 1, AugEvap1975Subset, texasGrid, vAugEvap1975.fit, nmax=20) #ordinary kriging
spplot(AugEvap1975krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                          # JANUARY 1965 #

# Evaporation values for January 1965 in Texas
JanEvap1965 = read_xlsx("1965/Jan/1965-Jan-Evap.xlsx")
JanEvap1965$LATITUDE = as.numeric(JanEvap1965$LATITUDE)
JanEvap1965$LONGITUDE = as.numeric(JanEvap1965$LONGITUDE)

#data frame with data for one day
JanEvap1965Subset = JanEvap1965
JanEvap1965Subset$DATE = as.character(JanEvap1965Subset$DATE)
JanEvap1965Subset = subset(JanEvap1965Subset, JanEvap1965Subset$DATE == "1965-01-01")
JanEvap1965Subset = na.approx(JanEvap1965Subset)

JanEvap1965Locations = JanEvap1965[c(2,3,4)]
JanEvap1965Locations = JanEvap1965Locations[!duplicated(JanEvap1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1965Locations) = JanEvap1965Locations[c(3,2)]

coordinates(JanEvap1965Subset) = JanEvap1965Subset[c(4,3)]

# variogram for the one day
vJanEvap1965 = variogram(JanEvap1965Subset$EVAP ~ 1, JanEvap1965Subset, cutoff=0.75)
plot(vJanEvap1965)

vJanEvap1965.fit = fit.variogram(vJanEvap1965, vgm(310, 'Lin', 0.8, 0))
plot(vJanEvap1965 ,vJanEvap1965.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap1965Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanEvap1965Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanEvap1965krige1 = krige(JanEvap1965Subset$EVAP ~ 1, JanEvap1965Subset, texasGrid, vJanEvap1965.fit) #ordinary kriging
spplot(JanEvap1965krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanEvap1965krige2 = krige(JanEvap1965Subset$EVAP ~ 1, JanEvap1965Subset, texasGrid, vJanEvap1965.fit, beta=10.0) #same with simple kriging
spplot(JanEvap1965krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanEvap1965krige3 = krige(JanEvap1965Subset$EVAP ~ 1, JanEvap1965Subset, texasGrid, vJanEvap1965.fit, nmax=20) #ordinary kriging
spplot(JanEvap1965krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                          # AUGUST 1965 #

# Evaporation values for Auguary 1965 in Texas
AugEvap1965 = read_xlsx("1965/Aug/1965-Aug-Evap.xlsx")
AugEvap1965$LATITUDE = as.numeric(AugEvap1965$LATITUDE)
AugEvap1965$LONGITUDE = as.numeric(AugEvap1965$LONGITUDE)

#data frame with data for one day
AugEvap1965Subset = AugEvap1965
AugEvap1965Subset$DATE = as.character(AugEvap1965Subset$DATE)
AugEvap1965Subset = subset(AugEvap1965Subset, AugEvap1965Subset$DATE == "1965-08-01")
AugEvap1965Subset = na.approx(AugEvap1965Subset)

AugEvap1965Locations = AugEvap1965[c(2,3,4)]
AugEvap1965Locations = AugEvap1965Locations[!duplicated(AugEvap1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1965Locations) = AugEvap1965Locations[c(3,2)]

coordinates(AugEvap1965Subset) = AugEvap1965Subset[c(4,3)]

# variogram for the one day
vAugEvap1965 = variogram(AugEvap1965Subset$EVAP ~ 1, AugEvap1965Subset, cutoff=0.75)
plot(vAugEvap1965)

vAugEvap1965.fit = fit.variogram(vAugEvap1965, vgm(310, 'Lin', 0.8, 0))
plot(vAugEvap1965 ,vAugEvap1965.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap1965Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugEvap1965Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugEvap1965krige1 = krige(AugEvap1965Subset$EVAP ~ 1, AugEvap1965Subset, texasGrid, vAugEvap1965.fit) #ordinary kriging
spplot(AugEvap1965krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugEvap1965krige2 = krige(AugEvap1965Subset$EVAP ~ 1, AugEvap1965Subset, texasGrid, vAugEvap1965.fit, beta=10.0) #same with simple kriging
spplot(AugEvap1965krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugEvap1965krige3 = krige(AugEvap1965Subset$EVAP ~ 1, AugEvap1965Subset, texasGrid, vAugEvap1965.fit, nmax=20) #ordinary kriging
spplot(AugEvap1965krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))







                                          # JANUARY 1955 #

# Evaporation values for January 1955 in Texas
JanEvap1955 = read_xlsx("1955/Jan/1955-Jan-Evap.xlsx")
JanEvap1955$LATITUDE = as.numeric(JanEvap1955$LATITUDE)
JanEvap1955$LONGITUDE = as.numeric(JanEvap1955$LONGITUDE)

#data frame with data for one day
JanEvap1955Subset = JanEvap1955
JanEvap1955Subset$DATE = as.character(JanEvap1955Subset$DATE)
JanEvap1955Subset = subset(JanEvap1955Subset, JanEvap1955Subset$DATE == "1955-01-01")
JanEvap1955Subset = na.approx(JanEvap1955Subset)

JanEvap1955Locations = JanEvap1955[c(2,3,4)]
JanEvap1955Locations = JanEvap1955Locations[!duplicated(JanEvap1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1955Locations) = JanEvap1955Locations[c(3,2)]

coordinates(JanEvap1955Subset) = JanEvap1955Subset[c(4,3)]

# variogram for the one day
vJanEvap1955 = variogram(JanEvap1955Subset$EVAP ~ 1, JanEvap1955Subset, cutoff=0.75)
plot(vJanEvap1955)

vJanEvap1955.fit = fit.variogram(vJanEvap1955, vgm(310, 'Lin', 0.8, 0))
plot(vJanEvap1955 ,vJanEvap1955.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanEvap1955Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanEvap1955Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanEvap1955krige1 = krige(JanEvap1955Subset$EVAP ~ 1, JanEvap1955Subset, texasGrid, vJanEvap1955.fit) #ordinary kriging
spplot(JanEvap1955krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanEvap1955krige2 = krige(JanEvap1955Subset$EVAP ~ 1, JanEvap1955Subset, texasGrid, vJanEvap1955.fit, beta=10.0) #same with simple kriging
spplot(JanEvap1955krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanEvap1955krige3 = krige(JanEvap1955Subset$EVAP ~ 1, JanEvap1955Subset, texasGrid, vJanEvap1955.fit, nmax=20) #ordinary kriging
spplot(JanEvap1955krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))






                                            # AUGUST 1955 #

# Evaporation values for Auguary 1955 in Texas
AugEvap1955 = read_xlsx("1955/Aug/1955-Aug-Evap.xlsx")
AugEvap1955$LATITUDE = as.numeric(AugEvap1955$LATITUDE)
AugEvap1955$LONGITUDE = as.numeric(AugEvap1955$LONGITUDE)

#data frame with data for one day
AugEvap1955Subset = AugEvap1955
AugEvap1955Subset$DATE = as.character(AugEvap1955Subset$DATE)
AugEvap1955Subset = subset(AugEvap1955Subset, AugEvap1955Subset$DATE == "1955-08-01")
AugEvap1955Subset = na.approx(AugEvap1955Subset)

AugEvap1955Locations = AugEvap1955[c(2,3,4)]
AugEvap1955Locations = AugEvap1955Locations[!duplicated(AugEvap1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1955Locations) = AugEvap1955Locations[c(3,2)]

coordinates(AugEvap1955Subset) = AugEvap1955Subset[c(4,3)]

# variogram for the one day
vAugEvap1955 = variogram(AugEvap1955Subset$EVAP ~ 1, AugEvap1955Subset, cutoff=0.75)
plot(vAugEvap1955)

vAugEvap1955.fit = fit.variogram(vAugEvap1955, vgm(310, 'Lin', 0.8, 0))
plot(vAugEvap1955 ,vAugEvap1955.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugEvap1955Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugEvap1955Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugEvap1955krige1 = krige(AugEvap1955Subset$EVAP ~ 1, AugEvap1955Subset, texasGrid, vAugEvap1955.fit) #ordinary kriging
spplot(AugEvap1955krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugEvap1955krige2 = krige(AugEvap1955Subset$EVAP ~ 1, AugEvap1955Subset, texasGrid, vAugEvap1955.fit, beta=10.0) #same with simple kriging
spplot(AugEvap1955krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

AugEvap1955krige3 = krige(AugEvap1955Subset$EVAP ~ 1, AugEvap1955Subset, texasGrid, vAugEvap1955.fit, nmax=20) #ordinary kriging
spplot(AugEvap1955krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))


# Not enough stations for 1945