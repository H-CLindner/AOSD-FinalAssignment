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


                                # JANUARY 2015 #


#define coordinates
# Temperature values for January 2015 in Texas
JanTmpMax2015 = read_xlsx("2015/Jan/2015-Jan-TMax.xlsx")
JanTmpMax2015$LATITUDE = as.numeric(JanTmpMax2015$LATITUDE)
JanTmpMax2015$LONGITUDE = as.numeric(JanTmpMax2015$LONGITUDE)

#data frame with data for one day
JanTmpMax2015Subset = JanTmpMax2015
JanTmpMax2015Subset$DATE = as.character(JanTmpMax2015Subset$DATE)
JanTmpMax2015Subset = subset(JanTmpMax2015Subset, JanTmpMax2015Subset$DATE == "2015-01-01")
JanTmpMax2015Subset = na.approx(JanTmpMax2015Subset)

JanTmpMax2015Locations = JanTmpMax2015Subset[c(2,3,4)]
JanTmpMax2015Locations = JanTmpMax2015Locations[!duplicated(JanTmpMax2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax2015Locations) = JanTmpMax2015Locations[c(3,2)]


coordinates(JanTmpMax2015Subset) = JanTmpMax2015Subset[c(4,3)]

# variogram for the one day
vJanTmpMax2015 = variogram(JanTmpMax2015Subset$TMAX ~ 1, JanTmpMax2015Subset, cutoff=9)
plot(vJanTmpMax2015)

vJanTmpMax2015.fit = fit.variogram(vJanTmpMax2015, vgm(60, 'Gau', 9, 5))
plot(vJanTmpMax2015 ,vJanTmpMax2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax2015Locations, add=TRUE, pch=20, col="red")
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanTmpMax2015Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanTmpMax2015krige1 = krige(JanTmpMax2015Subset$TMAX ~ 1, JanTmpMax2015Subset, texasGrid, vJanTmpMax2015.fit) #ordinary kriging
spplot(JanTmpMax2015krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanTmpMax2015krige2 = krige(JanTmpMax2015Subset$TMAX ~ 1, JanTmpMax2015Subset, texasGrid, vJanTmpMax2015.fit, beta=10.0) #same with simple kriging
spplot(JanTmpMax2015krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanTmpMax2015krige3 = krige(JanTmpMax2015Subset$TMAX ~ 1, JanTmpMax2015Subset, texasGrid, vJanTmpMax2015.fit, nmax=20) #ordinary kriging
spplot(JanTmpMax2015krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))



                            # AUGUST 2015 #



# Temperature values for Auguary 2015 in Texas
AugTmpMax2015 = read_xlsx("2015/Aug/2015-Aug-TMax.xlsx")
AugTmpMax2015$LATITUDE = as.numeric(AugTmpMax2015$LATITUDE)
AugTmpMax2015$LONGITUDE = as.numeric(AugTmpMax2015$LONGITUDE)

#data frame with data for one day
AugTmpMax2015Subset = AugTmpMax2015
AugTmpMax2015Subset$DATE = as.character(AugTmpMax2015Subset$DATE)
AugTmpMax2015Subset = subset(AugTmpMax2015Subset, AugTmpMax2015Subset$DATE == "2015-08-01")
AugTmpMax2015Subset = na.approx(AugTmpMax2015Subset)

AugTmpMax2015Locations = AugTmpMax2015Subset[c(2,3,4)]
AugTmpMax2015Locations = AugTmpMax2015Locations[!duplicated(AugTmpMax2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax2015Locations) = AugTmpMax2015Locations[c(3,2)]


coordinates(AugTmpMax2015Subset) = AugTmpMax2015Subset[c(4,3)]

# variogram for the one day
vAugTmpMax2015 = variogram(AugTmpMax2015Subset$TMAX ~ 1, AugTmpMax2015Subset, cutoff=9)
plot(vAugTmpMax2015)

vAugTmpMax2015.fit = fit.variogram(vAugTmpMax2015, vgm(60, 'Gau', 9, 5))
plot(vAugTmpMax2015 ,vAugTmpMax2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax2015Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugTmpMax2015Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugTmpMax2015krige1 = krige(AugTmpMax2015Subset$TMAX ~ 1, AugTmpMax2015Subset, texasGrid, vAugTmpMax2015.fit) #ordinary kriging
spplot(AugTmpMax2015krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax2015krige2 = krige(AugTmpMax2015Subset$TMAX ~ 1, AugTmpMax2015Subset, texasGrid, vAugTmpMax2015.fit, beta=10.0) #simple kriging
spplot(AugTmpMax2015krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax2015krige3 = krige(AugTmpMax2015Subset$TMAX ~ 1, AugTmpMax2015Subset, texasGrid, vAugTmpMax2015.fit, nmax=20) #ordinary kriging
spplot(AugTmpMax2015krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                  # JANUARY 2005 #


#define coordinates
# Temperature values for January 2005 in Texas
JanTmpMax2005 = read_xlsx("2005/Jan/2005-Jan-TMax.xlsx")
JanTmpMax2005$LATITUDE = as.numeric(JanTmpMax2005$LATITUDE)
JanTmpMax2005$LONGITUDE = as.numeric(JanTmpMax2005$LONGITUDE)

#data frame with data for one day
JanTmpMax2005Subset = JanTmpMax2005
JanTmpMax2005Subset$DATE = as.character(JanTmpMax2005Subset$DATE)
JanTmpMax2005Subset = subset(JanTmpMax2005Subset, JanTmpMax2005Subset$DATE == "2005-01-01")
JanTmpMax2005Subset = na.approx(JanTmpMax2005Subset)

JanTmpMax2005Locations = JanTmpMax2005Subset[c(2,3,4)]
JanTmpMax2005Locations = JanTmpMax2005Locations[!duplicated(JanTmpMax2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax2005Locations) = JanTmpMax2005Locations[c(3,2)]


coordinates(JanTmpMax2005Subset) = JanTmpMax2005Subset[c(4,3)]

# variogram for the one day
vJanTmpMax2005 = variogram(JanTmpMax2005Subset$TMAX ~ 1, JanTmpMax2005Subset, cutoff=9)
plot(vJanTmpMax2005)

vJanTmpMax2005.fit = fit.variogram(vJanTmpMax2005, vgm(60, 'Gau', 9, 5))
plot(vJanTmpMax2005 ,vJanTmpMax2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax2005Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanTmpMax2005Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanTmpMax2005krige1 = krige(JanTmpMax2005Subset$TMAX ~ 1, JanTmpMax2005Subset, texasGrid, vJanTmpMax2005.fit) #ordinary kriging
spplot(JanTmpMax2005krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanTmpMax2005krige2 = krige(JanTmpMax2005Subset$TMAX ~ 1, JanTmpMax2005Subset, texasGrid, vJanTmpMax2005.fit, beta=10.0) #same with simple kriging
spplot(JanTmpMax2005krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanTmpMax2005krige3 = krige(JanTmpMax2005Subset$TMAX ~ 1, JanTmpMax2005Subset, texasGrid, vJanTmpMax2005.fit, nmax=20) #ordinary kriging
spplot(JanTmpMax2005krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                  # AUGUST 2005 #



# Temperature values for Auguary 2005 in Texas
AugTmpMax2005 = read_xlsx("2005/Aug/2005-Aug-TMax.xlsx")
AugTmpMax2005$LATITUDE = as.numeric(AugTmpMax2005$LATITUDE)
AugTmpMax2005$LONGITUDE = as.numeric(AugTmpMax2005$LONGITUDE)

#data frame with data for one day
AugTmpMax2005Subset = AugTmpMax2005
AugTmpMax2005Subset$DATE = as.character(AugTmpMax2005Subset$DATE)
AugTmpMax2005Subset = subset(AugTmpMax2005Subset, AugTmpMax2005Subset$DATE == "2005-08-01")
AugTmpMax2005Subset = na.approx(AugTmpMax2005Subset)

AugTmpMax2005Locations = AugTmpMax2005Subset[c(2,3,4)]
AugTmpMax2005Locations = AugTmpMax2005Locations[!duplicated(AugTmpMax2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax2005Locations) = AugTmpMax2005Locations[c(3,2)]


coordinates(AugTmpMax2005Subset) = AugTmpMax2005Subset[c(4,3)]

# variogram for the one day
vAugTmpMax2005 = variogram(AugTmpMax2005Subset$TMAX ~ 1, AugTmpMax2005Subset, cutoff=9)
plot(vAugTmpMax2005)

vAugTmpMax2005.fit = fit.variogram(vAugTmpMax2005, vgm(60, 'Gau', 9, 5))
plot(vAugTmpMax2005 ,vAugTmpMax2005.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax2005Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugTmpMax2005Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugTmpMax2005krige1 = krige(AugTmpMax2005Subset$TMAX ~ 1, AugTmpMax2005Subset, texasGrid, vAugTmpMax2005.fit) #ordinary kriging
spplot(AugTmpMax2005krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax2005krige2 = krige(AugTmpMax2005Subset$TMAX ~ 1, AugTmpMax2005Subset, texasGrid, vAugTmpMax2005.fit, beta=10.0) #simple kriging
spplot(AugTmpMax2005krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax2005krige3 = krige(AugTmpMax2005Subset$TMAX ~ 1, AugTmpMax2005Subset, texasGrid, vAugTmpMax2005.fit, nmax=20) #ordinary kriging
spplot(AugTmpMax2005krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # JANUARY 1995 #


#define coordinates
# Temperature values for January 1995 in Texas
JanTmpMax1995 = read_xlsx("1995/Jan/1995-Jan-TMax.xlsx")
JanTmpMax1995$LATITUDE = as.numeric(JanTmpMax1995$LATITUDE)
JanTmpMax1995$LONGITUDE = as.numeric(JanTmpMax1995$LONGITUDE)

#data frame with data for one day
JanTmpMax1995Subset = JanTmpMax1995
JanTmpMax1995Subset$DATE = as.character(JanTmpMax1995Subset$DATE)
JanTmpMax1995Subset = subset(JanTmpMax1995Subset, JanTmpMax1995Subset$DATE == "1995-01-01")
JanTmpMax1995Subset = na.approx(JanTmpMax1995Subset)

JanTmpMax1995Locations = JanTmpMax1995Subset[c(2,3,4)]
JanTmpMax1995Locations = JanTmpMax1995Locations[!duplicated(JanTmpMax1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1995Locations) = JanTmpMax1995Locations[c(3,2)]


coordinates(JanTmpMax1995Subset) = JanTmpMax1995Subset[c(4,3)]

# variogram for the one day
vJanTmpMax1995 = variogram(JanTmpMax1995Subset$TMAX ~ 1, JanTmpMax1995Subset, cutoff=9)
plot(vJanTmpMax1995)

vJanTmpMax1995.fit = fit.variogram(vJanTmpMax1995, vgm(60, 'Gau', 9, 5))
plot(vJanTmpMax1995 ,vJanTmpMax1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1995Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanTmpMax1995Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanTmpMax1995krige1 = krige(JanTmpMax1995Subset$TMAX ~ 1, JanTmpMax1995Subset, texasGrid, vJanTmpMax1995.fit) #ordinary kriging
spplot(JanTmpMax1995krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanTmpMax1995krige2 = krige(JanTmpMax1995Subset$TMAX ~ 1, JanTmpMax1995Subset, texasGrid, vJanTmpMax1995.fit, beta=10.0) #same with simple kriging
spplot(JanTmpMax1995krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanTmpMax1995krige3 = krige(JanTmpMax1995Subset$TMAX ~ 1, JanTmpMax1995Subset, texasGrid, vJanTmpMax1995.fit, nmax=20) #ordinary kriging
spplot(JanTmpMax1995krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # AUGUST 1995 #



# Temperature values for Auguary 1995 in Texas
AugTmpMax1995 = read_xlsx("1995/Aug/1995-Aug-TMax.xlsx")
AugTmpMax1995$LATITUDE = as.numeric(AugTmpMax1995$LATITUDE)
AugTmpMax1995$LONGITUDE = as.numeric(AugTmpMax1995$LONGITUDE)

#data frame with data for one day
AugTmpMax1995Subset = AugTmpMax1995
AugTmpMax1995Subset$DATE = as.character(AugTmpMax1995Subset$DATE)
AugTmpMax1995Subset = subset(AugTmpMax1995Subset, AugTmpMax1995Subset$DATE == "1995-08-01")
AugTmpMax1995Subset = na.approx(AugTmpMax1995Subset)

AugTmpMax1995Locations = AugTmpMax1995Subset[c(2,3,4)]
AugTmpMax1995Locations = AugTmpMax1995Locations[!duplicated(AugTmpMax1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1995Locations) = AugTmpMax1995Locations[c(3,2)]


coordinates(AugTmpMax1995Subset) = AugTmpMax1995Subset[c(4,3)]

# variogram for the one day
vAugTmpMax1995 = variogram(AugTmpMax1995Subset$TMAX ~ 1, AugTmpMax1995Subset, cutoff=9)
plot(vAugTmpMax1995)

vAugTmpMax1995.fit = fit.variogram(vAugTmpMax1995, vgm(60, 'Gau', 9, 5))
plot(vAugTmpMax1995 ,vAugTmpMax1995.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1995Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugTmpMax1995Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugTmpMax1995krige1 = krige(AugTmpMax1995Subset$TMAX ~ 1, AugTmpMax1995Subset, texasGrid, vAugTmpMax1995.fit) #ordinary kriging
spplot(AugTmpMax1995krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1995krige2 = krige(AugTmpMax1995Subset$TMAX ~ 1, AugTmpMax1995Subset, texasGrid, vAugTmpMax1995.fit, beta=10.0) #simple kriging
spplot(AugTmpMax1995krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1995krige3 = krige(AugTmpMax1995Subset$TMAX ~ 1, AugTmpMax1995Subset, texasGrid, vAugTmpMax1995.fit, nmax=20) #ordinary kriging
spplot(AugTmpMax1995krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # JANUARY 1985 #


#define coordinates
# Temperature values for January 1985 in Texas
JanTmpMax1985 = read_xlsx("1985/Jan/1985-Jan-TMax.xlsx")
JanTmpMax1985$LATITUDE = as.numeric(JanTmpMax1985$LATITUDE)
JanTmpMax1985$LONGITUDE = as.numeric(JanTmpMax1985$LONGITUDE)

#data frame with data for one day
JanTmpMax1985Subset = JanTmpMax1985
JanTmpMax1985Subset$DATE = as.character(JanTmpMax1985Subset$DATE)
JanTmpMax1985Subset = subset(JanTmpMax1985Subset, JanTmpMax1985Subset$DATE == "1985-01-01")
JanTmpMax1985Subset = na.approx(JanTmpMax1985Subset)

JanTmpMax1985Locations = JanTmpMax1985Subset[c(2,3,4)]
JanTmpMax1985Locations = JanTmpMax1985Locations[!duplicated(JanTmpMax1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1985Locations) = JanTmpMax1985Locations[c(3,2)]


coordinates(JanTmpMax1985Subset) = JanTmpMax1985Subset[c(4,3)]

# variogram for the one day
vJanTmpMax1985 = variogram(JanTmpMax1985Subset$TMAX ~ 1, JanTmpMax1985Subset, cutoff=9)
plot(vJanTmpMax1985)

vJanTmpMax1985.fit = fit.variogram(vJanTmpMax1985, vgm(60, 'Gau', 9, 5))
plot(vJanTmpMax1985 ,vJanTmpMax1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1985Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanTmpMax1985Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanTmpMax1985krige1 = krige(JanTmpMax1985Subset$TMAX ~ 1, JanTmpMax1985Subset, texasGrid, vJanTmpMax1985.fit) #ordinary kriging
spplot(JanTmpMax1985krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanTmpMax1985krige2 = krige(JanTmpMax1985Subset$TMAX ~ 1, JanTmpMax1985Subset, texasGrid, vJanTmpMax1985.fit, beta=10.0) #same with simple kriging
spplot(JanTmpMax1985krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanTmpMax1985krige3 = krige(JanTmpMax1985Subset$TMAX ~ 1, JanTmpMax1985Subset, texasGrid, vJanTmpMax1985.fit, nmax=20) #ordinary kriging
spplot(JanTmpMax1985krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                                # AUGUST 1985 #



# Temperature values for Auguary 1985 in Texas
AugTmpMax1985 = read_xlsx("1985/Aug/1985-Aug-TMax.xlsx")
AugTmpMax1985$LATITUDE = as.numeric(AugTmpMax1985$LATITUDE)
AugTmpMax1985$LONGITUDE = as.numeric(AugTmpMax1985$LONGITUDE)

#data frame with data for one day
AugTmpMax1985Subset = AugTmpMax1985
AugTmpMax1985Subset$DATE = as.character(AugTmpMax1985Subset$DATE)
AugTmpMax1985Subset = subset(AugTmpMax1985Subset, AugTmpMax1985Subset$DATE == "1985-08-01")
AugTmpMax1985Subset = na.approx(AugTmpMax1985Subset)

AugTmpMax1985Locations = AugTmpMax1985Subset[c(2,3,4)]
AugTmpMax1985Locations = AugTmpMax1985Locations[!duplicated(AugTmpMax1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1985Locations) = AugTmpMax1985Locations[c(3,2)]


coordinates(AugTmpMax1985Subset) = AugTmpMax1985Subset[c(4,3)]

# variogram for the one day
vAugTmpMax1985 = variogram(AugTmpMax1985Subset$TMAX ~ 1, AugTmpMax1985Subset, cutoff=9)
plot(vAugTmpMax1985)

vAugTmpMax1985.fit = fit.variogram(vAugTmpMax1985, vgm(60, 'Gau', 9, 5))
plot(vAugTmpMax1985 ,vAugTmpMax1985.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1985Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugTmpMax1985Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugTmpMax1985krige1 = krige(AugTmpMax1985Subset$TMAX ~ 1, AugTmpMax1985Subset, texasGrid, vAugTmpMax1985.fit) #ordinary kriging
spplot(AugTmpMax1985krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1985krige2 = krige(AugTmpMax1985Subset$TMAX ~ 1, AugTmpMax1985Subset, texasGrid, vAugTmpMax1985.fit, beta=10.0) #simple kriging
spplot(AugTmpMax1985krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1985krige3 = krige(AugTmpMax1985Subset$TMAX ~ 1, AugTmpMax1985Subset, texasGrid, vAugTmpMax1985.fit, nmax=20) #ordinary kriging
spplot(AugTmpMax1985krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                        # JANUARY 1975 #


#define coordinates
# Temperature values for January 1975 in Texas
JanTmpMax1975 = read_xlsx("1975/Jan/1975-Jan-TMax.xlsx")
JanTmpMax1975$LATITUDE = as.numeric(JanTmpMax1975$LATITUDE)
JanTmpMax1975$LONGITUDE = as.numeric(JanTmpMax1975$LONGITUDE)

#data frame with data for one day
JanTmpMax1975Subset = JanTmpMax1975
JanTmpMax1975Subset$DATE = as.character(JanTmpMax1975Subset$DATE)
JanTmpMax1975Subset = subset(JanTmpMax1975Subset, JanTmpMax1975Subset$DATE == "1975-01-01")
JanTmpMax1975Subset = na.approx(JanTmpMax1975Subset)

JanTmpMax1975Locations = JanTmpMax1975Subset[c(2,3,4)]
JanTmpMax1975Locations = JanTmpMax1975Locations[!duplicated(JanTmpMax1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1975Locations) = JanTmpMax1975Locations[c(3,2)]


coordinates(JanTmpMax1975Subset) = JanTmpMax1975Subset[c(4,3)]

# variogram for the one day
vJanTmpMax1975 = variogram(JanTmpMax1975Subset$TMAX ~ 1, JanTmpMax1975Subset, cutoff=9)
plot(vJanTmpMax1975)

vJanTmpMax1975.fit = fit.variogram(vJanTmpMax1975, vgm(60, 'Gau', 9, 5))
plot(vJanTmpMax1975 ,vJanTmpMax1975.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1975Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanTmpMax1975Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanTmpMax1975krige1 = krige(JanTmpMax1975Subset$TMAX ~ 1, JanTmpMax1975Subset, texasGrid, vJanTmpMax1975.fit) #ordinary kriging
spplot(JanTmpMax1975krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanTmpMax1975krige2 = krige(JanTmpMax1975Subset$TMAX ~ 1, JanTmpMax1975Subset, texasGrid, vJanTmpMax1975.fit, beta=10.0) #same with simple kriging
spplot(JanTmpMax1975krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanTmpMax1975krige3 = krige(JanTmpMax1975Subset$TMAX ~ 1, JanTmpMax1975Subset, texasGrid, vJanTmpMax1975.fit, nmax=20) #ordinary kriging
spplot(JanTmpMax1975krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                          # AUGUST 1975 #



# Temperature values for Auguary 1975 in Texas
AugTmpMax1975 = read_xlsx("1975/Aug/1975-Aug-TMax.xlsx")
AugTmpMax1975$LATITUDE = as.numeric(AugTmpMax1975$LATITUDE)
AugTmpMax1975$LONGITUDE = as.numeric(AugTmpMax1975$LONGITUDE)

#data frame with data for one day
AugTmpMax1975Subset = AugTmpMax1975
AugTmpMax1975Subset$DATE = as.character(AugTmpMax1975Subset$DATE)
AugTmpMax1975Subset = subset(AugTmpMax1975Subset, AugTmpMax1975Subset$DATE == "1975-08-01")
AugTmpMax1975Subset = na.approx(AugTmpMax1975Subset)

AugTmpMax1975Locations = AugTmpMax1975Subset[c(2,3,4)]
AugTmpMax1975Locations = AugTmpMax1975Locations[!duplicated(AugTmpMax1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1975Locations) = AugTmpMax1975Locations[c(3,2)]


coordinates(AugTmpMax1975Subset) = AugTmpMax1975Subset[c(4,3)]

# variogram for the one day
vAugTmpMax1975 = variogram(AugTmpMax1975Subset$TMAX ~ 1, AugTmpMax1975Subset, cutoff=9)
plot(vAugTmpMax1975)

vAugTmpMax1975.fit = fit.variogram(vAugTmpMax1975, vgm(60, 'Gau', 9, 5))
plot(vAugTmpMax1975 ,vAugTmpMax1975.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1975Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugTmpMax1975Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugTmpMax1975krige1 = krige(AugTmpMax1975Subset$TMAX ~ 1, AugTmpMax1975Subset, texasGrid, vAugTmpMax1975.fit) #ordinary kriging
spplot(AugTmpMax1975krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1975krige2 = krige(AugTmpMax1975Subset$TMAX ~ 1, AugTmpMax1975Subset, texasGrid, vAugTmpMax1975.fit, beta=10.0) #simple kriging
spplot(AugTmpMax1975krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1975krige3 = krige(AugTmpMax1975Subset$TMAX ~ 1, AugTmpMax1975Subset, texasGrid, vAugTmpMax1975.fit, nmax=20) #ordinary kriging
spplot(AugTmpMax1975krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                      # JANUARY 1965 #


#define coordinates
# Temperature values for January 1965 in Texas
JanTmpMax1965 = read_xlsx("1965/Jan/1965-Jan-TMax.xlsx")
JanTmpMax1965$LATITUDE = as.numeric(JanTmpMax1965$LATITUDE)
JanTmpMax1965$LONGITUDE = as.numeric(JanTmpMax1965$LONGITUDE)

#data frame with data for one day
JanTmpMax1965Subset = JanTmpMax1965
JanTmpMax1965Subset$DATE = as.character(JanTmpMax1965Subset$DATE)
JanTmpMax1965Subset = subset(JanTmpMax1965Subset, JanTmpMax1965Subset$DATE == "1965-01-01")
JanTmpMax1965Subset = na.approx(JanTmpMax1965Subset)

JanTmpMax1965Locations = JanTmpMax1965Subset[c(2,3,4)]
JanTmpMax1965Locations = JanTmpMax1965Locations[!duplicated(JanTmpMax1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1965Locations) = JanTmpMax1965Locations[c(3,2)]


coordinates(JanTmpMax1965Subset) = JanTmpMax1965Subset[c(4,3)]

# variogram for the one day
vJanTmpMax1965 = variogram(JanTmpMax1965Subset$TMAX ~ 1, JanTmpMax1965Subset, cutoff=9)
plot(vJanTmpMax1965)

vJanTmpMax1965.fit = fit.variogram(vJanTmpMax1965, vgm(60, 'Gau', 9, 5))
plot(vJanTmpMax1965 ,vJanTmpMax1965.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1965Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanTmpMax1965Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanTmpMax1965krige1 = krige(JanTmpMax1965Subset$TMAX ~ 1, JanTmpMax1965Subset, texasGrid, vJanTmpMax1965.fit) #ordinary kriging
spplot(JanTmpMax1965krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanTmpMax1965krige2 = krige(JanTmpMax1965Subset$TMAX ~ 1, JanTmpMax1965Subset, texasGrid, vJanTmpMax1965.fit, beta=10.0) #same with simple kriging
spplot(JanTmpMax1965krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanTmpMax1965krige3 = krige(JanTmpMax1965Subset$TMAX ~ 1, JanTmpMax1965Subset, texasGrid, vJanTmpMax1965.fit, nmax=20) #ordinary kriging
spplot(JanTmpMax1965krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                            # AUGUST 1965 #



# Temperature values for Auguary 1965 in Texas
AugTmpMax1965 = read_xlsx("1965/Aug/1965-Aug-TMax.xlsx")
AugTmpMax1965$LATITUDE = as.numeric(AugTmpMax1965$LATITUDE)
AugTmpMax1965$LONGITUDE = as.numeric(AugTmpMax1965$LONGITUDE)

#data frame with data for one day
AugTmpMax1965Subset = AugTmpMax1965
AugTmpMax1965Subset$DATE = as.character(AugTmpMax1965Subset$DATE)
AugTmpMax1965Subset = subset(AugTmpMax1965Subset, AugTmpMax1965Subset$DATE == "1965-08-01")
AugTmpMax1965Subset = na.approx(AugTmpMax1965Subset)

AugTmpMax1965Locations = AugTmpMax1965Subset[c(2,3,4)]
AugTmpMax1965Locations = AugTmpMax1965Locations[!duplicated(AugTmpMax1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1965Locations) = AugTmpMax1965Locations[c(3,2)]


coordinates(AugTmpMax1965Subset) = AugTmpMax1965Subset[c(4,3)]

# variogram for the one day
vAugTmpMax1965 = variogram(AugTmpMax1965Subset$TMAX ~ 1, AugTmpMax1965Subset, cutoff=9)
plot(vAugTmpMax1965)

vAugTmpMax1965.fit = fit.variogram(vAugTmpMax1965, vgm(60, 'Gau', 9, 5))
plot(vAugTmpMax1965 ,vAugTmpMax1965.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1965Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugTmpMax1965Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugTmpMax1965krige1 = krige(AugTmpMax1965Subset$TMAX ~ 1, AugTmpMax1965Subset, texasGrid, vAugTmpMax1965.fit) #ordinary kriging
spplot(AugTmpMax1965krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1965krige2 = krige(AugTmpMax1965Subset$TMAX ~ 1, AugTmpMax1965Subset, texasGrid, vAugTmpMax1965.fit, beta=10.0) #simple kriging
spplot(AugTmpMax1965krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1965krige3 = krige(AugTmpMax1965Subset$TMAX ~ 1, AugTmpMax1965Subset, texasGrid, vAugTmpMax1965.fit, nmax=20) #ordinary kriging
spplot(AugTmpMax1965krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                    # JANUARY 1955 #


#define coordinates
# Temperature values for January 1955 in Texas
JanTmpMax1955 = read_xlsx("1955/Jan/1955-Jan-TMax.xlsx")
JanTmpMax1955$LATITUDE = as.numeric(JanTmpMax1955$LATITUDE)
JanTmpMax1955$LONGITUDE = as.numeric(JanTmpMax1955$LONGITUDE)

#data frame with data for one day
JanTmpMax1955Subset = JanTmpMax1955
JanTmpMax1955Subset$DATE = as.character(JanTmpMax1955Subset$DATE)
JanTmpMax1955Subset = subset(JanTmpMax1955Subset, JanTmpMax1955Subset$DATE == "1955-01-01")
JanTmpMax1955Subset = na.approx(JanTmpMax1955Subset)

JanTmpMax1955Locations = JanTmpMax1955Subset[c(2,3,4)]
JanTmpMax1955Locations = JanTmpMax1955Locations[!duplicated(JanTmpMax1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1955Locations) = JanTmpMax1955Locations[c(3,2)]


coordinates(JanTmpMax1955Subset) = JanTmpMax1955Subset[c(4,3)]

# variogram for the one day
vJanTmpMax1955 = variogram(JanTmpMax1955Subset$TMAX ~ 1, JanTmpMax1955Subset, cutoff=9)
plot(vJanTmpMax1955)

vJanTmpMax1955.fit = fit.variogram(vJanTmpMax1955, vgm(60, 'Gau', 9, 5))
plot(vJanTmpMax1955 ,vJanTmpMax1955.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1955Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanTmpMax1955Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanTmpMax1955krige1 = krige(JanTmpMax1955Subset$TMAX ~ 1, JanTmpMax1955Subset, texasGrid, vJanTmpMax1955.fit) #ordinary kriging
spplot(JanTmpMax1955krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanTmpMax1955krige2 = krige(JanTmpMax1955Subset$TMAX ~ 1, JanTmpMax1955Subset, texasGrid, vJanTmpMax1955.fit, beta=10.0) #same with simple kriging
spplot(JanTmpMax1955krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanTmpMax1955krige3 = krige(JanTmpMax1955Subset$TMAX ~ 1, JanTmpMax1955Subset, texasGrid, vJanTmpMax1955.fit, nmax=20) #ordinary kriging
spplot(JanTmpMax1955krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                        # AUGUST 1955 #



# Temperature values for Auguary 1955 in Texas
AugTmpMax1955 = read_xlsx("1955/Aug/1955-Aug-TMax.xlsx")
AugTmpMax1955$LATITUDE = as.numeric(AugTmpMax1955$LATITUDE)
AugTmpMax1955$LONGITUDE = as.numeric(AugTmpMax1955$LONGITUDE)

#data frame with data for one day
AugTmpMax1955Subset = AugTmpMax1955
AugTmpMax1955Subset$DATE = as.character(AugTmpMax1955Subset$DATE)
AugTmpMax1955Subset = subset(AugTmpMax1955Subset, AugTmpMax1955Subset$DATE == "1955-08-01")
AugTmpMax1955Subset = na.approx(AugTmpMax1955Subset)

AugTmpMax1955Locations = AugTmpMax1955Subset[c(2,3,4)]
AugTmpMax1955Locations = AugTmpMax1955Locations[!duplicated(AugTmpMax1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1955Locations) = AugTmpMax1955Locations[c(3,2)]


coordinates(AugTmpMax1955Subset) = AugTmpMax1955Subset[c(4,3)]

# variogram for the one day
vAugTmpMax1955 = variogram(AugTmpMax1955Subset$TMAX ~ 1, AugTmpMax1955Subset, cutoff=9)
plot(vAugTmpMax1955)

vAugTmpMax1955.fit = fit.variogram(vAugTmpMax1955, vgm(60, 'Gau', 9, 5))
plot(vAugTmpMax1955 ,vAugTmpMax1955.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1955Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugTmpMax1955Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugTmpMax1955krige1 = krige(AugTmpMax1955Subset$TMAX ~ 1, AugTmpMax1955Subset, texasGrid, vAugTmpMax1955.fit) #ordinary kriging
spplot(AugTmpMax1955krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1955krige2 = krige(AugTmpMax1955Subset$TMAX ~ 1, AugTmpMax1955Subset, texasGrid, vAugTmpMax1955.fit, beta=10.0) #simple kriging
spplot(AugTmpMax1955krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1955krige3 = krige(AugTmpMax1955Subset$TMAX ~ 1, AugTmpMax1955Subset, texasGrid, vAugTmpMax1955.fit, nmax=20) #ordinary kriging
spplot(AugTmpMax1955krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                        # JANUARY 1945 #


#define coordinates
# Temperature values for January 1945 in Texas
JanTmpMax1945 = read_xlsx("1945/Jan/1945-Jan-TMax.xlsx")
JanTmpMax1945$LATITUDE = as.numeric(JanTmpMax1945$LATITUDE)
JanTmpMax1945$LONGITUDE = as.numeric(JanTmpMax1945$LONGITUDE)

#data frame with data for one day
JanTmpMax1945Subset = JanTmpMax1945
JanTmpMax1945Subset$DATE = as.character(JanTmpMax1945Subset$DATE)
JanTmpMax1945Subset = subset(JanTmpMax1945Subset, JanTmpMax1945Subset$DATE == "1945-01-01")
JanTmpMax1945Subset = na.approx(JanTmpMax1945Subset)

JanTmpMax1945Locations = JanTmpMax1945Subset[c(2,3,4)]
JanTmpMax1945Locations = JanTmpMax1945Locations[!duplicated(JanTmpMax1945Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1945Locations) = JanTmpMax1945Locations[c(3,2)]


coordinates(JanTmpMax1945Subset) = JanTmpMax1945Subset[c(4,3)]

# variogram for the one day
vJanTmpMax1945 = variogram(JanTmpMax1945Subset$TMAX ~ 1, JanTmpMax1945Subset, cutoff=9)
plot(vJanTmpMax1945)

vJanTmpMax1945.fit = fit.variogram(vJanTmpMax1945, vgm(60, 'Gau', 9, 5))
plot(vJanTmpMax1945 ,vJanTmpMax1945.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax1945Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanTmpMax1945Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

JanTmpMax1945krige1 = krige(JanTmpMax1945Subset$TMAX ~ 1, JanTmpMax1945Subset, texasGrid, vJanTmpMax1945.fit) #ordinary kriging
spplot(JanTmpMax1945krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

JanTmpMax1945krige2 = krige(JanTmpMax1945Subset$TMAX ~ 1, JanTmpMax1945Subset, texasGrid, vJanTmpMax1945.fit, beta=10.0) #same with simple kriging
spplot(JanTmpMax1945krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white")) #looks pretty much the same

JanTmpMax1945krige3 = krige(JanTmpMax1945Subset$TMAX ~ 1, JanTmpMax1945Subset, texasGrid, vJanTmpMax1945.fit, nmax=20) #ordinary kriging
spplot(JanTmpMax1945krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))





                                              # AUGUST 1945 #



# Temperature values for Auguary 1945 in Texas
AugTmpMax1945 = read_xlsx("1945/Aug/1945-Aug-TMax.xlsx")
AugTmpMax1945$LATITUDE = as.numeric(AugTmpMax1945$LATITUDE)
AugTmpMax1945$LONGITUDE = as.numeric(AugTmpMax1945$LONGITUDE)

#data frame with data for one day
AugTmpMax1945Subset = AugTmpMax1945
AugTmpMax1945Subset$DATE = as.character(AugTmpMax1945Subset$DATE)
AugTmpMax1945Subset = subset(AugTmpMax1945Subset, AugTmpMax1945Subset$DATE == "1945-08-01")
AugTmpMax1945Subset = na.approx(AugTmpMax1945Subset)

AugTmpMax1945Locations = AugTmpMax1945Subset[c(2,3,4)]
AugTmpMax1945Locations = AugTmpMax1945Locations[!duplicated(AugTmpMax1945Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1945Locations) = AugTmpMax1945Locations[c(3,2)]


coordinates(AugTmpMax1945Subset) = AugTmpMax1945Subset[c(4,3)]

# variogram for the one day
vAugTmpMax1945 = variogram(AugTmpMax1945Subset$TMAX ~ 1, AugTmpMax1945Subset, cutoff=9)
plot(vAugTmpMax1945)

vAugTmpMax1945.fit = fit.variogram(vAugTmpMax1945, vgm(60, 'Gau', 9, 5))
plot(vAugTmpMax1945 ,vAugTmpMax1945.fit)

texas = map('state', 'texas', fill=TRUE)
plot(AugTmpMax1945Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugTmpMax1945Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
texasMap = map2SpatialPolygons(texas, ID=texas$names)

AugTmpMax1945krige1 = krige(AugTmpMax1945Subset$TMAX ~ 1, AugTmpMax1945Subset, texasGrid, vAugTmpMax1945.fit) #ordinary kriging
spplot(AugTmpMax1945krige1, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1945krige2 = krige(AugTmpMax1945Subset$TMAX ~ 1, AugTmpMax1945Subset, texasGrid, vAugTmpMax1945.fit, beta=10.0) #simple kriging
spplot(AugTmpMax1945krige2, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))

AugTmpMax1945krige3 = krige(AugTmpMax1945Subset$TMAX ~ 1, AugTmpMax1945Subset, texasGrid, vAugTmpMax1945.fit, nmax=20) #ordinary kriging
spplot(AugTmpMax1945krige3, sp.layout=list(texasMap, first=FALSE, lwd=2, col="white"))