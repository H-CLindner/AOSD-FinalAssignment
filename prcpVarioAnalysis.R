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
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")


                                # JANUARY 2015 #

# Precipitation values for January 2015 in Texas
JanPrcp2015 = read_xlsx("2015/Jan/2015-Jan-Prcp.xlsx")
JanPrcp2015$LATITUDE = as.numeric(JanPrcp2015$LATITUDE)
JanPrcp2015$LONGITUDE = as.numeric(JanPrcp2015$LONGITUDE)

#data frame with data for one day
JanPrcp2015Subset = JanPrcp2015
JanPrcp2015Subset$DATE = as.character(JanPrcp2015Subset$DATE)
JanPrcp2015Subset = subset(JanPrcp2015Subset, JanPrcp2015Subset$DATE == "2015-01-01")
JanPrcp2015Subset = na.approx(JanPrcp2015Subset)

JanPrcp2015Locations = JanPrcp2015[c(2,3,4)]
JanPrcp2015Locations = JanPrcp2015Locations[!duplicated(JanPrcp2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp2015Locations) = JanPrcp2015Locations[c(3,2)]

coordinates(JanPrcp2015Subset) = JanPrcp2015Subset[c(4,3)]

# variogram for the one day
vJanPrcp2015 = variogram(JanPrcp2015Subset$PRCP ~ 1, JanPrcp2015Subset, cutoff=2)
plot(vJanPrcp2015)

vJanPrcp2015.fit = fit.variogram(vJanPrcp2015, vgm(32, 'Sph', 1.8, 9))
plot(vJanPrcp2015 ,vJanPrcp2015.fit)

texas = map('state', 'texas')
plot(JanPrcp2015Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanPrcp2015Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

JanPrcp2015krige1 = krige(JanPrcp2015Subset$PRCP ~ 1, JanPrcp2015Subset, texasGrid, vJanPrcp2015.fit) #ordinary kriging
spplot(JanPrcp2015krige1)

JanPrcp2015krige2 = krige(JanPrcp2015Subset$PRCP ~ 1, JanPrcp2015Subset, texasGrid, vJanPrcp2015.fit, beta=10.0) #same with simple kriging
spplot(JanPrcp2015krige2) #looks pretty much the same

JanPrcp2015krige3 = krige(JanPrcp2015Subset$PRCP ~ 1, JanPrcp2015Subset, texasGrid, vJanPrcp2015.fit, nmax=20) #ordinary kriging
spplot(JanPrcp2015krige3)




  
                                  # AUGUST 2015 #

# Precipitation values for August 2015 in Texas
AugPrcp2015 = read_xlsx("2015/Aug/2015-Aug-Prcp.xlsx")
AugPrcp2015$LATITUDE = as.numeric(AugPrcp2015$LATITUDE)
AugPrcp2015$LONGITUDE = as.numeric(AugPrcp2015$LONGITUDE)

#data frame with data for one day
AugPrcp2015Subset = AugPrcp2015
AugPrcp2015Subset$DATE = as.character(AugPrcp2015Subset$DATE)
AugPrcp2015Subset = subset(AugPrcp2015Subset, AugPrcp2015Subset$DATE == "2015-08-01")
AugPrcp2015Subset = na.approx(AugPrcp2015Subset)

AugPrcp2015Locations = AugPrcp2015[c(2,3,4)]
AugPrcp2015Locations = AugPrcp2015Locations[!duplicated(AugPrcp2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp2015Locations) = AugPrcp2015Locations[c(3,2)]

coordinates(AugPrcp2015Subset) = AugPrcp2015Subset[c(4,3)]

# variogram for the one day
vAugPrcp2015 = variogram(AugPrcp2015Subset$PRCP ~ 1, AugPrcp2015Subset, cutoff=7)
plot(vAugPrcp2015)

vAugPrcp2015.fit = fit.variogram(vAugPrcp2015, vgm(32, 'Sph', 1.8, 9))
plot(vAugPrcp2015 ,vAugPrcp2015.fit)

texas = map('state', 'texas')
plot(AugPrcp2015Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugPrcp2015Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

AugPrcp2015krige1 = krige(AugPrcp2015Subset$PRCP ~ 1, AugPrcp2015Subset, texasGrid, vAugPrcp2015.fit) #ordinary kriging
spplot(AugPrcp2015krige1)

AugPrcp2015krige2 = krige(AugPrcp2015Subset$PRCP ~ 1, AugPrcp2015Subset, texasGrid, vAugPrcp2015.fit, beta=10.0) #same with simple kriging
spplot(AugPrcp2015krige2) #looks pretty much the same

AugPrcp2015krige3 = krige(AugPrcp2015Subset$PRCP ~ 1, AugPrcp2015Subset, texasGrid, vAugPrcp2015.fit, nmax=20) #ordinary kriging
spplot(AugPrcp2015krige3)






                                      # JANUARY 2005 #

# Precipitation values for January 2005 in Texas
JanPrcp2005 = read_xlsx("2005/Jan/2005-Jan-Prcp.xlsx")
JanPrcp2005$LATITUDE = as.numeric(JanPrcp2005$LATITUDE)
JanPrcp2005$LONGITUDE = as.numeric(JanPrcp2005$LONGITUDE)

#data frame with data for one day
JanPrcp2005Subset = JanPrcp2005
JanPrcp2005Subset$DATE = as.character(JanPrcp2005Subset$DATE)
JanPrcp2005Subset = subset(JanPrcp2005Subset, JanPrcp2005Subset$DATE == "2005-01-01")
JanPrcp2005Subset = na.approx(JanPrcp2005Subset)

JanPrcp2005Locations = JanPrcp2005[c(2,3,4)]
JanPrcp2005Locations = JanPrcp2005Locations[!duplicated(JanPrcp2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp2005Locations) = JanPrcp2005Locations[c(3,2)]

coordinates(JanPrcp2005Subset) = JanPrcp2005Subset[c(4,3)]

# variogram for the one day
vJanPrcp2005 = variogram(JanPrcp2005Subset$PRCP ~ 1, JanPrcp2005Subset, cutoff=2)
plot(vJanPrcp2005)

vJanPrcp2005.fit = fit.variogram(vJanPrcp2005, vgm(32, 'Sph', 1.8, 9))
plot(vJanPrcp2005 ,vJanPrcp2005.fit)

texas = map('state', 'texas')
plot(JanPrcp2005Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanPrcp2005Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

JanPrcp2005krige1 = krige(JanPrcp2005Subset$PRCP ~ 1, JanPrcp2005Subset, texasGrid, vJanPrcp2005.fit) #ordinary kriging
spplot(JanPrcp2005krige1)

JanPrcp2005krige2 = krige(JanPrcp2005Subset$PRCP ~ 1, JanPrcp2005Subset, texasGrid, vJanPrcp2005.fit, beta=10.0) #same with simple kriging
spplot(JanPrcp2005krige2) #looks pretty much the same

JanPrcp2005krige3 = krige(JanPrcp2005Subset$PRCP ~ 1, JanPrcp2005Subset, texasGrid, vJanPrcp2005.fit, nmax=20) #ordinary kriging
spplot(JanPrcp2005krige3)






                                    # AUGUST 2005 #

# Precipitation values for Auguary 2005 in Texas
AugPrcp2005 = read_xlsx("2005/Aug/2005-Aug-Prcp.xlsx")
AugPrcp2005$LATITUDE = as.numeric(AugPrcp2005$LATITUDE)
AugPrcp2005$LONGITUDE = as.numeric(AugPrcp2005$LONGITUDE)

#data frame with data for one day
AugPrcp2005Subset = AugPrcp2005
AugPrcp2005Subset$DATE = as.character(AugPrcp2005Subset$DATE)
AugPrcp2005Subset = subset(AugPrcp2005Subset, AugPrcp2005Subset$DATE == "2005-08-01")
AugPrcp2005Subset = na.approx(AugPrcp2005Subset)

AugPrcp2005Locations = AugPrcp2005[c(2,3,4)]
AugPrcp2005Locations = AugPrcp2005Locations[!duplicated(AugPrcp2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp2005Locations) = AugPrcp2005Locations[c(3,2)]

coordinates(AugPrcp2005Subset) = AugPrcp2005Subset[c(4,3)]

# variogram for the one day
vAugPrcp2005 = variogram(AugPrcp2005Subset$PRCP ~ 1, AugPrcp2005Subset, cutoff=2)
plot(vAugPrcp2005)

vAugPrcp2005.fit = fit.variogram(vAugPrcp2005, vgm(32, 'Sph', 1.8, 9))
plot(vAugPrcp2005 ,vAugPrcp2005.fit)

texas = map('state', 'texas')
plot(AugPrcp2005Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugPrcp2005Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

AugPrcp2005krige1 = krige(AugPrcp2005Subset$PRCP ~ 1, AugPrcp2005Subset, texasGrid, vAugPrcp2005.fit) #ordinary kriging
spplot(AugPrcp2005krige1)

AugPrcp2005krige2 = krige(AugPrcp2005Subset$PRCP ~ 1, AugPrcp2005Subset, texasGrid, vAugPrcp2005.fit, beta=10.0) #same with simple kriging
spplot(AugPrcp2005krige2) #looks pretty much the same

AugPrcp2005krige3 = krige(AugPrcp2005Subset$PRCP ~ 1, AugPrcp2005Subset, texasGrid, vAugPrcp2005.fit, nmax=20) #ordinary kriging
spplot(AugPrcp2005krige3)





                                        # JANUARY 1995 #

# Precipitation values for January 1995 in Texas
JanPrcp1995 = read_xlsx("1995/Jan/1995-Jan-Prcp.xlsx")
JanPrcp1995$LATITUDE = as.numeric(JanPrcp1995$LATITUDE)
JanPrcp1995$LONGITUDE = as.numeric(JanPrcp1995$LONGITUDE)

#data frame with data for one day
JanPrcp1995Subset = JanPrcp1995
JanPrcp1995Subset$DATE = as.character(JanPrcp1995Subset$DATE)
JanPrcp1995Subset = subset(JanPrcp1995Subset, JanPrcp1995Subset$DATE == "1995-01-01")
JanPrcp1995Subset = na.approx(JanPrcp1995Subset)

JanPrcp1995Locations = JanPrcp1995[c(2,3,4)]
JanPrcp1995Locations = JanPrcp1995Locations[!duplicated(JanPrcp1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1995Locations) = JanPrcp1995Locations[c(3,2)]

coordinates(JanPrcp1995Subset) = JanPrcp1995Subset[c(4,3)]

# variogram for the one day
vJanPrcp1995 = variogram(JanPrcp1995Subset$PRCP ~ 1, JanPrcp1995Subset, cutoff=2)
plot(vJanPrcp1995)

vJanPrcp1995.fit = fit.variogram(vJanPrcp1995, vgm(32, 'Sph', 1.8, 9))
plot(vJanPrcp1995 ,vJanPrcp1995.fit)

texas = map('state', 'texas')
plot(JanPrcp1995Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanPrcp1995Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

JanPrcp1995krige1 = krige(JanPrcp1995Subset$PRCP ~ 1, JanPrcp1995Subset, texasGrid, vJanPrcp1995.fit) #ordinary kriging
spplot(JanPrcp1995krige1)

JanPrcp1995krige2 = krige(JanPrcp1995Subset$PRCP ~ 1, JanPrcp1995Subset, texasGrid, vJanPrcp1995.fit, beta=10.0) #same with simple kriging
spplot(JanPrcp1995krige2) #looks pretty much the same

JanPrcp1995krige3 = krige(JanPrcp1995Subset$PRCP ~ 1, JanPrcp1995Subset, texasGrid, vJanPrcp1995.fit, nmax=20) #ordinary kriging
spplot(JanPrcp1995krige3)





                                            # AUGUST 1995 #

# Precipitation values for Auguary 1995 in Texas
AugPrcp1995 = read_xlsx("1995/Aug/1995-Aug-Prcp.xlsx")
AugPrcp1995$LATITUDE = as.numeric(AugPrcp1995$LATITUDE)
AugPrcp1995$LONGITUDE = as.numeric(AugPrcp1995$LONGITUDE)

#data frame with data for one day
AugPrcp1995Subset = AugPrcp1995
AugPrcp1995Subset$DATE = as.character(AugPrcp1995Subset$DATE)
AugPrcp1995Subset = subset(AugPrcp1995Subset, AugPrcp1995Subset$DATE == "1995-08-01")
AugPrcp1995Subset = na.approx(AugPrcp1995Subset)

AugPrcp1995Locations = AugPrcp1995[c(2,3,4)]
AugPrcp1995Locations = AugPrcp1995Locations[!duplicated(AugPrcp1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1995Locations) = AugPrcp1995Locations[c(3,2)]

coordinates(AugPrcp1995Subset) = AugPrcp1995Subset[c(4,3)]

# variogram for the one day
vAugPrcp1995 = variogram(AugPrcp1995Subset$PRCP ~ 1, AugPrcp1995Subset, cutoff=2)
plot(vAugPrcp1995)

vAugPrcp1995.fit = fit.variogram(vAugPrcp1995, vgm(32, 'Sph', 1.8, 9))
plot(vAugPrcp1995 ,vAugPrcp1995.fit)

texas = map('state', 'texas')
plot(AugPrcp1995Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugPrcp1995Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

AugPrcp1995krige1 = krige(AugPrcp1995Subset$PRCP ~ 1, AugPrcp1995Subset, texasGrid, vAugPrcp1995.fit) #ordinary kriging
spplot(AugPrcp1995krige1)

AugPrcp1995krige2 = krige(AugPrcp1995Subset$PRCP ~ 1, AugPrcp1995Subset, texasGrid, vAugPrcp1995.fit, beta=10.0) #same with simple kriging
spplot(AugPrcp1995krige2) #looks pretty much the same

AugPrcp1995krige3 = krige(AugPrcp1995Subset$PRCP ~ 1, AugPrcp1995Subset, texasGrid, vAugPrcp1995.fit, nmax=20) #ordinary kriging
spplot(AugPrcp1995krige3)





                                          # JANUARY 1985 #

# Precipitation values for January 1985 in Texas
JanPrcp1985 = read_xlsx("1985/Jan/1985-Jan-Prcp.xlsx")
JanPrcp1985$LATITUDE = as.numeric(JanPrcp1985$LATITUDE)
JanPrcp1985$LONGITUDE = as.numeric(JanPrcp1985$LONGITUDE)

#data frame with data for one day
JanPrcp1985Subset = JanPrcp1985
JanPrcp1985Subset$DATE = as.character(JanPrcp1985Subset$DATE)
JanPrcp1985Subset = subset(JanPrcp1985Subset, JanPrcp1985Subset$DATE == "1985-01-01")
JanPrcp1985Subset = na.approx(JanPrcp1985Subset)

JanPrcp1985Locations = JanPrcp1985[c(2,3,4)]
JanPrcp1985Locations = JanPrcp1985Locations[!duplicated(JanPrcp1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1985Locations) = JanPrcp1985Locations[c(3,2)]

coordinates(JanPrcp1985Subset) = JanPrcp1985Subset[c(4,3)]

# variogram for the one day
vJanPrcp1985 = variogram(JanPrcp1985Subset$PRCP ~ 1, JanPrcp1985Subset, cutoff=2)
plot(vJanPrcp1985)

vJanPrcp1985.fit = fit.variogram(vJanPrcp1985, vgm(32, 'Sph', 1.8, 9))
plot(vJanPrcp1985 ,vJanPrcp1985.fit)

texas = map('state', 'texas')
plot(JanPrcp1985Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanPrcp1985Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

JanPrcp1985krige1 = krige(JanPrcp1985Subset$PRCP ~ 1, JanPrcp1985Subset, texasGrid, vJanPrcp1985.fit) #ordinary kriging
spplot(JanPrcp1985krige1)

JanPrcp1985krige2 = krige(JanPrcp1985Subset$PRCP ~ 1, JanPrcp1985Subset, texasGrid, vJanPrcp1985.fit, beta=10.0) #same with simple kriging
spplot(JanPrcp1985krige2) #looks pretty much the same

JanPrcp1985krige3 = krige(JanPrcp1985Subset$PRCP ~ 1, JanPrcp1985Subset, texasGrid, vJanPrcp1985.fit, nmax=20) #ordinary kriging
spplot(JanPrcp1985krige3)






                                          # AUGUST 1985 #

# Precipitation values for Auguary 1985 in Texas
AugPrcp1985 = read_xlsx("1985/Aug/1985-Aug-Prcp.xlsx")
AugPrcp1985$LATITUDE = as.numeric(AugPrcp1985$LATITUDE)
AugPrcp1985$LONGITUDE = as.numeric(AugPrcp1985$LONGITUDE)

#data frame with data for one day
AugPrcp1985Subset = AugPrcp1985
AugPrcp1985Subset$DATE = as.character(AugPrcp1985Subset$DATE)
AugPrcp1985Subset = subset(AugPrcp1985Subset, AugPrcp1985Subset$DATE == "1985-08-01")
AugPrcp1985Subset = na.approx(AugPrcp1985Subset)

AugPrcp1985Locations = AugPrcp1985[c(2,3,4)]
AugPrcp1985Locations = AugPrcp1985Locations[!duplicated(AugPrcp1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1985Locations) = AugPrcp1985Locations[c(3,2)]

coordinates(AugPrcp1985Subset) = AugPrcp1985Subset[c(4,3)]

# variogram for the one day
vAugPrcp1985 = variogram(AugPrcp1985Subset$PRCP ~ 1, AugPrcp1985Subset, cutoff=2)
plot(vAugPrcp1985)

vAugPrcp1985.fit = fit.variogram(vAugPrcp1985, vgm(32, 'Sph', 1.8, 9))
plot(vAugPrcp1985 ,vAugPrcp1985.fit)

texas = map('state', 'texas')
plot(AugPrcp1985Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugPrcp1985Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

AugPrcp1985krige1 = krige(AugPrcp1985Subset$PRCP ~ 1, AugPrcp1985Subset, texasGrid, vAugPrcp1985.fit) #ordinary kriging
spplot(AugPrcp1985krige1)

AugPrcp1985krige2 = krige(AugPrcp1985Subset$PRCP ~ 1, AugPrcp1985Subset, texasGrid, vAugPrcp1985.fit, beta=10.0) #same with simple kriging
spplot(AugPrcp1985krige2) #looks pretty much the same

AugPrcp1985krige3 = krige(AugPrcp1985Subset$PRCP ~ 1, AugPrcp1985Subset, texasGrid, vAugPrcp1985.fit, nmax=20) #ordinary kriging
spplot(AugPrcp1985krige3)





                                                  # JANUARY 1975 #

# Precipitation values for January 1975 in Texas
JanPrcp1975 = read_xlsx("1975/Jan/1975-Jan-Prcp.xlsx")
JanPrcp1975$LATITUDE = as.numeric(JanPrcp1975$LATITUDE)
JanPrcp1975$LONGITUDE = as.numeric(JanPrcp1975$LONGITUDE)

#data frame with data for one day
JanPrcp1975Subset = JanPrcp1975
JanPrcp1975Subset$DATE = as.character(JanPrcp1975Subset$DATE)
JanPrcp1975Subset = subset(JanPrcp1975Subset, JanPrcp1975Subset$DATE == "1975-01-01")
JanPrcp1975Subset = na.approx(JanPrcp1975Subset)

JanPrcp1975Locations = JanPrcp1975[c(2,3,4)]
JanPrcp1975Locations = JanPrcp1975Locations[!duplicated(JanPrcp1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1975Locations) = JanPrcp1975Locations[c(3,2)]

coordinates(JanPrcp1975Subset) = JanPrcp1975Subset[c(4,3)]

# variogram for the one day
vJanPrcp1975 = variogram(JanPrcp1975Subset$PRCP ~ 1, JanPrcp1975Subset, cutoff=2)
plot(vJanPrcp1975)

vJanPrcp1975.fit = fit.variogram(vJanPrcp1975, vgm(32, 'Sph', 1.8, 9))
plot(vJanPrcp1975 ,vJanPrcp1975.fit)

texas = map('state', 'texas')
plot(JanPrcp1975Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanPrcp1975Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

JanPrcp1975krige1 = krige(JanPrcp1975Subset$PRCP ~ 1, JanPrcp1975Subset, texasGrid, vJanPrcp1975.fit) #ordinary kriging
spplot(JanPrcp1975krige1)

JanPrcp1975krige2 = krige(JanPrcp1975Subset$PRCP ~ 1, JanPrcp1975Subset, texasGrid, vJanPrcp1975.fit, beta=10.0) #same with simple kriging
spplot(JanPrcp1975krige2) #looks pretty much the same

JanPrcp1975krige3 = krige(JanPrcp1975Subset$PRCP ~ 1, JanPrcp1975Subset, texasGrid, vJanPrcp1975.fit, nmax=20) #ordinary kriging
spplot(JanPrcp1975krige3)





                                      # AUGUST 1975 #

# Precipitation values for Auguary 1975 in Texas
AugPrcp1975 = read_xlsx("1975/Aug/1975-Aug-Prcp.xlsx")
AugPrcp1975$LATITUDE = as.numeric(AugPrcp1975$LATITUDE)
AugPrcp1975$LONGITUDE = as.numeric(AugPrcp1975$LONGITUDE)

#data frame with data for one day
AugPrcp1975Subset = AugPrcp1975
AugPrcp1975Subset$DATE = as.character(AugPrcp1975Subset$DATE)
AugPrcp1975Subset = subset(AugPrcp1975Subset, AugPrcp1975Subset$DATE == "1975-08-01")
AugPrcp1975Subset = na.approx(AugPrcp1975Subset)

AugPrcp1975Locations = AugPrcp1975[c(2,3,4)]
AugPrcp1975Locations = AugPrcp1975Locations[!duplicated(AugPrcp1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1975Locations) = AugPrcp1975Locations[c(3,2)]

coordinates(AugPrcp1975Subset) = AugPrcp1975Subset[c(4,3)]

# variogram for the one day
vAugPrcp1975 = variogram(AugPrcp1975Subset$PRCP ~ 1, AugPrcp1975Subset, cutoff=2)
plot(vAugPrcp1975)

vAugPrcp1975.fit = fit.variogram(vAugPrcp1975, vgm(32, 'Sph', 1.8, 9))
plot(vAugPrcp1975 ,vAugPrcp1975.fit)

texas = map('state', 'texas')
plot(AugPrcp1975Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugPrcp1975Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

AugPrcp1975krige1 = krige(AugPrcp1975Subset$PRCP ~ 1, AugPrcp1975Subset, texasGrid, vAugPrcp1975.fit) #ordinary kriging
spplot(AugPrcp1975krige1)

AugPrcp1975krige2 = krige(AugPrcp1975Subset$PRCP ~ 1, AugPrcp1975Subset, texasGrid, vAugPrcp1975.fit, beta=10.0) #same with simple kriging
spplot(AugPrcp1975krige2) #looks pretty much the same

AugPrcp1975krige3 = krige(AugPrcp1975Subset$PRCP ~ 1, AugPrcp1975Subset, texasGrid, vAugPrcp1975.fit, nmax=20) #ordinary kriging
spplot(AugPrcp1975krige3)





                                    # JANUARY 1965 #

# Precipitation values for January 1965 in Texas
JanPrcp1965 = read_xlsx("1965/Jan/1965-Jan-Prcp.xlsx")
JanPrcp1965$LATITUDE = as.numeric(JanPrcp1965$LATITUDE)
JanPrcp1965$LONGITUDE = as.numeric(JanPrcp1965$LONGITUDE)

#data frame with data for one day
JanPrcp1965Subset = JanPrcp1965
JanPrcp1965Subset$DATE = as.character(JanPrcp1965Subset$DATE)
JanPrcp1965Subset = subset(JanPrcp1965Subset, JanPrcp1965Subset$DATE == "1965-01-01")
JanPrcp1965Subset = na.approx(JanPrcp1965Subset)

JanPrcp1965Locations = JanPrcp1965[c(2,3,4)]
JanPrcp1965Locations = JanPrcp1965Locations[!duplicated(JanPrcp1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1965Locations) = JanPrcp1965Locations[c(3,2)]

coordinates(JanPrcp1965Subset) = JanPrcp1965Subset[c(4,3)]

# variogram for the one day
vJanPrcp1965 = variogram(JanPrcp1965Subset$PRCP ~ 1, JanPrcp1965Subset, cutoff=2)
plot(vJanPrcp1965)

vJanPrcp1965.fit = fit.variogram(vJanPrcp1965, vgm(32, 'Sph', 1.8, 9))
plot(vJanPrcp1965 ,vJanPrcp1965.fit)

texas = map('state', 'texas')
plot(JanPrcp1965Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanPrcp1965Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

JanPrcp1965krige1 = krige(JanPrcp1965Subset$PRCP ~ 1, JanPrcp1965Subset, texasGrid, vJanPrcp1965.fit) #ordinary kriging
spplot(JanPrcp1965krige1)

JanPrcp1965krige2 = krige(JanPrcp1965Subset$PRCP ~ 1, JanPrcp1965Subset, texasGrid, vJanPrcp1965.fit, beta=10.0) #same with simple kriging
spplot(JanPrcp1965krige2) #looks pretty much the same

JanPrcp1965krige3 = krige(JanPrcp1965Subset$PRCP ~ 1, JanPrcp1965Subset, texasGrid, vJanPrcp1965.fit, nmax=20) #ordinary kriging
spplot(JanPrcp1965krige3)





                                  # AUGUST 1965 #

# Precipitation values for Auguary 1965 in Texas
AugPrcp1965 = read_xlsx("1965/Aug/1965-Aug-Prcp.xlsx")
AugPrcp1965$LATITUDE = as.numeric(AugPrcp1965$LATITUDE)
AugPrcp1965$LONGITUDE = as.numeric(AugPrcp1965$LONGITUDE)

#data frame with data for one day
AugPrcp1965Subset = AugPrcp1965
AugPrcp1965Subset$DATE = as.character(AugPrcp1965Subset$DATE)
AugPrcp1965Subset = subset(AugPrcp1965Subset, AugPrcp1965Subset$DATE == "1965-08-01")
AugPrcp1965Subset = na.approx(AugPrcp1965Subset)

AugPrcp1965Locations = AugPrcp1965[c(2,3,4)]
AugPrcp1965Locations = AugPrcp1965Locations[!duplicated(AugPrcp1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1965Locations) = AugPrcp1965Locations[c(3,2)]

coordinates(AugPrcp1965Subset) = AugPrcp1965Subset[c(4,3)]

# variogram for the one day
vAugPrcp1965 = variogram(AugPrcp1965Subset$PRCP ~ 1, AugPrcp1965Subset, cutoff=2)
plot(vAugPrcp1965)

vAugPrcp1965.fit = fit.variogram(vAugPrcp1965, vgm(32, 'Sph', 1.8, 9))
plot(vAugPrcp1965 ,vAugPrcp1965.fit)

texas = map('state', 'texas')
plot(AugPrcp1965Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugPrcp1965Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

AugPrcp1965krige1 = krige(AugPrcp1965Subset$PRCP ~ 1, AugPrcp1965Subset, texasGrid, vAugPrcp1965.fit) #ordinary kriging
spplot(AugPrcp1965krige1)

AugPrcp1965krige2 = krige(AugPrcp1965Subset$PRCP ~ 1, AugPrcp1965Subset, texasGrid, vAugPrcp1965.fit, beta=10.0) #same with simple kriging
spplot(AugPrcp1965krige2) #looks pretty much the same

AugPrcp1965krige3 = krige(AugPrcp1965Subset$PRCP ~ 1, AugPrcp1965Subset, texasGrid, vAugPrcp1965.fit, nmax=20) #ordinary kriging
spplot(AugPrcp1965krige3)





                                    # JANUARY 1955 #

# Precipitation values for January 1955 in Texas
JanPrcp1955 = read_xlsx("1955/Jan/1955-Jan-Prcp.xlsx")
JanPrcp1955$LATITUDE = as.numeric(JanPrcp1955$LATITUDE)
JanPrcp1955$LONGITUDE = as.numeric(JanPrcp1955$LONGITUDE)

#data frame with data for one day
JanPrcp1955Subset = JanPrcp1955
JanPrcp1955Subset$DATE = as.character(JanPrcp1955Subset$DATE)
JanPrcp1955Subset = subset(JanPrcp1955Subset, JanPrcp1955Subset$DATE == "1955-01-01")
JanPrcp1955Subset = na.approx(JanPrcp1955Subset)

JanPrcp1955Locations = JanPrcp1955[c(2,3,4)]
JanPrcp1955Locations = JanPrcp1955Locations[!duplicated(JanPrcp1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1955Locations) = JanPrcp1955Locations[c(3,2)]

coordinates(JanPrcp1955Subset) = JanPrcp1955Subset[c(4,3)]

# variogram for the one day
vJanPrcp1955 = variogram(JanPrcp1955Subset$PRCP ~ 1, JanPrcp1955Subset, cutoff=2)
plot(vJanPrcp1955)

vJanPrcp1955.fit = fit.variogram(vJanPrcp1955, vgm(32, 'Sph', 1.8, 9))
plot(vJanPrcp1955 ,vJanPrcp1955.fit)

texas = map('state', 'texas')
plot(JanPrcp1955Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanPrcp1955Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

JanPrcp1955krige1 = krige(JanPrcp1955Subset$PRCP ~ 1, JanPrcp1955Subset, texasGrid, vJanPrcp1955.fit) #ordinary kriging
spplot(JanPrcp1955krige1)

JanPrcp1955krige2 = krige(JanPrcp1955Subset$PRCP ~ 1, JanPrcp1955Subset, texasGrid, vJanPrcp1955.fit, beta=10.0) #same with simple kriging
spplot(JanPrcp1955krige2) #looks pretty much the same

JanPrcp1955krige3 = krige(JanPrcp1955Subset$PRCP ~ 1, JanPrcp1955Subset, texasGrid, vJanPrcp1955.fit, nmax=20) #ordinary kriging
spplot(JanPrcp1955krige3)






                                  # AUGUST 1955 #

# Precipitation values for Auguary 1955 in Texas
AugPrcp1955 = read_xlsx("1955/Aug/1955-Aug-Prcp.xlsx")
AugPrcp1955$LATITUDE = as.numeric(AugPrcp1955$LATITUDE)
AugPrcp1955$LONGITUDE = as.numeric(AugPrcp1955$LONGITUDE)

#data frame with data for one day
AugPrcp1955Subset = AugPrcp1955
AugPrcp1955Subset$DATE = as.character(AugPrcp1955Subset$DATE)
AugPrcp1955Subset = subset(AugPrcp1955Subset, AugPrcp1955Subset$DATE == "1955-08-01")
AugPrcp1955Subset = na.approx(AugPrcp1955Subset)

AugPrcp1955Locations = AugPrcp1955[c(2,3,4)]
AugPrcp1955Locations = AugPrcp1955Locations[!duplicated(AugPrcp1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1955Locations) = AugPrcp1955Locations[c(3,2)]

coordinates(AugPrcp1955Subset) = AugPrcp1955Subset[c(4,3)]

# variogram for the one day
vAugPrcp1955 = variogram(AugPrcp1955Subset$PRCP ~ 1, AugPrcp1955Subset, cutoff=2)
plot(vAugPrcp1955)

vAugPrcp1955.fit = fit.variogram(vAugPrcp1955, vgm(32, 'Sph', 1.8, 9))
plot(vAugPrcp1955 ,vAugPrcp1955.fit)

texas = map('state', 'texas')
plot(AugPrcp1955Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugPrcp1955Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

AugPrcp1955krige1 = krige(AugPrcp1955Subset$PRCP ~ 1, AugPrcp1955Subset, texasGrid, vAugPrcp1955.fit) #ordinary kriging
spplot(AugPrcp1955krige1)

AugPrcp1955krige2 = krige(AugPrcp1955Subset$PRCP ~ 1, AugPrcp1955Subset, texasGrid, vAugPrcp1955.fit, beta=10.0) #same with simple kriging
spplot(AugPrcp1955krige2) #looks pretty much the same

AugPrcp1955krige3 = krige(AugPrcp1955Subset$PRCP ~ 1, AugPrcp1955Subset, texasGrid, vAugPrcp1955.fit, nmax=20) #ordinary kriging
spplot(AugPrcp1955krige3)





                                  # JANUARY 1945 #

# Precipitation values for January 1945 in Texas
JanPrcp1945 = read_xlsx("1945/Jan/1945-Jan-Prcp.xlsx")
JanPrcp1945$LATITUDE = as.numeric(JanPrcp1945$LATITUDE)
JanPrcp1945$LONGITUDE = as.numeric(JanPrcp1945$LONGITUDE)

#data frame with data for one day
JanPrcp1945Subset = JanPrcp1945
JanPrcp1945Subset$DATE = as.character(JanPrcp1945Subset$DATE)
JanPrcp1945Subset = subset(JanPrcp1945Subset, JanPrcp1945Subset$DATE == "1945-01-01")
JanPrcp1945Subset = na.approx(JanPrcp1945Subset)

JanPrcp1945Locations = JanPrcp1945[c(2,3,4)]
JanPrcp1945Locations = JanPrcp1945Locations[!duplicated(JanPrcp1945Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1945Locations) = JanPrcp1945Locations[c(3,2)]

coordinates(JanPrcp1945Subset) = JanPrcp1945Subset[c(4,3)]

# variogram for the one day
vJanPrcp1945 = variogram(JanPrcp1945Subset$PRCP ~ 1, JanPrcp1945Subset, cutoff=2)
plot(vJanPrcp1945)

vJanPrcp1945.fit = fit.variogram(vJanPrcp1945, vgm(32, 'Sph', 1.8, 9))
plot(vJanPrcp1945 ,vJanPrcp1945.fit)

texas = map('state', 'texas')
plot(JanPrcp1945Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(JanPrcp1945Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

JanPrcp1945krige1 = krige(JanPrcp1945Subset$PRCP ~ 1, JanPrcp1945Subset, texasGrid, vJanPrcp1945.fit) #ordinary kriging
spplot(JanPrcp1945krige1)

JanPrcp1945krige2 = krige(JanPrcp1945Subset$PRCP ~ 1, JanPrcp1945Subset, texasGrid, vJanPrcp1945.fit, beta=10.0) #same with simple kriging
spplot(JanPrcp1945krige2) #looks pretty much the same

JanPrcp1945krige3 = krige(JanPrcp1945Subset$PRCP ~ 1, JanPrcp1945Subset, texasGrid, vJanPrcp1945.fit, nmax=20) #ordinary kriging
spplot(JanPrcp1945krige3)






                                      # AUGUST 1945 #

# Precipitation values for Auguary 1945 in Texas
AugPrcp1945 = read_xlsx("1945/Aug/1945-Aug-Prcp.xlsx")
AugPrcp1945$LATITUDE = as.numeric(AugPrcp1945$LATITUDE)
AugPrcp1945$LONGITUDE = as.numeric(AugPrcp1945$LONGITUDE)

#data frame with data for one day
AugPrcp1945Subset = AugPrcp1945
AugPrcp1945Subset$DATE = as.character(AugPrcp1945Subset$DATE)
AugPrcp1945Subset = subset(AugPrcp1945Subset, AugPrcp1945Subset$DATE == "1945-08-01")
AugPrcp1945Subset = na.approx(AugPrcp1945Subset)

AugPrcp1945Locations = AugPrcp1945[c(2,3,4)]
AugPrcp1945Locations = AugPrcp1945Locations[!duplicated(AugPrcp1945Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1945Locations) = AugPrcp1945Locations[c(3,2)]

coordinates(AugPrcp1945Subset) = AugPrcp1945Subset[c(4,3)]

# variogram for the one day
vAugPrcp1945 = variogram(AugPrcp1945Subset$PRCP ~ 1, AugPrcp1945Subset, cutoff=2)
plot(vAugPrcp1945)

vAugPrcp1945.fit = fit.variogram(vAugPrcp1945, vgm(32, 'Exp', 1.8, 9))
plot(vAugPrcp1945 ,vAugPrcp1945.fit)

texas = map('state', 'texas')
plot(AugPrcp1945Locations, add=TRUE, pch=20)
texas2 = data.frame(texas$x, texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]

texasGrid = spsample(AugPrcp1945Subset, type="regular")
texasGrid = spsample(texas2, type="regular")
map('state', 'texas')
plot(texasGrid, add = T, pch=".")

AugPrcp1945krige1 = krige(AugPrcp1945Subset$PRCP ~ 1, AugPrcp1945Subset, texasGrid, vAugPrcp1945.fit) #ordinary kriging
spplot(AugPrcp1945krige1)

AugPrcp1945krige2 = krige(AugPrcp1945Subset$PRCP ~ 1, AugPrcp1945Subset, texasGrid, vAugPrcp1945.fit, beta=10.0) #same with simple kriging
spplot(AugPrcp1945krige2) #looks pretty much the same

AugPrcp1945krige3 = krige(AugPrcp1945Subset$PRCP ~ 1, AugPrcp1945Subset, texasGrid, vAugPrcp1945.fit, nmax=20) #ordinary kriging
spplot(AugPrcp1945krige3)