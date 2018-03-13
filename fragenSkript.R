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
proj4string(JanTmpMax2015Subset) = CRS("+init=epsg:4326")
#JanTmpMax2015Subset = spTransform(JanTmpMax2015Subset, CRS("+proj=utm +zone=13 +datum=NAD83"))
JanTmpMax2015Subset = spTransform(JanTmpMax2015Subset, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
JanTmpMax2015SubsetHelp = coordinates(JanTmpMax2015Subset)
JanTmpMax2015Subset$LONGITUDE = JanTmpMax2015SubsetHelp[,1]
JanTmpMax2015Subset$LATITUDE = JanTmpMax2015SubsetHelp[,2]

# variogram for the one day
vJanTmpMax2015 = variogram(TMAX ~ LATITUDE+LONGITUDE, JanTmpMax2015Subset)
plot(vJanTmpMax2015)


vJanTmpMax2015.fit = fit.variogram(vJanTmpMax2015, vgm(6, 'Lin', 0, 5))
#vJanTmpMax2015.fit = fit.variogram(vJanTmpMax2015, vgm('Lin'))
plot(vJanTmpMax2015 ,vJanTmpMax2015.fit)

texas = map('state', 'texas', fill=TRUE)
plot(JanTmpMax2015Locations, add=TRUE, pch=20, col="red")
texas2 = data.frame(LONGITUDE = texas$x, LATITUDE = texas$y)
texas2 = na.omit(texas2)
coordinates(texas2) = texas2[c(1,2)]
proj4string(texas2) = CRS("+init=epsg:4326")
texas2 = spTransform(texas2, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

texasGrid = spsample(texas2, type="regular")
#texasGrid = spTransform(texasGrid, CRS("+proj=utm +zone=13 +datum=WGS84"))
tx = as.data.frame(coordinates(texasGrid))
names(tx) = c("LONGITUDE", "LATITUDE")
coordinates(tx) = tx[c(1,2)]
texasGrid = tx
proj4string(texasGrid) = CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
#texasGrid = spTransform(texasGrid, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
map('state', 'texas')
plot(texasGrid, add = T, pch=".")
#proj4string(texasGrid) = CRS("+init=epsg:4326")

texasMap = map2SpatialPolygons(texas, ID=texas$names)
proj4string(texasMap) = CRS("+init=epsg:4326")
texasMap = spTransform(texasMap, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#texasGrid = spTransform(texasGrid, CRS("+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#proj4string(texasGrid) = CRS(proj4string(JanTmpMax2015Subset))


JanTmpMax2015krige1 = krige(TMAX ~ JanTmpMax2015Subset$LATITUDE+JanTmpMax2015Subset$LONGITUDE, JanTmpMax2015Subset, texasGrid, vJanTmpMax2015.fit) #ordinary kriging
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
plot(AugTmpMax2015Locations, add=TRUE, pch=20, col="red")
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






# stuff
spplot(JanTmpMax2015krige1[1], sp.layout=list(list(JanTmpMax2015Subset), list(texasMap, first=FALSE, lwd=2, col="white")), scales = list(draw=T))

JanTmpMax2015krige1 = krige(TMAX ~ 1, JanTmpMax2015Subset, texasGrid, vJanTmpMax2015.fit) #ordinary kriging
