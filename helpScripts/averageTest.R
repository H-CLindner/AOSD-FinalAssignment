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


# JANUARY 2015 #

# Precipitation values for January 2015 in Texas
JanPrcp2015 = read_xlsx("2015/Jan/2015-Jan-Prcp.xlsx")
JanPrcp2015$LATITUDE = as.numeric(JanPrcp2015$LATITUDE)
JanPrcp2015$LONGITUDE = as.numeric(JanPrcp2015$LONGITUDE)

nameAggregate = aggregate(JanPrcp2015, by=JanPrcp2015$STATION, FUN=mean)

JanPrcp2015Subset <- mutate(JanPrcp2015, month = format(DATE, "%m"))

mean_station <- group_by(JanPrcp2015Subset, STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, month) %>% summarise(mean_temp = mean(PRCP, na.rm = T))

computePrcpAverage = function(dataFrame){
  dataFrameSubset = mutate(dataFrame, month = format(DATE, "%m"))
  dataFrameMean = group_by(dataFrameSubset, STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, month) %>% summarise(meanPrcp = mean(PRCP, na.rm = T))
  return(dataFrameMean)
}
