#script for the analysis of the precipitation time series

library("readxl")
library("xts")
library("maps")
library("maptools")
library("mapdata")
library("plyr")
library("forecast")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

# Angleton, Texas in the South East near Houston
# Temperature timeseries for Angleton in the SouthEast of Texas