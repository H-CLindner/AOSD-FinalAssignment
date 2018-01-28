library("readxl")
library("maps")
library("maptools")
library("mapdata")
setwd("C:/Users/hans-/Documents/Master/1.Semester/ASTD/FinalAssignment/data")

# Temperature values for January 2015 in Texas
JanTmpMax2015 = read_xlsx("2015/Jan/2015-Jan-TMax.xlsx")
JanTmpMax2015$LATITUDE = as.numeric(JanTmpMax2015$LATITUDE)
JanTmpMax2015$LONGITUDE = as.numeric(JanTmpMax2015$LONGITUDE)

JanTmpMax2015Locations = JanTmpMax2015[c(2,3,4)]
JanTmpMax2015Locations = JanTmpMax2015Locations[!duplicated(JanTmpMax2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax2015Locations) = JanTmpMax2015Locations[c(3,2)]

# Evaporation values for January 2015 in Texas
JanEvap2015 = read_xlsx("2015/Jan/2015-Jan-Evap.xlsx")
JanEvap2015$LATITUDE = as.numeric(JanEvap2015$LATITUDE)
JanEvap2015$LONGITUDE = as.numeric(JanEvap2015$LONGITUDE)

JanEvap2015Locations = JanEvap2015[c(2,3,4)]
JanEvap2015Locations = JanEvap2015Locations[!duplicated(JanEvap2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap2015Locations) = JanEvap2015Locations[c(3,2)]

# Average wind speed values for January 2015 in Texas
JanWind2015 = read_xlsx("2015/Jan/2015-Jan-Wind.xlsx")
JanWind2015$LATITUDE = as.numeric(JanWind2015$LATITUDE)
JanWind2015$LONGITUDE = as.numeric(JanWind2015$LONGITUDE)

JanWind2015Locations = JanWind2015[c(2,3,4)]
JanWind2015Locations = JanWind2015Locations[!duplicated(JanWind2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanWind2015Locations) = JanWind2015Locations[c(3,2)]

# Precipitation values for January 2015 in Texas
JanPrcp2015 = read_xlsx("2015/Jan/2015-Jan-Prcp.xlsx")
JanPrcp2015$LATITUDE = as.numeric(JanPrcp2015$LATITUDE)
JanPrcp2015$LONGITUDE = as.numeric(JanPrcp2015$LONGITUDE)

JanPrcp2015Locations = JanPrcp2015[c(2,3,4)]
JanPrcp2015Locations = JanPrcp2015Locations[!duplicated(JanPrcp2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp2015Locations) = JanPrcp2015Locations[c(3,2)]

# all together for January 2015
map('state', 'texas')
plot(JanPrcp2015Locations, add=TRUE, pch=20)
plot(JanEvap2015Locations, add=TRUE, pch=0, col='red')
plot(JanTmpMax2015Locations, add=TRUE, pch=3, col='green')
plot(JanWind2015Locations, add=TRUE, pch=16, col='blue')

#---------------------------------------------------------------------------------------------------------------------

# Temperature values for August 2015 in Texas
AugTmpMax2015 = read_xlsx("2015/Aug/2015-Aug-TMax.xlsx")
AugTmpMax2015$LATITUDE = as.numeric(AugTmpMax2015$LATITUDE)
AugTmpMax2015$LONGITUDE = as.numeric(AugTmpMax2015$LONGITUDE)

AugTmpMax2015Locations = AugTmpMax2015[c(2,3,4)]
AugTmpMax2015Locations = AugTmpMax2015Locations[!duplicated(AugTmpMax2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax2015Locations) = AugTmpMax2015Locations[c(3,2)]

# Evaporation values for August 2015 in Texas
AugEvap2015 = read_xlsx("2015/Aug/2015-Aug-Evap.xlsx")
AugEvap2015$LATITUDE = as.numeric(AugEvap2015$LATITUDE)
AugEvap2015$LONGITUDE = as.numeric(AugEvap2015$LONGITUDE)

AugEvap2015Locations = AugEvap2015[c(2,3,4)]
AugEvap2015Locations = AugEvap2015Locations[!duplicated(AugEvap2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap2015Locations) = AugEvap2015Locations[c(3,2)]

# Average wind speed values for August 2015 in Texas
AugWind2015 = read_xlsx("2015/Aug/2015-Aug-Wind.xlsx")
AugWind2015$LATITUDE = as.numeric(AugWind2015$LATITUDE)
AugWind2015$LONGITUDE = as.numeric(AugWind2015$LONGITUDE)

AugWind2015Locations = AugWind2015[c(2,3,4)]
AugWind2015Locations = AugWind2015Locations[!duplicated(AugWind2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugWind2015Locations) = AugWind2015Locations[c(3,2)]

# Precipitation values for August 2015 in Texas
AugPrcp2015 = read_xlsx("2015/Aug/2015-Aug-Prcp.xlsx")
AugPrcp2015$LATITUDE = as.numeric(AugPrcp2015$LATITUDE)
AugPrcp2015$LONGITUDE = as.numeric(AugPrcp2015$LONGITUDE)

AugPrcp2015Locations = AugPrcp2015[c(2,3,4)]
AugPrcp2015Locations = AugPrcp2015Locations[!duplicated(AugPrcp2015Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp2015Locations) = AugPrcp2015Locations[c(3,2)]

#-------------------------------------------------------------------------------------------------------------------
  
# Temperature values for January 2005 in Texas
JanTmpMax2005 = read_xlsx("2005/Jan/2005-Jan-TMax.xlsx")
JanTmpMax2005$LATITUDE = as.numeric(JanTmpMax2005$LATITUDE)
JanTmpMax2005$LONGITUDE = as.numeric(JanTmpMax2005$LONGITUDE)

JanTmpMax2005Locations = JanTmpMax2005[c(2,3,4)]
JanTmpMax2005Locations = JanTmpMax2005Locations[!duplicated(JanTmpMax2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax2005Locations) = JanTmpMax2005Locations[c(3,2)]

# Evaporation values for January 2005 in Texas
JanEvap2005 = read_xlsx("2005/Jan/2005-Jan-Evap.xlsx")
JanEvap2005$LATITUDE = as.numeric(JanEvap2005$LATITUDE)
JanEvap2005$LONGITUDE = as.numeric(JanEvap2005$LONGITUDE)

JanEvap2005Locations = JanEvap2005[c(2,3,4)]
JanEvap2005Locations = JanEvap2005Locations[!duplicated(JanEvap2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap2005Locations) = JanEvap2005Locations[c(3,2)]

# Average wind speed values for January 2005 in Texas
JanWind2005 = read_xlsx("2005/Jan/2005-Jan-Wind.xlsx")
JanWind2005$LATITUDE = as.numeric(JanWind2005$LATITUDE)
JanWind2005$LONGITUDE = as.numeric(JanWind2005$LONGITUDE)

JanWind2005Locations = JanWind2005[c(2,3,4)]
JanWind2005Locations = JanWind2005Locations[!duplicated(JanWind2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanWind2005Locations) = JanWind2005Locations[c(3,2)]

# Precipitation values for January 2005 in Texas
JanPrcp2005 = read_xlsx("2005/Jan/2005-Jan-Prcp.xlsx")
JanPrcp2005$LATITUDE = as.numeric(JanPrcp2005$LATITUDE)
JanPrcp2005$LONGITUDE = as.numeric(JanPrcp2005$LONGITUDE)

JanPrcp2005Locations = JanPrcp2005[c(2,3,4)]
JanPrcp2005Locations = JanPrcp2005Locations[!duplicated(JanPrcp2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp2005Locations) = JanPrcp2005Locations[c(3,2)]

#---------------------------------------------------------------------------------------------------------------------
  
# Temperature values for August 2005 in Texas
AugTmpMax2005 = read_xlsx("2005/Aug/2005-Aug-TMax.xlsx")
AugTmpMax2005$LATITUDE = as.numeric(AugTmpMax2005$LATITUDE)
AugTmpMax2005$LONGITUDE = as.numeric(AugTmpMax2005$LONGITUDE)

AugTmpMax2005Locations = AugTmpMax2005[c(2,3,4)]
AugTmpMax2005Locations = AugTmpMax2005Locations[!duplicated(AugTmpMax2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax2005Locations) = AugTmpMax2005Locations[c(3,2)]

# Evaporation values for August 2005 in Texas
AugEvap2005 = read_xlsx("2005/Aug/2005-Aug-Evap.xlsx")
AugEvap2005$LATITUDE = as.numeric(AugEvap2005$LATITUDE)
AugEvap2005$LONGITUDE = as.numeric(AugEvap2005$LONGITUDE)

AugEvap2005Locations = AugEvap2005[c(2,3,4)]
AugEvap2005Locations = AugEvap2005Locations[!duplicated(AugEvap2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap2005Locations) = AugEvap2005Locations[c(3,2)]

# Average wind speed values for August 2005 in Texas
AugWind2005 = read_xlsx("2005/Aug/2005-Aug-Wind.xlsx")
AugWind2005$LATITUDE = as.numeric(AugWind2005$LATITUDE)
AugWind2005$LONGITUDE = as.numeric(AugWind2005$LONGITUDE)

AugWind2005Locations = AugWind2005[c(2,3,4)]
AugWind2005Locations = AugWind2005Locations[!duplicated(AugWind2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugWind2005Locations) = AugWind2005Locations[c(3,2)]

# Precipitation values for August 2005 in Texas
AugPrcp2005 = read_xlsx("2005/Aug/2005-Aug-Prcp.xlsx")
AugPrcp2005$LATITUDE = as.numeric(AugPrcp2005$LATITUDE)
AugPrcp2005$LONGITUDE = as.numeric(AugPrcp2005$LONGITUDE)

AugPrcp2005Locations = AugPrcp2005[c(2,3,4)]
AugPrcp2005Locations = AugPrcp2005Locations[!duplicated(AugPrcp2005Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp2005Locations) = AugPrcp2005Locations[c(3,2)]

#-------------------------------------------------------------------------------------------------------------------
  
# Temperature values for January 1995 in Texas
JanTmpMax1995 = read_xlsx("1995/Jan/1995-Jan-TMax.xlsx")
JanTmpMax1995$LATITUDE = as.numeric(JanTmpMax1995$LATITUDE)
JanTmpMax1995$LONGITUDE = as.numeric(JanTmpMax1995$LONGITUDE)

JanTmpMax1995Locations = JanTmpMax1995[c(2,3,4)]
JanTmpMax1995Locations = JanTmpMax1995Locations[!duplicated(JanTmpMax1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1995Locations) = JanTmpMax1995Locations[c(3,2)]

# Evaporation values for January 1995 in Texas
JanEvap1995 = read_xlsx("1995/Jan/1995-Jan-Evap.xlsx")
JanEvap1995$LATITUDE = as.numeric(JanEvap1995$LATITUDE)
JanEvap1995$LONGITUDE = as.numeric(JanEvap1995$LONGITUDE)

JanEvap1995Locations = JanEvap1995[c(2,3,4)]
JanEvap1995Locations = JanEvap1995Locations[!duplicated(JanEvap1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1995Locations) = JanEvap1995Locations[c(3,2)]

# Average wind speed values for January 1995 in Texas
JanWind1995 = read_xlsx("1995/Jan/1995-Jan-Wind.xlsx")
JanWind1995$LATITUDE = as.numeric(JanWind1995$LATITUDE)
JanWind1995$LONGITUDE = as.numeric(JanWind1995$LONGITUDE)

JanWind1995Locations = JanWind1995[c(2,3,4)]
JanWind1995Locations = JanWind1995Locations[!duplicated(JanWind1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanWind1995Locations) = JanWind1995Locations[c(3,2)]

# Precipitation values for January 1995 in Texas
JanPrcp1995 = read_xlsx("1995/Jan/1995-Jan-Prcp.xlsx")
JanPrcp1995$LATITUDE = as.numeric(JanPrcp1995$LATITUDE)
JanPrcp1995$LONGITUDE = as.numeric(JanPrcp1995$LONGITUDE)

JanPrcp1995Locations = JanPrcp1995[c(2,3,4)]
JanPrcp1995Locations = JanPrcp1995Locations[!duplicated(JanPrcp1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1995Locations) = JanPrcp1995Locations[c(3,2)]

#---------------------------------------------------------------------------------------------------------------------
  
# Temperature values for August 1995 in Texas
AugTmpMax1995 = read_xlsx("1995/Aug/1995-Aug-TMax.xlsx")
AugTmpMax1995$LATITUDE = as.numeric(AugTmpMax1995$LATITUDE)
AugTmpMax1995$LONGITUDE = as.numeric(AugTmpMax1995$LONGITUDE)

AugTmpMax1995Locations = AugTmpMax1995[c(2,3,4)]
AugTmpMax1995Locations = AugTmpMax1995Locations[!duplicated(AugTmpMax1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1995Locations) = AugTmpMax1995Locations[c(3,2)]

# Evaporation values for August 1995 in Texas
AugEvap1995 = read_xlsx("1995/Aug/1995-Aug-Evap.xlsx")
AugEvap1995$LATITUDE = as.numeric(AugEvap1995$LATITUDE)
AugEvap1995$LONGITUDE = as.numeric(AugEvap1995$LONGITUDE)

AugEvap1995Locations = AugEvap1995[c(2,3,4)]
AugEvap1995Locations = AugEvap1995Locations[!duplicated(AugEvap1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1995Locations) = AugEvap1995Locations[c(3,2)]

# Average wind speed values for August 1995 in Texas
AugWind1995 = read_xlsx("1995/Aug/1995-Aug-Wind.xlsx")
AugWind1995$LATITUDE = as.numeric(AugWind1995$LATITUDE)
AugWind1995$LONGITUDE = as.numeric(AugWind1995$LONGITUDE)

AugWind1995Locations = AugWind1995[c(2,3,4)]
AugWind1995Locations = AugWind1995Locations[!duplicated(AugWind1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugWind1995Locations) = AugWind1995Locations[c(3,2)]

# Precipitation values for August 1995 in Texas
AugPrcp1995 = read_xlsx("1995/Aug/1995-Aug-Prcp.xlsx")
AugPrcp1995$LATITUDE = as.numeric(AugPrcp1995$LATITUDE)
AugPrcp1995$LONGITUDE = as.numeric(AugPrcp1995$LONGITUDE)

AugPrcp1995Locations = AugPrcp1995[c(2,3,4)]
AugPrcp1995Locations = AugPrcp1995Locations[!duplicated(AugPrcp1995Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1995Locations) = AugPrcp1995Locations[c(3,2)]

#-------------------------------------------------------------------------------------------------------------------
  
# Temperature values for January 1985 in Texas
JanTmpMax1985 = read_xlsx("1985/Jan/1985-Jan-TMax.xlsx")
JanTmpMax1985$LATITUDE = as.numeric(JanTmpMax1985$LATITUDE)
JanTmpMax1985$LONGITUDE = as.numeric(JanTmpMax1985$LONGITUDE)

JanTmpMax1985Locations = JanTmpMax1985[c(2,3,4)]
JanTmpMax1985Locations = JanTmpMax1985Locations[!duplicated(JanTmpMax1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1985Locations) = JanTmpMax1985Locations[c(3,2)]

# Evaporation values for January 1985 in Texas
JanEvap1985 = read_xlsx("1985/Jan/1985-Jan-Evap.xlsx")
JanEvap1985$LATITUDE = as.numeric(JanEvap1985$LATITUDE)
JanEvap1985$LONGITUDE = as.numeric(JanEvap1985$LONGITUDE)

JanEvap1985Locations = JanEvap1985[c(2,3,4)]
JanEvap1985Locations = JanEvap1985Locations[!duplicated(JanEvap1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1985Locations) = JanEvap1985Locations[c(3,2)]

# Average wind speed values for January 1985 in Texas
JanWind1985 = read_xlsx("1985/Jan/1985-Jan-Wind.xlsx")
JanWind1985$LATITUDE = as.numeric(JanWind1985$LATITUDE)
JanWind1985$LONGITUDE = as.numeric(JanWind1985$LONGITUDE)

JanWind1985Locations = JanWind1985[c(2,3,4)]
JanWind1985Locations = JanWind1985Locations[!duplicated(JanWind1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanWind1985Locations) = JanWind1985Locations[c(3,2)]

# Precipitation values for January 1985 in Texas
JanPrcp1985 = read_xlsx("1985/Jan/1985-Jan-Prcp.xlsx")
JanPrcp1985$LATITUDE = as.numeric(JanPrcp1985$LATITUDE)
JanPrcp1985$LONGITUDE = as.numeric(JanPrcp1985$LONGITUDE)

JanPrcp1985Locations = JanPrcp1985[c(2,3,4)]
JanPrcp1985Locations = JanPrcp1985Locations[!duplicated(JanPrcp1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1985Locations) = JanPrcp1985Locations[c(3,2)]

#---------------------------------------------------------------------------------------------------------------------
  
# Temperature values for August 1985 in Texas
AugTmpMax1985 = read_xlsx("1985/Aug/1985-Aug-TMax.xlsx")
AugTmpMax1985$LATITUDE = as.numeric(AugTmpMax1985$LATITUDE)
AugTmpMax1985$LONGITUDE = as.numeric(AugTmpMax1985$LONGITUDE)

AugTmpMax1985Locations = AugTmpMax1985[c(2,3,4)]
AugTmpMax1985Locations = AugTmpMax1985Locations[!duplicated(AugTmpMax1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1985Locations) = AugTmpMax1985Locations[c(3,2)]

# Evaporation values for August 1985 in Texas
AugEvap1985 = read_xlsx("1985/Aug/1985-Aug-Evap.xlsx")
AugEvap1985$LATITUDE = as.numeric(AugEvap1985$LATITUDE)
AugEvap1985$LONGITUDE = as.numeric(AugEvap1985$LONGITUDE)

AugEvap1985Locations = AugEvap1985[c(2,3,4)]
AugEvap1985Locations = AugEvap1985Locations[!duplicated(AugEvap1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1985Locations) = AugEvap1985Locations[c(3,2)]

# Average wind speed values for August 1985 in Texas
AugWind1985 = read_xlsx("1985/Aug/1985-Aug-Wind.xlsx")
AugWind1985$LATITUDE = as.numeric(AugWind1985$LATITUDE)
AugWind1985$LONGITUDE = as.numeric(AugWind1985$LONGITUDE)

AugWind1985Locations = AugWind1985[c(2,3,4)]
AugWind1985Locations = AugWind1985Locations[!duplicated(AugWind1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugWind1985Locations) = AugWind1985Locations[c(3,2)]

# Precipitation values for August 1985 in Texas
AugPrcp1985 = read_xlsx("1985/Aug/1985-Aug-Prcp.xlsx")
AugPrcp1985$LATITUDE = as.numeric(AugPrcp1985$LATITUDE)
AugPrcp1985$LONGITUDE = as.numeric(AugPrcp1985$LONGITUDE)

AugPrcp1985Locations = AugPrcp1985[c(2,3,4)]
AugPrcp1985Locations = AugPrcp1985Locations[!duplicated(AugPrcp1985Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1985Locations) = AugPrcp1985Locations[c(3,2)]

#-------------------------------------------------------------------------------------------------------------------
  
# Temperature values for January 1975 in Texas
JanTmpMax1975 = read_xlsx("1975/Jan/1975-Jan-TMax.xlsx")
JanTmpMax1975$LATITUDE = as.numeric(JanTmpMax1975$LATITUDE)
JanTmpMax1975$LONGITUDE = as.numeric(JanTmpMax1975$LONGITUDE)

JanTmpMax1975Locations = JanTmpMax1975[c(2,3,4)]
JanTmpMax1975Locations = JanTmpMax1975Locations[!duplicated(JanTmpMax1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1975Locations) = JanTmpMax1975Locations[c(3,2)]

# Evaporation values for January 1975 in Texas
JanEvap1975 = read_xlsx("1975/Jan/1975-Jan-Evap.xlsx")
JanEvap1975$LATITUDE = as.numeric(JanEvap1975$LATITUDE)
JanEvap1975$LONGITUDE = as.numeric(JanEvap1975$LONGITUDE)

JanEvap1975Locations = JanEvap1975[c(2,3,4)]
JanEvap1975Locations = JanEvap1975Locations[!duplicated(JanEvap1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1975Locations) = JanEvap1975Locations[c(3,2)]

# Precipitation values for January 1975 in Texas
JanPrcp1975 = read_xlsx("1975/Jan/1975-Jan-Prcp.xlsx")
JanPrcp1975$LATITUDE = as.numeric(JanPrcp1975$LATITUDE)
JanPrcp1975$LONGITUDE = as.numeric(JanPrcp1975$LONGITUDE)

JanPrcp1975Locations = JanPrcp1975[c(2,3,4)]
JanPrcp1975Locations = JanPrcp1975Locations[!duplicated(JanPrcp1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1975Locations) = JanPrcp1975Locations[c(3,2)]

#---------------------------------------------------------------------------------------------------------------------
  
# Temperature values for August 1975 in Texas
AugTmpMax1975 = read_xlsx("1975/Aug/1975-Aug-TMax.xlsx")
AugTmpMax1975$LATITUDE = as.numeric(AugTmpMax1975$LATITUDE)
AugTmpMax1975$LONGITUDE = as.numeric(AugTmpMax1975$LONGITUDE)

AugTmpMax1975Locations = AugTmpMax1975[c(2,3,4)]
AugTmpMax1975Locations = AugTmpMax1975Locations[!duplicated(AugTmpMax1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1975Locations) = AugTmpMax1975Locations[c(3,2)]

# Evaporation values for August 1975 in Texas
AugEvap1975 = read_xlsx("1975/Aug/1975-Aug-Evap.xlsx")
AugEvap1975$LATITUDE = as.numeric(AugEvap1975$LATITUDE)
AugEvap1975$LONGITUDE = as.numeric(AugEvap1975$LONGITUDE)

AugEvap1975Locations = AugEvap1975[c(2,3,4)]
AugEvap1975Locations = AugEvap1975Locations[!duplicated(AugEvap1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1975Locations) = AugEvap1975Locations[c(3,2)]

# Precipitation values for August 1975 in Texas
AugPrcp1975 = read_xlsx("1975/Aug/1975-Aug-Prcp.xlsx")
AugPrcp1975$LATITUDE = as.numeric(AugPrcp1975$LATITUDE)
AugPrcp1975$LONGITUDE = as.numeric(AugPrcp1975$LONGITUDE)

AugPrcp1975Locations = AugPrcp1975[c(2,3,4)]
AugPrcp1975Locations = AugPrcp1975Locations[!duplicated(AugPrcp1975Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1975Locations) = AugPrcp1975Locations[c(3,2)]

#-------------------------------------------------------------------------------------------------------------------
  
# Temperature values for January 1965 in Texas
JanTmpMax1965 = read_xlsx("1965/Jan/1965-Jan-TMax.xlsx")
JanTmpMax1965$LATITUDE = as.numeric(JanTmpMax1965$LATITUDE)
JanTmpMax1965$LONGITUDE = as.numeric(JanTmpMax1965$LONGITUDE)

JanTmpMax1965Locations = JanTmpMax1965[c(2,3,4)]
JanTmpMax1965Locations = JanTmpMax1965Locations[!duplicated(JanTmpMax1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1965Locations) = JanTmpMax1965Locations[c(3,2)]

# Evaporation values for January 1965 in Texas
JanEvap1965 = read_xlsx("1965/Jan/1965-Jan-Evap.xlsx")
JanEvap1965$LATITUDE = as.numeric(JanEvap1965$LATITUDE)
JanEvap1965$LONGITUDE = as.numeric(JanEvap1965$LONGITUDE)

JanEvap1965Locations = JanEvap1965[c(2,3,4)]
JanEvap1965Locations = JanEvap1965Locations[!duplicated(JanEvap1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1965Locations) = JanEvap1965Locations[c(3,2)]

# Precipitation values for January 1965 in Texas
JanPrcp1965 = read_xlsx("1965/Jan/1965-Jan-Prcp.xlsx")
JanPrcp1965$LATITUDE = as.numeric(JanPrcp1965$LATITUDE)
JanPrcp1965$LONGITUDE = as.numeric(JanPrcp1965$LONGITUDE)

JanPrcp1965Locations = JanPrcp1965[c(2,3,4)]
JanPrcp1965Locations = JanPrcp1965Locations[!duplicated(JanPrcp1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1965Locations) = JanPrcp1965Locations[c(3,2)]

#---------------------------------------------------------------------------------------------------------------------
  
# Temperature values for August 1965 in Texas
AugTmpMax1965 = read_xlsx("1965/Aug/1965-Aug-TMax.xlsx")
AugTmpMax1965$LATITUDE = as.numeric(AugTmpMax1965$LATITUDE)
AugTmpMax1965$LONGITUDE = as.numeric(AugTmpMax1965$LONGITUDE)

AugTmpMax1965Locations = AugTmpMax1965[c(2,3,4)]
AugTmpMax1965Locations = AugTmpMax1965Locations[!duplicated(AugTmpMax1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1965Locations) = AugTmpMax1965Locations[c(3,2)]

# Evaporation values for August 1965 in Texas
AugEvap1965 = read_xlsx("1965/Aug/1965-Aug-Evap.xlsx")
AugEvap1965$LATITUDE = as.numeric(AugEvap1965$LATITUDE)
AugEvap1965$LONGITUDE = as.numeric(AugEvap1965$LONGITUDE)

AugEvap1965Locations = AugEvap1965[c(2,3,4)]
AugEvap1965Locations = AugEvap1965Locations[!duplicated(AugEvap1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1965Locations) = AugEvap1965Locations[c(3,2)]

# Precipitation values for August 1965 in Texas
AugPrcp1965 = read_xlsx("1965/Aug/1965-Aug-Prcp.xlsx")
AugPrcp1965$LATITUDE = as.numeric(AugPrcp1965$LATITUDE)
AugPrcp1965$LONGITUDE = as.numeric(AugPrcp1965$LONGITUDE)

AugPrcp1965Locations = AugPrcp1965[c(2,3,4)]
AugPrcp1965Locations = AugPrcp1965Locations[!duplicated(AugPrcp1965Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1965Locations) = AugPrcp1965Locations[c(3,2)]

#-------------------------------------------------------------------------------------------------------------------
  
# Temperature values for January 1955 in Texas
JanTmpMax1955 = read_xlsx("1955/Jan/1955-Jan-TMax.xlsx")
JanTmpMax1955$LATITUDE = as.numeric(JanTmpMax1955$LATITUDE)
JanTmpMax1955$LONGITUDE = as.numeric(JanTmpMax1955$LONGITUDE)

JanTmpMax1955Locations = JanTmpMax1955[c(2,3,4)]
JanTmpMax1955Locations = JanTmpMax1955Locations[!duplicated(JanTmpMax1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1955Locations) = JanTmpMax1955Locations[c(3,2)]

# Evaporation values for January 1955 in Texas
JanEvap1955 = read_xlsx("1955/Jan/1955-Jan-Evap.xlsx")
JanEvap1955$LATITUDE = as.numeric(JanEvap1955$LATITUDE)
JanEvap1955$LONGITUDE = as.numeric(JanEvap1955$LONGITUDE)

JanEvap1955Locations = JanEvap1955[c(2,3,4)]
JanEvap1955Locations = JanEvap1955Locations[!duplicated(JanEvap1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1955Locations) = JanEvap1955Locations[c(3,2)]

# Precipitation values for January 1955 in Texas
JanPrcp1955 = read_xlsx("1955/Jan/1955-Jan-Prcp.xlsx")
JanPrcp1955$LATITUDE = as.numeric(JanPrcp1955$LATITUDE)
JanPrcp1955$LONGITUDE = as.numeric(JanPrcp1955$LONGITUDE)

JanPrcp1955Locations = JanPrcp1955[c(2,3,4)]
JanPrcp1955Locations = JanPrcp1955Locations[!duplicated(JanPrcp1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1955Locations) = JanPrcp1955Locations[c(3,2)]

#---------------------------------------------------------------------------------------------------------------------
  
# Temperature values for August 1955 in Texas
AugTmpMax1955 = read_xlsx("1955/Aug/1955-Aug-TMax.xlsx")
AugTmpMax1955$LATITUDE = as.numeric(AugTmpMax1955$LATITUDE)
AugTmpMax1955$LONGITUDE = as.numeric(AugTmpMax1955$LONGITUDE)

AugTmpMax1955Locations = AugTmpMax1955[c(2,3,4)]
AugTmpMax1955Locations = AugTmpMax1955Locations[!duplicated(AugTmpMax1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1955Locations) = AugTmpMax1955Locations[c(3,2)]

# Evaporation values for August 1955 in Texas
AugEvap1955 = read_xlsx("1955/Aug/1955-Aug-Evap.xlsx")
AugEvap1955$LATITUDE = as.numeric(AugEvap1955$LATITUDE)
AugEvap1955$LONGITUDE = as.numeric(AugEvap1955$LONGITUDE)

AugEvap1955Locations = AugEvap1955[c(2,3,4)]
AugEvap1955Locations = AugEvap1955Locations[!duplicated(AugEvap1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1955Locations) = AugEvap1955Locations[c(3,2)]

# Precipitation values for August 1955 in Texas
AugPrcp1955 = read_xlsx("1955/Aug/1955-Aug-Prcp.xlsx")
AugPrcp1955$LATITUDE = as.numeric(AugPrcp1955$LATITUDE)
AugPrcp1955$LONGITUDE = as.numeric(AugPrcp1955$LONGITUDE)

AugPrcp1955Locations = AugPrcp1955[c(2,3,4)]
AugPrcp1955Locations = AugPrcp1955Locations[!duplicated(AugPrcp1955Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1955Locations) = AugPrcp1955Locations[c(3,2)]

#-------------------------------------------------------------------------------------------------------------------
  
# Temperature values for January 1945 in Texas
JanTmpMax1945 = read_xlsx("1945/Jan/1945-Jan-TMax.xlsx")
JanTmpMax1945$LATITUDE = as.numeric(JanTmpMax1945$LATITUDE)
JanTmpMax1945$LONGITUDE = as.numeric(JanTmpMax1945$LONGITUDE)

JanTmpMax1945Locations = JanTmpMax1945[c(2,3,4)]
JanTmpMax1945Locations = JanTmpMax1945Locations[!duplicated(JanTmpMax1945Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanTmpMax1945Locations) = JanTmpMax1945Locations[c(3,2)]

# Evaporation values for January 1945 in Texas
JanEvap1945 = read_xlsx("1945/Jan/1945-Jan-Evap.xlsx")
JanEvap1945$LATITUDE = as.numeric(JanEvap1945$LATITUDE)
JanEvap1945$LONGITUDE = as.numeric(JanEvap1945$LONGITUDE)

JanEvap1945Locations = JanEvap1945[c(2,3,4)]
JanEvap1945Locations = JanEvap1945Locations[!duplicated(JanEvap1945Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanEvap1945Locations) = JanEvap1945Locations[c(3,2)]

# Precipitation values for January 1945 in Texas
JanPrcp1945 = read_xlsx("1945/Jan/1945-Jan-Prcp.xlsx")
JanPrcp1945$LATITUDE = as.numeric(JanPrcp1945$LATITUDE)
JanPrcp1945$LONGITUDE = as.numeric(JanPrcp1945$LONGITUDE)

JanPrcp1945Locations = JanPrcp1945[c(2,3,4)]
JanPrcp1945Locations = JanPrcp1945Locations[!duplicated(JanPrcp1945Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(JanPrcp1945Locations) = JanPrcp1945Locations[c(3,2)]


# all together for January 2015
map('state', 'texas')
plot(JanPrcp1945Locations, add=TRUE, pch=20)
plot(JanEvap1945Locations, add=TRUE, pch=0, col='red')
plot(JanTmpMax1945Locations, add=TRUE, pch=3, col='green')

#---------------------------------------------------------------------------------------------------------------------
  
# Temperature values for August 1945 in Texas
AugTmpMax1945 = read_xlsx("1945/Aug/1945-Aug-TMax.xlsx")
AugTmpMax1945$LATITUDE = as.numeric(AugTmpMax1945$LATITUDE)
AugTmpMax1945$LONGITUDE = as.numeric(AugTmpMax1945$LONGITUDE)

AugTmpMax1945Locations = AugTmpMax1945[c(2,3,4)]
AugTmpMax1945Locations = AugTmpMax1945Locations[!duplicated(AugTmpMax1945Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugTmpMax1945Locations) = AugTmpMax1945Locations[c(3,2)]

# Evaporation values for August 1945 in Texas
AugEvap1945 = read_xlsx("1945/Aug/1945-Aug-Evap.xlsx")
AugEvap1945$LATITUDE = as.numeric(AugEvap1945$LATITUDE)
AugEvap1945$LONGITUDE = as.numeric(AugEvap1945$LONGITUDE)

AugEvap1945Locations = AugEvap1945[c(2,3,4)]
AugEvap1945Locations = AugEvap1945Locations[!duplicated(AugEvap1945Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugEvap1945Locations) = AugEvap1945Locations[c(3,2)]

# Precipitation values for August 1945 in Texas
AugPrcp1945 = read_xlsx("1945/Aug/1945-Aug-Prcp.xlsx")
AugPrcp1945$LATITUDE = as.numeric(AugPrcp1945$LATITUDE)
AugPrcp1945$LONGITUDE = as.numeric(AugPrcp1945$LONGITUDE)

AugPrcp1945Locations = AugPrcp1945[c(2,3,4)]
AugPrcp1945Locations = AugPrcp1945Locations[!duplicated(AugPrcp1945Locations[,c('NAME','LATITUDE','LONGITUDE')]),]
coordinates(AugPrcp1945Locations) = AugPrcp1945Locations[c(3,2)]