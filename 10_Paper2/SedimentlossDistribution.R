#**********************************************
# Visualize soil loss in Gerda watershed
# 
#**********************************************


library(ggmap)
library(dplyr)
library(rgdal)
library(sp)
library(ggplot2)
library(rgeos)
library(maptools)
library(gpclib)


# Read the output file from SWAT simulation----
# Daily simulations
setwd("E:/ArcSWAT_Examples/4_Gerda NNR/FAO/Scenarios/2015-11-17_FAO_Daily/TxtInOut")
output <- read.table("output.hru", skip=9)
# for days
output1 <- output[, c(1:10, 31,39)] # keep only the basic columns and the sediment yield and water yield
# names for daily
names(output1) <- c("LULC", "HRU", "GIS", "SUB", "MGT", "MO", "DA", "YR", "AREAkm2", "PRECIPmm", "WYLDmm", "SYLDt/ha")

#******************************************************************************************************
# # Monthly simulations
# setwd("E:/ArcSWAT_Examples/4_Gerda NNR/FAO/Scenarios/2015-11-11_FAO_NwLU_HeatUnits/TxtInOut")
# 
# output <- read.table("output.hru", skip=9)
# # for months
# output1 <- output[, c(1:7,28,35)]
# 
# # names for monthly
# names(output1) <- c("LULC", "HRU", "GIS", "SUB", "MGT", "AREAkm2", "PRECIPmm", "WYLDmm", "SYLDt/ha")

#******************************************************************************************************


# Re-write the leading zeros to HRU_GIS----
output1$GIS <- paste0("0000", output1$GIS)
head(output1)
years <- seq(1986, 2014, 1)
months <- seq(1,12,1)


# Subset data by year and month
for(i in years){
   subset.temp <- subset(output1, YR == i & MO == 7)
   name <- paste("Subset.", i, sep="")
   assign(name, subset.temp)
   rm(subset.temp)
}
head(Subset.2000)




# load the shapefile layer for Gerda
setwd("E:/ArcSWAT_Examples/4_Gerda/GerdaNewLU/Watershed/Shapes/")


# gerda <- readShapeSpatial("hru1.shp")
gerda <- readShapePoly("hru1.shp")
rivers <- readShapeLines("riv1.shp")


# gerdadata <- gerda@data
gerda@data <- merge(gerda@data, Subset.2000, by.x="HRU_GIS", by.y="GIS", sort=FALSE)

# define color palette for plotting
rbPal <- colorRampPalette(c("red", "white"))
gerda@data$Col <- rbPal(10)[as.numeric(cut(gerda@data$'SYLDt/ha', 10))]

plot(gerda, col=gerda@data$Col)

# does not work yet-----
writeOGR(gerda, "Gerda", layer, driver="ESRI Shapefile")
writePolyShape(gerda, "~/R/Gerda")
