
library(ggmap)
library(dplyr)
library(rgdal)
library(sp)
library(ggplot2)
library(rgeos)
library(maptools)
library(gpclib)

setwd("C:/Modelling/ArcSWAT_Examples/4_Gerda/GerdaClassic/Watershed/Shapes/")


gerda <- readShapeSpatial("hru1.shp")
gerda <- readShapePoly("hru1.shp")
rivers <- readShapeLines("riv1.shp")


gerdadata <- gerda@data
gerda@data$HRU_ID

# Load csv data from output from SWAT
sediment <- read.csv("~/R/Data/Gerda/hru.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
sediment$HRUGIS <- paste0("0000", sediment$HRUGIS)
sediment10 <- subset(sediment, YEAR=="2010")


Gerda4manipulation <- SpatialPolygonsDataFrame(gerda, data=as(gerda, "data.frame"))
GerdaData <- merge(Gerda4manipulation@data, sediment, by.x="HRU_GIS", by.y="HRUGIS", sort=FALSE)

# define color palette for plotting
rbPal <- colorRampPalette(c("red", "blue"))
Gerda4manipulation@data$Col <- rbPal(10)[as.numeric(cut(GerdaData$SYD))]

plot(gerda, col=gray(gerda@data$SYLDt_ha))



#*************************************************
# With dplyr
shp <- gerda

shp@data <- dplyr::left_join(x=shp@data,
                             y=sediment,
                             by=setNames("HRU_GIS", "HRUGIS"))














