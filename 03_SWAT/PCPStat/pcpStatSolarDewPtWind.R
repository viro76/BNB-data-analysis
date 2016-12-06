#*****************************************#
# Calculate Solar radiation files         #
# Calculate Dew Point files               #
# Calculate Wind direction                #
#*****************************************#
# Author:   Vincent Roth                  #
# Date:     02.05.2014                    #
#*****************************************#

library(ggplot2)
library(zoo)
library(hydroTSM)
library(hydroGOF)
library(MASS)
library(e1071) 
library(foreach)
library(tools)
library(stringr)

# Anjeni    E:/ArcSWAT_Examples/1_Anjeni NNR/climate/pcp/Original
#           E:/ArcSWAT_Examples/1_Anjeni NNR/climate/pcpTempStatistics/
# Maybar    E:/ArcSWAT_Examples/2_Maybar NNR/climate
#           E:/ArcSWAT_Examples/2_Maybar NNR/climate
# AnditTid  E:/ArcSWAT_Examples/3_AnditTid NNR/climate/pcp/Original
#           E:/ArcSWAT_Examples/3_AnditTid NNR/climate
# Gerda     E:/ArcSWAT_Examples/4_Gerda NNR/climate/Originals
#           E:/ArcSWAT_Examples/4_Gerda NNR/climate/


inFolderS <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/solar/original"
inFolderW <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/wind/original"
inFolderRH <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/rh/original"
outFolder <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/pcpStats/"




# Solar radiation----
#********************
# Read the single files in a folder accordig to name----
setwd(inFolderS)
# for CFSR data
filess <- list.files(path=".", pattern="(s0|s1|s2|s3|s4|s5|s6|s7|s8|s9).*\\.txt$")
# for SCRP/WLRC data
# filess <- list.files(path=".", pattern="(sol).*\\.csv$")

for(i in filess){
   x <- read.table(i, header=T)
   names(x) <- basename(file_path_sans_ext(i))
   assign(i, x)
}

# Combine each data column of files to one single file
x <- foreach(i=filess,.combine=cbind)%do%{
   x <- read.table(i, header=T, sep=";", stringsAsFactor=FALSE)
   names(x) <- basename(file_path_sans_ext(i))
   x
}
n <- paste("Solar",ncol(x), "St", ".csv", sep="_")

# Input the start and the end date of your time series
date <- seq(as.Date("1979/01/01"), as.Date("2014/07/31"), by="days")
x <- cbind(date, x)

write.csv(x, n, row.names=FALSE, quote=FALSE)

#*******************************************************************************
# Read the solardata file with date and data columns
solardata <- read.csv(n, header=TRUE)
solardata$date <- as.Date(solardata$date, format="%Y-%m-%d")
# Define NAs
NAs <- solardata == -99
is.na(solardata)[NAs] <- TRUE
# Define date and min/max data
date <- solardata[,1]


#*******************************************************************************
# Count columns and define the names for each column according to header
namesC <- colnames(solardata)[2:ncol(solardata)]
month <- month.abb
MeanMonthSolar <- data.frame()

for (i in namesC){
   solar <- solardata[i]
   solar <- cbind(date,solar)
   names(solar) <- c("date", "sol")
   
   # Mean monthly data calculation and writing the file
   solar$mo <- strftime(solar$date, "%m")
   solar$yr <- strftime(solar$date, "%Y")
   dd.agg <- aggregate(sol ~ mo + yr, solar, FUN=mean, na.rm=TRUE, na.action=NULL)
   mm.agg <- aggregate(sol ~ mo, dd.agg, FUN=mean, na.rm=TRUE, na.action=NULL)
   mm.agg <- mm.agg$sol
   mm.agg <- mm.agg * 0.0864
   mm.agg <- round(data.frame(t(mm.agg)),3)
   ifelse(ncol(mm.agg)==12, 
          names(mm.agg) <- month, 
          mm.agg <- cbind(mm.agg,NA))
   names(mm.agg) <- month
      
   MeanMonthSolar <- rbind(MeanMonthSolar, mm.agg)
}

setwd(outFolder)
writeLines(c("Mean monthly solar radiation", 
             "------------------------------------", 
             ""), "SolarAV.csv")
write.table(MeanMonthSolar, "SolarAV.csv", quote=FALSE, 
            sep="\t", row.names=FALSE, append=TRUE)


#*******************************************************************************
#*******************************************************************************
# Wind calculations

setwd(inFolderW)
# for CFSR data
filesw <- list.files(path=".", pattern="(w1|w2|w3|w4|w5|w6|w7|w8|w9).*\\.txt$")

# for SCRP/WLRC data
# filesw <- list.files(path=".", pattern="(wind).*\\.csv$")

for(i in filesw){
   x <- read.table(i, header=T)
   names(x) <- basename(file_path_sans_ext(i))
   assign(i, x)
}


# Combine each data column of 640 files to one file
x <- foreach(i=filesw,.combine=cbind)%do%{
   x <- read.table(i, header=T, sep=";", stringsAsFactor=FALSE)
   names(x) <- basename(file_path_sans_ext(i))
   x
}
n <- paste("Wind",ncol(x), "St", ".csv", sep="_")

# Input the start and the end date of your time series
date <- seq(as.Date("1979/01/01"), as.Date("2014/07/31"), by="days")
x <- cbind(date, x)

write.csv(x, n, row.names=FALSE, quote=FALSE)

#*******************************************************************************
# Read the winddata file with date and data columns
winddata <- read.csv(n, header=TRUE)
winddata$date <- as.Date(winddata$date, format="%Y-%m-%d")
# Define NAs
NAs <- winddata == -99
is.na(winddata)[NAs] <- TRUE
# Define date and min/max data
date <- winddata[,1]


#*******************************************************************************
# Count columns and define the names for each column according to header
namesC <- colnames(winddata)[2:ncol(winddata)]
month <- month.abb
MeanMonthWind <- data.frame()

for (i in namesC){
   wind <- winddata[i]
   wind <- cbind(date,wind)
   names(wind) <- c("date", "win")
   
   # Mean monthly data calculation and writing the file
   wind$mo <- strftime(wind$date, "%m")
   wind$yr <- strftime(wind$date, "%Y")
   dd.agg <- aggregate(win ~ mo + yr, wind, FUN=mean, na.rm=TRUE, na.action=NULL)
   mm.agg <- aggregate(win ~ mo, dd.agg, FUN=mean, na.rm=TRUE, na.action=NULL)
   mm.agg <- mm.agg$win
   mm.agg <- round(data.frame(t(mm.agg)),3)
     
   names(mm.agg) <- month
   
   MeanMonthWind <- rbind(MeanMonthWind, mm.agg)
}

setwd(outFolder)
writeLines(c("Mean monthly wind speed", 
             "------------------------------------",""), 
           "WNDAV.csv")
write.table(MeanMonthWind, "WNDAV.csv", quote=FALSE, 
            sep="\t", row.names=FALSE, append=TRUE)











