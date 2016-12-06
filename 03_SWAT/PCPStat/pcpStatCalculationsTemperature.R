#***********************************************#
# Calculate temperature statistics for SWAT     #
#***********************************************#
# Author:   Vincent Roth                        #
# Date:     02.05.2014                          #
#***********************************************#

library(ggplot2)
library(zoo)
library(hydroTSM)
library(hydroGOF)
library(MASS)
library(e1071) 
library(foreach)
library(tools)
library(stringr)

# Prepare input and output folders----
#*****************************************************************************#
# Anjeni    E:/ArcSWAT_Examples/1_Anjeni NNR/Climate/tmp/Original
#           E:/ArcSWAT_Examples/1_Anjeni NNR/climate/pcpTempStatistics/
# Maybar    E:/ArcSWAT_Examples/2_Maybar NNR/climate
#           E:/ArcSWAT_Examples/2_Maybar NNR/climate
# AnditTid  E:/ArcSWAT_Examples/3_AnditTid NNR/climate/pcp/Original
#           E:/ArcSWAT_Examples/3_AnditTid NNR/climate
# Gerda     E:/ArcSWAT_Examples/4_Gerda NNR/climate/Originals
#           E:/ArcSWAT_Examples/4_Gerda NNR/climate/tmp/tmpStats

inFolder <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/tmp/original/"
outFolder <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/pcpStats/"


#*****************************************************************************#


# Prepare temperature----
#********************
setwd(inFolder)
# for CFSR data
filesp <- list.files(path=".", pattern="(t0|t1|t2|t3|t4|t5|t6|t7|t8|t9).*\\.txt$")
# for SCRP/WLRC data
# filesp <- list.files(path=".", pattern="(t).*\\.txt$")


for(i in filesp){
   x <- read.table(i, header=F, sep=",", skip=1)
   names(x) <- paste(basename(file_path_sans_ext(i)),c("_Max","_Min"), sep="")
   assign(i, x)
   rm(x)
}

# Combine each data column of files to one single file
x <- foreach(i=filesp,.combine=cbind)%do%{
   x <- read.table(i, header=F, sep=",", stringsAsFactor=FALSE, skip=1)
   names(x) <- paste(basename(file_path_sans_ext(i)), c("Max", "Min"), sep="")
   x
}
n <- paste("tmp",ncol(x)/2, "St", ".csv", sep="_")

# Input the start and the end date of your time series
date <- seq(as.Date("1979/01/01"), as.Date("2014/07/31"), by="days")
x <- cbind(date, x)

write.csv(x, n, row.names=FALSE, quote=FALSE)
#*******************************************************************************
#*******************************************************************************


# Read data in folder----
# data needs to be in file calles "tempNNR.csv" with the following order
# date --> MaxXY --> MinXY --> MaxXZ --> MinXZ
#**************************************************#
tempdata <- read.csv(n, header=TRUE)   #
#**************************************************#
# set date format for file----
tempdata$date <- as.Date(tempdata$date, format="%Y-%m-%d")

# Define NAs for R to recognize----
NAs <- tempdata == -99
is.na(tempdata)[NAs] <- TRUE
# Define date and min/max data
date <- tempdata[,1]
max <- tempdata[,grep("Max",colnames(tempdata))]
min <- tempdata[,grep("Min",colnames(tempdata))]


nameColMax  <- colnames(max)[1:ncol(max)]
nameColMin <- colnames(min)[1:ncol(max)]

#*******************************************************************************
# Prepare the Maximum temperature data----
MeanMaxMonthTemp <- data.frame()
month <- month.abb


for (i in nameColMax){
   tmp <- max[i]
   tmp <- cbind(date, tmp)
   names(tmp) <- c("date", "temp")
   
   # Mean monthly maximum data calculation and writing the file
   tmp$mo <- strftime(tmp$date, "%m")
   tmp$yr <- strftime(tmp$date, "%Y")
   dd.agg <- aggregate(temp ~ mo + yr, tmp, FUN="mean")
   mm.agg <- aggregate(temp ~ mo, dd.agg, FUN="mean")
   mm.agg <- mm.agg$temp
   
   # occurences <- as.data.frame(table(tmp$mo))
   # occurences <- occurences$Freq
   # dfMax <- data.frame()
   # 
   # for (j in 1:12){
   #   tempMax <- mm.agg[j]/occurences[j]
   #   dfMax <- rbind(dfMax, tempMax)
   #   }
   
   dfMax <- t(mm.agg)
   dfMax <- round(dfMax, 2)
   ifelse(ncol(dfMax)==12, 
       colnames(dfMax) <- month, 
       dfMax <- cbind(dfMax, NA))
   colnames(dfMax) <- month
   rownames(dfMax) <- i
   
   MeanMaxMonthTemp <- rbind(MeanMaxMonthTemp, dfMax)
}

setwd(outFolder)
writeLines(c("Mean Monthly Maximal Temperature", 
             "---------------------------------------------",""),
           "TMPMX.csv")

write.table(MeanMaxMonthTemp, "TMPMX.csv", quote=FALSE, 
            sep="\t", row.names=FALSE, append=TRUE)

#*******************************************************************************
# Mean minimal monthly temperature-----

setwd(inFolder)
MeanMinMonthTemp <- data.frame()

for (f in nameColMin){
   tmp <- min[f]
   tmp <- cbind(date, tmp)
   names(tmp) <- c("date", "temp")
   
   # Mean monthly data calculation and writing the file
   tmp$mo <- strftime(tmp$date, "%m")
   tmp$yr <- strftime(tmp$date, "%Y")
   dd.agg <- aggregate(temp ~ mo + yr, tmp, FUN="mean")
   mm.agg <- aggregate(temp ~ mo, dd.agg, FUN="mean")
   mm.agg <- mm.agg$temp
 
#******************************************************  
#    occurences <- as.data.frame(table(tmp$mo))
#    occurences <- occurences$Freq
#    month <- month.abb
#    dfMin <- data.frame()
#    
#    for (j in 1:12){
#       tempMin <- mm.agg[j]/occurences[j]
#       dfMin <- rbind(dfMin, tempMin)
#    }
   
#    dfMin <- t(dfMin)
#*******************************************************  
   
   dfMin <- t(mm.agg)
   dfMin <- round(dfMin, 2)
   ifelse(ncol(dfMin)==12, 
          colnames(dfMin) <- month, 
          dfMin <- cbind(dfMin, NA))
   colnames(dfMin) <- month
   rownames(dfMin) <- paste(i)
   
   MeanMinMonthTemp <- rbind(MeanMinMonthTemp, dfMin)
}
setwd(outFolder)
writeLines(c("Mean  Monthly Minimal Temperature", 
             "----------------------------------------",""),
           "TMPMN.csv")
write.table(MeanMinMonthTemp, "TMPMN.csv", quote=FALSE,
            sep="\t", row.names=FALSE, append=TRUE)



#*******************************************************************************
#*******************************************************************************
# Calculate standard deviation in monthly max and min temperature data----
setwd(inFolder)
StDevMaxima <- data.frame()

for (i in nameColMax){
   sdtmp <- max[i]
   sdtmp <- cbind(date, sdtmp)
   names(sdtmp) <- c("date", "sd")
   
   # Mean monthly data calculation and writing the file
   sdtmp$mo <- strftime(sdtmp$date, "%m")
   sdtmp$yr <- strftime(sdtmp$date, "%Y")
   # Create standard deviation for months in every year
   dd.sdagg <- aggregate(sd ~ mo + yr, sdtmp, FUN=sd)
   # Create the standard deviation for the years
   # mm.sdagg <- aggregate(sd ~ mo, dd.sdagg, FUN=mean)
   mm.sdagg <- aggregate(sd ~ mo, sdtmp, FUN=sd)
   # Adapt the data for export
   mm.sdagg <- mm.sdagg[,2]
   mm.sdagg <- t(mm.sdagg)
   mm.sdagg <- data.frame(round(mm.sdagg, 2))
   ifelse(ncol(mm.sdagg)==12, 
          names(mm.sdagg) <- month,
          mm.sdagg <- cbind(mm.sdagg, NA))
   names(mm.sdagg) <- month
   StDevMaxima <- rbind(StDevMaxima, mm.sdagg)
   
}
setwd(outFolder)
writeLines(c("Mean Standard Deviation from mean maximum temperature", 
             "----------------------------------------",""),
           "TEMPSTDMX.csv")

write.table(StDevMaxima, "TEMPSTDMX.csv", quote=FALSE,
            sep="\t", row.names=FALSE, append=TRUE)


#*******************************************************************************
# Standard deviation for minima----
setwd(inFolder)
StDevMinima <- data.frame()

for (i in nameColMin){
   sdtmp <- min[i]
   sdtmp <- cbind(date, sdtmp)
   names(sdtmp) <- c("date", "sd")
   
   
   # Mean monthly data calculation and writing the file
   sdtmp$mo <- strftime(sdtmp$date, "%m")
   sdtmp$yr <- strftime(sdtmp$date, "%Y")
   dd.sdagg <- aggregate(sd ~ mo + yr, sdtmp, FUN=sd)
   # mm.sdagg <- aggregate(sd ~ mo, dd.sdagg, FUN=mean)
   mm.sdagg <- aggregate(sd ~ mo, sdtmp, FUN=sd)
   mm.sdagg <- mm.sdagg[,2]
   mm.sdagg <- t(mm.sdagg)
   mm.sdagg <- data.frame(round(mm.sdagg, 2))
   ifelse(ncol(mm.sdagg)==12, 
          names(mm.sdagg) <- month,
          mm.sdagg <- cbind(mm.sdagg, NA))
   names(mm.sdagg) <- month
   
   StDevMinima <- rbind(StDevMinima, mm.sdagg)
   
   #    write.table(mm.sdagg, paste(i, "StDev.csv", sep=""), quote=FALSE,
   #                sep="\t", row.names=FALSE)
   
}
setwd(outFolder)
writeLines(c("Standard Deviation from Mean Monthly Minimal Temperature", 
             "----------------------------------------",""),
           "TEMPSTDMN.csv")

write.table(StDevMinima, "TEMPSTDMN.csv", quote=FALSE,
            sep="\t", row.names=FALSE, append=TRUE)
