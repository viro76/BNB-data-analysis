#*******************************************************#
# Calculate Precipitation statistics for SWAT           #
#*******************************************************#
# Author:   Vincent Roth                                #
# Date:     01/05/2014                                  #
#                                                      #
#*******************************************************#

library(ggplot2)                                                                  
library(zoo)                                                                     
library(hydroTSM)                                                                
library(hydroGOF)                                                                
library(MASS)                                                                    
library(e1071)                                                                   
library(foreach)                                                                 
library(tools)  
library(stringr)
source("~/R/Scripts/17_Functions/myfunction.R")

# Data must be stored in a folder. The folder must contain files with
# daily precipitation data per station. Each file must have a distinct
# file name, which will then be taken as column name. 
# Change the name next to parameters according to your file system



# Define the names of the folders where you store input data and ----
# where you want to save output data

# Anjeni    E:/ArcSWAT_Examples/1_Anjeni NNR/climate/pcp/Original
#           E:/ArcSWAT_Examples/1_Anjeni NNR/climate/pcpTempStatistics/
# Maybar    E:/ArcSWAT_Examples/2_Maybar NNR/climate
#           E:/ArcSWAT_Examples/2_Maybar NNR/climate
# AnditTid  E:/ArcSWAT_Examples/3_AnditTid NNR/climate/pcp/Original
#           E:/ArcSWAT_Examples/3_AnditTid NNR/climate
# Gerda     E:/ArcSWAT_Examples/4_Gerda NNR/climate/Originals
#           E:/ArcSWAT_Examples/4_Gerda NNR/climate/


inFolder <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/pcp/original/"
outFolder <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/pcpStats/"


#*******************************************************************************
# This code looks for files in a folder, which are 3.txt and contain
# "p0 - p9" in their file name. It then reads each file and combines 
# the data in one file with rownames according to the filename
# This paragraph is only necessary if you do not have one single file with
# date --> data1 --> data2 --> data3
#******************************************************************************
# Set the path to the folder containing files----
setwd(inFolder)   

# for CFSR data
filesp <- list.files(path=".", pattern="(p0|p1|p2|p3|p4|p5|p6|p7|p8|p9).*\\.txt$")

# filesp <- list.files(path=".", pattern="(p).*\\.txt$")

# for SCRP data
# filesp <- list.files(path=".", pattern="(ajpcp).*\\.txt$")


# for(i in filesp){
#    x <- read.table(i, header=T)
#    names(x) <- basename(file_path_sans_ext(i))
#    assign(i, x)
#    rm(x)
# }

# Combine each data column of files to one single file----
x <- foreach(i=filesp,.combine=cbind)%do%{
   x <- read.table(i, header=T, sep=";", stringsAsFactor=FALSE, na.strings = "NA")
   names(x) <- basename(file_path_sans_ext(i))
   x
}
date <- seq(as.Date("1979/01/01"), as.Date("2014/07/31"), by="days")
y <- cbind(date,x)
#***************************************************
#***************************************************
# # y <- data.frame()
# 
# date <- seq(as.Date("1979/01/01"), as.Date("2014/07/31"), by="days")
# 
# for (i in filesp){
#   x <- read.table(i, header=T, sep=";", stringsAsFactors = FALSE, na.strings = "-99")
#   names(x) <- basename(file_path_sans_ext(i))
#   # y <- x
#   y <- cbind(date,x)
# }

#***************************************************
#***************************************************
n <- paste("pcp",ncol(y)-1, "St", ".csv", sep="_")

# # Input the start and the end date of your time series
# date <- seq(as.Date("1979/01/01"), as.Date("2014/07/31"), by="days")
# x <- cbind(date, x)

write.csv(y, n, row.names=FALSE, quote=FALSE)
#*******************************************************************************
#*******************************************************************************


#*******************************************************************************
# Read the precipitation file with date and data columns----
# setwd("...")                                                                   # set your working directory accordingly
pcpdata <- read.csv(n, header=TRUE)                                              # here you can change the filename from n to your file
pcpdata$date <- as.Date(pcpdata$date, format="%Y-%m-%d")
# Define NAs
NAs <- pcpdata == -99
is.na(pcpdata)[NAs] <- TRUE
# Define date and min/max data
date <- pcpdata[,1]


#*******************************************************************************
# Count columns and define the names for each column according to header
namesC <- colnames(pcpdata)[2:ncol(pcpdata)]
month <- month.abb
monthlyMean <- data.frame()

for (i in namesC){
   # combine date and every column of data
   date <- pcpdata[,1]
   pcp <- pcpdata[i]
   pcp <- cbind(date, pcp)
   
   pcp.z <- read.zoo(pcp, sep=",", format=c("%Y-%m-%d"))
   monthlyM.pcp <- daily2monthly(pcp.z, FUN=mean, na.rm=TRUE) # adapt the nr.rm if necessary
   monthlyS.pcp <- daily2monthly(pcp.z, FUN=sum, na.rm=FALSE)
   
   dates <- time(pcp.z)
   nyrs.pcp <- yip(from=start(pcp.z), to=end(pcp.z), out.type="nmbr")
   
   # Mean daily rainfall per month calculation and writing the file-----
   pcp.M <- matrix(monthlyM.pcp, ncol=12, byrow=TRUE)
   pcp.M <- round(pcp.M, 3)
   colnames(pcp.M) <- month.abb
   rownames(pcp.M) <- unique(format(time(monthlyM.pcp), "%Y"))
   pcp.M <- round(pcp.M, 2)
   datecol <- row.names(pcp.M)
   pcp.dmean <- as.data.frame(pcp.M, row.names=FALSE)
   pcp.dmean <- cbind(Date=datecol, pcp.dmean)
   # Write the data into one file per station (MonthlyMean_XY.csv)
#    write.table(pcp.mean, paste("MonthlyMean_",i, ".csv",sep=""), quote=FALSE,
#                sep="\t", row.names=FALSE)
      
   # Monthly sum data-----
   pcp.S <- matrix(monthlyS.pcp, ncol=12, byrow=TRUE)
#    pcp.S <- round(pcp.S, 3)
   colnames(pcp.S) <- month.abb
   rownames(pcp.S) <- unique(format(time(monthlyS.pcp), "%Y"))
   pcp.S <- round(pcp.S, 2)
   datecol <- row.names(pcp.S)
   pcp.sum <- as.data.frame(pcp.S, row.names=FALSE)
   pcp.sum <- cbind(Date=datecol, pcp.sum)
   # Write the data into the one file per Station (MonthlySum_XY.csv)
#    write.table(pcp.sum, paste("MonthlySum_",i,".csv", sep=""), quote=FALSE, 
#                sep="\t", row.names=FALSE)
   
   # Prepare monthly mean rainfall----
   meanpcp <- pcp
   names(meanpcp) <- c("date", "pcp")
   meanpcp$mo <- strftime(meanpcp$date, "%m")
   meanpcp$yr <- strftime(meanpcp$date, "%Y")
   dailySum <- aggregate(pcp ~ mo + yr, meanpcp, FUN=sum, na.rm=TRUE)
   monthlyAgg <- aggregate(pcp ~ mo, dailySum, FUN=mean, na.rm=TRUE)
   monthlyAgg <- t(monthlyAgg[,2])
   monthlyAgg <- data.frame(round(monthlyAgg,2))
   names(monthlyAgg) <- month
   rownames(monthlyAgg) <- i
#    monthlyAgg <- cbind(loc=i, monthlyAgg)
   monthlyMean <- rbind(monthlyMean, monthlyAgg)

}

setwd(outFolder)
writeLines(c("Mean monthly rainfall", 
             "------------------------------------", 
             ""), "PCP_MM.csv")
write.table(monthlyMean, "PCP_MM.csv", quote=FALSE, sep="\t", 
            row.names=TRUE, append=TRUE)


#*******************************************************************************
#*******************************************************************************
# Calculate standard deviation for daily precipitation----

setwd(inFolder)
StDevPCP <- data.frame()

for (i in namesC){
   sdpcp <- pcpdata[i]
   sdpcp <- cbind(date, sdpcp)
   names(sdpcp) <- c("date", "sd")
   
   # Mean monthly data calculation and writing the file
   sdpcp$mo <- strftime(sdpcp$date, "%m")
   sdpcp$yr <- strftime(sdpcp$date, "%Y")
   # Create standard deviation for months in every year
   dd.sdagg <- aggregate(sd ~ mo + yr, sdpcp, FUN=sd)
   # Create the standard deviation for the years
   mm.sdagg <- aggregate(sd ~ mo, dd.sdagg, FUN=mean)
   # Adapt the data for export
   mm.sdagg <- mm.sdagg[,2]
   mm.sdagg <- t(mm.sdagg)
   mm.sdagg <- data.frame(round(mm.sdagg, 2))
   names(mm.sdagg) <- month
#    mm.sdagg <- cbind(loc=i, mm.sdagg)
   
   StDevPCP <- rbind(StDevPCP, mm.sdagg)
   
}

setwd(outFolder)
writeLines(c("Standard Deviation of daily rainfall in Month", 
             "---------------------------------------------",""),
           "PCPSTD.csv")
write.table(StDevPCP, "PCPSTD.csv", quote=FALSE,
            sep="\t", row.names=FALSE, append=TRUE)


#*******************************************************************************
#*******************************************************************************
# Calculate skewness----

setwd(inFolder)
month <- month.abb
skewpcp <- data.frame()

# Start the loop
for(i in namesC){
   date <- pcpdata[,1]
   SkewPCP <- data.frame()
   skpcp <- pcpdata[i]
   skpcp <- cbind(date, skpcp)
   names(skpcp) <- c("date", "skewness")
   sk.z <- read.zoo(skpcp, sep=",", format=c("%Y-%m-%d"))
   
   for (j in month){
      x <- coredata(sk.z[months(time(sk.z), TRUE) %in% j])
      sk <- round(skewness(x, na.rm=TRUE),4)
      SkewPCP <- rbind(SkewPCP,sk)
   }
   SkewPCP <- t(SkewPCP)
   SkewPCP <- data.frame(SkewPCP)
   names(SkewPCP) <- month
#    SkewPCP <- cbind(loc=i, SkewPCP)
   skewpcp <- rbind(skewpcp, SkewPCP)
   
}

# Write the file
setwd(outFolder)
writeLines(c("Skewness of daily rainfall in Month", 
             "---------------------------------------------",""),
           "PCPSKW.csv")
write.table(skewpcp, "PCPSKW.csv", quote=FALSE,
            sep="\t", row.names=FALSE, append=TRUE)

#*******************************************************************************
#*******************************************************************************
# Count number of rainfall events----
# 
setwd(inFolder)
PCPD <- data.frame()
# Start the loop
for (i in namesC){
   date <- pcpdata[,1]
   pcp <- pcpdata[i]
   pcp <- cbind(date, pcp)
   pcpd <- data.frame()
   
   pcpd.z <- read.zoo(pcp, sep=",", format=c("%Y-%m-%d"))
   # Number of years
   nyrs <- yip(from=start(pcpd.z),to=end(pcpd.z),out.type="nmbr")
   
   # Start loop for every month   
   for (j in month){
      x <- coredata(pcpd.z[months(time(pcpd.z), TRUE) %in% j])                   # x %in% y --> x is an element of y
      sx <- round(sum(x>0.1, na.rm=TRUE)/nyrs, 4)
      pcpd <- rbind(pcpd, sx)
   }
   pcpd <- t(pcpd)
   pcpd <- data.frame(pcpd)
   names(pcpd) <- month
   PCPD <- rbind(PCPD, pcpd)
}

# Write the file
setwd(outFolder)
writeLines(c("Mean number of days of Rainfall higher than 0.1 mm in Month", 
             "---------------------------------------------",""),
           "PCPD.csv")
write.table(PCPD, "PCPD.csv", quote=FALSE,
            sep="\t", row.names=FALSE, append=TRUE)






#*******************************************************************************
# Probability of wet day following a dry day in a month
monthlyDry <- data.frame()
thr <- 0.1


for (i in namesC){
   date <- pcpdata[,1]
   pcp <- pcpdata[i]
   pcp <- cbind(date, pcp)
   monthlydd <- data.frame()
   pcpd.z <- read.zoo(pcp, sep=",", format=c("%Y-%m-%d"))
   
   # combine date and every column of data
   for(m in 1:12){
#       x <- pcpd.z
      pcp.m <- extract(pcpd.z, m)
      # define wet days and dry days with threshold 0.1 mm
      wetday <- which(pcp.m >= thr)
      dryday <- which(pcp.m < thr)
      probs <- data.frame()
      
      # calculate the number of dry and wet days for every month
      for (j in 1:(length(dryday)-1)){
         a <- dryday[j]
         b <- dryday[j+1]
         c <- b-a==1
         probs <- rbind(probs, c)
      }
      numdd <- as.numeric(table(probs)["FALSE"])
      moprob <- round(numdd/length(dryday), 3)
      monthlydd <- rbind(monthlydd, moprob)
   }
   monthlydd <- t(monthlydd)
   monthlydd <- data.frame(monthlydd)
   names(monthlydd) <- month
   monthlyDry <- rbind(monthlyDry, monthlydd)
   monthlyDry[is.na(monthlyDry)] <- 1
#    return(monthlyDry)
}

# Write the file
setwd(outFolder)
writeLines(c("Probability of a wet day following a dry day in a month", 
             "---------------------------------------------",""),
           "PR_W1.csv")
write.table(monthlyDry, "PR_W1.csv", quote=FALSE,
            sep="\t", row.names=FALSE, append=TRUE)

#******************************************************************************
#******************************************************************************
#******************************************************************************
# Probability of a wet day following a wet day in month

monthlyWet <- data.frame()
thr <- 0.1


for (i in namesC){
   date <- pcpdata[,1]
   pcp <- pcpdata[i]
   pcp <- cbind(date, pcp)
   monthlywd <- data.frame()
   pcpd.z <- read.zoo(pcp, sep=",", format=c("%Y-%m-%d"))
   
   # combine date and every column of data
   for(m in 1:12){
      #       x <- pcpd.z
      pcp.m <- extract(pcpd.z, m)
      # define wet days and dry days with threshold 0.1 mm
      wetday <- which(pcp.m >= thr)
      dryday <- which(pcp.m < thr)
      probs <- data.frame()
      
      # calculate the number of dry and wet days for every month
      for (j in 2:(length(wetday))){
         a <- wetday[j-1]
         b <- wetday[j]
         c <- b-a==1
         probs <- rbind(probs, c)
      }
      numdd <- as.numeric(table(probs)["TRUE"])
      moprob <- round(numdd/length(wetday), 3)
      monthlywd <- rbind(monthlywd, moprob)
   }
   monthlywd <- t(monthlywd)
   monthlywd <- data.frame(monthlywd)
   names(monthlywd) <- month
   monthlyWet <- rbind(monthlyWet, monthlywd)
   monthlyWet[is.na(monthlyWet)] <- 1
   #    return(monthlyDry)
}


rownames(monthlyWet) <- namesC

# Write the file
setwd(outFolder)
writeLines(c("Probability of a wet day following a wet day in a month", 
             "---------------------------------------------",""),
           "PR_W2.csv")
write.table(monthlyWet, "PR_W2.csv", quote=FALSE,
            sep="\t", row.names=FALSE, append=TRUE)

#************************************************************
#************************************************************
# Maximum half-hourly rainfall RAINHHMX ----
# According to Srinivasan it is 1/3 of maximum daily prec for a given month

setwd(inFolder)
RainHHMax <- data.frame()

namesC <- colnames(pcpdata)[2:ncol(pcpdata)]
month <- month.abb
monthlyMean <- data.frame()

for (i in namesC){
  maxhh <- pcpdata[i]
  maxhh <- cbind(date, maxhh)
  names(maxhh) <- c("date", "pcp")
  
  # Mean monthly data calculation and writing the file
  maxhh$mo <- strftime(maxhh$date, "%m")
  maxhh$yr <- strftime(maxhh$date, "%Y")
  # Create standard deviation for months in every year
  mm.maxhh <- aggregate(pcp ~ mo + yr, maxhh, FUN=max)
  mm.maxhh$pcp <- round(mm.maxhh$pcp/3, 2)
  mm.maxhh <- aggregate(pcp ~ mo, mm.maxhh, FUN=max)
  mm.maxhh <- mm.maxhh[,2]
  
  
  # # maxhh <- mm.maxhh[,2]/3
  # maxhh <- t(mm.maxhh[,2])
  # # maxhh <- data.frame(round(maxhh, 2))
  # names(maxhh) <- month
  
  #    mm.sdagg <- cbind(loc=i, mm.sdagg)
  
  RainHHMax <- rbind(RainHHMax, mm.maxhh)
  
}
names(RainHHMax) <- month

setwd(outFolder)
writeLines(c("Maximum half-hour rainfall for month", 
             "---------------------------------------------",""),
           "PCPHHMAX.csv")
write.table(RainHHMax, "PCPHHMAX.csv", quote=FALSE,
            sep="\t", row.names=FALSE, append=TRUE)

