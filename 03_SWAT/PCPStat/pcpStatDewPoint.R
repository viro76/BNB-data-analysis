#***************************
# Dew Point calculations   *
#***************************


library(stringr)
library(foreach)
library(tools)
library(ggplot2)
library(zoo)
library(hydroTSM)
library(hydroGOF)
library(MASS)
library(e1071) 


# Calculate dewpoint
# All formulas are taken from 
# Y:\23_SWAT_Model_and_Software\01_Software\16_DewpointEstimator\ manual_dew.pdf


# Folders
# Anjeni       E:/ArcSWAT_Examples/1_Anjeni NNR/Climate/tmp/Original/
#              E:/ArcSWAT_Examples/1_Anjeni NNR/climate/pcpTempStatistics/
# Maybar       E:/ArcSWAT_Examples/2_Maybar NNR/climate/tmp/Original/
#              E:/ArcSWAT_Examples/2_Maybar NNR/climate/pcpStats
# AnditTid:    E:/ArcSWAT_Examples/2_Maybar NNR/climate/tmp/Original/
#              E:/ArcSWAT_Examples/2_Maybar NNR/climate/pcpStats
# Gerda        E:/ArcSWAT_Examples/4_Gerda NNR/climate/rh/Original
#              E:/ArcSWAT_Examples/4_Gerda NNR/climate/pcpStats


# Set the necessary folders
inFolderRH <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/rh/original/"
inFolderT <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/tmp/original/"
outFolder <- "C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin NNR/WLRC/climate/pcpStats/"

setwd(inFolderT)
tmpt <- list.files(path=".", pattern="().*\\.csv$")

# Start with the relative humidity
setwd(inFolderRH)
# for CFSR data
# filesrh <- list.files(path=".", pattern="(r0|r1|r2|r3|r4|r5|r6|r7|r8|r9).*\\.txt$")
# for SCRP/WLRC data
filesrh <- list.files(path=".", pattern="(r0|r1|r2|r3|r4|r5|r6|r7|r8|r9).*\\.txt$")
# setwd(inFolderT)
# tmpt <- list.files(path=".", pattern="().*\\.csv$")


for(i in filesrh){
   x <- read.table(i, header=T)
   names(x) <- basename(file_path_sans_ext(i))
   assign(i, x)
}


# Combine each data column of 640 files to one file
x <- foreach(i=filesrh,.combine=cbind)%do%{
   x <- read.table(i, header=TRUE, sep=";", stringsAsFactor=FALSE)
   names(x) <- basename(file_path_sans_ext(i))
   x
}
n <- paste("RH",ncol(x), "St", ".csv", sep="_")

# Input the start and the end date of your time series
date <- seq(as.Date("1979/01/01"), as.Date("2014/07/31"), by="days")
x <- cbind(date, x)

write.csv(x, n, row.names=FALSE)

#*******************************************************************************
# Read the RH file with date and data columns
rhdata <- read.csv(n, header=TRUE)
rhdata$date <- as.Date(rhdata$date, format="%Y-%m-%d")
# Define NAs
NAs <- rhdata == -99
is.na(rhdata)[NAs] <- TRUE
# Define date and min/max data
date <- rhdata[,1]

setwd(inFolderT)
tempdata <- read.csv(paste(inFolderT,tmpt,sep=""), header=TRUE)
tempdata$date <- as.Date(tempdata$date, format="%Y-%m-%d")
is.na(tempdata)[NAs] <- TRUE


#*******************************************************************************
# Count columns and define the names for each column according to header
namesC <- colnames(rhdata)[2:ncol(rhdata)]
# for CFSR
nameStrings <- str_extract_all(namesC, "\\(?[0-9,.]+\\)?"[[1]])
# for SCRP/WLRC
# nameStrings <- colnames(tempdata)[2:ncol(tempdata)]

month <- month.abb
MeanMonthRH <- data.frame()
MeanDewpoint <- data.frame()

for (i in nameStrings){
   rh <- paste("r",i, sep="")
   
#    tmpMAX <- paste("Max", i, sep="")
#    tmpMIN <- paste("Min", i, sep="")

   # for CFSR
   tmpMAX <- paste("t",i,"Max", sep="")
   tmpMIN <- paste("t",i,"Max", sep="")

   # for SCRP/WLRC
   # tmpMax <- paste("tmp", i, "Max", sep="")
   # tmpMin <- paste("tmp", i, "Min", sep="")
   
   rhdt <- rhdata[rh]
   tmpMXdata <- tempdata[tmpMAX]
   tmpMNdata <- tempdata[tmpMIN]
   data <- cbind(rhdt, tmpMXdata, tmpMNdata)
   names(data) <- c("rh", "tmpMX", "tmpMN")
   
   maxRw <- nrow(data)
   Dewpoint <- data.frame()
      
   for(j in 1:maxRw){
      
      temp <- data[j,]
      temprh <- temp$rh*100
      tempMX <- temp$tmpMX
      tempMN <- temp$tmpMN
      
      # Saturation vapour pressure
      # es = 0.6108 x exp((17.27 x T)/(T + 237.3))
      esMX <- (0.6108 * exp((17.27 * tempMX)/(tempMX + 237.3))*10)
      esMN <- (0.6108 * exp((17.27 * tempMN)/(tempMN + 237.3))*10)
      # Average daily actual vapour pressure ea
      es <- (esMN + esMX)/2
      ea <- temprh * (es/100)
      # Dewpoint calculation
      # dew = (234.18 * log10(ea) - 184.2)/(8.204 - log10(ea))
      D <- (234.18 * log10(ea) - 184.2)/(8.204 - log10(ea))
      
      Dewpoint <- rbind(Dewpoint, D)
   }
   
   # Aggregate mean dewpoint for month
   Dewpoint <- cbind(date, Dewpoint)
   names(Dewpoint) <- c("date", "dwpt")
   Dewpoint$mo <- strftime(Dewpoint$date, "%m")
   Dewpoint$yr <- strftime(Dewpoint$date, "%Y")
   dd.agg <-aggregate(dwpt ~ mo + yr, Dewpoint, FUN=mean, na.rm=TRUE)
   mm.agg <- aggregate(dwpt ~ mo, dd.agg, FUN=mean, na.rm=TRUE)
   mm.agg <- mm.agg[,2]
   mm.agg <- data.frame(t(mm.agg))
   names(mm.agg) <- month
   MeanDewpoint <- rbind(MeanDewpoint, round(mm.agg,3))
   
}


setwd(outFolder)
writeLines(c("Mean monthly dew point", 
             "------------------------------------", 
             ""), "DEWPT.csv")
write.table(MeanDewpoint, "DEWPT.csv", quote=FALSE, 
            sep="\t", row.names=FALSE, append=TRUE)


#******************************************************************************