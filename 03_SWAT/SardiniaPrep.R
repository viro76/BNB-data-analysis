#*****************************************************
# Data comparison SWAT-Cup
# Comparison of discharge and soil loss for FAO data
# Date: 16/02/2015
#
#*****************************************************
library(ggplot2)
library(xts)
library(reshape2)
library(scales)
library(zoo)
library(Cairo)
library(plotrix)
source("~/R/Scripts/17_Functions/multiplot.R")


# FAO data only
FAOCal <- "Y:/vincent/Sardinia/SWAT-CupData/FAO/Calibration/"
WLRCCal <- "Y:/vincent/Sardinia/SWAT-CupData/WLRC/Calibration/"
FAOVal <- "Y:/vincent/Sardinia/SWAT-CupData/FAO/Validation/"
WLRCVal <- "Y:/vincent/Sardinia/SWAT-CupData/WLRC/Validation/"


#************************
# CALIBRATION DATA
DischAjFAO <- read.table(paste(FAOCal, "DischAJ_95ppu_Calib_FAO.txt", sep=""), header=TRUE, 
                         stringsAsFactors=FALSE, na.strings="NA")
DischAjFAO$date <- as.Date(DischAjFAO$date, format="%Y-%m-%d")
SedAjFAO <- read.table(paste(FAOCal, "SedAj_95PPU_Calib_FAO.txt", sep=""), header=TRUE, 
                       stringsAsFactors=FALSE, na.strings="NA")
SedAjFAO$date <- as.Date(SedAjFAO$date, format="%Y-%m-%d")
# Replace NAs with 0
SedAjFAO[is.na(SedAjFAO)] <- 0


# WLRC data

DischAjWLRC <- read.table(paste(WLRCCal, "DischAJ_95ppu_Valid_WLRC.txt", sep=""), header=TRUE, 
                          stringsAsFactors=FALSE, na.strings="NA")
DischAjWLRC$date <- as.Date(DischAjWLRC$date, format="%Y-%m-%d")
 

SedAjWLRC <- read.table(paste(WLRCCal, "SedAj_95PPU_Calib_WLRC.txt", sep=""), header=TRUE, 
                       stringsAsFactors=FALSE, na.strings="NA")
SedAjWLRC$date <- as.Date(SedAjWLRC$date, format="%Y-%m-%d")
# Replace NAs with 0
SedAjWLRC[is.na(SedAjWLRC)] <- 0




#***********************************************************
#***********************************************************
#***********************************************************
#***********************************************************
# VALIDATION DATA

DischAjFAO <- read.table(paste(FAOVal, "DischAJ_95ppu_Valid_FAO.txt", sep=""), header=TRUE, 
                         stringsAsFactors=FALSE, na.strings="NA")
DischAjFAO$date <- as.Date(DischAjFAO$date, format="%Y-%m-%d")
DischGeFAO <- read.table(paste(FAOVal, "DischGE_95ppu_Valid_FAO.txt", sep=""), header=TRUE, sep="",
                       stringsAsFactors=TRUE, na.strings="NA")
DischGeFAO$date <- as.Date(DischGeFAO$date, format="%Y-%m-%d")

# Sediment
SedAjFAO <- read.table(paste(FAOVal, "SedAj_95PPU_Valid_FAO.txt", sep=""), header=TRUE, 
                       stringsAsFactors=FALSE, na.strings="NA")
SedAjFAO$date <- as.Date(SedAjFAO$date, format="%Y-%m-%d")
SedGeFAO <- read.table(paste(FAOVal, "SedGe_95PPU_Valid_FAO.txt", sep=""), header=TRUE, 
                       stringsAsFactors=FALSE, na.strings="NA")
SedGeFAO$date <- as.Date(SedGeFAO$date, format="%Y-%m-%d")

#**************************************************
# VALIDATION DATA FROM WLRC----
DischAjWLRC <- read.table(paste(WLRCVal, "DischAJ_95ppu_Valid_WLRC.txt", sep=""), header=TRUE, 
                         stringsAsFactors=FALSE, na.strings="NA")
DischAjWLRC$date <- as.Date(DischAjWLRC$date, format="%Y-%m-%d")
DischGeWLRC <- read.table(paste(WLRCVal, "DischGE_95ppu_Valid_WLRC.txt", sep=""), header=TRUE, sep="",
                         stringsAsFactors=TRUE, na.strings="NA")
DischGeWLRC$date <- as.Date(DischGeWLRC$date, format="%Y-%m-%d")

# Sediment
SedAjWLRC <- read.table(paste(WLRCVal, "SedAj_95PPU_Valid_WLRC.txt", sep=""), header=TRUE, 
                       stringsAsFactors=FALSE, na.strings="NA")
SedAjWLRC$date <- as.Date(SedAjWLRC$date, format="%Y-%m-%d")
SedGeWLRC <- read.table(paste(WLRCVal, "SedGe_95PPU_Valid_WLRC.txt", sep=""), header=TRUE, 
                       stringsAsFactors=FALSE, na.strings="NA")
SedGeWLRC$date <- as.Date(SedGeWLRC$date, format="%Y-%m-%d")

SedAjWLRC[is.na(SedAjWLRC)] <- 0

SedGeWLRC[is.na(SedGeWLRC)] <- 0


# Define a date sequence
# x.Date <- as.Date(paste(rep(1986:2014, each = 12), rep(1:12, 2), 1, sep = "-"))
# 
# gerda$date <- x.Date
#origin <- DischGeFAO
# origin <- DischAjFAO
# origin <- SedAjFAO
# origin <- SedGeFAO

#*******************************************
# Iteration
setwd(FAOVal)

names <- c("DischAjFAO", "DischGeFAO", "SedAjFAO", "SedGeFAO")
names <- c("DischAjWLRC", "DischGeWLRC", "SedAjWLRC", "SedGeWLRC")

i <- "SedGeWLRC"

for (i in names){
   x <- get(i)
   date <- x$date
   obs <- x$observed
   upper <- x$U95PPU
   lower <- x$L95PPU
   sim <- x$Best_Sim
   x$year <- format(as.Date(x$date), "%Y")
   x$m <- format(as.Date(x$date), "%b")
   
   # add zoo data
   obs <- zoo(obs, date)
   upper <- zoo(upper, date)
   lower <- zoo(lower, date)
   sim <- zoo(sim, date)
      
   years <- as.numeric(unique(x$year))
   
   # Export plot to pdf or eps
   pdf(file=paste(WLRCVal, i, ".pdf", sep=""), width=24, height=14,
       family="Helvetica")
   
   # Plot data
   d <- ggplot(x, aes(date))+
      geom_ribbon(aes(ymin=L95PPU, ymax=U95PPU), colour="#A1CE79", fill="#A1CE79", alpha=.2)+
      geom_line(aes(y=observed, group=year), colour="#61739A", size=.87) + 
      geom_line(aes(y=Best_Sim, group=year), colour="#A6373F", size=.87) + 
      #    theme_bw() +
      labs(x="", y="Discharge [m3/s]")+
      ylim(0, max(upper))+
      ggtitle(i)+
      theme(plot.title=element_text(size=rel(2), face="bold", vjust=2), 
            axis.text.x=element_text(angle=-90))+
      scale_x_date(labels=date_format("%b"), breaks=date_breaks("month"))
   #    geom_vline(xintercept=as.numeric(date[c(5,9,17,)]),linetype=5, colour="black") 
   
   
   d + facet_wrap(~year, scales="free")  
   
   dev.off()
      
}




















