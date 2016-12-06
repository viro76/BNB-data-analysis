#**********************************************
# Plot all iterations data in R
# no usage of GGPLOT2
#*********************************************
# Date:  2015-03-21



#********************************************
# Plot polygons without ggplot2----

# Define if you want the FAO or the WLRC data
FAO <- "Y:/vincent/Sardinia/SWAT-CupData/FAO"
WLRC <- "Y:/vincent/Sardinia/SWAT-CupData/WLRC"

calib <- "Calibration"
valid <- "Validation"





# Outflow at Anjeni from the WLRC calibration-----
ajCalibWLRC <- read.table(paste(WLRC, calib, "DischAJ_95ppu_Calib_WLRC.txt", sep="/"), header=TRUE, 
                   stringsAsFactors=TRUE)
# ajCalibWLRC$date <- as.Date(ajCalibWLRC$date, format="%Y-%m-%d")

ajCalibFAO <- read.table(paste(FAO, calib, "DischAJ_95ppu_Calib_FAO.txt", sep="/"), header=TRUE, 
                      stringsAsFactors=TRUE)
# ajCalibFAO$date <- as.Date(ajCalibFAO$date, "%Y-%m-%d")


ydate <- as.Date(ajCalibWLRC$date, format="%Y-%m-%d")
yobs <- ajCalibWLRC$observed
yupper <- ajCalibWLRC$U95PPU
ylower <- ajCalibWLRC$L95PPU
ysim <- ajCalibWLRC$Best_Sim
ajCalibWLRC$year <- format(as.Date(ajCalibWLRC$date), "%Y")
ajCalibWLRC$m <- format(as.Date(ajCalibWLRC$date), "%b")

yFAO <- ajCalibFAO$Best_Sim



nonNA <- which(!is.na(ysim))


# pdf(file=paste(WLRC, calib, "CalibrationWLRCAj.pdf", sep="/"), width=20, height=14,
#     family="Helvetica")
plot(ydate, yupper, type="n", ylim=c(0,0.6), axes=FALSE, xlab="", ylab="")
abline(v=ydate, h=seq(0,0.6,0.05), col="gray", lty=3)
abline(v=ydate[13])
# Because of problems with the NAs we define the polygon only for datapoints with data
polygon(c(ydate[nonNA], rev(ydate[nonNA])), c(ylower[nonNA], rev(yupper[nonNA])), 
        col="#98CF6F50", border=NA)
lines(ydate, ysim, col="brown", lwd=2)
lines(ydate, yobs, col="darkblue", lwd=2)
lines(ydate, yupper, col="#53A976")
lines(ydate, ylower, col="#53A976")
lines(ydate, yFAO, col="#A97095", lwd=2, lty=3)

axis(1, as.Date(ydate), label=format(ydate, "%b-%y"), las=2)
axis(2, seq(0,0.4,0.1), las=2)
box()
title(main="Simulated and observed discharge\nAnjeni (1986-2000)\n", 
      ylab="Discharge [m3/s]")

legend(x=9186.157, y=0.5894, legend=c("Simulated", "Observed", "95 PPU", "Simulated FAO"),
       y.intersp=.7, col=c("darkblue","brown", "#98CF6F50","#A97095"), 
       lty=c(1,1,1,3), bty="n", lwd=2)
# dev.off()

#*****************************************************************************
# Outflow at Anjeni from the FAO calibration-----
ajFAO <- read.table(paste(FAO, calib, "DischAJ_95ppu_Calib_FAO.txt", sep="/"), header=TRUE,  
                   stringsAsFactors=TRUE)

ajFAO$date <- as.Date(ajFAO$date, "%Y-%m-%d")
ydate <- as.Date(ajFAO$date)
yobs <- ajFAO$observed
yupper <- ajFAO$U95PPU
ylower <- ajFAO$L95PPU
ysim <- ajFAO$Best_Sim
ajFAO$year <- format(as.Date(ajFAO$date), "%Y")
ajFAO$m <- format(as.Date(ajFAO$date), "%b")


nonNA <- which(!is.na(ysim))
pdf(file=paste(FAO, calib, "CalibrationDischAJFAO_3.pdf", sep="/"), width=24, height=14,
    family="Helvetica")
plot(ydate, yupper, type="n", ylim=c(0,0.2), axes=FALSE, xlab="", ylab="")
abline(h=seq(0,0.2,0.05), col="gray", lty=3)
# abline(v=ydate[13])
# Because of problems with the NAs we define the polygon only for datapoints with data
polygon(c(ydate[nonNA], rev(ydate[nonNA])), c(ylower[nonNA], rev(yupper[nonNA])), 
        col="#98CF6F50", border=NA)
lines(ydate, ysim, col="brown", lwd=2)
lines(ydate, yobs, col="darkblue", lwd=2)
lines(ydate, yupper, col="#53A976")
lines(ydate, ylower, col="#53A976")

axis(1, as.Date(ydate), label=format(ydate, "%b-%y"), las=2)
axis(2, seq(0,0.2,0.05), las=2, cex.axis=0.8)
box()
title(main="Simulated and observed discharge\n FAO data\nAnjeni (1986-2000)\n", 
      ylab="Discharge [m3/s]")
dev.off()


#**************************************************************************************************
#**************************************************************************************************
#**************************************************************************************************
# SEDIMENT

# FAO data CALIBRATION
ajFAO <- read.table(paste(FAO, calib, "SedAJ_95ppu_Calib_FAO.txt", sep="/"), header=TRUE,  
                    stringsAsFactors=TRUE)

ajFAO$date <- as.Date(ajFAO$date, "%Y-%m-%d")
ydate <- as.Date(ajFAO$date)
yobs <- ajFAO$observed
yupper <- ajFAO$U95PPU
ylower <- ajFAO$L95PPU
ysim <- ajFAO$Best_Sim
ajFAO$year <- format(as.Date(ajFAO$date), "%Y")
ajFAO$m <- format(as.Date(ajFAO$date), "%b")


nonNA <- which(!is.na(ysim))
pdf(file=paste(FAO, calib, "CalibrationDischAJFAO_SED.pdf", sep="/"), width=24, height=14,
    family="Helvetica")
plot(ydate, yupper, type="n", ylim=c(0,2000), axes=FALSE, xlab="", ylab="")
abline(h=seq(0,2000,500), col="gray", lty=3)
# abline(v=ydate[13])
# Because of problems with the NAs we define the polygon only for datapoints with data
polygon(c(ydate[nonNA], rev(ydate[nonNA])), c(ylower[nonNA], rev(yupper[nonNA])), 
        col="#98CF6F50", border=NA)
lines(ydate, ysim, col="brown", lwd=2)
lines(ydate, yobs, col="darkblue", lwd=2)
lines(ydate, yupper, col="#53A976")
lines(ydate, ylower, col="#53A976")

axis(1, as.Date(ydate), label=format(ydate, "%b-%y"), las=2)
axis(2, seq(0,2000,250), las=2, cex.axis=0.8)
box()
title(main="Simulated and observed sediment loss\n FAO data\nAnjeni (1986-2000)\n", 
      ylab="Sediment loss [t]")
dev.off()


#*******************************************************************************************
#*******************************************************************************************
# WLRC Sediment data CALIBRATION----
ajWLRC <- read.table(paste(WLRC, calib, "SedAJ_95ppu_Calib_WLRC.txt", sep="/"), header=TRUE,  
                    stringsAsFactors=TRUE)

ajWLRC $date <- as.Date(ajWLRC$date, "%Y-%m-%d")
ydate <- as.Date(ajWLRC$date)
yobs <- ajWLRC$observed
yupper <- ajWLRC$U95PPU
ylower <- ajWLRC$L95PPU
ysim <- ajWLRC$Best_Sim
ajWLRC$year <- format(as.Date(ajWLRC$date), "%Y")
ajWLRC$m <- format(as.Date(ajWLRC$date), "%b")


nonNA <- which(!is.na(ysim))
pdf(file=paste(WLRC, calib, "CalibrationAJFAO_SED.pdf", sep="/"), width=24, height=14,
    family="Helvetica")
plot(ydate, yupper, type="n", ylim=c(0,2000), axes=FALSE, xlab="", ylab="")
abline(h=seq(0,2000,500), col="gray", lty=3)
# abline(v=ydate[13])
# Because of problems with the NAs we define the polygon only for datapoints with data
polygon(c(ydate[nonNA], rev(ydate[nonNA])), c(ylower[nonNA], rev(yupper[nonNA])), 
        col="#98CF6F50", border=NA)
lines(ydate, ysim, col="brown", lwd=2)
lines(ydate, yobs, col="darkblue", lwd=2)
lines(ydate, yupper, col="#53A976")
lines(ydate, ylower, col="#53A976")

axis(1, as.Date(ydate), label=format(ydate, "%b-%y"), las=2)
axis(2, seq(0,2000,250), las=2, cex.axis=0.8)
box()
title(main="Simulated and observed sediment loss\n WLRC data\nMinchet catchment (1986-2000)\n", 
      ylab="Sediment loss [t]")
dev.off()




















#****************************************************************************
# One plot with all iterations form the FAO calibration

path <- "E:/Calibration/Gerda/2_FAO_WLRC_31yrs_2yrsk_mon.Sufi2.SwatCup/Iterations"
target <- "Sufi2.Out"
names <- c("NoAd", "Iter1", "Iter2", "Iter3", "Iter4", "Iter5", "Iter6", "Iter7", "Iter8")
date <- seq(as.Date("2013-06-01"), as.Date("2014-11-01"), by = "months")
fulldate <- data.frame(date=seq(as.Date("2013-01-01"), as.Date("2014-12-01"), by="months"),data=NA)
setwd(path)

# read the WLRC simulation
simWLRC <- read.table("E:/Calibration/Gerda/1_WLRC_NwLU_31yrs_mon.Sufi2.SwatCup/Iterations/Iter2_500Sim_SoilParameters/Sufi2.Out/95ppu.txt", skip=202, fill=TRUE, header=TRUE, stringsAsFactors=FALSE)
simWLRC <- simWLRC[c(1:18),c(4)]
simWLRC <- cbind(date, as.numeric(simWLRC))
simWLRC <- merge(fulldate, simWLRC, all=TRUE)
simW <- simWLRC$V2


filelist <- list.files()
# pdf(file="~/R/Data/SWAT-Cup/yechDisch_AllIterations.pdf", width=20, height=17,
#     family="Helvetica")
par(mfrow=c(3,3), mar=c(5.1, 4.1, 2,1))
for(j in 1:9){
   togo <- paste(path, filelist[j], target, "95ppu.txt", sep="/")
   file <- read.table(togo, skip=202, fill=TRUE, header=TRUE, stringsAsFactors = FALSE)
   file <- file[c(1:18),c(1:4)]
   file <- cbind(date, file)
   
   assign(names[j], file)
   # Merge file with a timeseries containing NA but all dates from 2013 to 2014
   file <- merge(fulldate, file, all=TRUE)
   file <- file[,c(1,3:6)]
   ydate <- file$date
   yobs <- as.numeric(file$observed)
   ysim <- as.numeric(file$Best_Sim)
   yupper <- as.numeric(file$U95PPU)
   ylower <- as.numeric(file$L95PPU)
   
   # Plot data
   
   nonNA <- which(!is.na(ysim))
   plot(ydate, yupper, type="n", ylim=c(0,6), axes=FALSE, xlab="", ylab="")
   abline(v=ydate, h=seq(0,6,1), col="gray", lty=3)
   abline(v=ydate[13])
   # Because of problems with the NAs we define the polygon only for datapoints with data
   polygon(c(ydate[nonNA], rev(ydate[nonNA])), c(ylower[nonNA], rev(yupper[nonNA])), 
           col="#98CF6F50", border=NA)
   lines(ydate, ysim, col="brown", lwd=2)
   lines(ydate, yobs, col="darkblue", lwd=2)
   lines(ydate, yupper, col="#53A976")
   lines(ydate, ylower, col="#53A976")
   lines(ydate, simW, col="#A97095", lwd=2, lty=3)
   
   axis(1, as.Date(ydate), label=format(ydate, "%b"), las=2)
   axis(2, seq(0,6,1), las=2)
   box()
   title(main=paste( names[j]), line=.7)
   title(ylab="Discharge [m3/s]", line=2)
   rm(file)
}
# dev.off()
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

#******************************************************************************
