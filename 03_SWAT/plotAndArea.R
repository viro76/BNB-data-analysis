#**********************************************
# Plot all iterations data in R
# no usage of GGPLOT2
#*********************************************
# Date:  2015-03-21



#********************************************
# Plot polygons without ggplot2----

# Define if you want the FAO or the WLRC data
FAO <- "~/R/Data/SWAT-Cup/FAO/"
WLRC <- "~/R/Data/SWAT-Cup/WLRC/"



# Outflow at Yechereka from the WLRC calibration-----
yech <- read.table(paste(WLRC,"NwLU_95ppuFLWOut11.txt", sep=""), header=TRUE, sep="\t", 
                   stringsAsFactors=TRUE)
yechFAO <- read.table(paste(FAO," ", sep=""), header=TRUE, sep="\t", 
                      stringsAsFactors=TRUE)

yech$date <- as.Date(yech$date, "%m/%d/%Y")
ydate <- as.Date(yech$date)
yobs <- yech$observed
yupper <- yech$U95PPU
ylower <- yech$L95PPU
ysim <- yech$Best_Sim
yech$year <- format(as.Date(yech$date), "%Y")
yech$m <- format(as.Date(yech$date), "%b")
yFAO <- yechFAO$Best_Sim


nonNA <- which(!is.na(ysim))
# pdf(file="~/R/Data/SWAT-Cup/yechDisch_normPlot.pdf", width=20, height=17,
#     family="Helvetica")
plot(ydate, yupper, type="n", ylim=c(0,7), axes=FALSE, xlab="", ylab="")
abline(v=ydate, h=seq(0,7,1), col="gray", lty=3)
abline(v=ydate[13])
# Because of problems with the NAs we define the polygon only for datapoints with data
polygon(c(ydate[nonNA], rev(ydate[nonNA])), c(ylower[nonNA], rev(yupper[nonNA])), 
        col="#98CF6F50", border=NA)
lines(ydate, ysim, col="darkblue", lwd=2)
lines(ydate, yobs, col="brown", lwd=2)
lines(ydate, yupper, col="#53A976")
lines(ydate, ylower, col="#53A976")
lines(ydate, yFAO, col="#A97095", lwd=2, lty=3)

axis(1, as.Date(ydate), label=format(ydate, "%b"), las=2)
axis(2, seq(0,7,1), las=2)
box()
title(main="Simulated and observed discharge\nYechereka (2013-2014)\n", 
      ylab="Discharge [m3/s]")
legend(x=15714.21, y=6.8548, legend=c("Simulated", "Observed", "95 PPU", "Simulated FAO"),
       y.intersp=.7, col=c("darkblue","brown", "#98CF6F50","#A97095"), 
       lty=c(1,1,1,3), bty="n", lwd=2)
# dev.off()

#*****************************************************************************
# Outflow at Yechereka from the FAO calibration-----
yech <- read.table(paste(WLRC,"NwLU_95ppuFLWOut11.txt", sep=""), header=TRUE, sep="\t", 
                   stringsAsFactors=TRUE)

yech$date <- as.Date(yech$date, "%m/%d/%Y")
ydate <- as.Date(yech$date)
yobs <- yech$observed
yupper <- yech$U95PPU
ylower <- yech$L95PPU
ysim <- yech$Best_Sim
yech$year <- format(as.Date(yech$date), "%Y")
yech$m <- format(as.Date(yech$date), "%b")


nonNA <- which(!is.na(ysim))
# pdf(file="~/R/Data/SWAT-Cup/yechDisch_normPlot.pdf", width=20, height=17,
#     family="Helvetica")
plot(ydate, yupper, type="n", ylim=c(0,7), axes=FALSE, xlab="", ylab="")
abline(v=ydate, h=seq(0,7,1), col="gray", lty=3)
abline(v=ydate[13])
# Because of problems with the NAs we define the polygon only for datapoints with data
polygon(c(ydate[nonNA], rev(ydate[nonNA])), c(ylower[nonNA], rev(yupper[nonNA])), 
        col="#98CF6F50", border=NA)
lines(ydate, ysim, col="darkblue", lwd=2)
lines(ydate, yobs, col="brown", lwd=2)
lines(ydate, yupper, col="#53A976")
lines(ydate, ylower, col="#53A976")

axis(1, as.Date(ydate), label=format(ydate, "%b"), las=2)
axis(2, seq(0,7,1), las=2)
box()
title(main="Simulated and observed discharge\n FAO data\nYechereka (2013-2014)\n", 
      ylab="Discharge [m3/s]")
# dev.off()



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
