#***************************************************
# Prepare graphics for calibration in SWAT-Cup
#
#
#**************************************************



# Define if you want the FAO or the WLRC data
FAO <- "~/R/Data/SWAT-Cup/FAO/"
WLRC <- "~/R/Data/SWAT-Cup/WLRC/"



# Outflow at Yechereka from the WLRC calibration-----
yech <- read.table(paste(WLRC,"NwLU_95ppuFLWOut11.txt", sep=""), header=TRUE, sep="\t", 
                   stringsAsFactors=TRUE)

# Read different calibrations
files <- list.files("~/R/Data/SWAT-Cup/FAO/", pattern="YechFAO")
nbr <- seq(1, length(files))

for(i in files){
   name <- read.table(paste(FAO, i, sep=""), header=TRUE, sep="\t")
   name <- name$Best_Sim
   assign(i, name)
}


#**********************************************


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
lines(ydate, YechFAO_1.txt, col="#A97095", lwd=2, lty=3)
lines(ydate, YechFAO_2.txt, col="#A97095", lwd=2, lty=3)
lines(ydate, YechFAO_3.txt, col="#A97095", lwd=2, lty=3)
lines(ydate, YechFAO_4.txt, col="#A97095", lwd=2, lty=3)
lines(ydate, YechFAO_5.txt, col="#A97095", lwd=2, lty=3)
lines(ydate, YechFAO_6.txt, col="#A97095", lwd=2, lty=3)
lines(ydate, YechFAO_7.txt, col="#A97095", lwd=2, lty=3)
lines(ydate, YechFAO_8.txt, col="#A97095", lwd=2, lty=3)



axis(1, as.Date(ydate), label=format(ydate, "%b"), las=2)
axis(2, seq(0,7,1), las=2)
box()
title(main="Simulated and observed discharge\nYechereka (2013-2014)\n", 
      ylab="Discharge [m3/s]")
legend(x=15714.21, y=6.8548, legend=c("Simulated", "Observed", "95 PPU", "Simulated FAO"),
       y.intersp=.7, col=c("darkblue","brown", "#98CF6F50","#A97095"), 
       lty=c(1,1,1,3), bty="n", lwd=2)

