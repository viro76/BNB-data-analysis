#*********************************************
# One pager for Sediment modelling CFSR
#********************************************

setwd("~/R/Data/SWAT-Cup/1_Paper1/AnditTid")

AT <- read.csv("at_sedi_calib_cfsr.txt", sep="\t", na.strings="NA")
AT$date <- as.Date(AT$date, format="%m/%d/%Y")

obs <- AT$observed
sim <- AT$Best_Sim
upperlim <- AT$U95PPU
lowerlim <- AT$L95PPU

date <- AT$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,108,12)

nonNA <- which(!is.na(sim))
par(mfrow=c(3,1))
plot(date, upperlim, type="n", ylim=c(0,13200), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
lines(date, obs, col="darkblue", lwd=2)
lines(date, sim, col="#BF3EFF", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,13200,1000), las=2)
box()
title(main="Andit Tid--CFSR \n(1984-1997)", 
      ylab="Discharge [m3/s]")

#**************************************************************************
# Anjeni Sediment CFSR----
setwd("~/R/Data/SWAT-Cup/1_Paper1/Anjeni")

# CFSR Data
# Anjeni CFSR data----
AJ <- read.csv("aj_sedi_calib_cfsr.txt", sep="\t", na.string="NA")
AJ$date <- as.Date(AJ$date, format="%m/%d/%Y")
upperlim <- AJ$U95PPU
lowerlim <- AJ$L95PPU

#**************************************************************************************************

# Define simulated and observed variables----
obs <- AJ$observed
sim <- AJ$Best_Sim

# Prepare data for plotting----
upperlim <- AJ$U95PPU
lowerlim <- AJ$L95PPU
date <- AJ$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,156,12)


#********************************************
# Plot WLRC vs observed data--------
# par(mfrow=c(3,1))
nonNA <- which(!is.na(sim))
# cairo_pdf(filename="~/R/Data/SWAT-Cup/1_Paper1/AJ_WLRC_Calib.pdf", pointsize = 12, family="arial", width=17, height=12)
plot(date[nonNA], upperlim[nonNA], type="n", ylim=c(0,5000), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_w_c, col="darkblue", lwd=2)
lines(date, sim, col="#BF3EFF", lwd=2)

axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,5000,500), las=2)
box()
title(main="Anjeni--CFSR\n(1984-2014)", 
      ylab="Soil loss [t]")


#**********************************
# Maybar-----

setwd("~/R/Data/SWAT-Cup/1_Paper1/Maybar")

MA <- read.csv("ma_sedi_calib_cfsr.txt", sep="\t", na.strings="NA")
MA$date <- as.Date(MA$date, format="%m/%d/%Y")

obs <- MA$observed[!is.na(MA$observed)]
sim <- MA$Best_Sim[!is.na(MA$Best_Sim)]

#****************
upperlim <- MA$U95PPU[!is.na(MA$U95PPU)]
lowerlim <- MA$L95PPU[!is.na(MA$L95PPU)]
date <- MA[complete.cases(MA[,2]),]
date <- date$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,96,12)

nonNA <- which(!is.na(sim_w_c))

plot(date, upperlim, type="n", ylim=c(0,2400), axes=FALSE, xlab="", ylab="")
polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
# polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim_c[nonNA], rev(upperlim_c[nonNA])),
#         col="#E0EEEE", border=NA)
# lines(date, upperlim_c, col="#838B8B")
# lines(date, lowerlim_c, col="#838B8B")
lines(date, obs, col="darkblue", lwd=2)
lines(date, sim, col="#BF3EFF", lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,2400, 300), las=2)
box()
title(main="Maybar--CFSR \n(1983-2014)", 
      ylab="Discharge [m3/s]")












