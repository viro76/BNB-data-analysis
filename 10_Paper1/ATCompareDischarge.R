#**********************************************
#
# Compare WLRC and CFSR discharge data
# Andit Tid only
#
#**********************************************

setwd("~/R/Data/SWAT-Cup/1_Paper1/AnditTid")

# nbrD <- read.csv("NmbDays.csv", sep=",")

# Andit Tid - 

AT <- read.csv("at_disch_Calib-Valid_wlrc.txt", sep="\t")
AT$date <- as.Date(AT$date, format="%m/%d/%Y")

obs_c <- AT$observed
sim_w_c <- AT$Best_Sim
upperlim <- AT$U95PPU
lowerlim <- AT$L95PPU

date <- AT$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,168,12)

nonNA <- which(!is.na(sim_w_c))
par(mfrow=c(3,1))
plot(date, upperlim, type="n", ylim=c(0,1.3), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
          col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
lines(date, obs_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,1.2,0.2), las=2)
box()
title(main="Andit Tid \n(1984-1997)", 
      ylab="Discharge [m3/s]")

#***********************************************
# CFSR data
setwd("~/R/Data/SWAT-Cup/1_Paper1/AnditTid")
AT <- read.csv("at_disch_calib_cfsr.txt", sep="\t", header=TRUE, na.strings="NA")
AT$date <- as.Date(AT$date, format="%m/%d/%Y")

obs_c <- AT$observed
sim_w_c <- AT$Best_Sim
upperlim <- AT$U95PPU
lowerlim <- AT$L95PPU

date <- AT$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,108,12)

nonNA <- which(!is.na(sim_w_c))
par(mfrow=c(3,1))
plot(date, upperlim, type="n", ylim=c(0,1.3), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
lines(date, obs_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,1.2,0.2), las=2)
box()
title(main="Andit Tid \n(1984-1997)", 
      ylab="Discharge [m3/s]")






#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#                                                     Sediment
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************

AT <- read.csv("at_sedi_calib_wlrc.txt", sep="\t", na.strings="NA")
AT$date <- as.Date(AT$date, format="%m/%d/%Y")

obs_c <- AT$observed
sim_w_c <- AT$Best_Sim
upperlim <- AT$U95PPU
lowerlim <- AT$L95PPU

date <- AT$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,108,12)

nonNA <- which(!is.na(sim_w_c))
# par(mfrow=c(3,1))
plot(date, upperlim, type="n", ylim=c(0,13200), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
lines(date, obs_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,13200,1000), las=2)
box()
title(main="Andit Tid \n(1984-1997)", 
      ylab="Discharge [m3/s]")














