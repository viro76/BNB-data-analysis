#***************************
# All CFSR discharge data on one graph
#*************************************

# CFSR data
setwd("~/R/Data/SWAT-Cup/1_Paper1/AnditTid/")
AT <- read.csv("at_disch_calib-valid_cfsr.txt", sep="\t", header=TRUE, na.strings="NA")
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
title(main="Andit Tid --CFSR \n(1984-1997)", 
      ylab="Discharge [m3/s]")


# Anjeni Data-----

setwd("~/R/Data/SWAT-Cup/1_Paper1/Anjeni/")
# Plot CFSR vs observed data--------
AJ <- read.csv("1_aj_disch_Calib-Valid_cfsr.txt", sep="\t", header=TRUE, na.strings="NA")
AJ$date <- as.Date(AJ$date, format="%m/%d/%Y")

obs_c <- AJ$observed
sim_w_c <- AJ$Best_Sim
upperlim <- AJ$U95PPU
lowerlim <- AJ$L95PPU

date <- AJ$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,348,12)

nonNA <- which(!is.na(sim_w_c))
# par(mfrow=c(3,1))
plot(date, upperlim, type="n", ylim=c(0,0.35), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
lines(date, obs_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,0.35,0.05), las=2)
box()
title(main="Anjeni \n(1986-1998)", 
      ylab="Discharge [m3/s]")


# Maybar data------

setwd("~/R/Data/SWAT-Cup/1_Paper1/Maybar")

MA <- read.csv("ma_disch_calib_cfsr.txt", sep="\t", na.strings="NA")
MA$date <- as.Date(MA$date, format="%m/%d/%Y")

obs_c <- MA$observed[!is.na(MA$observed)]
sim_w_c <- MA$Best_Sim[!is.na(MA$Best_Sim)]

#****************
upperlim <- MA$U95PPU[!is.na(MA$U95PPU)]
lowerlim <- MA$L95PPU[!is.na(MA$L95PPU)]
date <- MA[complete.cases(MA[,2]),]
date <- date$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,180,12)

nonNA <- which(!is.na(sim_w_c))

plot(date, upperlim, type="n", ylim=c(0,0.15), axes=FALSE, xlab="", ylab="")
polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,0.15,0.05), las=2)
box()
title(main="Maybar \n(1983-2014)", 
      ylab="Discharge [m3/s]")

par(mfrow=c(1,1))

















