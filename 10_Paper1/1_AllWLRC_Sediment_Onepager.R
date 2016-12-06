#*********************************************
# One pager for Sediment modelling CFSR
#********************************************


setwd("~/R/Data/SWAT-Cup/1_Paper1/AnditTid")

# Andit Tid ----

AT <- read.csv("at_sedi_calib-valid_wlrc.txt", sep="\t")
AT$date <- as.Date(AT$date, format="%m/%d/%Y")

# CFSR data
AT_cfsr <- read.csv("at_sedi_calib-valid_cfsr.txt", sep="\t")
AT_cfsr$date <- as.Date(AT_cfsr$date, format="%m/%d/%Y")

upperlim_c <- AT_cfsr$U95PPU
lowerlim_c <- AT_cfsr$L95PPU
obs_cfsr <- AT_cfsr$observed
sim_cfsr <- AT_cfsr$Best_Sim


obs <- AT$observed
sim <- AT$Best_Sim
upperlim <- AT$U95PPU
lowerlim <- AT$L95PPU

date <- AT$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,length(unique(year))*12,12)

nonNA <- which(!is.na(sim))
par(mfrow=c(2,1))
plot(date, upperlim, type="n", ylim=c(0,12000), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#F0F8FF", border=NA)    #  "#E0EEEE"
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
lines(date, obs, col="#104E8B", lwd=2)
lines(date, sim, col="#8B1A1A", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,12000, 1000), las=2)
box()
title(main="Andit Tid--WLRC\n(1984-1997)", 
      ylab="Sediment loss [t]")

#*********************
# Plot CFSR data
date <- AT_cfsr$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,length(unique(year))*12,12)

plot(date, upperlim_c, type="n", ylim=c(0,10000), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim_c[nonNA], rev(upperlim_c[nonNA])),
        col="#F0F8FF", border=NA)    #  "#E0EEEE"
lines(date, upperlim_c, col="#838B8B")
lines(date, lowerlim_c, col="#838B8B")
lines(date, obs_cfsr, col="#104E8B", lwd=2)
lines(date, sim_cfsr, col="#8B1A1A", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,10000, 1000), las=2)
box()
title(main="Andit Tid--CFSR\n(1984-1997)", 
      ylab="Sediment loss [t]")



#**************************************************************************************************
#**************************************************************************************************
setwd("~/R/Data/SWAT-Cup/1_Paper1/Anjeni")

# Anjeni WLRC data----
AJ <- read.csv("aj_sedi_calib-valid_wlrc.txt", sep="\t", header=TRUE)
AJ$date <- as.Date(AJ$date, format="%m/%d/%Y")

# CFSR Data
# Anjeni CFSR data----
AJ_cfsr <- read.csv("aj_sedi_calib-valid_cfsr.txt", sep="\t", na.string="NA")
AJ_cfsr$date <- as.Date(AJ_cfsr$date, format="%m/%d/%Y")
upperlim_c <- AJ_cfsr$U95PPU
lowerlim_c <- AJ_cfsr$L95PPU
obs_cfsr <- AJ_cfsr$observed
sim_cfsr <- AJ_cfsr$Best_Sim


# Define simulated and observed variables----
obs_w_c <- AJ$observed
sim_w_c <- AJ$Best_Sim
sim_c_c <- AJ_cfsr$Best_Sim

# Prepare data for plotting----
upperlim <- AJ$U95PPU
lowerlim <- AJ$L95PPU
date <- AJ$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,length(unique(year))*12,12)


#********************************************
# Plot WLRC vs observed data--------
# par(mfrow=c(3,1))
nonNA <- which(!is.na(sim_w_c))
# cairo_pdf(filename="~/R/Data/SWAT-Cup/1_Paper1/AJ_WLRC_Calib.pdf", pointsize = 12, family="arial", width=17, height=12)
plot(date, upperlim, type="n", ylim=c(0,4500), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date, rev(date)), c(lowerlim, rev(upperlim)),
        col="#F0F8FF", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_w_c, col="#104E8B", lwd=2)
lines(date, sim_w_c, col="#8B1A1A", lwd=2)
# Add Validation-Calibration line
# abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,4500,1000), las=2)
box()
title(main="Anjeni - WLRC\n(1984-2014)", 
      ylab="Sediment [t]")


#***************************
# Plot CFSR sediment-----

plot(date, upperlim_c, type="n", ylim=c(0,4500), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date, rev(date)), c(lowerlim_c, rev(upperlim_c)),
        col="#F0F8FF", border=NA)
lines(date, upperlim_c, col="#838B8B")
lines(date, lowerlim_c, col="#838B8B")

lines(date, obs_cfsr, col="#104E8B", lwd=2)
lines(date, sim_cfsr, col="#8B1A1A", lwd=2)
# Add Validation-Calibration line
# abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,4500,1000), las=2)
box()
title(main="Anjeni - CFSR\n(1984-2014)", 
      ylab="Sediment [t]")



#**************************************************************************************************
#**************************************************************************************************
setwd("~/R/Data/SWAT-Cup/1_Paper1/Maybar")


# Maybar WLRC data
MA <- read.csv("ma_sedi_calib-valid_wlrc.txt", sep="\t", na.strings="NA")
MA$date <- as.Date(MA$date, format="%m/%d/%Y")

# CFSR Data
# Maybar CFSR data----
MA_cfsr <- read.csv("ma_sedi_calib-valid_cfsr.txt", sep="\t", na.string="NA")
MA_cfsr$date <- as.Date(MA_cfsr$date, format="%m/%d/%Y")
upperlim_c <- MA_cfsr$U95PPU
lowerlim_c <- MA_cfsr$L95PPU
obs_cfsr <- MA_cfsr$observed
sim_cfsr <- MA_cfsr$Best_Sim


obs_c <- MA$observed
sim_w_c <- MA$Best_Sim

#****************
upperlim <- MA$U95PPU
lowerlim <- MA$L95PPU
date <- MA$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,length(unique(year))*12,12)

nonNA <- which(!is.na(sim_w_c))

plot(date, upperlim, type="n", ylim=c(0,1500), axes=FALSE, xlab="", ylab="")
polygon(c(date, rev(date)), c(lowerlim, rev(upperlim)),
        col="#F0F8FF", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_c, col="#104E8B", lwd=2)
lines(date, sim_w_c, col="#8B1A1A", lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,1500,250), las=2)
box()
title(main="Maybar - WLRC\n (1983-2014)", 
      ylab="Sediment [t]")


#***************************
# Plot CFSR sediment-----

plot(date, upperlim_c, type="n", ylim=c(0,15000), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date, rev(date)), c(lowerlim_c, rev(upperlim_c)),
        col="#F0F8FF", border=NA)
lines(date, upperlim_c, col="#838B8B")
lines(date, lowerlim_c, col="#838B8B")

lines(date, obs_cfsr, col="#104E8B", lwd=2)
lines(date, sim_cfsr, col="#8B1A1A", lwd=2)
# Add Validation-Calibration line
# abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,15000,1000), las=2)
box()
title(main="Maybar - CFSR\n(1984-2014)", 
      ylab="Sediment [t]")

