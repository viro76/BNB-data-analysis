#***************************
# All WLRC discharge data on one graph
#*************************************

setwd("~/R/Data/SWAT-Cup/1_Paper1/AnditTid")

# nbrD <- read.csv("NmbDays.csv", sep=",")

# Andit Tid - 

AT <- read.csv("at_disch_calib-valid_wlrc.txt", sep="\t")
AT$date <- as.Date(AT$date, format="%m/%d/%Y")

# CFSR Data
# Andit Tid CFSR data----
AT_cfsr <- read.csv("at_disch_calib-valid_cfsr.txt", sep="\t", na.string="NA")
AT_cfsr$date <- as.Date(AT_cfsr$date, format="%m/%d/%Y")
upperlim_c <- AT_cfsr$U95PPU
lowerlim_c <- AT_cfsr$L95PPU
sim_cfsr <- AT_cfsr$Best_Sim


obs_c <- AT$observed
sim_w_c <- AT$Best_Sim
upperlim <- AT$U95PPU
lowerlim <- AT$L95PPU

date <- AT$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,length(unique(year))*12,12)

nonNA <- which(!is.na(sim_w_c))
# Plot 3 of the same
# par(mfrow=c(3,1))
# Plot 3x2 CFSR and WLRC
par(mfrow=c(2,1))
# Plot 1x6
# par(mfrow=c(6,1))
# cairo_pdf("~/R/Data/SWAT-Cup/1_Paper1/AllonOne.pdf", width=17, height=12,
#           onefile = TRUE, family = "helvetica")

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
title(main="Andit Tid - WLRC\n(1984-1997)", 
      ylab="Discharge [m3/s]")


# Plot CFSR data
plot(date, upperlim_c, type="n", ylim=c(0,1.3), axes=FALSE, xlab="", ylab="")

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim_c[nonNA], rev(upperlim_c[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim_c, col="#838B8B")
lines(date, lowerlim_c, col="#838B8B")
lines(date, obs_c, col="darkblue", lwd=2)
lines(date, sim_cfsr, col="#BF3EFF", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,1.2,0.2), las=2)
box()
title(main="Andit Tid - CFSR\n(1984-1997)", 
      ylab="Discharge [m3/s]")



#**************************************************************************************************
#**************************************************************************************************
setwd("~/R/Data/SWAT-Cup/1_Paper1/Anjeni")

# Anjeni WLRC data----
AJ <- read.csv("aj_disch_calib-valid_wlrc.txt", sep="\t", header=TRUE)
AJ$date <- as.Date(AJ$date, format="%m/%d/%Y")

# CFSR Data
# Anjeni CFSR data----
AJ_cfsr <- read.csv("aj_disch_calib_valid_cfsr.txt", sep="\t", na.string="NA")
AJ_cfsr$date <- as.Date(AJ_cfsr$date, format="%m/%d/%Y")
upperlim_c <- AJ_cfsr$U95PPU
lowerlim_c <- AJ_cfsr$L95PPU
sim_cfsr <- AJ_cfsr$Best_Sim
obs_cfsr <- AJ_cfsr$observed


# Define simulated and observed variables----
obs_w_c <- AJ$observed
sim_w_c <- AJ$Best_Sim

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
plot(date, upperlim, type="n", ylim=c(0,0.34), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date, rev(date)), c(lowerlim, rev(upperlim)),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_w_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
# Add Validation-Calibration line
# abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,0.34,0.1), las=2)
box()
title(main="Anjeni - WLRC\n(1984-2014)", 
      ylab="Discharge [m3/s]")

#*****************
# Plot CFSR data
nonNA <- which(!is.na(sim_cfsr))
date <- AJ_cfsr$date
year <- format(as.Date(date), "%Y")
points <- seq(1,length(unique(year))*12,12)
plot(date, upperlim_c, type="n", ylim=c(0,0.34), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date, rev(date)), c(lowerlim_c, rev(upperlim_c)),
        col="#E0EEEE", border=NA)
lines(date, upperlim_c, col="#838B8B")
lines(date, lowerlim_c, col="#838B8B")

lines(date, obs_cfsr, col="darkblue", lwd=2)
lines(date, sim_cfsr, col="#BF3EFF", lwd=2)
# Add Validation-Calibration line
# abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,0.34,0.1), las=2)
box()
title(main="Anjeni - CFSR\n(1984-2014)", 
      ylab="Discharge [m3/s]")

#**************************************************************************************************
#**************************************************************************************************
setwd("~/R/Data/SWAT-Cup/1_Paper1/Maybar")


# Maybar WLRC data
MA <- read.csv("ma_disch_calib-valid_wlrc.txt", sep="\t", na.strings="NA")
MA$date <- as.Date(MA$date, format="%m/%d/%Y")

obs_c <- MA$observed #[!is.na(MA$observed)]
sim_w_c <- MA$Best_Sim #[!is.na(MA$Best_Sim)]

#***************
# Maybar CFSR
MA_cfsr <- read.csv("ma_disch_calib-valid_cfsr.txt", sep="\t", na.string="NA")
MA_cfsr$date <- as.Date(MA_cfsr$date, format="%m/%d/%Y")
upperlim_c <- MA_cfsr$U95PPU
lowerlim_c <- MA_cfsr$L95PPU
sim_cfsr <- MA_cfsr$Best_Sim
obs_cfsr <- MA_cfsr$observed



#****************
upperlim <- MA$U95PPU   # [!is.na(MA$U95PPU)]
lowerlim <- MA$L95PPU
date <- MA$date                # MA[complete.cases(MA[,2]),]
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,length(unique(year))*12,12)

nonNA <- which(!is.na(sim_w_c))

plot(date, upperlim, type="n", ylim=c(0,0.15), axes=FALSE, xlab="", ylab="")
polygon(c(date, rev(date)), c(lowerlim, rev(upperlim)),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,0.15,0.05), las=2)
box()
title(main="Maybar - WLRC \n(1983-2014)", 
      ylab="Discharge [m3/s]")


#***********************************
# Plot CFSR data
date <- MA_cfsr$date
year <- format(as.Date(date), "%Y")
points <- seq(1,length(unique(year))*12,12)

plot(date, upperlim_c, type="n", ylim=c(0,0.15), axes=FALSE, xlab="", ylab="")
polygon(c(date, rev(date)), c(lowerlim_c, rev(upperlim_c)),
        col="#E0EEEE", border=NA)
lines(date, upperlim_c, col="#838B8B")
lines(date, lowerlim_c, col="#838B8B")

lines(date, obs_cfsr, col="darkblue", lwd=2)
lines(date, sim_cfsr, col="#BF3EFF", lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,0.15,0.05), las=2)
box()
title(main="Maybar - CFSR \n(1983-2014)", 
      ylab="Discharge [m3/s]")


#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
# Sediment - Onepager----
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************


setwd("~/R/Data/SWAT-Cup/1_Paper1/AnditTid")



# Andit Tid ----

AT <- read.csv("at_sedi_calib_wlrc.txt", sep="\t")
AT$date <- as.Date(AT$date, format="%m/%d/%Y")

obs <- AT$observed
sim <- AT$Best_Sim
upperlim <- AT$U95PPU
lowerlim <- AT$L95PPU

date <- AT$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,length(unique(year))*12,12)

nonNA <- which(!is.na(sim))
par(mfrow=c(3,1))
plot(date, upperlim, type="n", ylim=c(0,14000), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#F0F8FF", border=NA)    #  "#E0EEEE"
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
lines(date, obs, col="#104E8B", lwd=2)
lines(date, sim, col="#8B1A1A", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,14000, 1000), las=2)
box()
title(main="Andit Tid--WLRC\n(1984-1997)", 
      ylab="Sediment loss [t]")


#**************************************************************************************************
#**************************************************************************************************
setwd("~/R/Data/SWAT-Cup/1_Paper1/Anjeni")

# Anjeni WLRC data----
AJ <- read.csv("aj_sedi_calib_wlrc.txt", sep="\t", header=TRUE)
AJ$date <- as.Date(AJ$date, format="%m/%d/%Y")

# CFSR Data
# Anjeni CFSR data----
AJ_cfsr <- read.csv("aj_sedi_calib_cfsr.txt", sep="\t", na.string="NA")
AJ_cfsr$date <- as.Date(AJ_cfsr$date, format="%m/%d/%Y")
upperlim_c <- AJ_cfsr$U95PPU
lowerlim_c <- AJ_cfsr$L95PPU


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
plot(date[nonNA], upperlim[nonNA], type="n", ylim=c(0,5000), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#F0F8FF", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_w_c, col="#104E8B", lwd=2)
lines(date, sim_w_c, col="#8B1A1A", lwd=2)
# Add Validation-Calibration line
# abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,5000,1000), las=2)
box()
title(main="Anjeni \n(1984-2014)", 
      ylab="Sediment [t]")


#**************************************************************************************************
#**************************************************************************************************
setwd("~/R/Data/SWAT-Cup/1_Paper1/Maybar")


# Maybar WLRC data
MA <- read.csv("ma_sedi_calib_wlrc.txt", sep="\t", na.strings="NA")
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
points <- seq(1,96,12)

nonNA <- which(!is.na(sim_w_c))

plot(date, upperlim, type="n", ylim=c(0,2500), axes=FALSE, xlab="", ylab="")
polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#F0F8FF", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_c, col="#104E8B", lwd=2)
lines(date, sim_w_c, col="#8B1A1A", lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,2500,500), las=2)
box()
title(main="Maybar\n (1983-2014)", 
      ylab="Sediment [t]")









