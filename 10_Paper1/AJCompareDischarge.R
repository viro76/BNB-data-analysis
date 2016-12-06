#**********************************************
#
# Compare WLRC and CFSR discharge data
# Andit Tid only
#
#**********************************************

setwd("~/R/Data/SWAT-Cup/1_Paper1/Anjeni")

# Anjeni WLRC data----
AJ <- read.csv("aj_disch_wlrc.csv", sep=",", header=TRUE)
AJ$date <- as.Date(AJ$date, format="%m/%d/%Y")

# CFSR Data
# Anjeni CFSR data----
AJ_cfsr <- read.csv("aj_disch_calib_cfsr.csv", sep=",", na.string="NA")
AJ_cfsr$date <- as.Date(AJ_cfsr$date, format="%m/%d/%Y")
upperlim_c <- AJ_cfsr$U95PPU
lowerlim_c <- AJ_cfsr$L95PPU

#**************************************************************************************************

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
points <- seq(1,204,12)


#********************************************
# Plot WLRC vs observed data--------
# par(mfrow=c(3,1))
nonNA <- which(!is.na(sim_w_c))
# cairo_pdf(filename="~/R/Data/SWAT-Cup/1_Paper1/AJ_WLRC_Calib.pdf", pointsize = 12, family="arial", width=17, height=12)
plot(date[nonNA], upperlim[nonNA], type="n", ylim=c(0,0.15), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_w_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
# Add Validation-Calibration line
# abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,0.15,0.05), las=2)
box()
title(main="Anjeni \n(1984-2014)", 
      ylab="Discharge [m3/s]")

# dev.off()

#******************************************************
# CFSR data

setwd("~/R/Data/SWAT-Cup/1_Paper1/Anjeni")
# Plot CFSR vs observed data--------
AJ <- read.csv("aj_disch_calib_cfsr.txt", sep="\t", header=TRUE, na.strings="NA")
AJ$date <- as.Date(AJ$date, format="%m/%d/%Y")

obs_c <- AJ$observed
sim_w_c <- AJ$Best_Sim
upperlim <- AJ$U95PPU
lowerlim <- AJ$L95PPU

date <- AJ$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,156,12)

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

# dev.off()



#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
# Sediment

# Anjeni WLRC data----
AJ.sed <- read.csv("aj_sedi_calib_wlrc.txt", sep="\t", header=TRUE, stringsAsFactors=FALSE, 
                     na.strings="NA")
AJ.sed$date <- as.Date(AJ.sed$date, format="%m/%d/%Y")


# CFSR Data
# Anjeni CFSR data----
AJ_cfsr.sed <- read.csv("aj_sedi_calib_cfsr.txt", sep="\t", na.string="NA")
AJ_cfsr.sed$date <- as.Date(AJ_cfsr.sed$date, format="%m/%d/%Y")
upperlim_c <- AJ_cfsr.sed$U95PPU
lowerlim_c <- AJ_cfsr.sed$L95PPU

#**************************************************************************************************

# Define simulated and observed variables----
obs_w_c <- AJ.sed$observed
sim_w_c <- AJ.sed$Best_Sim
sim_c_c <- AJ_cfsr.sed$Best_Sim



# Prepare data for plotting----
upperlim <- AJ.sed$U95PPU
lowerlim <- AJ.sed$L95PPU
date <- AJ.sed$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,156,12)


#********************************************
# Plot WLRC vs observed data--------
# par(mfrow=c(3,1))
nonNA <- which(!is.na(sim_w_c))
# cairo_pdf(filename="~/R/Data/SWAT-Cup/1_Paper1/AJ_WLRC_Calib.pdf", pointsize = 12, family="arial", width=17, height=12)
plot(date[nonNA], upperlim[nonNA], type="n", ylim=c(0,5000), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_w_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
# Add Validation-Calibration line
# abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,5000,500), las=2)
box()
title(main="Anjeni \n(1984-2014)", 
      ylab="Soil loss [t]")











#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************




#**********************************************************
#**********************************************************
# Seasonal comparison -----


# extract data for monthly comparison
obs.w <- xts(AJ$observed, as.Date(AJ$date, format="%Y-%m-%d"))
obs.w.jjas <- obs.w[.indexmon(obs.w) %in% c(4,5,6,7,8,9)]
plot(obs.w.jjas)
#Back to dataframe
obs.df <- data.frame(date=index(obs.w.jjas), coredata(obs.w.jjas))

# prepare data----
obs <- obs.df$observed
sim <- obs.df$Best_Sim



# Prepare data for plotting----
upperlim <- obs.df$U95PPU
lowerlim <- obs.df$L95PPU
date <- obs.df$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,102,6)


# Plot WLRC vs observed data--------
nonNA <- which(!is.na(sim))
# cairo_pdf(filename="~/R/Data/SWAT-Cup/1_Paper1/AJ_WLRC_Calib.pdf", pointsize = 12, family="arial", width=17, height=12)
plot(date[nonNA], upperlim[nonNA], type="n", ylim=c(0,0.15), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs, col="darkblue", lwd=2)
lines(date, sim, col="#BF3EFF", lwd=2)
# Add Validation-Calibration line
abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,0.15,0.05), las=2)
box()
title(main="Seasonal comparison for JJAS", 
      ylab="Discharge [m3/s]")

