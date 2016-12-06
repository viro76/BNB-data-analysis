#*****************************************************
# Compare calibration data from SWAT-Cup
#
#
#*****************************************************


setwd("~/R/Data/SWAT-Cup/2_Paper2")

# # CFSR Data
# # Anjeni CFSR data----
# AJ_cfsr <- read.csv("0_FAO_behFLWOut1.txt", sep="\t", na.string="NA")
# AJ_cfsr$date <- as.Date(AJ_cfsr$date, format="%m/%d/%Y")
# upperlim_c <- AJ_cfsr$U95PPU
# lowerlim_c <- AJ_cfsr$L95PPU

# CFSR data
par(mfrow=c(2,1))
# Plot CFSR vs observed data--------
AJ <- read.csv("9_FAO_All_behFLWOut1.txt", sep="\t", header=TRUE, na.strings="NA")
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

#*************************
# Plot data
# par(mfrow=c(3,1))
plot(date, upperlim, type="n", ylim=c(0,0.15), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
lines(date, obs_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,0.15,0.03), las=2)
box()
title(main="Minchet FAO", 
      ylab="Discharge [m3/s]")

#**************************************************************************************************


# Anjeni WLRC data----
AJ <- read.csv("9_WLRC_All_behFLWOut1.txt", sep="\t", header=TRUE, na.strings="NA")
AJ$date <- as.Date(AJ$date, format="%m/%d/%Y")


# Define simulated and observed variables----
obs_w_c <- AJ$observed
sim_w_c <- AJ$Best_Sim
# sim_c_c <- AJ_cfsr$Best_Sim

# Prepare data for plotting----
upperlim <- AJ$U95PPU
lowerlim <- AJ$L95PPU
date <- AJ$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,348,12)


# Plot WLRC vs observed data--------

nonNA <- which(!is.na(sim_w_c))
# cairo_pdf(filename="~/R/Data/SWAT-Cup/1_Paper1/AJ_WLRC_Calib.pdf", pointsize = 12, family="arial", width=17, height=12)
plot(date[nonNA], upperlim[nonNA], type="n", ylim=c(0,0.15), axes=FALSE, xlab="", ylab="")

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
axis(2, seq(0,0.15,0.03), las=2)
box()
title(main="Minchet WLRC", 
      ylab="Discharge [m3/s]")

# dev.off()


#******************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
# Sediment




#***************************************
# Plot FAO vs observed data
# CFSR Data
# Anjeni CFSR data----
AJ_cfsr.sed <- read.csv("9_FAO_All_behSEDOut1.txt", sep="\t", na.string="NA")
AJ_cfsr.sed$date <- as.Date(AJ_cfsr.sed$date, format="%m/%d/%Y")
upperlim_c <- AJ_cfsr.sed$U95PPU
lowerlim_c <- AJ_cfsr.sed$L95PPU

# Prepare data for plotting----
# Define simulated and observed variables----
obs <- AJ_cfsr.sed$observed
sim <- AJ_cfsr.sed$Best_Sim

upperlim <- AJ_cfsr.sed$U95PPU
lowerlim <- AJ_cfsr.sed$L95PPU
date <- AJ_cfsr.sed$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,348,12)


nonNA <- which(!is.na(sim))
# cairo_pdf(filename="~/R/Data/SWAT-Cup/1_Paper1/AJ_WLRC_Calib.pdf", pointsize = 12, family="arial", width=17, height=12)
par(mfrow=c(2,1))
plot(date[nonNA], upperlim[nonNA], type="n", ylim=c(0,2000), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
legend("topleft", col=c("#2ca25f", "#ae017e"), lty=1, legend=c("Observed", "Simulated"), bty="n", cex=.7)
lines(date, obs, col="#2ca25f", lwd=2)
lines(date, sim, col="#ae017e", lwd=2)
# Add Validation-Calibration line
# abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,2000,250), las=2)
box()
title(main="Minchet FAO\n(1984-2014)", 
      ylab="Soil loss [t]")




#**************************************************************************************************
# Anjeni WLRC data----
AJ.sed <- read.csv("9_WLRC_All_behSEDOut.txt", sep="\t", header=TRUE, stringsAsFactors=FALSE, 
                   na.strings="NA")
AJ.sed$date <- as.Date(AJ.sed$date, format="%m/%d/%Y")


# # FAO Data
# # Anjeni FAO data----
# AJ_cfsr.sed <- read.csv("9_FAO_All_behSEDOut1.txt", sep="\t", na.string="NA")
# AJ_cfsr.sed$date <- as.Date(AJ_cfsr.sed$date, format="%m/%d/%Y")
# upperlim_c <- AJ_cfsr.sed$U95PPU
# lowerlim_c <- AJ_cfsr.sed$L95PPU


# Define simulated and observed variables----
obs_w_c <- AJ.sed$observed
sim_w_c <- AJ.sed$Best_Sim
# sim_c_c <- AJ_cfsr.sed$Best_Sim


# Prepare data for plotting----
upperlim <- AJ.sed$U95PPU
lowerlim <- AJ.sed$L95PPU
date <- AJ.sed$date
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
points <- seq(1,348,12)


# Plot WLRC vs observed data--------

nonNA <- which(!is.na(sim_w_c))
# cairo_pdf(filename="~/R/Data/SWAT-Cup/1_Paper1/AJ_WLRC_Calib.pdf", pointsize = 12, family="arial", width=17, height=12)
plot(date[nonNA], upperlim[nonNA], type="n", ylim=c(0,2000), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs_w_c, col="#2ca25f", lwd=2)
lines(date, sim_w_c, col="#ae017e", lwd=2)
legend("topleft", col=c("#2ca25f", "#ae017e"), lty=1, legend=c("Observed", "Simulated"), bty="n", cex=.7)
# Add Validation-Calibration line
# abline(v=14620, col="darkred", lty=2, lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,2000,250), las=2)
box()
title(main="Minchet WLRC\n(1984-2014)", 
      ylab="Soil loss [t]")






#***************************************************************************************************
#************************************  GERDA WTSH  *************************************************
#***************************************************************************************************
# Compare validated Discharge Gerda data

dischWLRC <- read.csv("9_WLRC_behFLWOut11.txt", sep="\t", na.string="NA")
dischFAO <- read.csv("9_FAO_behFLWOut11.txt", sep="\t", na.string="NA")

points <- seq(1,24,1)
upperlim <- dischWLRC$U95PPU
lowerlim <- dischWLRC$L95PPU
date <- as.Date(dischWLRC$date, format="%m/%d/%Y")
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
obs <- dischWLRC$observed
sim <- dischWLRC$Best_Sim
nonNA <- which(!is.na(sim))


# FAO data validation
par(mfrow=c(2,1))
plot(date[nonNA], dischFAO$U95PPU[nonNA], type="n", ylim=c(0,7), axes=FALSE, xlab="", ylab="")
polygon(c(date[nonNA], rev(date[nonNA])), c(dischFAO$L95PPU[nonNA], rev(dischFAO$U95PPU[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, dischFAO$L95PPU, col="#838B8B")
lines(date, dischFAO$U95PPU, col="#838B8B")
lines(date, obs, col="darkblue", lwd=2)
lines(date, dischFAO$Best_Sim, col="#BF3EFF", lwd=2)

axis(1, date[points], label=mon, las=2)
axis(2, seq(0,7,1), las=2)
box()
title(main="Gerda FAO validation", ylab="Discharge [m3/s]")


# WLRC data validation

plot(date[nonNA], upperlim[nonNA], type="n", ylim=c(0,7), axes=FALSE, xlab="", ylab="")
polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs, col="darkblue", lwd=2)
lines(date, sim, col="#BF3EFF", lwd=2)

axis(1, date[points], labels=mon, las=2)
axis(2, seq(0,7,1), las=2)
box()
title(main="Gerda WLRC Validation", ylab="Discharge [m3/s]")



#*******************************************************************
#*******************************************************************
#*******************************************************************
# Same with sediment comp

SedWLRC <- read.csv("9_WLRC_behSEDOut11.txt", sep="\t", na.string="NA")
SedFAO <- read.csv("9_FAO_behSEDOut11.txt", sep="\t", na.string="NA")

# "#2ca25f" "#ae017e" "#838B8B"


points <- seq(1,24,1)
upperlim <- SedWLRC$U95PPU
lowerlim <- SedWLRC$L95PPU
date <- as.Date(SedWLRC$date, format="%m/%d/%Y")
year <- format(as.Date(date), "%Y")
mon <- format(as.Date(date), "%b")
obs <- SedWLRC$observed
sim <- SedWLRC$Best_Sim
nonNA <- which(!is.na(sim))

# 
# par(mfrow=c(2,1))

# 
# plot(date[nonNA], dischFAO$U95PPU[nonNA], type="n", ylim=c(0,170000), axes=FALSE, xlab="", ylab="")
# polygon(c(date[nonNA], rev(date[nonNA])), c(SedFAO$L95PPU[nonNA], rev(SedFAO$U95PPU[nonNA])),
#         col="#E0EEEE", border=NA)
# lines(date, SedFAO$L95PPU, col="#838B8B")
# lines(date, SedFAO$U95PPU, col="#838B8B")
# lines(date, obs, col="#2ca25f", lwd=2)
# lines(date, SedFAO$Best_Sim, col="#ae017e", lwd=2)
# 
# axis(1, date[points], label=mon, las=2)
# axis(2, seq(0,170000,20000), las=2)
# box()
# title(main="Yechereka FAO validation", ylab="Sediment loss [t]")



plot(date[nonNA], upperlim[nonNA], type="n", ylim=c(0,170000), axes=FALSE, xlab="", ylab="")
polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")

lines(date, obs, col="#2ca25f", lwd=2)
lines(date, sim, col="#ae017e", lwd=2)

axis(1, date[points], labels=mon, las=2)
axis(2, seq(0,170000,20000), las=2)
box()
title(main="Yechereka WLRC Validation", ylab="Sediment loss [t]")








