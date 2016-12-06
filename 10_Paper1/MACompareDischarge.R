#**********************************************
#
# Compare WLRC and CFSR discharge data
# Maybar only
#
#**********************************************

setwd("~/R/Data/SWAT-Cup/1_Paper1/Maybar")


# Maybar WLRC data
MA <- read.csv("ma_disch_Calib-Valid_wlrc.txt", sep="\t", na.strings="NA")
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
points <- seq(1,252,12)

nonNA <- which(!is.na(sim_w_c))
# plot(date, upperlim, type="n", ylim=c(0,0.12), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

# cairo_pdf(filename="~/R/Data/SWAT-Cup/1_Paper1/MA_WLRC_Calib.pdf", pointsize = 12, family="arial", width=17, height=12)
# par(mfrow=c(3,1))
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
title(main="Maybar (1983-2014)", 
      ylab="Discharge [m3/s]")
# dev.off()

#**************************************************
# CFSR data
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




#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#                                      Sediment
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************

# Maybar WLRC data
setwd("~/R/Data/SWAT-Cup/1_Paper1/Maybar")

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
# plot(date, upperlim, type="n", ylim=c(0,0.12), axes=FALSE, xlab="", ylab="")
# abline(v=year, h=seq(0,1.2,0.2), col="gray", lty=3)
# abline(v=date[points], col="gray", lty=3)

# cairo_pdf(filename="~/R/Data/SWAT-Cup/1_Paper1/MA_WLRC_Calib.pdf", pointsize = 12, family="arial", width=17, height=12)
# par(mfrow=c(3,1))
plot(date, upperlim, type="n", ylim=c(0,2400), axes=FALSE, xlab="", ylab="")
polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim[nonNA], rev(upperlim[nonNA])),
        col="#E0EEEE", border=NA)
lines(date, upperlim, col="#838B8B")
lines(date, lowerlim, col="#838B8B")
# polygon(c(date[nonNA], rev(date[nonNA])), c(lowerlim_c[nonNA], rev(upperlim_c[nonNA])),
#         col="#E0EEEE", border=NA)
# lines(date, upperlim_c, col="#838B8B")
# lines(date, lowerlim_c, col="#838B8B")
lines(date, obs_c, col="darkblue", lwd=2)
lines(date, sim_w_c, col="#BF3EFF", lwd=2)
# lines(date, sim_c_c, col="#006400", lwd=2)
axis(1, date[points], labels=unique(year), las=2)
axis(2, seq(0,2400, 300), las=2)
box()
title(main="Maybar (1983-2014)", 
      ylab="Discharge [m3/s]")








