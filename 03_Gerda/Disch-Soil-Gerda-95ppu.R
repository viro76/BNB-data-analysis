#*****************************************************
# Data comparison SWAT-Cup
# Comparison of discharge and soil loss for FAO data
# Date: 16/02/2015
#
#*****************************************************
library(ggplot2)
library(xts)
library(reshape2)
library(scales)
library(zoo)
library(Cairo)
library(plotrix)
source("~/R/Scripts/17_Functions/multiplot.R")


# FAO data only
gerdaFAO <- read.table("~/R/Data/SWAT-Cup/FAO/FLWOut11-Yech-FAO.txt", header=TRUE, sep="",
                    stringsAsFactors=TRUE)
gerdaFAO$date <- as.Date(gerdaFAO$date, format="%m/%d/%Y")
# Define a date sequence
# x.Date <- as.Date(paste(rep(1986:2014, each = 12), rep(1:12, 2), 1, sep = "-"))
# 
# gerda$date <- x.Date

date <- gerdaFAO$date
obs <- gerdaFAO$observed
upper <- gerdaFAO$U95PPU
lower <- gerdaFAO$L95PPU
sim <- gerdaFAO$Best_Sim
gerdaFAO$year <- format(as.Date(gerdaFAO$date), "%Y")
gerdaFAO$m <- format(as.Date(gerdaFAO$date), "%b")


# Add date to data----
obs <- zoo(obs, date)
upper <- zoo(upper, date)
lower <- zoo(lower, date)
sim <- zoo(sim, date)


years <- as.numeric(unique(gerdaFAO$year))


# may <- seq(5,192,12)
# sep <- seq(9,192,12)
# mome <- aggregate(may, sep, FUN=min, by=DF)
# 
# #************************************************
# # Plot comparison
# lmfit <- lm(obs ~ sim)
# 
# plot(obs~sim, pch=19, col="lightblue", xlim=c(0,7), ylim=c(0,7))
# abline(lmfit, col="brown", lty=2)
# grid()

#***********************************************************
pdf(file="~/R/Data/SWAT-Cup/FAO/YecherekaDisch-FAO.pdf", width=20, height=17,
    family="Helvetica")

d <- ggplot(gerdaFAO, aes(date))+
   geom_ribbon(aes(ymin=L95PPU, ymax=U95PPU), colour="#A1CE79", fill="#A1CE79", alpha=.2)+
   geom_line(aes(y=observed, group=year), colour="#61739A", size=.87) + 
   geom_line(aes(y=Best_Sim, group=year), colour="#A6373F", size=.87) + 
   #    theme_bw() +
   labs(x="", y="Discharge [m3/s]")+
   ylim(0,6)+
   ggtitle("Comparison of discharge, Yechereka \n (2013-2014)")+
   theme(plot.title=element_text(size=rel(2), face="bold", vjust=2), 
         axis.text.x=element_text(angle=-90))+
   scale_x_date(labels=date_format("%b"), breaks=date_breaks("month"))
#    geom_vline(xintercept=as.numeric(date[c(5,9,17,)]),linetype=5, colour="black") 


d + facet_wrap(~year, scales="free")         # Adding scales='free' plots single years


dev.off()

#**********************************************************************
# FAO discharge in Anjeni, all years----

ajFAO <- read.table("~/R/Data/SWAT-Cup/FAO/95ppu_FAO_Calibrated.txt", header=TRUE, sep="",
                       stringsAsFactors=TRUE)
ajFAO$date <- as.Date(ajFAO$date, format="%m/%d/%Y")
# Define a date sequence
# x.Date <- as.Date(paste(rep(1986:2014, each = 12), rep(1:12, 2), 1, sep = "-"))
# 
# gerda$date <- x.Date

date <- ajFAO$date
obs <- ajFAO$observed
upper <- ajFAO$U95PPU
lower <- ajFAO$L95PPU
sim <- ajFAO$Best_Sim
ajFAO$year <- format(as.Date(ajFAO$date), "%Y")
ajFAO$m <- format(as.Date(ajFAO$date), "%b")


# Add date to data----
obs <- zoo(obs, date)
upper <- zoo(upper, date)
lower <- zoo(lower, date)
sim <- zoo(sim, date)

years <- as.numeric(unique(ajFAO$year))


#******************************************
# Plot years
pdf(file="~/R/Data/SWAT-Cup/FAO/AnjeniDisch-FAO.pdf", width=20, height=17,
    family="Helvetica")

d <- ggplot(ajFAO, aes(date))+
   geom_ribbon(aes(ymin=L95PPU, ymax=U95PPU), colour="#A1CE79", fill="#A1CE79", alpha=.2)+
   geom_line(aes(y=observed, group=year), colour="#61739A", size=.87) + 
   geom_line(aes(y=Best_Sim, group=year), colour="#A6373F", size=.87) + 
   #    theme_bw() +
   labs(x="", y="Discharge [m3/s]")+
   ylim(0,0.2)+
   ggtitle("Comparison of discharge, Anjeni \n (2013-2014)")+
   theme(plot.title=element_text(size=rel(2), face="bold", vjust=2), 
         axis.text.x=element_text(angle=-90))+
   scale_x_date(labels=date_format("%b"), breaks=date_breaks("month"))
#    geom_vline(xintercept=as.numeric(date[c(5,9,17,)]),linetype=5, colour="black") 


d + facet_wrap(~year, scales="free")         # Adding scales='free' plots single years


dev.off()




#********************************************
#*********** SEDIMENT CALIBRATION ***********
#********************************************
# Sediment calibration at Anjeni----
sed.aj <- read.table("~/R/Data/SWAT-Cup/FAO/SEDOut1-Anj-FAO.txt", header=TRUE, sep="",
                        stringsAsFactors=TRUE)
sed.aj$date <- as.Date(sed.aj$date, format="%m/%d/%Y")
# Define a date sequence
# x.Date <- as.Date(paste(rep(1986:2014, each = 12), rep(1:12, 2), 1, sep = "-"))
# 
# gerda$date <- x.Date

date <- sed.aj$date
obs <- sed.aj$observed
upper <- sed.aj$U95PPU
lower <- sed.aj$L95PPU
sim <- sed.aj$Best_Sim
sed.aj$year <- format(as.Date(sed.aj$date), "%Y")
sed.aj$m <- format(as.Date(sed.aj$date), "%b")


# Add date to data----
obs <- zoo(obs, date)
upper <- zoo(upper, date)
lower <- zoo(lower, date)
sim <- zoo(sim, date)


years <- as.numeric(unique(sed.gerda$year))



# Plot years
pdf(file="~/R/Data/SWAT-Cup/FAO/AnjeniSoilL-FAO.pdf", width=20, height=17,
    family="Helvetica")

d <- ggplot(sed.aj, aes(date))+
   geom_ribbon(aes(ymin=L95PPU, ymax=U95PPU), colour="#A1CE79", fill="#A1CE79", alpha=.2)+
   geom_line(aes(y=observed, group=year), colour="#61739A", size=.87) + 
   geom_line(aes(y=Best_Sim, group=year), colour="#A6373F", size=.87) + 
   #    theme_bw() +
   labs(x="", y="Sediment loss [t]")+
   ylim(0,6000)+
   ggtitle("Comparison of sediment loss, Anjeni \n (1986-2014)")+
   theme(plot.title=element_text(size=rel(2), face="bold", vjust=2), 
         axis.text.x=element_text(angle=-90))+
   scale_x_date(labels=date_format("%b"), breaks=date_breaks("month"))
#    geom_vline(xintercept=as.numeric(date[c(5,9,17,)]),linetype=5, colour="black") 


d + facet_wrap(~year, scales="free")         # Adding scales='free' plots single years


dev.off()



#******************************
# Sediment calibration at Yechereka----
sed.ye <- read.table("~/R/Data/SWAT-Cup/FAO/SEDOut11-Yech-FAO.txt", header=TRUE, sep="",
                     stringsAsFactors=TRUE)
sed.ye$date <- as.Date(sed.ye$date, format="%m/%d/%Y")
# Define a date sequence
# x.Date <- as.Date(paste(rep(1986:2014, each = 12), rep(1:12, 2), 1, sep = "-"))
# 
# gerda$date <- x.Date

date <- sed.ye$date
obs <- sed.ye$observed
upper <- sed.ye$U95PPU
lower <- sed.ye$L95PPU
sim <- sed.ye$Best_Sim
sed.ye$year <- format(as.Date(sed.ye$date), "%Y")
sed.ye$m <- format(as.Date(sed.ye$date), "%b")


# Add date to data----
obs <- zoo(obs, date)
upper <- zoo(upper, date)
lower <- zoo(lower, date)
sim <- zoo(sim, date)


years <- as.numeric(unique(sed.gerda$year))



# Plot years
pdf(file="~/R/Data/SWAT-Cup/FAO/YecherekaSoilL-FAO.pdf", width=20, height=17,
    family="Helvetica")

d <- ggplot(sed.ye, aes(date))+
   geom_ribbon(aes(ymin=L95PPU, ymax=U95PPU), colour="#A1CE79", fill="#A1CE79", alpha=.2)+
   geom_line(aes(y=observed, group=year), colour="#61739A", size=.87) + 
   geom_line(aes(y=Best_Sim, group=year), colour="#A6373F", size=.87) + 
   #    theme_bw() +
   labs(x="", y="Sediment loss [t]")+
   ylim(0,500000)+
   ggtitle("Comparison of sediment loss, Yechereka \n (2013-2014)")+
   theme(plot.title=element_text(size=rel(2), face="bold", vjust=2), 
         axis.text.x=element_text(angle=-90))+
   scale_x_date(labels=date_format("%b"), breaks=date_breaks("month"))
#    geom_vline(xintercept=as.numeric(date[c(5,9,17,)]),linetype=5, colour="black") 


d + facet_wrap(~year, scales="free")         # Adding scales='free' plots single years


dev.off()






















#*************************************************************
#******************************************************************************
#********************************************
# Outflow at Yechereka-----
yech <- read.table("~/R/Data/SWAT-Cup/WLRC/NwLU_95ppuFLWOut11.txt", header=TRUE, sep="\t", 
                   stringsAsFactors=TRUE)
yech$date <- as.Date(yech$date, "%m/%d/%Y")
ydate <- as.Date(yech$date)
yobs <- yech$observed
yupper <- yech$U95PPU
ylower <- yech$L95PPU
ysim <- yech$Best_Sim
yech$year <- format(as.Date(yech$date), "%Y")
yech$m <- format(as.Date(yech$date), "%b")


# Add date to data----
yobs <- zoo(yobs, date)
yupper <- zoo(yupper, date)
ylower <- zoo(ylower, date)
ysim <- zoo(ysim, date)


years <- as.numeric(unique(yech$year))



#***********************************************************
pdf(file="~/R/Data/SWAT-Cup/yechDischarge_Iter1-NwLU.pdf", width=20, height=17,
    family="Helvetica")

d <- ggplot(yech, aes(ydate))+
   geom_ribbon(aes(ymin=L95PPU, ymax=U95PPU), colour="#A1CE79", fill="#A1CE79", alpha=.2)+
   geom_line(aes(y=observed, group=year), colour="#61739A", size=.87) + 
   geom_line(aes(y=Best_Sim, group=year), colour="#A6373F", size=.87) + 
   theme_bw() +
   labs(x="", y="Discharge [m3/s]")+
   ylim(0,7)+
   ggtitle("Comparison of discharge, Yechereka \n Iteration 1 (2013-2014)")+
   theme(plot.title=element_text(size=rel(2), face="bold", vjust=2), 
         axis.text.x=element_text(angle=-90))+
   scale_x_date(labels=date_format("%b"), breaks=date_breaks("month"))
#    geom_vline(xintercept=as.numeric(date[c(5,9,17,)]),linetype=5, colour="black") 


d + facet_wrap(~year, scales="free")         # Adding scales='free' plots single years

dev.off()

#***************************************************************
# Compare all three iterations ----

setwd("~/R/Data/SWAT-Cup/FAO/")
files.iter <- list.files(pattern=".*Iter.*txt")

names <- c("Iter1","Iter2", "Iter3")

for(i in seq_along(files.iter)){
   
   x <- read.table(files.iter[i], row.names=NULL, header=TRUE)           
   x$date <- date                                  # change date row to date format
   assign(paste("Iter",i,sep=""),x)
   
   # Create a zoo object from the data frames
   xobs <- x$observed
   xupper <- x$U95PPU
   xlower <- x$L95PPU
   xsim <- x$Best_Sim
   x$year <- format(as.Date(x$date), "%Y")
   x$m <- format(as.Date(x$date), "%b")
   
   
   # Add date to data----
   xobs <- zoo(xobs, date)
   xupper <- zoo(xupper, date)
   xlower <- zoo(xlower, date)
   xsim <- zoo(xsim, date)
   
   assign(paste("obsIter", i, sep=""), xobs)
   assign(paste("simIter", i, sep=""), xsim)
   assign(paste("upperIter", i, sep=""), xupper)
   assign(paste("lowerIter", i, sep=""), xlower)
   
   years <- as.numeric(unique(x$year))
   
   par(mar=c(0,3,5,0))
   d <- ggplot(x, aes(date))+
      geom_ribbon(aes(ymin=L95PPU, ymax=U95PPU), colour="#A1CE79", fill="#A1CE79", alpha=.2)+
      geom_line(aes(y=observed, group=year), colour="#61739A", size=.87) + 
      geom_line(aes(y=Best_Sim, group=year), colour="#A6373F", size=.87) + 
      theme_bw() +
      labs(x="", y="Discharge [m3/s]")+
      ylim(0,max(xupper)+0.02)+
      ggtitle(paste("Comparison of discharge, Yechereka \n (1986-2013)\n Iteration",i,sep=" "))+
      theme(plot.title=element_text(size=rel(2), face="bold", vjust=2), 
            axis.text.x=element_text(angle=-90))+
      scale_x_date(labels=date_format("%b"), breaks=date_breaks("month"))
   #    geom_vline(xintercept=as.numeric(date[c(5,9,17,)]),linetype=5, colour="black") 
   
   
   d + facet_wrap(~year, scales="free")
   print(d + facet_wrap(~year, scales="free"))  
   
}

#******************************************
# Scatterplot obs vs sim----

# lms
iter1lm <- lm(Iter1$observed~Iter1$Best_Sim)
iter2lm <- lm(Iter2$observed~Iter2$Best_Sim)
iter3lm <- lm(Iter3$observed~Iter3$Best_Sim)

years <- unique(format(index(obsIter1), "%Y"))
xl <- seq(1, length(obsIter1), 12)
xla <- format(index(obsIter1[xl]), "%Y")
maxy <- max(obsIter1, simIter1, simIter2, simIter3)

# pdf("SimulationIteration.pdf", family = "Helvetica", paper = "a4r")
par(mfrow=c(2,1), mar=c(5,5,4,1))
# Discharge plot
plot(window(obsIter1, start="1986-01-01", end="1999-12-01"), col="#7093B1", lwd=2,
     yaxt="n", xaxt="n", ylab="", xlab="", ylim=c(0,0.15))
abline(v=index(obsIter1)[xl], h=seq(0, round(maxy+0.01, digits=2), 0.01), col="gray85", lty=1)
lines(window(obsIter1, start="1986-01-01", end="1999-12-01"), col="#7093B1", lwd=3)
lines(window(simIter1, start="1986-01-01", end="1999-12-01"), col="#0E3D5945", 
      type="l", lty=3, lwd=2)
lines(window(simIter2, start="1986-01-01", end="1999-12-01"), col="#F29F05", 
      type="l", lty=3, lwd=2)
lines(window(simIter3, start="1986-01-01", end="1999-12-01"), col="#D92525", 
      type="l", lty=3, lwd=2)
legend(5815, 0.1573, col=c("#7093B1","#0E3D5945","#F29F05","#D92525"), 
       legend=c("Observed","Iteration 1", "Iteration 2", "Iteration 3"),
       cex=.8, bty="n", lty=c(1, 3,3,3), lwd=2, y.intersp=.3)
axis(1, at=index(obsIter1[xl]), labels = years, las=2, cex=.8)
axis(2, at=seq(0, round(maxy+0.01, digits=2), 0.01), las=2, cex=.8)

title(main="Comparison of observed and simuated values\n Anjeni, 1986-2013")


# Lm plot
plot(obs, sim, col="#0E3D5925", pch=19, cex=.8, las=2,
     xlab="Observed discharge [m3/s]", xlim=c(0,0.14),
     ylab="Simulated discharge [m3/s]", ylim=c(0,0.14))
grid()
abline(iter1lm, col="#0E3D5925", lty=2, lwd=2)

points(Iter2$observed, Iter2$Best_Sim, pch=19, col="#88A61B45")
abline(iter2lm, col="#88A61B45", lty=2, lwd=2)

points(Iter3$observed, Iter3$Best_Sim, pch=19, col="#C1513845")
abline(iter3lm, col="#C1513845", lty=2, lwd=2)

legend("topleft", col=c("#D8E063","#91BA6D45", "#C1513845"), pch = 19, cex=.8,
       legend=c("Iteration 1", "Iteration 2", "Iteration 3"), bty="n", y.intersp=.3)
# dev.off()


par(mfrow=c(1,1))


#******************************************************************************
#********************************************
# Plot polygons without ggplot2----

# Outflow at Yechereka-----
yech <- read.table("~/R/Data/SWAT-Cup/WLRC/NwLU_95ppuFLWOut11.txt", header=TRUE, sep="\t", 
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
title(main="Simulated and observed discharge\nYechereka (2013-2014)\n", 
      ylab="Discharge [m3/s]")
# dev.off()

#****************************************************************************
# One plot with all iterations

path <- "E:/Calibration/Gerda/2_FAO_WLRC_31yrs_2yrsk_mon.Sufi2.SwatCup/Iterations"
target <- "Sufi2.Out"
setwd(path)

filelist <- list.files()
for(i in filelist){
   togo <- paste(path, i, target, "95ppu.txt", sep="/")
   file <- read.table(togo, skip=202, fill=TRUE, header=TRUE)
   file <- file[c(1:18),]
   
}

























