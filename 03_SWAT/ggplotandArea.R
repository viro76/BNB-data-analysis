#*****************************************************
# Data comparison SWAT-Cup
# WLRC data only
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

# Compare WLRC modelled data only
gerda <- read.table("~/R/Data/SWAT-Cup/WLRC/NwLU_95ppuFLWOut1.txt", header=TRUE, sep="",
                    stringsAsFactors=TRUE)
gerda$date <- as.Date(gerda$date, format="%m/%d/%Y")

# Generate the time series
date <- gerda$date
obs <- gerda$observed
upper <- gerda$U95PPU
lower <- gerda$L95PPU
sim <- gerda$Best_Sim
gerda$year <- format(as.Date(gerda$date), "%Y")
gerda$m <- format(as.Date(gerda$date), "%b")


# Add date to data----
obs <- zoo(obs, date)
upper <- zoo(upper, date)
lower <- zoo(lower, date)
sim <- zoo(sim, date)


years <- as.numeric(unique(gerda$year))


# may <- seq(5,192,12)
# sep <- seq(9,192,12)
# mome <- aggregate(may, sep, FUN=min, by=DF)
# 
# #************************************************
# # Plot comparison
# lmfit <- lm(obs ~ sim)
# 
# plot(obs~sim, pch=19, col="lightblue", xlim=c(0,0.15), ylim=c(0,0.15))
# abline(lmfit, col="brown", lty=2)
# grid()

#***********************************************************
pdf(file="~/R/Data/SWAT-Cup/WLRC/Discharge-Anjeni-WLRC.pdf", width=20, height=17,
    family="Helvetica")

d <- ggplot(gerda, aes(date))+
   geom_ribbon(aes(ymin=L95PPU, ymax=U95PPU), colour="#A1CE79", fill="#A1CE79", alpha=.2)+
   geom_line(aes(y=observed, group=year), colour="#61739A", size=.87) + 
   geom_line(aes(y=Best_Sim, group=year), colour="#A6373F", size=.87) + 
#    theme_bw() +
   labs(x="", y="Discharge [m3/s]")+
   ylim(0,0.17)+
   ggtitle("Comparison of discharge, Anjeni \n Iteration 1 - NwLU \n (1986-2013)")+
   theme(plot.title=element_text(size=rel(2), face="bold", vjust=2), 
         axis.text.x=element_text(angle=-90))+
   scale_x_date(labels=date_format("%b"), breaks=date_breaks("month"))
#    geom_vline(xintercept=as.numeric(date[c(5,9,17,)]),linetype=5, colour="black") 
   

d + facet_wrap(~year, scales="free")         # Adding scales='free' plots single years


dev.off()

#********************************************
# Discharge calibration at Yechereka----
dis.ge <- read.table("~/R/Data/SWAT-Cup/WLRC/NwLU_95ppuFLWOut11.txt", header=TRUE, sep="",
                    stringsAsFactors=TRUE)
dis.ge$date <- as.Date(dis.ge$date, format="%m/%d/%Y")


date <- dis.ge$date
obs <- dis.ge$observed
upper <- dis.ge$U95PPU
lower <- dis.ge$L95PPU
sim <- dis.ge$Best_Sim
dis.ge$year <- format(as.Date(dis.ge$date), "%Y")
dis.ge$m <- format(as.Date(dis.ge$date), "%b")


# Add date to data----
obs <- zoo(obs, date)
upper <- zoo(upper, date)
lower <- zoo(lower, date)
sim <- zoo(sim, date)


years <- as.numeric(unique(dis.ge$year))

#***********************************************************
pdf(file="~/R/Data/SWAT-Cup/WLRC/Discharge-Yech-WLRC.pdf", width=20, height=17,
    family="Helvetica")

d <- ggplot(dis.ge, aes(date))+
   geom_ribbon(aes(ymin=L95PPU, ymax=U95PPU), colour="#A1CE79", fill="#A1CE79", alpha=.2)+
   geom_line(aes(y=observed, group=year), colour="#61739A", size=.87) + 
   geom_line(aes(y=Best_Sim, group=year), colour="#A6373F", size=.87) + 
   #    theme_bw() +
   labs(x="", y="Discharge [m3/s]")+
   ylim(0,7)+
   ggtitle("Comparison of discharge, Anjeni \n Iteration 1 - NwLU \n (1986-2013)")+
   theme(plot.title=element_text(size=rel(2), face="bold", vjust=2), 
         axis.text.x=element_text(angle=-90))+
   scale_x_date(labels=date_format("%b"), breaks=date_breaks("month"))
#    geom_vline(xintercept=as.numeric(date[c(5,9,17,)]),linetype=5, colour="black") 


d + facet_wrap(~year, scales="free")         # Adding scales='free' plots single years


dev.off()

#******************************************************
#****************** SEDIMENT **************************
#******************************************************

sed.ge <- read.table("~/R/Data/SWAT-Cup/WLRC/NwLU_95ppuSEDOut1.txt", header=TRUE, sep="",
                     stringsAsFactors=TRUE)
sed.ge$date <- as.Date(sed.ge$date, format="%m/%d/%Y")


date <- sed.ge$date
obs <- sed.ge$observed
upper <- sed.ge$U95PPU
lower <- sed.ge$L95PPU
sim <- sed.ge$Best_Sim
sed.ge$year <- format(as.Date(sed.ge$date), "%Y")
sed.ge$m <- format(as.Date(sed.ge$date), "%b")


# Add date to data----
obs <- zoo(obs, date)
upper <- zoo(upper, date)
lower <- zoo(lower, date)
sim <- zoo(sim, date)


years <- as.numeric(unique(dis.ge$year))

#***********************************************************
pdf(file="~/R/Data/SWAT-Cup/WLRC/Sediment_Anj-WLRC1.pdf", width=20, height=17,
    family="Helvetica")

d <- ggplot(sed.ge, aes(date))+
   geom_ribbon(aes(ymin=L95PPU, ymax=U95PPU), colour="#A1CE79", fill="#A1CE79")+
   geom_line(aes(y=observed, group=year), colour="#61739A", size=.87) + 
   geom_line(aes(y=Best_Sim, group=year), colour="#A6373F", size=.87) + 
   #    theme_bw() +
   labs(x="", y="Discharge [m3/s]")+
   ylim(0,3000)+
   ggtitle("Comparison of discharge, Anjeni \n Iteration 1 - NwLU \n (1986-2013)")+
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
anj <- read.table("~/R/Data/SWAT-Cup/WLRC/NwLU_95ppuSEDOut1.txt", header=TRUE, sep="\t",
                  stringsAsFactors = TRUE)


# yech$date <- as.Date(yech$date, "%m/%d/%Y")
# ydate <- as.Date(yech$date)
# yobs <- yech$observed
# yupper <- yech$U95PPU
# ylower <- yech$L95PPU
# ysim <- yech$Best_Sim
# yech$year <- format(as.Date(yech$date), "%Y")
# yech$m <- format(as.Date(yech$date), "%b")

anj$date <- as.Date(anj$date, "%m/%d/%Y")
ydate <- as.Date(anj$date)
yobs <- anj$observed
yupper <- anj$U95PPU
ylower <- anj$L95PPU
ysim <- anj$Best_Sim
anj$year <- format(as.Date(anj$date), "%Y")
anj$m <- format(as.Date(anj$date), "%b")



nonNA <- which(!is.na(ysim))

#***********************
pdf(file="~/R/Data/SWAT-Cup/WLRC/Discharge-Yech-WLRC-NoGGPLot.pdf", width=20, height=17,
    family="Helvetica")
plot(ydate, yupper, type="n", ylim=c(0,50000), axes=FALSE, xlab="", ylab="")
abline(v=ydate, h=seq(0,7,1), col="gray", lty=3)
# abline(v=ydate[13])
# Because of problems with the NAs we define the polygon only for datapoints with data
polygon(c(ydate[nonNA], rev(ydate[nonNA])), c(ylower[nonNA], rev(yupper[nonNA])), 
        col="#98CF6F50", border=NA)
lines(ydate, ysim, col="darkblue", lwd=2)
lines(ydate, yobs, col="brown", lwd=2)
lines(ydate, yupper, col="#53A976")
lines(ydate, ylower, col="#53A976")

axis(1, as.Date(ydate), label=format(ydate, "%b"), las=2, cex.axis=1.5)
axis(2, seq(0,7,1), las=2, cex.axis=1.5)
box()
title(main="Simulated and observed discharge\nYechereka (2013-2014)\n",
      ylab="Discharge [m3/s]")
dev.off()

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





#*********************************************************************************
# Make 1:1 plot
pdf(file="~/R/Data/SWAT-Cup/WLRC/Regr_line_Sed.pdf", width=20, height=17,
    family="Helvetica")
plot(obs,sim, col="cornflowerblue", pch = 20, cex=4, ylim=c(0,2000), xlim=c(0,2000), axes=FALSE, xlab="", ylab="")
box()
axis(2,at=seq(0,2000,200))
axis(1,at=seq(0,2000,200))

abline(0,1, lty=3, col="darkgrey", lwd=3)
abline(h=seq(0,2000,200), v=seq(0,2000,200), col="gray", lwd=1, lty=1)

reg1 <- lm(obs~sim)
abline(reg1, col="cornflowerblue", lwd=3)

dev.off()


names(reg1)
coef(reg1)
residuals(reg1)
fitted(reg1) 

names(summary(reg1))
m <- summary(reg1)
str(m)
m$r.squared
m$adj.r.squared

plot(reg1, which=1:6)









