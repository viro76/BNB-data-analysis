#********************************************#
# Comparing CFSR and SCRP rainfall data      #
#                                            #
#********************************************#
# Author:   V. Roth                          #
# Date:     06/25/2014                       #
# Adapted:  --                               #
#********************************************#


# Load the libraries
library(zoo)
library(hydroGOF)   
library(ggplot2)   
library(xts)   
library(timeSeries)
library(RColorBrewer)

# Define the data sources
# Observed data
ATobsFld.mon <- "E:/ArcSWAT_Examples/3_AnditTid/Scenarios/AT_Monthly_SCRPPCP/TxtInOut"
AJobsFld.mon <- "E:/ArcSWAT_Examples/1_Anjeni/Scenarios/AJ_Monthly_SCRPPCP/TxtInOut"

# Simulated discharge with conventional rainfall
ATsimFld.mon <- "E:/ArcSWAT_Examples/3_AnditTid NNR/Scenarios/Test/TxtInOut"
AJsimFld.mon <- "E:/ArcSWAT_Examples/1_Anjeni NNR/Scenarios/AJ_Monthly_CFSRPCP/TxtInOut"


# Define the rainfall data sources
AJpcpCFSR <- "E:/ArcSWAT_Examples/1_Anjeni NNR/climate/pcp/Original"
AJpcpSCRP <- "E:ArcSWAT_Examples/1_Anjeni/climate/pcp/"


#*******************************************************************************
# Load the SCRP rainfall and sediment loss time series and adapt them
setwd(ATobsFld.mon)
Qobs <- read.table("output.rch", skip=9)
subobs <- subset(Qobs, V4<13)                                                    # remove the yearly data at the end of each year
row.names(subobs) <- NULL
Qobs <- subobs[subobs$V2==2,7]                                                   # select data from Col 7 for each V2==2
Qobs <- Qobs[1:300]
Qobs.ts <- ts(Qobs, start=1986, freq=12)
# Add Sediment
Sedobs <- subobs[subobs$V2==2, 11]
# Sedobs <- Sedobs[1:300]
Sedobs.ts <- ts(Sedobs, start=1986, freq=12)


#*******************************************************************************
# Load the CFSR rainfall time series and adapt them
setwd(ATsimFld.mon)
Qsim <- read.table("output.rch", skip=9)
subsim <- subset(Qsim, V4<13)
row.names(subsim) <- NULL
Qsim <- subsim[subsim$V2==2,7]
Qsim.ts <- ts(Qsim, start=1979, freq=12)
# Add Sediment
Sedsim <- subsim[subsim$V2==2, 11]
Sedsim.ts <- ts(Sedsim, start=1986, freq=12)

#**************************************************
# combine the two ts into one data frame
SimObs <- ts.intersect(Qobs.ts, Qsim.ts)
SedSimObs <- ts.intersect(Sedobs.ts, Sedsim.ts)

#**************************************************
# Calculate Statistics for discharge----
z <- as.data.frame(SimObs)
names(z) <- c("scrp", "cfsr")

SSR <- sum((z$cfsr - z$scrp)^2, na.rm=TRUE)
NS <- 1-(SSR/(sum(z$scrp-mean(z$scrp))^2))
PBIAS <- 100*(sum(z$cfsr)-sum(z$scrp))/sum(z$scrp)
RMSE <- rmse(z$cfsr, z$scrp, na.rm=TRUE)
SSR;NS;PBIAS;RMSE

#**************************************************
# Plot the data and add statistics
year <- seq(min(index(SimObs)), max(index(SimObs)+12), by=1)
disch <- seq(min(SimObs), max(SimObs+0.1), by=0.1)
left <- min(index(SimObs))
up <- max(disch)


#**************************************************
plot.ts(SimObs, plot.type = "single", axes=FALSE, ylim=c(0,max(disch)),
        col=c("dodgerblue", "brown"), lwd=2,
        main="SCRP and CFSR discharge simulation with SWAT\nAndit Tid (1986-2010)",
        ylab="Discharge [m3/s]", xlab="")
box()
axis(1, at=year, las=2)
axis(2, at=disch, las=2)
abline(v=year, lty=3, col="lightgrey")
abline(h=disch, lty=3, col="lightgrey")
text(left+.5,up, "SSR:", pos=2, cex=.8)
text(left+2, up, round(SSR,3), cex=.8, pos=2)
# text(left+.5, up-0.03, "NS:", pos=2, cex=.8)
# text(left+6, up-0.03, round(NS,3), cex=.8, pos=2)
text(left+.5, up-0.06, "PBIAS:", pos=2, cex=.8)
text(left+2, up-0.06, round(PBIAS, 3), pos=2, cex=.8)
text(left+.5, up-0.09, "RMSE:", cex=.8, pos=2)
text(left+2, up-0.09, round(RMSE, 3), pos=2, cex=.8)
legend("topright", legend=c("SCRP pcp data", "CFSR pcp data"), lty=1, 
       col=c("dodgerblue", "brown"), bty="n", cex=.8)

#*******************************************************************************
# Plot rainfall and discharge on one plot----
# Load rainfall first
setwd(AJpcpCFSR)
pcpcfsr <- read.csv("pcpNNRAll.csv", header=TRUE)
pcpcfsr$date <- as.Date(pcpcfsr$date, format="%m/%d/%Y")

# NAs <- pcpcfsr == -99
# is.na(pcpcfsr)[NAs] <- TRUE

# Calculate rainfall mean
rain <- pcpcfsr[, c(2:5)]
rain$mean <- rowMeans(rain, na.rm=TRUE)
rain <- cbind(date=pcpcfsr$date, rain)
rain <- rain[,c(1,6)]
# aggregate data to monthly
rain$mo <- strftime(rain$date, "%m")
rain$yr <- strftime(rain$date, "%Y")
rain.agg <- aggregate(rain$mean ~ rain$mo + rain$yr, FUN=sum)
names(rain.agg) <- c("month", "year", "pcp")
# Intersect the dates of the time series
rain.ts <- ts(rain.agg$pcp, start=1979, freq=12)
AllData <- ts.intersect(SedSimObs, rain.ts)
# Create a data frame for further use with hydroplot
z <- as.data.frame(AllData)
names(z) <- c("scrp", "cfsr", "pcp")
zdate <- cbind(index(AllData), z)
names(zdate) <- c("date", "scrp", "cfsr", "pcp")
months <- seq(as.Date("1986-01-01"), as.Date("2010-12-31"), "month")
zdate$date <- months



streamflow1 <- zdate[,2]
streamflow2 <- zdate[,3]
date <- format(zdate[,1])
precip <- zdate[,4]
begin=1
endindex=length(date)
# Define number of years
beg <- as.numeric(format(as.yearmon(min(date)), "%Y"))
end <- as.numeric(format(as.yearmon(max(date)), "%Y"))
year <- seq(as.numeric(beg), as.numeric(end), by=1)

disch <- seq(0, max(max(streamflow1), max(streamflow2))+1, by=10)

col <- brewer.pal(11, "RdBu")
par(mar=c(3,5,1,4))
barplot(precip, yaxt = "n", space = NULL, border=NA,
        ylim = rev(c(0, 4 * max(precip))),                                          # "rev" stands for "Reverse Element"
        xaxt = "n", col=col[4])
axis(side=3, pos=0, tck=0, xaxt="n")
axis(side=4, at=seq(0, floor(max(na.omit(precip))+1), 
                    length=(1+ifelse(floor(max(na.omit(precip))+1)<10,
                                     floor(max(na.omit(precip))+1),4))),
     labels=as.integer(seq(0, floor(max(na.omit(precip))+1),
                           length=(1+ifelse(floor(max(na.omit(precip))+1)<10,
                                            floor(max(na.omit(precip))+1),
                                            4)))))
mtext(paste("Precipitation [mm]"), 4, line=2, cex=.8, adj=1)
par(new=TRUE)
plot(streamflow1, col=col[2], type="l", lwd=1, lty=1, xaxt="n", ylab="",
     xlab="date", ylim=c(0,1.5*max(max(na.omit(streamflow1)),
                                   max(na.omit(streamflow2)))), 
     axes=FALSE)
lines(streamflow2, col=col[10], lwd=1, lty=1, xaxt="n")
axis(side=1, at=seq(1, (endindex-begin+1), length=25), pos=.5, 
     labels=year, las=2)
axis(side=2, pos=0, cex=.8, las=2)

mtext(expression(paste("Discharge [", m^3/s, "]", sep="")), 2, 3, cex=.8)
# grid()

# abline(v=seq(1,(endindex-begin+1), length=25), col="lightgrey", lty=3)
# abline(h=disch, col="lightgrey", lty=3)

#*******************************************************************************
#*******************************************************************************
# Anjeni
# Load the SCRP rainfall and sediment time series and adapt it
setwd(AJobsFld.mon)
Qobs <- read.table("output.rch", skip=9)
subobs <- subset(Qobs, V4<13)                                                    # remove the yearly data at the end of each year
row.names(subobs) <- NULL
Qobs <- subobs[subobs$V2==14,7]                                                   # select data from Col 7 for each V2==2
Qobs <- Qobs[1:300]
Qobs.ts <- ts(Qobs, start=1986, freq=12)
# Sediment
Sedobs <- subobs[subobs$V2==14,11]
Sedobs.ts <- ts(Sedobs, start=1986, freq=12)

#**************************************************
# Load the CFSR rainfall time series and adapt them
setwd(AJsimFld.mon)
Qsim <- read.table("output.rch", skip=9)
subsim <- subset(Qsim, V4<13)
row.names(subsim) <- NULL
Qsim <- subsim[subsim$V2==14,7]
Qsim.ts <- ts(Qsim, start=1986, freq=12)
# Sediment
Sedsim <- subsim[subsim$V2==14, 11]
Sedsim.ts <- ts(Sedsim, start=1986, freq=12)

#**************************************************
# combine the two ts into one data frame
SimObs <- ts.intersect(Qobs.ts, Qsim.ts)
SedSimObs <- ts.intersect(Sedobs.ts, Sedsim.ts)

#**************************************************
# Calculate Statistics for discharge----
z <- as.data.frame(SimObs)
names(z) <- c("scrp", "cfsr")
zdate <- cbind(index(SimObs), z)
names(zdate) <- c("date", "scrp", "cfsr")
months <- seq(as.Date("1986-01-01"), as.Date("2010-12-31"), "month")
zdate$date <- months


SSR <- sum((z$cfsr - z$scrp)^2, na.rm=TRUE)
NS <- 1-(SSR/(sum(z$scrp-mean(z$scrp))^2))
PBIAS <- 100*(sum(z$cfsr)-sum(z$scrp))/sum(z$scrp)
RMSE <- rmse(z$cfsr, z$scrp, na.rm=TRUE)
SSR;NS;PBIAS;RMSE

#**************************************************
# Plot the data and add statistics
year <- seq(min(index(SimObs)), max(index(SimObs)+12), by=1)
disch <- round(seq(min(SimObs), max(SimObs+0.1), by=0.1),3)
left <- min(index(SimObs))
up <- max(disch)

plot.ts(SimObs, plot.type = "single", axes=FALSE, ylim=c(0,max(disch)),
        col=c("dodgerblue", "brown"), lwd=2,
        main="SCRP and CFSR discharge simulation with SWAT\nAnjeni (1986-2010)",
        ylab="Discharge [m3/s]", xlab="")
box()
axis(1, at=year, las=2)
axis(2, at=disch, las=2)
abline(v=year, lty=3, col="lightgrey")
abline(h=disch, lty=3, col="lightgrey")
text(left+.5,up, "SSR:", pos=2, cex=.8)
text(left+2, up, round(SSR,3), cex=.8, pos=2)
# text(left+.5, up-0.02, "NS:", pos=2, cex=.8)
# text(left+2, up-0.02, round(NS,3), cex=.8, pos=2)
text(left+.5, up-0.04, "PBIAS:", pos=2, cex=.8)
text(left+2, up-0.04, round(PBIAS, 3), pos=2, cex=.8)
text(left+.5, up-0.06, "RMSE:", cex=.8, pos=2)
text(left+2, up-0.06, round(RMSE, 3), pos=2, cex=.8)
legend("topright", legend=c("SCRP pcp data", "CFSR pcp data"), lty=1, 
       col=c("dodgerblue", "brown"), bty="n", cex=.8)

#*******************************************************************************
#*******************************************************************************
# Maybar


