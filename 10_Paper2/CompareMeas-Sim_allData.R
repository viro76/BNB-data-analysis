#*****************************************************#
# Compare simulated and measured data from SWAT 2012  #
#*****************************************************#
# Author: Vincent Roth                                #
# Date: August 2013                                   #
#*****************************************************#

require(ggplot2) 
library(timeSeries)
require(zoo)
require(hydroGOF)
require(xts)

obsFolder <- "~/R/Data/SWAT-Cup/2_Paper2/"


# read observed data----
setwd(obsFolder)
QobsTable  <- read.table("9_WLRC_All_behFLWOut1.txt", sep="\t", 
                    header=TRUE, na.strings="NA")                                # obsdata needs to be copied to the folder
Qobs <- QobsTable[,2]
Qobs.ts <- ts(Qobs, start=1986, freq=12)

# read simulated data for both variants----
QsimWLRC <- QobsTable[,5]
QsimWLRC.ts <- ts(QsimWLRC, start=1986, freq=12)

QsimFAO <- read.table("9_FAO_All_behFLWOut1.txt", sep="\t", header=TRUE, na.strings="NA")
QsimFAO <- QsimFAO[,5]
QsimFAO.ts <- ts(QsimFAO, start=1986, freq=12)
#***************************************************************************************************
# 
# Qsim1 <- read.table("output.rch", skip=9)
# subsim <- subset(Qsim1, V4<13)                     # remove the yearly sum
# row.names(subsim) <- NULL
# Qsim <- subsim[subsim$V2==19,7]                    # Select data from column 7 for each V2==2
# Qsim <- Qsim[1:300]                                   
# #Qsim <- Qsim*2592000
# #Qsim1 <- read.table("output.rch")[,]
# Qsim.ts <- ts(Qsim, start=1986, freq=12)

#***************************************************************************************************

#*******************************************************************************
# Step 3: Calculate model performance statistics
# Merge the TS first

Data <- merge(Qobs, QsimWLRC, QsimFAO)
names(Data) <- c("Qobs", "QsimWLRC", "QsimFAO")

simNA <- na.omit(QsimWLRC)
simNAFAO <- na.omit(QsimFAO)
obsNA <- na.omit(Qobs)

SSR <- sum((simNA-obsNA)^2)                   # Sum of squares due to Regression
NS <- 1-(SSR/(sum(obsNA-mean(obsNA))^2))                  # Nash Sutcliffe Efficiency
PBIAS <- 100*sum(simNA)-sum(obsNA)/sum(obsNA)          # Percent Bias (average tendency of sim values to be </> than obs values)
RMSE <- rmse(simNA, obsNA)                         # Root mean square error 
SSR;NS;PBIAS;RMSE

# FAO
SSRFAO <- sum((simNAFAO-obsNA)^2)                   # Sum of squares due to Regression
NSFAO <- 1-(SSR/(sum(obsNA-mean(obsNA))^2))                  # Nash Sutcliffe Efficiency
PBIASFAO <- 100*sum(simNAFAO)-sum(obsNA)/sum(obsNA)          # Percent Bias (average tendency of sim values to be </> than obs values)
RMSEFAO <- rmse(simNAFAO, obsNA)                         # Root mean square error 
SSRFAO;NSFAO;PBIASFAO;RMSEFAO

#*******************************************************************************
# Step 4: Plotting Obs vs. Simulated relationship----
Qo1 <- Qobs
Qs1 <- QsimWLRC
vectory <- c(0,1.1*max(max(Qs1, na.rm=TRUE),max(Qo1, na.rm=TRUE)))
vectorx <- c(0,length(Qs1))
dates <- c("1986","1987","1988","1989",
           "1990","1991","1992","1993",
           "1994","1995","1996","1997",
           "1998","1999","2000","2001",
           "2002","2003","2004","2005",
           "2006","2007","2008","2009",
           "2010","2011","2012","2014")
yrs <- seq(1986,2014,1)
date.ticks <- sapply(0:(length(yrs)-1), function(i) {i * 12})
up <- max(vectory)-0.05


#***********************
#*******************************************************************************
# Plot regression----
Q.obs <- as.ts(Qobs, start=1986, freq=12)
Q.simu <- as.ts(QsimWLRC, start=1986, freq=12)
# FAO data
Q.simuFAO <- as.ts(QsimFAO, start=1986, freq=12)
# Q.obs.z <- ts(Qobs, start=1986,  freq=12)
# Q.simu.z <- ts(QsimWLRC, start=1986,  freq=12)
# Plot scatterplot
linReg <- lm(Q.obs~Q.simu)
linRegFAO <- lm(Q.obs~Q.simuFAO)
m <- summary(linReg)
m2 <- summary(linRegFAO)
rtwo <- round(m$r.squared,4)
lm_coef <- round(coef(linReg), 3) # extract coefficients
rFAO <- round(m$r.squared, 4)
lm_coefFAO <- round(coef(linRegFAO), 3)

# Plot all data
par(mar=c(5,6,4,2))
plot(Q.obs ~ Q.simu, ylim=c(0,0.2), xlim=c(0,0.2), pch=19, 
     ylab=expression("Measured Q ["*m^3*s^{-1}*"]"), col="#3182bd",
     xlab=expression("Simulated Q ["*m^3*s^{-1}*"]"),
     main="Discharge simulations, Anjeni")

text(0.04, 0.19, bquote(paste(R^2 ~ ": " ~ .(rtwo))), pos=2, cex=.8,col="#3182bd")
text(0.04, 0.18, bquote(paste("SSR: " ~ .(round(SSR,4)))), cex=0.8, pos=2,col="#3182bd")
text(0.04, 0.17, bquote(paste("RMSE: " ~ .(round(RMSE,4)))), cex=0.8, pos=2,col="#3182bd")
text(0.04, 0.16, bquote(paste("PBIAS: " ~ .(round(PBIAS,4)))), cex=0.8, pos=2,col="#3182bd")
text(0.17, 0.02, bquote(y == .(lm_coef[2])*x + .(lm_coef[1])), cex=.8, pos=2,col="#3182bd")

text(0.08, 0.19, bquote(paste(R^2 ~ ": " ~ .(rFAO))), pos=2, cex=.8,col="#31a354")
text(0.08, 0.18, bquote(paste("SSR: " ~ .(round(SSRFAO,4)))), cex=0.8, pos=2,col="#31a354")
text(0.08, 0.17, bquote(paste("RMSE: " ~ .(round(RMSEFAO,4)))), cex=0.8, pos=2,col="#31a354")
text(0.08, 0.16, bquote(paste("PBIAS: " ~ .(round(PBIASFAO,4)))), cex=0.8, pos=2,col="#31a354")
text(0.17, 0.01, bquote(y == .(lm_coefFAO[2])*x + .(lm_coef[1])), cex=.8, pos=2,col="#31a354")

# Add FAO data
points(Q.obs ~ Q.simuFAO, ylim=c(0,0.2), xlim=c(0,0.2), pch=19,col="#31a354")
grid(nx=NULL, ny=NULL)
points(Q.obs ~ Q.simu, ylim=c(0,0.2), xlim=c(0,0.2), pch=19,col="#3182bd")
abline(linReg, col="brown", lty=2, lwd=2)
abline(linRegFAO, col="#31a354", lty=2, lwd=2)
abline(0,1, col="darkgray", lty=4, lwd=2)
text(0.1433, 0.1581, "1:1 line", cex=.8, col="darkgray")
text(0.1773, 0.1359, "regression line WLRC", cex=.8, col="brown")
text(0.1773, 0.1950, "regression line FAO", cex=.8, col="#31a354")





# points(Q.obs ~ Q.simu, ylim=c(0,0.2), xlim=c(0,0.2), pch=19,col="lightblue")




# Plotting sediment loss in the catchment #####
#******************************************************************************

# Step 2: Read observed and simulated data
Sedobser  <- read.table("9_WLRC_All_behSEDOut.txt", sep="\t", na.strings = "NA", header=TRUE)              # obsdata needs to be copied to the folder
sedo.ts <- ts(Sedobser[,2], start=c(1986,1,1), end=c(2014,12,31),freq=12)

# data
Sedobs <- Sedobser[,2]
SedsWLRC <- Sedobser[,5]                          # Select data from column 7 for each V2==15
SedsFAO <- read.table("9_FAO_All_behSEDOut1.txt", sep="\t", na.strings = "NA", header=TRUE)
SedsFAO <- SedsFAO[,5]

#********************************
# Calculate Statistics sediment----
simNA <- na.omit(SedsWLRC)
simNAFAO <- na.omit(SedsFAO)
obsNA <- na.omit(Sedobs)

SSR <- sum((simNA-obsNA)^2)                   # Sum of squares due to Regression
NS <- 1-(SSR/(sum(obsNA-mean(obsNA))^2))                  # Nash Sutcliffe Efficiency
PBIAS <- 100*sum(simNA)-sum(obsNA)/sum(obsNA)          # Percent Bias (average tendency of sim values to be </> than obs values)
RMSE <- rmse(simNA, obsNA)                         # Root mean square error 
SSR;NS;PBIAS;RMSE

# FAO
SSRFAO <- sum((simNAFAO-obsNA)^2)                   # Sum of squares due to Regression
NSFAO <- 1-(SSR/(sum(obsNA-mean(obsNA))^2))                  # Nash Sutcliffe Efficiency
PBIASFAO <- 100*sum(simNAFAO)-sum(obsNA)/sum(obsNA)          # Percent Bias (average tendency of sim values to be </> than obs values)
RMSEFAO <- rmse(simNAFAO, obsNA)                         # Root mean square error 
SSRFAO;NSFAO;PBIASFAO;RMSEFAO

#*******************************
# Plotting linear regression
# Plot regression----
S.obs <- as.ts(Sedobs, start=1986, freq=12)
S.simuWLRC <- as.ts(SedsWLRC, start=1986, freq=12)
# FAO data
S.simuFAO <- as.ts(SedsFAO, start=1986, freq=12)
# Q.obs.z <- ts(Qobs, start=1986,  freq=12)
# Q.simu.z <- ts(QsimWLRC, start=1986,  freq=12)
# Plot scatterplot
linReg <- lm(Sedobs~SedsWLRC)
linRegFAO <- lm(Sedobs~SedsFAO)
m <- summary(linReg)
m2 <- summary(linRegFAO)
rtwo <- round(m$r.squared,4)
lm_coef <- round(coef(linReg), 3) # extract coefficients
rFAO <- round(m2$r.squared, 4)
lm_coefFAO <- round(coef(linRegFAO), 3)

# Plot all data
par(mar=c(5,6,4,2))
plot(S.obs ~ S.simuWLRC, ylim=c(0,2000), xlim=c(0,2000), pch=19, 
     ylab=expression("Measured sediment ["*t*"*"*ha^{-1}*"]"), col="#3182bd",
     xlab=expression("Simulated sediment ["*t*"*"*ha^{-1}*"]"),
     main="Sediment loss, Anjeni")

text(300, 2000, bquote(paste(R^2 ~ ": " ~ .(rtwo))), pos=2, cex=.8,col="#3182bd")
text(300, 1950, bquote(paste("SSR: " ~ .(round(SSR,4)))), cex=0.8, pos=2,col="#3182bd")
text(300, 1900, bquote(paste("RMSE: " ~ .(round(RMSE,4)))), cex=0.8, pos=2,col="#3182bd")
text(300, 1850, bquote(paste("PBIAS: " ~ .(round(PBIAS,4)))), cex=0.8, pos=2,col="#3182bd")
text(1765, 267, bquote("Regression line" ~ y == .(lm_coef[2])*x + .(lm_coef[1])), cex=.8, pos=2,col="#3182bd")

text(650, 2000, bquote(paste(R^2 ~ ": " ~ .(rFAO))), pos=2, cex=.8,col="#31a354")
text(650, 1950, bquote(paste("SSR: " ~ .(round(SSRFAO,4)))), cex=0.8, pos=2,col="#31a354")
text(650, 1900, bquote(paste("RMSE: " ~ .(round(RMSEFAO,4)))), cex=0.8, pos=2,col="#31a354")
text(650, 1850, bquote(paste("PBIAS: " ~ .(round(PBIASFAO,4)))), cex=0.8, pos=2,col="#31a354")
text(1765, 217, bquote("Regression line" ~ y == .(round(lm_coefFAO[2],2))*x + .(lm_coef[1])), cex=.8, pos=2,col="#31a354")

# Add FAO data
points(Sedobs ~ S.simuFAO, ylim=c(0,2000), xlim=c(0,2000), pch=19,col="#31a354")
grid(nx=NULL, ny=NULL)
# points(Sedobs ~ S.simuFAO, ylim=c(0,2000), xlim=c(0,2000), pch=19,col="#3182bd")
abline(linReg, col="#3182bd", lty=2, lwd=2)
abline(linRegFAO, col="#31a354", lty=2, lwd=2)
abline(0,1, col="darkgray", lty=4, lwd=2)
text(1460, 1592, "1:1 line", cex=.8, col="darkgray")
text(1724, 1946, "regression line WLRC", cex=.8, col="#3182bd")
text(1708, 1213, "regression line FAO", cex=.8, col="#31a354")
























