#------------------------------------------------------
# Package hydroGOF
# is used for: 
# Goodness of fit functions for comparison of simulated and 
# observed hydrological time series
# Look up hydroGOF for more information
#------------------------------------------------------

 library(hydroTSM)
 library(hydroGOF)
 library(zoo)


# load and adapt data to the needs
# when comparing the objects must have the same length

maAllData <- read.csv("~/R/Data/SCRP/maAllData.csv")                             # measured data

ma.disch <- read.zoo(maAllData[,1:2], sep=",", format="%d-%m-%Y")                 # Discharge data
ma.sed <- read.zoo(maAllData[, c(1,3)], sep=",", format="%d-%m-%Y")              # sediment data
ma.prec <- read.zoo(maAllData[, c(1,4)], sep=",", format="%d-%m-%Y")             # precipiation data

ma.disch.obs <- window(ma.disch, start="1986-01-01", end="1993-12-31")           # prepare data window from 86 to 93

# Window of measured discharge data for 1988
ma.d.obs.88 <- window(ma.disch, start="1988-01-01", end="1988-12-31")            # prepare data window for 1988


MaybarSim <- read.csv("~/R/Data/SCRP/Simulations/MaybarSim.csv")                 # simulated data in a data frame
ma.disch.simulated <- read.zoo(MaybarSim, sep=",", format="%d-%m-%Y")            # zoo object with simulated data
ma.disch.sim <- window(ma.disch.simulated, 
                       start="1986-01-01", end="1993-12-31")                     # prepare data window from 86 to 93

# Window of simulated discharge data for 1988
ma.d.sim.88 <- window(ma.disch.simulated, 
                      start="1988-01-01", end="1988-12-31")                      # prepare data window for 1988


#---------------------------------------------------------------
# numerical outputs of data comparison
g <- gof(ma.disch.sim, ma.disch.obs)
g


#---------------------------------------------------------------
# plotting observed vs simulated data
ggof(ma.disch.obs, ma.disch.sim, main="Observation vs Simulation in Maybar", 
     col=c("darkblue", "brown"), lty=c(1,3),lwd=c(2,0.5), ylab="Q [m3]",
     pch=c(46,46), xlab="")       # graphical goodness of fit (ggof)



#------------------------------
# pt.style="ts" --> plotting as line or bar
# ftype="dm" --> o=original, dm=daily ts, ma=daily or monthly

ggof(ma.d.obs.88, ma.d.sim.88, main="Observation vs Simulation, Maybar 1988", 
     col=c("blue", "brown"), lty=c(19,2), pt.style="ts", ylab="Q [m3]",
     ftype="dm", FUN=mean)                                                       # goodness of fit one year
#------------------------------

ggof(ma.disch.obs, ma.disch.sim, main="Observation vs Simulation, Maybar",
     col=c("blue", "darkred"), lty=c(19,2), pt.style="ts", ylab="Q [m3]",
     ftype="dma", FUN=mean)                                                      # goodness of fit all years


# Plotting the two flows individually
plot2(ma.disch.sim, ma.disch.obs, col=c("darkblue", "brown"),
      ylab=c("Observed discharge", "Simulated discharge"),
      lab.tstep="years", main="Maybar observed vs simulated discharge",
      xlab="", cex.axis=1, lty=1, pch="")

# Plotting the two flows on one graph
plot2(ma.disch.sim, ma.disch.obs, col=c("darkblue", "brown"),
      ylab=c("Observed discharge", "Simulated discharge"),
      lab.tstep="years", main="Maybar observed vs simulated discharge",
      xlab="", cex.axis=1, lty=c(1,1), plot.type="single", pch=c(20,20))








###########################################################
###########################################################
# Plot into a PDF file
##########################################################

pdf("2_Maybar_SWAT_Comp.pdf",paper="a4r")
ggof(ma.disch.obs, ma.disch.sim, main="Observation vs Simulation in Maybar", 
     col=c("darkblue", "brown"), lty=c(19,19),lwd=c(2,0.5), ylab="Q [m3]")

ggof(ma.d.obs.88, ma.d.sim.88, main="Observation vs Simulation, Maybar 1988", 
     col=c("blue", "darkred"), lty=c(19,2), pt.style="ts", ylab="Q [m3]",
     ftype="dm", FUN=mean)  

ggof(ma.disch.obs, ma.disch.sim, main="Observation vs Simulation, Maybar",
     col=c("blue", "darkred"), lty=c(19,2), pt.style="ts", ylab="Q [m3]",
     ftype="dma", FUN=mean) 

plot2(ma.disch.sim, ma.disch.obs, col=c("darkblue", "brown"),
      ylab=c("Observed discharge", "Simulated discharge"),
      lab.tstep="years", main="Maybar observed vs simulated discharge",
      xlab="", cex.axis=1, lty=1, pch="")

plot2(ma.disch.sim, ma.disch.obs, col=c("darkblue", "brown"),
      ylab=c("Observed discharge", "Simulated discharge"),
      lab.tstep="years", main="Maybar observed vs simulated discharge",
      xlab="", cex.axis=1, lty=c(1,1), plot.type="single", pch=c(20,20))

dev.off()

##################################################################
# Sink data into text file
##################################################################


# Sink the output from above into a file
sink("2_Output.Maybar_flow.txt", type=c("output", "message"))
cat("Simulated flow with SWAT","\n")
cat("MAYBAR\n")
cat("----------------------------------------\n")
cat("","\n")
cat("Summary of Andit Tid flow simulation\n")
cat("","\n")
smry(MaybarSim)
cat("","\n")
cat("----------------------------------------","\n")
cat("","\n")
cat("Goodness of Fit for observed and simulated data")
cat("","\n")
gof(ma.disch.sim, ma.disch.obs, na.rm=T, do.spearman=T)
cat("","\n")
cat("me              Mean Error\n")
cat("mae             Mean absolute error\n")
cat("rmse            Root mean square error\n")
cat("nrmse           Normalized root mean square error\n")
cat("PBIAS           Percent bias\n")
cat("pbiasfdc        PBIAS in the midsegment of the flow duration curve\n")
cat("RSR             Ratio of RMSE to the standard deviation of observations\n")
cat("rSD             Ratio of standard deviation, rSD=sd(sim)/sd(obs)\n")
cat("NSE             Nash-Sutcliffe Efficiency (-inf <= NSE <= 1\n")
cat("mNSE            Modified Nash-Sutcliffe Efficiency\n")
cat("rNSE            Relative Nash-Sutcliffe Efficiency\n")
cat("d               Index of agreement (0 <= d <= 1\n")
cat("md              Modified index of agreement\n")
cat("rd              relative index of agreement\n")
cat("cp              Persitence index\n")
cat("r               Pearson product moment correclation coefficient (-1 <=r<=1)\n")
cat("r.spearman      Spearman correlation coefficient\n")
cat("R2              Coefficient of determination (0<=R<=1)\n")
cat("bR2             R2 multiplied by the coefficient of the regression line\n")
cat("KGE             Kling-Gupta efficiency between sim and obs\n")
cat("VE              Volumetric efficiency between sim and obs\n")
cat("","\n")
cat("----------------------------------------\n")
cat("","\n")
cat("RMSE only\n")
g["RMSE",]
cat("","\n")
cat("----------------------------------------\n")
cat("","\n")
cat("KGE only\n")
cat("Kling-Gupta efficiency\n")
KGE(ma.disch.sim, ma.disch.obs)
sink()





