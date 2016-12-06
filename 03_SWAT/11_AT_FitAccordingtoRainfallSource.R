#*****************************************************#
# Compare simulated and measured data from SWAT 2012  #
#*****************************************************#
# Author: Vincent Roth                                #
# Date: June 2014                                     #
#*****************************************************#

require(ggplot2)
require(timeSeries)
require(zoo);require(hydroGOF);require(xts)

# Observed data
# obsFolderD <- "E:/Calibration/Anjeni/ScenariosforR/CalibrationwithR/Daily/TxtInOut"
obsFolderM <- "E:/DataSCRP/1_AnditTid/Discharge"

# Simulated data
simFolder <- "E:/ArcSWAT_Examples/3_AnditTid/Scenarios/AT_Monthly_SCRPPCP/TxtInOut"
simFolderNNR <- "E:/ArcSWAT_Examples/3_AnditTid NNR/Scenarios/AT_Monthly_CFSRPCP/TxtInOut"


# read observed data----
setwd(obsFolderM)
Qobs  <- read.table("AT_Discharge_Mon.csv", sep=",", 
                    header=TRUE, na.strings="NA")                                # obsdata needs to be copied to the folder
Qobs <- Qobs[,2]
Qobs.ts <- ts(Qobs, start=1982, freq=12)
Qobs <- as.numeric(window(Qobs.ts, start=1986, end=2010.97))

# read simulated data----
setwd(simFolder)
Qsim1 <- read.table("output.rch", skip=9)
subsim <- subset(Qsim1, V4<13)                     # remove the yearly sum
row.names(subsim) <- NULL
Qsim <- subsim[subsim$V2==2,7]                     # Select data from column 7 for each V2==2
Qsim <- Qsim[1:300]                                   
#Qsim <- Qsim*2592000
#Qsim1 <- read.table("output.rch")[,]
Qsim.ts <- ts(Qsim, start=1986, freq=12)

# read simulated NNR data
setwd(simFolderNNR)
Qsim2 <- read.table("output.rch", skip=9)
subsim2 <- subset(Qsim2, V4<13)                     # remove the yearly sum
row.names(subsim2) <- NULL
Qsim2 <- subsim2[subsim2$V2==2,7]                     # Select data from column 7 for each V2==2
Qsim2 <- Qsim2[1:300]                                   
#Qsim <- Qsim*2592000
#Qsim1 <- read.table("output.rch")[,]
Qsim2.ts <- ts(Qsim2, start=1986, freq=12)


#*******************************************************************************
# Step 3: Calculate model performance statistics
# Merge the TS first

Data <- as.data.frame(cbind(Qobs, Qsim, Qsim2))
names(Data) <- c("Qobs", "Qsim", "QSim2")


SSR <- sum((Data$Qsim-Data$Qobs)^2, na.rm=TRUE)                # Sum of squares due to Regression
NS <- 1-(SSR/(sum((Data$Qobs-mean(Data$Qobs))^2)))             # Nash Sutcliffe Efficiency
PBIAS <- 100*(sum(Data$Qsim)-sum(Data$Qobs))/sum(Data$Qobs)    # Percent Bias (average tendency of sim values to be </> than obs values)
RMSE <- rmse(Data$Qsim, Data$Qobs, na.rm=TRUE)                 # Root mean square error 
SSR;NS;PBIAS;RMSE

#*******************************************************************************
# Step 4: Plotting Obs vs. Simulated relationship----
Qo1 <- Qobs
Qs1 <- Qsim
vectory <- c(0,1.1*max(max(Qs1),max(Qo1, na.rm=TRUE)))
vectorx <- c(0,length(Qs1))
dates <- c("1984","1985",
           "1986","1987","1988","1989",
           "1990","1991","1992","1993",
           "1994","1995","1996","1997",
           "1998","1999","2000","2001",
           "2002","2003","2004","2005",
           "2006","2007","2008","2009",
           "2010")                                         
yrs <- seq(1984,2010,1)
date.ticks <- sapply(0:(length(yrs)-1), function(i) {i * 12})
up <- max(vectory)
right <- max(vectorx)-50
left <- min(vectorx)+5

par(mar=c(5,5,4,2))
plot(vectorx,vectory,type="n",xlab="", ylim=c(0,max(vectory)+0.01),
     ylab=expression("Q ["*m^3*s^{-1}*"]"), axes=F,
     main="SWAT simulated discharge vs. measured SCRP discharge")
axis(1,at=date.ticks,labels=dates, las=2)
axis(2,at=NULL,labels=NULL)
grid(nx=500,ny=500,col="lightgrey",lty="solid")
lines(Qo1,lwd=1, col="cornflowerblue")
lines(Qs1,lwd=1,col="red3")
abline(v=date.ticks, lty=3)
box(which='plot')
legend(right, up, legend=c("observed","simulated"), cex=0.8,
       lty=c(1,1), ,lwd=c(1,1),col=c("cornflowerblue", "red3"),
       y.intersp=0.7, bty="n")
text(left,up, "SSR:", pos=2, cex=0.8)
text(left+20,up, round(SSR,3), cex=0.8, pos=2)
text(left, up-0.05,"NS.:", cex=0.8, pos=2)
text(left+20, up-0.05, round(NS,3), cex=0.8,pos=2)
text(left, up-0.1, "PBias:", cex=0.8, pos=2)
text(left+20, up-0.1, round(PBIAS,3),cex=0.8, pos=2)
text(left, up-0.15, "RMSE:", cex=0.8, pos=2)
text(left+20, up-0.15, round(RMSE,3),cex=0.8, pos=2)

#***********************
#*******************************************************************************
# Added own Plotting
Q.obs <- as.ts(Qobs, start=1984, freq=12)
Q.simu <- as.ts(Qsim, start=1984, freq=12)
Q.obs.z <- ts(Qobs, start=1984,  freq=12)
Q.simu.z <- ts(Qsim, start=1984,  freq=12)

# Plot scatterplot
obs.z <- window(Q.obs.z, start=1984, end=2007)
sim.z <- window(Q.simu.z, start=1984, end=2007)

linReg <- lm(sim.z~obs.z, na.rm=TRUE)
par(mar=c(5,5,4,2))
plot(Q.obs ~ Q.simu, ylim=c(0,0.8), xlim=c(0,0.8),
     ylab=expression("Measured Q ["*m^3*s^{-1}*"]"), 
     xlab=expression("Simulated Q ["*m^3*s^{-1}*"]"),
     main="Measured vs. simulated discharge, Anjeni")
m <- summary(linReg)
text(0.6, 0.5, expression(R^2 ~ ":"), pos=2, cex=0.8)
text(0.6,0.5, round(m$r.squared,4),cex=0.8, pos=4)
text(0.6, 0.47, "Sigma:", cex=0.8, pos=2)
text(0.6, 0.47, round(m$sigma,4),cex=0.8, pos=4)
grid(nx=NULL, ny=NULL)
abline(linReg, col="red", lty=3)

# Plot Time series
par(mar=c(5,5,4,2))
plot(Q.obs.z, type="l", col="cornflowerblue",
     xlab="", ylab=expression("Measured Q ["*m^3*s^{-1}*"]"),
     main="Plotting daily data", lwd=2,
     ylim=c(0,max(max(Q.obs.z))))
points(Q.simu.z, type="l", col="red3", lwd=1, lty=3)
grid(nx=NULL, ny=NULL)


# Plotting sediment loss in the catchment #####
#******************************************************************************
# Plotting sediment loss in the catchment

inFoldSim <- "E:/ArcSWAT_Examples/1_Anjeni NNR/Scenarios/AJ_84-2010_monthly_NYSKIP2/TxtInOut"
inFoldObs <- "E:/DataSCRP/2_Anjeni/Sediment/"


# Step 2: Read observed and simulated data
setwd(inFoldObs)
Sedo  <- read.table("sedloss.monthly.txt")[,2]              # obsdata needs to be copied to the folder
sedo.ts <- ts(Sedo, start=c(1984,1,1), end=c(1993,12,31),freq=12)

setwd(inFoldSim)
Seds1 <- read.table("output.rch", skip=9)
Seds <- Seds1[Seds1$V2==19,11]                            # Select data from column 7 for each V2==15

seds.ts <- ts(Seds, start=c(1986,1,1), end=c(1993,12,31), freq=12)
dates <- as.character(seq(1985,2010,1))                                          # dates as character
yrs <- seq(1985,2010,1)
date.ticks <- sapply(0:(length(yrs)-1), function(i) {i * 12})

months <- month.abb
mth <- seq(1,12,1)
linreg.sed <- lm(seds.ts ~ sedo.ts)


plot(seds.ts, col="darkblue",lty=1,type="l",
     ylab="Sediment loss [t]", xlab="",
     main="Sediment loss at Anjeni Station",
     ylim=c(0,2000))
points(sedo.ts, type="l", col="brown")
legend(1986,2000, legend=c("simulated (SWAT)", "observed (SCRP)"), 
       seg.len=1, col=c("darkblue", "brown"), lty=c(1,1), cex=0.7, bty="n")
text(1994,2000, )
abline(v=dates, col="black", lty=3)
grid(nx=NULL, ny=NULL)

#*******************************
# Plotting sediment loss per year on one single plot
par(mfrow=c(2,4))
for(i in yrs){
   max.y <- (max(max(window(sedo.ts,start=i,end=i+1),na.rm=T),
                 max(window(seds.ts, start=i, end=i+1),na.rm=T)))
   plot(window(sedo.ts, start=i, end=i+1), col="darkblue", 
        ylim=c(0,max.y), xlab="",ylab="Sediment loss [t]",
        main=paste("Sediment loss at Anjeni\n",i))
   points(window(seds.ts, start=i, end=i+1), col="brown",
          type="l")
   legend(i, max.y, legend=c("simulated (SWAT)", "observed (SCRP)"),
          seg.len=1,
          col=c("darkblue", "brown"), lty=c(1,1), cex=0.8, bty="n")
   grid(nx=NULL, ny=NULL)
}
par(mfrow=c(1,1))

#****************************
# each year on a single plot
for(i in yrs){
   max.y <- (max(max(window(sedo.ts,start=i,end=i+1),na.rm=T),
                 max(window(seds.ts, start=i, end=i+1),na.rm=T)))
   plot(window(sedo.ts, start=i, end=i+1), col="darkblue", 
        ylim=c(0,max.y), xlab="",ylab="Sediment loss [t]",
        main=paste("Sediment loss at Anjeni\n",i),
        type="h", lwd=2)
   points(window(seds.ts, start=i, end=i+1), col="brown",
          type="h", lwd=2)
   legend(i, max.y, legend=c("simulated (SWAT)", "observed (SCRP)"),
          seg.len=1, col=c("darkblue", "brown"), lty=c(1,1), cex=0.8, bty="n")
   grid(nx=NULL, ny=NULL)
}



# Export to PDF ################################################################
#############################################################################
# Export to pdf file
#**********************
# Destination File
# pdf("E:/Calibration/Anjeni/calibration.pdf", title="Calibration",paper="a4r", 
#     onefile=T, height=12, width=14)
# 
# #******************************************
# # Plot all the years on one plot
# Qo1 <- Qo[c(1:2556)];
# Qs1 <- Qs[c(1:2556)];
# vectory <- c(0,1.1*max(max(Qs1),max(Qo1)))
# vectorx <- c(0,length(Qs1))
# dates <- c("1986","1987","1988","1989",
#            "1990","1991","1992","1993")                     # ,"1994","1995","1996","1997"
# yrs <- c(1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997)           # 
# date.ticks<-c(1,365,731,1096,1460,1826,2191,2556)           # ,2920,3285,3650,4015
# par(mar=c(5,5,4,2))
# plot(vectorx,vectory,type="n",xlab="",
#      ylab=expression("Q ["*m^3*s^{-1}*"]"),axes=F,
#      main="SWAT simulated discharge vs. measured SCRP discharge")
# axis(1,at=date.ticks,labels=dates)
# axis(2,at=NULL,labels=NULL)
# grid(nx=500,ny=500,col="lightgrey",lty="solid")
# lines(Qo1,lwd=2, col="cornflowerblue")
# lines(Qs1,lwd=1,col="red3")
# abline(v=date.ticks, lty=3)
# box(which='plot')
# legend(0, 0.7, legend=c("observed", "simulated"), cex=0.8,
#        lty=c(1,1), ,lwd=c(2,1),col=c("cornflowerblue", "red3"))
# text(2300,0.68, "Sum of squares due to regression:", pos=2, cex=0.8)
# text(2470,0.68, round(SSR,3), cex=0.8, pos=2)
# text(2300, 0.66,"Nash-Sutcliffe Efficiency:", cex=0.8, pos=2)
# text(2470,0.66, round(NS,3), cex=0.8,pos=2)
# text(2300, 0.64, "Percent bias:", cex=0.8, pos=2)
# text(2470, 0.64, round(PBIAS,3),cex=0.8, pos=2)
# text(2300, 0.62, "Root mean square error:", cex=0.8, pos=2)
# text(2470, 0.62, round(RMSE,3),cex=0.8, pos=2)
# 
# 
# # Plotting to single Year ####
# #******************************************************************************
# # Plotting single years
# for(i in yrs){
#    SSR.j <- sum((Qs.ts-Qo.ts)^2)                      # Sum of squares due to Regression
#    NS.j <- 1-(SSR/(sum((Qo.ts-mean(Qo.ts))^2)))       # Nash Sutcliffe Efficiency
#    PBIAS.j <- 100*sum(Qs.ts)-sum(Qo.ts)/sum(Qo.ts)    # Percent Bias (average tendency of sim values to be </> than obs values)
#    RMSE.j <- rmse(Qs.ts, Qo.ts)                       # Root mean square error 
#    SSR.j;NS.j;PBIAS.j;RMSE.j
#    
#    lmD <- lm(window(Qs.ts, start=i, end=i+1)~window(Qo.ts,start=i, end=i+1))
#    m <- summary(lmD)
#    m.df <- m[[4]]
#    maxY <- max(max(window(Qs.ts,start=i, end=i+1),na.rm=T),
#                max(window(Qo.ts, start=i,end=i+1),na.rm=T))
#    #    xD <- window(Qs.ts, start=i,end=i+1)
#    #    yD <- window(Qo.ts, start=i, end=i+1)
#    #    corXY <- cor(xD,yD)
#    plot(window(Qs.ts, start=i, end=i+1), col="cornflowerblue",
#         main=paste("Discharge Anjeni\n",i),
#         xlab="", ylab=expression("Q ["*m^3*s^{-1}*"]"),
#         ylim=c(0,0.8))                                   # if all plots should be same height
#    #         ylim=c(0,maxY))                             # if all plots should adapt their height
#    points(window(Qo.ts, start=i, end=i+1),col="red3",
#           type="l", lwd=2)
#    grid(nx=NULL, ny=NULL)
#    legend(i, 0.8, legend=c("simulated (SWAT)", "observed (SCRP)"),
#           col=c("cornflowerblue", "red3"), lty=c(1,1), cex=0.8,bty="n",
#           lwd=c(1,2))
#    text(i+0.8,0.8,expression(R^2 ~ ":"),pos=2,cex=0.8)
#    text(i+0.8,0.8,round(m$r.squared,4),pos=4,cex=0.8)
#    text(i+0.8,0.78,expression("Correlation:"),pos=2,cex=0.8)
#    text(i+0.8,0.78, round(cor(window(Qs.ts, start=i, end=i+1),
#                               window(Qo.ts, start=i, end=i+1)),4),
#         pos=4,cex=0.8)
#    text(i+0.8, 0.76, "Sum of squares:", cex=0.8, pos=2)
#    text(i+0.8, 0.76, round(SSR.j,4), cex=0.8, pos=4)
#    text(i+0.8, 0.74, "Nash Sutcliffe Efficiency:", cex=0.8, pos=2)
#    text(i+0.8, 0.74, round(NS.j,4), cex=0.8,pos=4)
#    text(i+0.8, 0.72, "Percent Bias:", cex=0.8, pos=2)
#    text(i+0.8, 0.72, round(PBIAS.j,4), cex=0.8,pos=4)
#    text(i+0.8, 0.70, "Root Mean Square Error:", cex=0.8, pos=2)
#    text(i+0.8, 0.70, round(RMSE.j,4), cex=0.8,pos=4)
# }
# dev.off()


