#****************************************
# Run SWAT in R
# Script from Kazi Rahman
# University of Geneva
#****************************************
# Adapted:     VR
# Date:        2013-03-14
#****************************************

require(zoo)
require(hydroGOF)
require(xts)

setwd("E:/Calibration/Anjeni/ScenariosforR/CalibrationwithR/Daily/TxtInOut")



# Step 1: Execute SWAT
# system("swat2012.exe")                       # SWAT 2012
# system("rev528_64_rel.exe")               # SWAT 2009

                                                  

# Step 2: Read observed and simulated data
Qobs  <- read.table("obsdata.daily.txt")[,4]       # obsdata needs to be copied to the folder
Qobs.ts <- ts(Qobs, start=1986, freq=365)

Qsim1 <- read.table("output.rch", skip=9)
Qsim <- Qsim1[Qsim1$V2==15,7]                         # Select data from column 7 for each V2==15
Qsim <- Qsim*2592000
#Qsim1 <- read.table("output.rch")[,]
Qsim.ts <- ts(Qsim, start=1986, freq=365)




# Step 3: Calculate model performance statistics
SSR <- sum((Qsim-Qobs)^2)                              # Sum of squares due to Regression
NS <- 1-(SSR/(sum((Qobs-mean(Qobs))^2)))               # Nash Sutcliffe Efficiency
PBIAS <- 100*sum(Qsim)-sum(Qobs)/sum(Qobs)               # Percent Bias (average tendency of sim values to be </> than obs values)
RMSE <- rmse(Qsim, Qobs)                               # Root mean square error 
SSR;NS;PBIAS;RMSE

# Step 4: Plotting Obs vs. Simulated relationship
Qo1 <- Qobs#[c(1:2556)];
Qs1 <- Qsim#[c(1:2556)];
vectory <- c(0,1.1*max(max(Qs1),max(Qo1)))
vectorx <- c(0,length(Qs1))
dates <- c("1986","1987","1988","1989",
           "1990","1991","1992","1993",
           "1994","1995","1996","1997")                                         # ,"1994","1995","1996","1997"
yrs <- c(1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997)           # 
date.ticks<-c(1,365,731,1096,1460,1826,2191,2556,2920,3285,3650,4015)           # ,2920,3285,3650,4015
par(mar=c(5,5,4,2))
plot(vectorx,vectory,type="n",xlab="",
     ylab=expression("Q ["*m^3*s^{-1}*"]"),axes=F,
     main="SWAT simulated discharge vs. measured SCRP discharge")
axis(1,at=date.ticks,labels=dates)
axis(2,at=NULL,labels=NULL)
grid(nx=500,ny=500,col="lightgrey",lty="solid")
lines(Qs1,lwd=1, col="cornflowerblue")
lines(Qo1,lwd=2,col="red3")
abline(v=date.ticks, lty=3)
box(which='plot')
legend(0, 0.7, legend=c("simulated","observed"), cex=0.8,
       lty=c(1,1), ,lwd=c(2,1),col=c("cornflowerblue", "red3"))
text(2300,0.68, "Sum of squares due to regression:", pos=2, cex=0.8)
text(2470,0.68, round(SSR,3), cex=0.8, pos=2)
text(2300, 0.66,"Nash-Sutcliffe Efficiency:", cex=0.8, pos=2)
text(2470,0.66, round(NS,3), cex=0.8,pos=2)
text(2300, 0.64, "Percent bias:", cex=0.8, pos=2)
text(2470, 0.64, round(PBIAS,3),cex=0.8, pos=2)
text(2300, 0.62, "Root mean square error:", cex=0.8, pos=2)
text(2470, 0.62, round(RMSE,3),cex=0.8, pos=2)

#***********************
# Plot single years
# par(mfrow=c(2,4)

par(mar=c(5,5,4,2))
for(i in yrs){
   SSR.j <- sum((Qsim.ts-Qobs.ts)^2)                      # Sum of squares due to Regression
   NS.j <- 1-(SSR/(sum((Qobs.ts-mean(Qobs.ts))^2)))       # Nash Sutcliffe Efficiency
   PBIAS.j <- 100*sum(Qsim.ts)-sum(Qobs.ts)/sum(Qobs.ts)  # Percent Bias (average tendency of sim values to be </> than obs values)
   RMSE.j <- rmse(Qsim.ts, Qobs.ts)                       # Root mean square error 
   SSR.j;NS.j;PBIAS.j;RMSE.j
   
   lmD <- lm(window(Qsim.ts, start=i, end=i+1)~window(Qobs.ts,start=i, end=i+1))
   m <- summary(lmD)
   m.df <- m[[4]]
   maxY <- max(max(window(Qsim.ts,start=i, end=i+1),na.rm=T),
               max(window(Qobs.ts, start=i,end=i+1),na.rm=T))
#    xD <- window(Qs.ts, start=i,end=i+1)
#    yD <- window(Qo.ts, start=i, end=i+1)
#    corXY <- cor(xD,yD)
   plot(window(Qsim.ts, start=i, end=i+1), col="cornflowerblue",
        main=paste("Discharge Anjeni\n",i),
        xlab="", ylab=expression("Q ["*m^3*s^{-1}*"]"),
        ylim=c(0,0.8))                                                           # if all plots should be same height
#         ylim=c(0,maxY))                                                        # if all plots should adapt their height
   points(window(Qobs.ts, start=i, end=i+1),col="red3",
          type="l", lwd=2)
   grid(nx=NULL, ny=NULL)
   legend(i, 0.8, legend=c("simulated (SWAT)", "observed (SCRP)"),
          col=c("cornflowerblue", "red3"), lty=c(1,1), cex=0.8,bty="n",
          lwd=c(1,2))
   text(i+0.8,0.8,expression(R^2 ~ ":"),pos=2,cex=0.8)
   text(i+0.8,0.8,round(m$r.squared,4),pos=4,cex=0.8)
   text(i+0.8,0.78,expression("Correlation:"),pos=2,cex=0.8)
   text(i+0.8,0.78, round(cor(window(Qsim.ts, start=i, end=i+1),
                          window(Qobs.ts, start=i, end=i+1)),4),
        pos=4,cex=0.8)
   text(i+0.8, 0.76, "Sum of squares:", cex=0.8, pos=2)
   text(i+0.8, 0.76, round(SSR.j,4), cex=0.8, pos=4)
   text(i+0.8, 0.74, "Nash Sutcliffe Efficiency:", cex=0.8, pos=2)
   text(i+0.8, 0.74, round(NS.j,4), cex=0.8,pos=4)
   text(i+0.8, 0.72, "Percent Bias:", cex=0.8, pos=2)
   text(i+0.8, 0.72, round(PBIAS.j,4), cex=0.8,pos=4)
   text(i+0.8, 0.70, "Root Mean Square Error:", cex=0.8, pos=2)
   text(i+0.8, 0.70, round(RMSE.j,4), cex=0.8,pos=4)
}




#*******************************************************************************
# Added own Plotting
Q.meas <- as.ts(Qobs, start=1986, freq=365)
Q.simu <- as.ts(Qsim, start=1986, freq=365)
Q.meas.z <- ts(Qobs, start=1986, freq=365)
Q.simu.z <- ts(Qsim, start=1986, freq=365)
# Plot scatterplot
linReg <- lm(Q.meas~Q.simu)
par(mar=c(5,5,4,2))
plot(Q.meas ~ Q.simu, ylim=c(0,0.8), xlim=c(0,0.8),
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
plot(Q.meas.z, type="l", col="cornflowerblue",
     xlab="", ylab=expression("Measured Q ["*m^3*s^{-1}*"]"),
     main="Plotting daily data", lwd=2,
     ylim=c(0,0.7))
points(Q.simu.z, type="l", col="red3", lwd=1, lty=3)
grid(nx=NULL, ny=NULL)

#*******************************************************************************
# Monthly plotting
Qo.mo <- read.table("E:/Calibration/Anjeni/Scenarios/AJ84-97-monthly-limited/TxtInOut/obsdata.monthly.txt")[,3]      # obsdata for monthly simulation

Qs.mo.1 <- read.table("E:/Calibration/Anjeni/Scenarios/AJ84-97-monthly-limited/TxtInOut/output.rch", skip=9)
Qs.mo <- Qs.mo.1[Qs.mo.1$V2==15,7]

EveryThird <- c(13, 26, 39, 52, 65, 78, 91, 104, 117, 130, 143, 156, 169)
# list13 <- seq(13,length(Qo.mo),13)        # every 13th element is the year --> has to be remove
# rem_13 <- function(x) x[-c(list13)]
# Qs.mo <- rem_13(Qs.mo)
Qs.mo <- Qs.mo[-c(EveryThird)]


# Stats
SSR.mo <- sum((Qs.mo-Qo.mo)^2)  
NS.mo <- 1-(SSR.mo/(sum((Qo.mo-mean(Qo.mo))^2)))      # Nash Sutcliffe Efficiency
PBIAS.mo <- 100*sum(Qs.mo)-sum(Qo.mo)/sum(Qo.mo)      # Percent Bias (average tendency of sim values to be </> than obs values)
RMSE.mo <- rmse(Qs.mo, Qo.mo)                         # Root mean square error 
SSR.mo;NS.mo;PBIAS.mo;RMSE.mo

# Plot monthly data
Qo1.mo <- Qo.mo[c(1:84)];
Qs1.mo <- Qs.mo[c(1:84)];
vectory <- c(0,1.1*max(max(Qs1.mo),max(Qo1.mo)))
vectorx <- c(0,length(Qs1.mo))
dates <- c("1986","1987","1988","1989",
           "1990","1991","1992","1993")               # "1994","1995","1996","1997","1998"

date.ticks<-seq(0,84,12)           
par(mar=c(5,5,4,2))
plot(vectorx,vectory,type="n", ylim=c(0,0.15),
     xlab="Month",
     ylab=expression("Measured Discharge ["*m^3*s^{-1}*"]"),
     axes=F,
     main="SWAT simulated discharge vs. measured SCRP discharge\n monthly data")
axis(1,at=date.ticks,labels=dates)
axis(2,at=NULL,labels=NULL)
grid(nx=500,ny=500,col="lightgrey",lty="solid")
lines(Qo1.mo,lwd=2, col="cornflowerblue")
lines(Qs1.mo,lwd=1,col="red3")
abline(v=date.ticks, lty=3)
box(which='plot')
legend(0, 0.135, legend=c("observed", "simulated"), cex=0.8,
       lty=c(1,1), ,lwd=c(2,1),col=c("cornflowerblue", "red3"))
text(70,0.15, "Sum of squares due to regression (SSR):", pos=2, cex=0.8)
text(70,0.15, round(SSR.mo,3), cex=0.8, pos=4)
text(70,0.145,"Nash-Sutcliffe Efficiency (NSE):", cex=0.8, pos=2)
text(70,0.145, round(NS.mo,3), cex=0.8,pos=4)
text(70,0.140, "Percent bias (PBIAS):", cex=0.8, pos=2)
text(70,0.140, round(PBIAS.mo,3),cex=0.8, pos=4)
text(70,0.135, "Root mean square error (RMSE):", cex=0.8, pos=2)
text(70,0.135, round(RMSE.mo,3),cex=0.8, pos=4)

# Plotting sediment loss in the catchment #####
#******************************************************************************
# Plotting sediment loss in the catchment

setwd("E:/Calibration/Anjeni/Scenarios/AJ84-97-daily-limited/TxtInOut/")
# Step 2: Read observed and simulated data
Sedo  <- read.table("sedloss.daily.txt")[,2]              # obsdata needs to be copied to the folder
sedo.ts <- ts(Sedo, start=c(1986,1,1), end=c(1993,12,31),freq=365)


Seds1 <- read.table("output.rch", skip=9)
Seds <- Seds1[Seds1$V2==15,11]                            # Select data from column 7 for each V2==15

seds.ts <- ts(Seds, start=c(1986,1,1), end=c(1993,12,31), freq=365)
dates <- c("1986","1987","1988","1989",
           "1990","1991","1992","1993",
           "1994","1995","1996","1997",
           "1998")                              # 
yrs <- c(1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul","Aug",
            "Sep","Oct","Now","Dec")
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
