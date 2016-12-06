# Run SWAT in R
# Script from Mahmud.Geneva

# setwd("C:/Swat/ArcSWAT")

# Step 1: Execute SWAT
# system("swat2012.exe")

# setwd("E:/ArcSWAT_Examples/1_Anjeni/Scenarios/AnjeniDaily_27yrs/TxtInOut")     # set the directory to TxtInOut of your ex.                                                        

# Step 2: Read observed and simulated data
AjAll  <- read.table("~/R/Data/SCRP/ajAllData.csv", sep=",",header=TRUE)
AjAll$date <- as.Date(AjAll$date, format="%d-%m-%Y")

Qo <- AjAll[,c(1,2)]
Qs1 <- read.table("output.rch", skip=9)
Qs <- Qs1[Qs1$V2==20]

# Step 3: Calculate model performance statistics
SSR <- sum((Qs-Qo)^2)
NS <- 1-(SSR/(sum((Qo-mean(Qo))^2)))
PBIAS <- 100*sum(Qs)-sum(Qo))/sum(Qo)
SSR;NS;PBIAS

# Step 4: Plotting Obs vs. Simulated relationship
Qo1 <- Qo[c(1:1431)];
Qs1 <- Qs[c(1:1431)];
vectory <- c(0,1.1*max(max(Qs1),max(Qo1)))
vectorx <- c(0,length(Qs1))
dates <- c("01/01/1984", "01/01/1985")
date.ticks<-c(1,365,731,1096,1400)
plot(vectorx,vectory,type="n",xlab="Day",ylab="Discharge (cms)",axes=F)
axi(1,at=date.ticks,labels=dates)
axis(2,at=NULL,labels=NULL)
grid(nx=500,ny=500,col="lightgrey",lty="solid")
lines(Qo1,lwd=2)
lines(Qs1,lwd=2,col="blue")
box(which='plot')

