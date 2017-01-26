#*********************************************
# Compare CFSR stations to NMA station in BNB
#
# Date: 2016-04-14
# Author: VR
#*********************************************


library(zoo)
library(lattice)
library(hydroTSM)
library(hydroGOF)


# Set the watershed (Dabus, Beles, SouthGojam, Kessie, Didessa)
setwd("C:/Modelling/Data_Paper3/6_Paper3/BasinsComparison/Didessa")

files <- list.files(pattern = "*.txt")
# line <- list.files(pattern="NMALine")
# matrix <- list.files(pattern="Matrix")

#*************************************************
# First of all create the Boxplot for the NMA data --> in this case the Nedjo NMA pcp

nma.X <- read.csv("NMALine.csv", header=TRUE, sep=",", na.strings="NA")
nma.X$date <- as.Date(nma.X$date, format="%m/%d/%Y")   # format(, "%b-%y")

# Create a zoo object from the rainfall data


# use if monthly data 
nma.zoo <- zoo(nma.X[,2], as.yearmon(nma.X[,1], "%b-%y"))
# date ----
nma.dates <- time(nma.zoo)
# Number of years----
nma.nyrs <- yip(from=start(nma.zoo), to=end(nma.zoo), out.type="nmbr")


# Creating a matrix with monthly values per year in each column----
# NMA
nma.Matrix <- as.matrix(read.table("MatrixNMA.csv", header=TRUE, sep=",", row.names=1, as.is=TRUE))
rownames(nma.Matrix) <- month.abb
colnames(nma.Matrix) <- unique(format(time(nma.zoo), "%Y"))
nma.Matrix <- t(nma.Matrix)
nma.yrly <- monthly2annual(nma.zoo, FUN=sum, na.rm=FALSE)
# Mean value of all mean values of all years
all.yrly.mean.nma <- annualfunction(nma.zoo, FUN=sum)/nma.nyrs

# Create data for boxplot----
nma.cmonth <- format(time(nma.zoo), "%b")
nma.months <- factor(nma.cmonth, levels=unique(nma.cmonth), ordered=TRUE)

nma.mo.med <- round(monthlyfunction(nma.zoo, FUN=median, na.rm=TRUE),2)
nma.mo.mean <- round(monthlyfunction(nma.zoo, FUN="mean", na.rm=TRUE),2)


#**************************************************************************************************
#*********************************************************
# Now load all the CFSR stations in the Basin
#

for (i in seq_along(files)) {
  x <- read.csv(files[i], header=TRUE, sep=",", na.strings="-99")
  x$date <- seq(as.Date("1979-01-01"), as.Date("1979-01-01")+nrow(x)-1, by=1)
  name <- sub("[.][^.]*$", "", files[i], perl=TRUE)
  names(x) <- c(sub("[.][^.]*$", "", files[i], perl=TRUE), "date")
  x <- x[,c(2,1)]
  
  # Create a zoo object from the rainfall data
  x.zoo <- read.zoo(x, sep=",", format="%Y-%m-%d")
  x.zoo <- window(x.zoo, start=as.Date("1979-01-01"), end=as.Date("2010-12-31"))
  
  # dates from CFSR data
  x.dates <- time(x.zoo)
  xnyrs <- yip(from=start(x.zoo), to=end(x.zoo), out.type="nmbr")
  
  #summary----
  smry <- round(smry(coredata(x.zoo)),2)
  names(smry) <- names(x)[2]
  
  
  # daily zoo to monthly zoo----
  x.m <- daily2monthly(x.zoo, FUN=sum, na.rm=FALSE)
  
  # Create an ordered monthly ts object
  x.ts <- ts(x.zoo, freq=12, start=c(1979,1))
  
  # Creating matrix with monthly values per year in each column----
  # nnr1
  
  x.matrix <- matrix(x.m, ncol=12, byrow=T)
  colnames(x.matrix) <- month.abb
  rownames(x.matrix) <- unique(format(time(x.m),"%Y"))
  
  # assign(files[i], x.matrix)
  
  #*******************************************************************************
  # Create MEAN yearly sums of NNR----
  x.yrly <- (daily2annual(x.zoo, FUN=sum, na.rm=TRUE))
  x.mean.yr <- annualfunction(x.zoo, FUN=sum, na.rm=TRUE)/xnyrs
  
  # Monthly analysis
  x.mo.med <- round(monthlyfunction(x.m, FUN=median, na.rm=TRUE),2)
  x.mo.mean <- round(monthlyfunction(x.m, FUN="mean", na.rm=TRUE),2)
  
  x.cmonth <- format(time(x.m), "%b")
  x.months <- factor(x.cmonth, levels=unique(x.cmonth), ordered=TRUE)
  
  #********************************************************************
  # And plot
  
  # 2 on one plot----
  cairo_ps(filename=paste(name, ".eps", sep=""), width = 11, height = 8, # "Rplot%03d.eps"
           onefile = FALSE, family = "Helvetica")
  par(mfrow=c(1,2), mar=c(4, 6, 5, 2), oma=c(3,4,4,1))
  
  # First the CFSR data
  max <- 1050
  yl <- seq(0,max, 50)
  boxplot(coredata(x.m)~x.months, col="lightblue",
          ylab="", xlab="", yaxt="n", xaxt="n",
          ylim=c(0,max))
  axis(1, seq(1,12,1), month.abb, las=2, cex.axis=2)
  axis(2, yl, yl, las=2, cex.axis=1.6)
  abline(v=time(x.mo.mean), col="lightgrey", lty=3)
  abline(h=mean(x.mo.mean), col="brown", lty=2)
  abline(h=yl, col="lightgray", lty=3)
  mtext(text=paste("CFSR", name, sep=" "), 3, 2.5, cex=1.5)
  mtext("1979-2010", 3, .5)
  
  #**************************************
  # NMA data
  boxplot(coredata(nma.zoo)~nma.months, col="olivedrab3",
          xaxt="n", yaxt="n", ylim=c(0,max))
  axis(1, seq(1,12,1), month.abb, las=2, cex.axis=2)
  axis(2, yl, yl, las=2, cex.axis=1.6)
  abline(h=yl, v=time(nma.mo.mean), col="lightgrey", lty=3)
  par(new=TRUE)
  boxplot(coredata(nma.zoo)~nma.months, col="olivedrab3", 
          axes=FALSE, ylim=c(0,max))
  abline(h=mean(nma.mo.mean), col="brown", lty=2)
  abline(h=mean(nma.mo.mean), col="brown", lty=2)
  mtext(text="NMA Nedjo", 3, 2.5, cex=1.5)
  mtext("1952-2003", 3, .5)
  title(main="Precipitation distribution CFSR and NMA", outer=TRUE, line=0.5,
        cex.main=3)
  
  dev.off()

}



