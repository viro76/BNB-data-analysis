#*********************************************
# Take CFSR data and transpose the mean monthly data to Excel
#
# Date: 2016-04-17
# Author: VR
#*********************************************


library(zoo)
library(lattice)
library(hydroTSM)
library(hydroGOF)


# Set folder where the CFSR data is located
setwd("C:/Modelling/Data_Paper3/Climat/CFSR/Neue Daten Vine/37995_2016-03-09-19-58-50")

# Read in the precipitation files and nothing else
filesp <- list.files(path=".", pattern="(p0|p1|p2|p3|p4|p5|p6|p7|p8|p9).*\\.txt$")

# read in files and change them to zoo object---- 

st.mean <- data.frame()
name.df <- data.frame()
date <- seq(as.Date("1979-01-01"), as.Date("2014-07-31"), by="days")

for (i in seq_along(filesp)){
  
  x <- read.table(filesp[i], header=TRUE, sep=",",na.strings="-99.000")
  
  x$date <- date
  names(x) <- c("pcp", "date")
  x <- x[,c(2,1)]
  x.zoo <- read.zoo(x, format="%Y-%m-%d")
  
  # Calculate the monthly mean of the series
  x.mo <- daily2monthly(x.zoo, FUN="sum", na.rm=TRUE)
  x.mo.mean <- round(monthlyfunction(x.mo, FUN="mean", na.rm=TRUE),2)
  
  y <- coredata(x.mo.mean)
  
  st.mean <- rbind(st.mean, y)
  
}
  names(st.mean) <- month.abb
  rownames(st.mean) <- sub("[.][^.]*$", "", filesp, perl=TRUE)
  
  write.csv(st.mean, "C:/Modelling/Data_Paper3/Climat/Mean Monthly pcp CFSR stations BNB.csv", quote=FALSE)
  
  
