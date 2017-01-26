#*************************************
# Compare rainfall data for BNB
#
#*************************************
# Date:   2016-06-24
# Author: VR
# ************************************

library(tools)
library(zoo)
library(hydroGOF)
library(hydroTSM)


setwd("~/R/Data/BNB/pcp/KNMI_Data_5288_daily/")

files <- list.files(pattern="\\.txt", include.dirs=FALSE, no.. = TRUE)

for(i in seq_along(files)){
  x <- read.csv(files[i], skip = 6, header=FALSE, sep="\t", stringsAsFactors = FALSE)
  colnames(x) <- c("year", "month", "day", "pcp")
  x$date <- as.Date(paste(x$year, x$month, x$day, sep="-"), format = "%Y-%m-%d")
  x <- x[,c(5,4)]
  
  namekn <- file_path_sans_ext(files[i])
  
  assign(namekn, x)
  
  # create zoo objects from df
  x.zo <- read.zoo(x, format="%Y-%m-%d")
  assign(paste(namekn, ".z",sep=""), x.zo)
  # nyrs <- yip(from=start(x.zo), to=end(x.zo), out.type="nmbr")
  
  
  # make boxplots from data
  
  x.month <- daily2monthly(x.zo, FUN=sum, na.rm=FALSE)
  x.month <- window(x.month, start=as.Date("1964-01-01"))
  nyrs <- yip(from=start(x.month), to=end(x.month), out.type="nmbr")
  
  
  x.ts <- ts(x.month, freq=12, start=c(as.numeric(format(start(x.month), "%Y")),
                                       as.numeric(format(start(x.month), "%m"))))
  # x.ts <- window(x.ts, start=c(1962,1))
  
  # creating matrix with values ordered
  x.matrix <- matrix(x.ts, ncol=12, byrow=TRUE)
  
  
  # yearly analysis
  x.yrl <- daily2annual(x.zo, FUN=sum, na.rm=FALSE)
  ann.mean <- annualfunction(x.yrl, FUN=sum)/nyrs
  
  cmonth <- format(time(x.month), "%b")
  mos <- factor(cmonth, level=unique(cmonth), ordered=TRUE)
  
  posy <- max(x.month)-0.1*max(x.month)
  abst <- 3*0.01*posy
  
  boxplot(coredata(x.month)~mos, col="brown",
          main=paste("Monthly precipitation distribution \n", namekn, " (KNMI_Data)"),
          ylab="Precipitation [mm]")
  text(2, posy, "Min: ",pos=2, cex=.8)
  text(2, posy, paste(smry(x.month)[1,2], "mm"), pos=4, cex=.8)
  text(2, posy-abst, "1st Qu.: ", pos=2, cex=.8)
  text(2, posy-abst, paste(round(smry(x.month)[2,2],0), "mm"), pos=4, cex=.8)
  text(2, posy-2*abst, "Median: ", pos=2, cex=.8) 
  text(2, posy-2*abst, paste(round(smry(x.month)[3,2], 0), "mm"), pos=4, cex=.8)
  text(2, posy-3*abst, "Mean: ", pos=2, cex=.8) 
  text(2, posy-3*abst, paste(round(smry(x.month)[4,2], 0), "mm"), pos=4, cex=.8)
  text(2, posy-4*abst, "3rd Qu.: ", pos=2, cex=.8) 
  text(2, posy-4*abst, paste(round(smry(x.month)[5,2], 0), "mm"), pos=4, cex=.8)
  text(2, posy-5*abst, "Max: ", pos=2, cex=.8) 
  text(2, posy-5*abst, paste(round(smry(x.month)[6,2], 0), "mm"), pos=4, cex=.8)
  
  
  text(10,posy, "IQR: ", pos=2, cex=.8)
  text(10, posy, round(smry(x.month)[7,2], 2), pos=4, cex=.8)
  text(10, posy-abst, "sd: ", pos=2, cex=.8)
  text(10,posy-abst, round(smry(x.month)[8,2],2), pos=4, cex=.8)
  text(10,posy-2*abst, "cv: ", pos=2, cex=.8) 
  text(10,posy-2*abst, round(smry(x.month)[9,2],2), pos=4, cex=.8)
  text(10,posy-3*abst, "Skewness: ", pos=2, cex=.8)
  text(10, posy-3*abst, round(smry(x.month)[10,2],2), pos=4, cex=.8)
  text(10,posy-4*abst, "Kurtosis: ", pos=2, cex=.8)
  text(10, posy-4*abst, round(smry(x.month)[11,2],2), pos=4, cex=.8)
  
  # Prepare nice matrix
  matrixknmi <- matrix(x.month, ncol=12, byrow=TRUE)
  colnames(matrixknmi) <- month.abb
  rownames(matrixknmi) <- unique(format(time(x.month), "%Y"))
  
  matrixplot(matrixknmi, ColorRamp="Precipitation", 
             main=paste("Monthly precipitation distribution\n", namekn, "(KNMI_Data)"))
}





###############################################################################
# CFSR Data-----

setwd("D:/3_GIS-Data/1_Blue Nile Basin/2_BNB_Basins_Tat/Climat/CFSR/Neue Daten Vine/37995_2016-03-09-19-58-50/pcp")
list.files(pattern="^.*p117*.*.txt$")
filesP <- c("p117372.txt", "p117375.txt", "p114372.txt", "p114375.txt")


par(mfrow=c(3,2))
for(i in seq_along(filesP)){
  p <- read.csv(filesP[i], header=TRUE, na.strings="-99.000")
  colnames(p) <- "pcp"
  date <- seq(as.Date("1979-01-01"), as.Date("1979-01-01")+nrow(p)-1, by=1)
  p <- cbind(date, p)
  p.zo <- read.zoo(p, format="%Y-%m-%d")
  
  name <- file_path_sans_ext(filesP[i])
  
  p.month <- daily2monthly(p.zo, FUN=sum, na.rm=FALSE)
  p.month <- window(p.month, start=as.Date("1979-01-01"), end=as.Date("2013-12-01"))
  nyrs <- yip(from=start(p.month), to=end(p.month), out.type="nmbr")
  
  
  x.ts <- ts(p.month, freq=12, start=c(1979,1), end=c(2013,12))
  # x.ts <- window(x.ts, start=c(1962,1))
  
  # creating matrix with values ordered
  x.matrix <- matrix(x.ts, ncol=12, byrow=TRUE)
  
  
  # yearly analysis
  p.yrl <- daily2annual(p.zo, FUN=sum, na.rm=FALSE)
  ann.mean <- annualfunction(p.yrl, FUN=sum)/nyrs
  
  cmonth <- format(time(p.month), "%b")
  mos.p <- factor(cmonth, level=unique(cmonth), ordered=TRUE)
  
  
  boxplot(coredata(p.month)~mos.p, col="lightblue",
          ylim=c(0,900),
          main=paste("Monthly precipitation distribution \n", name, " (CFSR_Data)"),
          ylab="Precipitation [mm]")
  

  text(2, 800, "Min: ",pos=2, cex=.8)
    text(2, 800, paste(smry(p.month)[1,2], "mm"), pos=4, cex=.8)
  text(2, 770, "1st Qu.: ", pos=2, cex=.8)
    text(2, 770, paste(round(smry(p.month)[2,2],0), "mm"), pos=4, cex=.8)
  text(2, 740, "Median: ", pos=2, cex=.8) 
    text(2, 740, paste(round(smry(p.month)[3,2], 0), "mm"), pos=4, cex=.8)
  text(2, 710, "Mean: ", pos=2, cex=.8) 
    text(2, 710, paste(round(smry(p.month)[4,2], 0), "mm"), pos=4, cex=.8)
  text(2, 680, "3rd Qu.: ", pos=2, cex=.8) 
    text(2, 680, paste(round(smry(p.month)[5,2], 0), "mm"), pos=4, cex=.8)
  text(2, 650, "Max: ", pos=2, cex=.8) 
    text(2, 650, paste(round(smry(p.month)[6,2], 0), "mm"), pos=4, cex=.8)
  
  
  text(10,800, "IQR: ", pos=2, cex=.8)
    text(10, 800, round(smry(p.month)[7,2], 2), pos=4, cex=.8)
  text(10, 770, "sd: ", pos=2, cex=.8)
    text(10,770, round(smry(p.month)[8,2],2), pos=4, cex=.8)
  text(10,740, "cv: ", pos=2, cex=.8) 
    text(10,740, round(smry(p.month)[9,2],2), pos=4, cex=.8)
  text(10,710, "Skewness: ", pos=2, cex=.8)
    text(10, 710, round(smry(p.month)[10,2],2), pos=4, cex=.8)
  text(10,680, "Kurtosis: ", pos=2, cex=.8)
    text(10, 680, round(smry(p.month)[11,2],2), pos=4, cex=.8)
    
}
par(mfrow=c(1,1))

# Prepare rainfall matrix overview----
matrixcfsr <- matrix(p.month, ncol=12, byrow=TRUE)
colnames(matrixcfsr) <- month.abb
rownames(matrixcfsr) <- unique(format(time(p.month), "%Y"))

matrixplot(matrixcfsr, ColorRamp="Precipitation", 
           main=paste("Monthly precipitation distribution\n", name, "(CFSR_Data)"))









###############################################################################
# IWMI Data-----

setwd("~/R/Data/BNB/pcp/Raw Data_IWMI/METEOROLOGICAL DATA/Precipitation_d_8706")
df <- readWorksheetFromFile(file=precipitation.xls, sheet=1)



