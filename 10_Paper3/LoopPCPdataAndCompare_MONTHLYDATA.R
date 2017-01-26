#******************************************************
# Compare the different pcp stations with CFSR data
# ONLY MONTHLY DATA 
#
#******************************************************

library(tools)
library(zoo)
library(hydroGOF)
library(hydroTSM)
library(Cairo)
library(grDevices)

setwd("~/R/Data/BNB/01_Comparison")

dirs <- list.dirs(full.names = FALSE, recursive = FALSE)


for(j in seq_along(dirs)){
  name <- dirs[j]
  setwd(paste("~/R/Data/BNB/01_Comparison/", name, sep=""))
  
  # files <- list.files(pattern="\\.txt", include.dirs=FALSE, no.. = TRUE)
  filesCF <- list.files(pattern="^[p]")
  
  # nbr.fig <- length(filesCF)+1
  # par(mfrow=c(nbr.fig/2,2))
  
  filesNMA <- list.files(pattern="^[M_].*\\.csv" , include.dirs=FALSE)
  # filesSCRP <- list.files(pattern="^[SCRP]")
  # filesNMA <- c(filesNMA, filesSCRP)
  # filesNMA <- filesNMA[c(2:length(filesNMA))]
  
  
  #************************
  # Prepare the NMA data---
  for(l in seq_along(filesNMA)){
    n <- read.csv(filesNMA[l], header=FALSE, na.strings="NA", stringsAsFactors=FALSE)
    #************************
    # SCRP files only
    # n <- read.csv(filesSCRP[l], header=FALSE, stringsAsFactors=FALSE, na.strings="NA")
    #************************
    colnames(n) <- c("date", "pcp")
    n$date <- as.Date(n$date, format="%m/%d/%Y")
    
    n.zo <- read.zoo(n, format="%Y-%m-%d", na.omit=TRUE)
    
    namesSt.n <- file_path_sans_ext(filesNMA)[l]
    #************************
    # SCRP files only
    # namesSt.n <- file_path_sans_ext(filesSCRP)[l]
    #************************
    n.month <- n.zo       #daily2monthly(n.zo, FUN=sum, na.rm=TRUE)
    n.nyrs <- yip(from=start(n.month), to=end(n.month), out.type="nmbr")
    
    yr <- as.numeric(format(min(index(n.month)), "%Y"))
    yr.e <- as.numeric(format(max(index(n.month)), "%Y"))
    mo <- as.numeric(format(min(index(n.month)), "%m"))
    mo.e <- as.numeric(format(max(index(n.month)), "%m"))
    n.ts <- ts(n.month, freq=12, start=c(yr,mo), end=c(yr.e,mo.e))
    
    n.yrl <- daily2annual(n.zo, FUN=sum, na.rm=TRUE)
    ann.mean.n <- annualfunction(n.yrl, FUN=sum)/n.nyrs
    
    nmonth <- format(time(n.month), "%b")
    mos.n <- factor(nmonth, level=unique(nmonth), ordered=TRUE)
    
    n.matrix <- matrix(n.ts, ncol=12, byrow=TRUE)
    
    #************************************
    # Statistically compare the time-series
    
    
    # Find the number of boxplots and divide canvas accordingly----
    # nmbr.fig <- length(filesCF)+1
    # if((nmbr.fig %% 2) == 0){
    #   par(mfrow=c(2,nmbr.fig))
    # } else {
    #   par(mfrow=c(2,nmbr.fig+1))
    # }
    
    # Export the plots----
    # 
    cairo_pdf(paste(namesSt.n,".pdf", sep=""), width=8.27, height=11.69,onefile=TRUE,
              family="sans")
    dirOut <- getwd()   # paste(getwd(), "/Figures/", sep="")
    
    # Cairo_pdf(width=8.2, height=11.6, paste(dirOut, namesSt.n, ".pdf", sep=""))
    
    par(mfrow=c(4,2), mar=c(2,4,4,1))
    
    mx <- max(n.zo, na.rm=TRUE)+400
    
    # mx <- max(coredata(n.month), na.rm=TRUE)+400
    # mn <- min(coredata(n.month), na.rm=TRUE)
    
    # Plot the boxplots
    boxplot(coredata(n.month)~mos.n, col="brown",
            ylim=c(0,mx),
            main=paste("Monthly pcp \n", namesSt.n, " (CFSR_Data)", "in ", name),
            ylab="Precipitation [mm]")
    
    top <- mx
    dis <- round(mx/14,0)
    
    text(2, top-dis, "Min: ",pos=2, cex=.8)
    text(2, top-dis, paste(smry(n.month)[1,2], "mm"), pos=4, cex=.8)
    text(2, top-2*dis, "1st Qu.: ", pos=2, cex=.8)
    text(2, top-2*dis, paste(round(smry(n.month)[2,2],0), "mm"), pos=4, cex=.8)
    text(2, top-3*dis, "Median: ", pos=2, cex=.8) 
    text(2, top-3*dis, paste(round(smry(n.month)[3,2], 0), "mm"), pos=4, cex=.8)
    text(2, top-4*dis, "Mean: ", pos=2, cex=.8) 
    text(2, top-4*dis, paste(round(smry(n.month)[4,2], 0), "mm"), pos=4, cex=.8)
    text(2, top-5*dis, "3rd Qu.: ", pos=2, cex=.8) 
    text(2, top-5*dis, paste(round(smry(n.month)[5,2], 0), "mm"), pos=4, cex=.8)
    text(2, top-6*dis, "Max: ", pos=2, cex=.8) 
    text(2, top-6*dis, paste(round(smry(n.month)[6,2], 0), "mm"), pos=4, cex=.8)
    
    
    text(11,top-dis, "IQR: ", pos=2, cex=.8)
    text(11, top-dis, round(smry(n.month)[7,2], 2), pos=4, cex=.8)
    text(11, top-2*dis, "sd: ", pos=2, cex=.8)
    text(11,top-2*dis, round(smry(n.month)[8,2],2), pos=4, cex=.8)
    text(11,top-3*dis, "cv: ", pos=2, cex=.8) 
    text(11,top-3*dis, round(smry(n.month)[9,2],2), pos=4, cex=.8)
    text(11,top-4*dis, "Skewness: ", pos=2, cex=.8)
    text(11, top-4*dis, round(smry(n.month)[10,2],2), pos=4, cex=.8)
    text(11,top-5*dis, "Kurtosis: ", pos=2, cex=.8)
    text(11, top-5*dis, round(smry(n.month)[11,2],2), pos=4, cex=.8)
    text(11, top-6*dis, "Nbr of yrs: ", pos=2, cex=.8)
    text(11, top-6*dis, paste(n.nyrs, " (",yr,"-", yr.e, ")", sep=""), pos=4, cex=.8)
    text(11, top-7*dis, "Yrly mean: ", pos=2, cex=.8)
    text(11, top-7*dis, paste(round(ann.mean.n, 0), " mm"), pos=4, cex=.8)
    
    
    
    for(i in seq_along(filesCF)){
      # Prepare the CFSR data----
      p <- read.csv(filesCF[i], header=TRUE, na.strings="-99.000")
      colnames(p) <- "pcp"
      date <- seq(as.Date("1979-01-01"), as.Date("1979-01-01")+nrow(p)-1, by=1)
      p <- cbind(date, p)
      p.zo <- read.zoo(p, format="%Y-%m-%d")
      
      nameSt <- file_path_sans_ext(filesCF[i])
      
      p.month <- daily2monthly(p.zo, FUN=sum, na.rm=FALSE)
      p.month <- window(p.month, start=as.Date("1979-01-01"), end=as.Date("2013-12-01"))
      nyrs <- yip(from=start(p.month), to=end(p.month), out.type="nmbr")
      
      #**********************************************  
      # Prepare for Boxplot----
      p.ts <- ts(p.month, freq=12, start=c(1979,1), end=c(2013,12))
      
      # x.ts <- window(x.ts, start=c(1962,1))
      
      # creating matrix with values ordered
      p.matrix <- matrix(p.ts, ncol=12, byrow=TRUE)
      
      # yearly analysis
      
      p.yrl <- daily2annual(p.zo, FUN=sum, na.rm=FALSE)
      ann.mean <- annualfunction(p.yrl, FUN=sum)/nyrs
      
      cmonth <- format(time(p.month), "%b")
      mos.p <- factor(cmonth, level=unique(cmonth), ordered=TRUE)
      
      
      boxplot(coredata(p.month)~mos.p, col="lightblue",
              ylim=c(0,mx),
              main=paste("Monthly pcp \n", nameSt, " (CFSR_Data)", "in ", name),
              ylab="Precipitation [mm]")
      
      top <- mx
      dis <- round(mx/14,0)
      
      text(2, top-dis, "Min: ",pos=2, cex=.8)
      text(2, top-dis, paste(smry(p.month)[1,2], "mm"), pos=4, cex=.8)
      text(2, top-2*dis, "1st Qu.: ", pos=2, cex=.8)
      text(2, top-2*dis, paste(round(smry(p.month)[2,2],0), "mm"), pos=4, cex=.8)
      text(2, top-3*dis, "Median: ", pos=2, cex=.8) 
      text(2, top-3*dis, paste(round(smry(p.month)[3,2], 0), "mm"), pos=4, cex=.8)
      text(2, top-4*dis, "Mean: ", pos=2, cex=.8) 
      text(2, top-4*dis, paste(round(smry(p.month)[4,2], 0), "mm"), pos=4, cex=.8)
      text(2, top-5*dis, "3rd Qu.: ", pos=2, cex=.8) 
      text(2, top-5*dis, paste(round(smry(p.month)[5,2], 0), "mm"), pos=4, cex=.8)
      text(2, top-6*dis, "Max: ", pos=2, cex=.8) 
      text(2, top-6*dis, paste(round(smry(p.month)[6,2], 0), "mm"), pos=4, cex=.8)
      
      
      text(11,top-dis, "IQR: ", pos=2, cex=.8)
      text(11, top-dis, round(smry(p.month)[7,2], 2), pos=4, cex=.8)
      text(11, top-2*dis, "sd: ", pos=2, cex=.8)
      text(11,top-2*dis, round(smry(p.month)[8,2],2), pos=4, cex=.8)
      text(11,top-3*dis, "cv: ", pos=2, cex=.8) 
      text(11,top-3*dis, round(smry(p.month)[9,2],2), pos=4, cex=.8)
      text(11,top-4*dis, "Skewness: ", pos=2, cex=.8)
      text(11, top-4*dis, round(smry(p.month)[10,2],2), pos=4, cex=.8)
      text(11,top-5*dis, "Kurtosis: ", pos=2, cex=.8)
      text(11, top-5*dis, round(smry(p.month)[11,2],2), pos=4, cex=.8)
      text(11, top-6*dis, "Nbr of yrs: ", pos=2, cex=.8)
      text(11, top-6*dis, nyrs, pos=4, cex=.8)
      text(11, top-7*dis, "Yrly mean: ", pos=2, cex=.8)
      text(11, top-7*dis, paste(round(ann.mean, 0), " mm"), pos=4, cex=.8)
      
      # calculate stats
      stats <- ts.intersect(n.ts, p.ts)
      obs <- stats[,1]
      sim <- stats[,2]
      
      statis <- gof(sim=sim, obs=obs)
      
      text(2, top-7*dis, "Goodness of fit:", cex=.8)
      text(2, top-8*dis, "ME: ", pos=2, cex=.8)
      text(2, top-8*dis, round(statis[1],2), pos=4, cex=.8)
      text(2, top-9*dis, "PBIAS %: ", pos=2, cex=.8)
      text(2, top-9*dis, round(statis[6],2), pos=4, cex=.8)
      text(2, top-10*dis, "RSR: ", pos=2, cex=.8)
      text(2, top-10*dis, round(statis[7],2), pos=4, cex=.8)
      text(2, top-11*dis, "NSE: ", pos=2, cex=.8)
      text(2, top-11*dis, round(statis[9],2), pos=4, cex=.8)
      text(2, top-12*dis, "R2: ", pos=2, cex=.8)
      text(2, top-12*dis, round(statis[17],2), pos=4, cex=.8)
      
    }
    dev.off()
  }
  
  
  
 }


par(mfrow=c(1,1))
