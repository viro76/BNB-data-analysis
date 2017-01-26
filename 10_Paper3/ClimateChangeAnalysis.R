#**********************************
# Climate data scenarios
#
#*********************************
#

library(tools)
library(zoo)
library(hydroGOF)
library(hydroTSM)
library(Cairo)
library(grDevices)
library(lattice)
library(gridExtra)

ini <- "~/R/Data/BNB/02_ClimateScenarios"

# set initial folder----
setwd(ini)
station <- list.dirs(full.names = FALSE, recursive = FALSE)

# define other necessary variables----
start <- as.Date("1979-01-01", format="%Y-%m-%d")
startScen1 <- as.Date("2046-01-01", format="%Y-%m-%d")
startScen2 <- as.Date("2081-01-01", format="%Y-%m-%d")


# Iterate through each station----
# first get the current climate data
# second add the future prediction data and plot

for (i in seq_along(station)){
  n <- station[i]
  setwd(paste(getwd(), n, sep="/"))
  files <- list.files(pattern="^[p]")
  
  for(j in seq_along(files)){
    # setwd(ini)
    n <- read.csv(files[j], header=TRUE, na.strings=c("-99.000", "NA", "-99"))
    dd <- seq(as.Date(start), as.Date(start + nrow(n)-1), by="days")
    n <- cbind(dd,n)
    names(n) <- c("date", "pcp")
    nameSt <- file_path_sans_ext(files[j])
    
    # Prepare the different formats
    nowD <- read.zoo(n, format="%Y-%m-%d")
    nowD <- window(nowD, start="1979-01-01", end="2013-12-31")
    nowM <- daily2monthly(nowD, FUN=sum, na.rm=TRUE)
    nowY <- daily2annual(nowD, FUN=sum, na.rm=TRUE)
    
    # Compile different necessary parameters----
    n.yrs <- yip(from=start(nowM), to=end(nowM), out.type="nmbr")
    beg.yrs <- as.numeric(format(min(index(nowY)), "%Y"))
    end.yrs <- as.numeric(format(max(index(nowY)), "%Y"))
    beg.mo <- as.numeric(format(min(index(nowM)), "%m"))
    end.mo <- as.numeric(format(max(index(nowM)), "%m"))
    
    now.ts <- ts(nowM, freq=12, start(c(beg.yrs,beg.mo), end=c(end.yrs, end.mo)))
    
    nmonth <- format(time(nowM), "%b")
    months <- factor(nmonth, level=unique(nmonth), ordered=TRUE)
    
    n.matrix <- matrix(now.ts, ncol=12, byrow=TRUE)
    colnames(n.matrix) <- month.abb
    rownames(n.matrix) <- as.numeric(format(index(nowY), "%Y"))
    
    # plotOne <- matrixplot(n.matrix)
    
    # summary(nowD)
    
    #*************************************
    # change to the ClimateChange data from 2046 to 2064----
    setwd(paste(ini, station[i], "climate", sep="/"))
    climfolder <- list.dirs(full.names = FALSE, recursive = FALSE)
    
    for(k in seq_along(climfolder)){
      setwd(paste(ini, station[i], "climate", climfolder[k], "pcp", sep="/"))
      filesClCh <- list.files(pattern="^[PCP]")
      
      # Boxplot everything----
      cairo_pdf(paste(ini, "/", station[i], "_", climfolder[k],"_", nameSt, ".pdf", sep=""),
                width=8.27, height=11.69, onefile=TRUE)
      
      par(mfrow=c(4,2))
      mx <- max(coredata(nowM), na.rm=TRUE)+400
      
      boxplot(coredata(nowM)~months, col="brown", 
              ylim=c(0, mx),
              main=paste("Monthly pcp from ", nameSt, " (CFSR Data) in ", station[i], sep=""),
              ylab="Precipitation [mm]")
      top <- mx
      dis <- round(mx/14,2)
      
      # add stats to plot
      text(2, top-dis, "Min: ",pos=2, cex=.8)
      text(2, top-dis, paste(smry(nowM)[1,2], "mm"), pos=4, cex=.8)
      text(2, top-2*dis, "1st Qu.: ", pos=2, cex=.8)
      text(2, top-2*dis, paste(round(smry(nowM)[2,2],0), "mm"), pos=4, cex=.8)
      text(2, top-3*dis, "Median: ", pos=2, cex=.8) 
      text(2, top-3*dis, paste(round(smry(nowM)[3,2], 0), "mm"), pos=4, cex=.8)
      text(2, top-4*dis, "Mean: ", pos=2, cex=.8) 
      text(2, top-4*dis, paste(round(smry(nowM)[4,2], 0), "mm"), pos=4, cex=.8)
      text(2, top-5*dis, "3rd Qu.: ", pos=2, cex=.8) 
      text(2, top-5*dis, paste(round(smry(nowM)[5,2], 0), "mm"), pos=4, cex=.8)
      text(2, top-6*dis, "Max: ", pos=2, cex=.8) 
      text(2, top-6*dis, paste(round(smry(nowM)[6,2], 0), "mm"), pos=4, cex=.8)
      
      
      
      # plots.nmb <- data.frame()
      
      for(l in seq_along(filesClCh)){
        m <- read.csv(filesClCh[l], header=TRUE, na.strings=c("NA", "-99.000", "-99"))
        dScen1 <- seq(as.Date(startScen1), as.Date(startScen1 + nrow(m)-1), by="days")
        m <- cbind(dScen1, m)
        names(m) <- c("date", "pcp")
        nameSc1St <- file_path_sans_ext(filesClCh[l])
        
        Scen1D <- read.zoo(m, format="%Y-%m-%d")
        Scen1M <- daily2monthly(Scen1D, FUN=sum, na.rm=TRUE)
        Scen1Y <- daily2annual(Scen1D, FUN=sum, na.rm=TRUE)
        n.yrs.scen1 <- yip(from=start(Scen1Y), to=end(Scen1Y), out.type="nmbr")
        
        b.yrs <- as.numeric(format(min(index(Scen1M)), "%Y"))
        e.yrs <- as.numeric(format(max(index(Scen1M)), "%Y"))
        b.mo <- as.numeric(format(min(index(Scen1M)), "%m"))
        e.mo <- as.numeric(format(max(index(Scen1M)), "%m"))
        
        Scen1.ts <- ts(Scen1M, freq=12, start=c(b.yrs, b.mo), end=c(e.yrs, e.mo))
        
        sc1.nmonth <- format(time(Scen1M), "%b")
        sc1.months <- factor(sc1.nmonth, level=unique(sc1.nmonth), ordered=TRUE)
        
        sc1.matrix <- matrix(Scen1.ts, ncol=12, byrow=TRUE)
        colnames(sc1.matrix) <- month.abb
        rownames(sc1.matrix) <- as.numeric(format(index(Scen1Y), "%Y"))
        
        # nm <- paste("plot", l, sep="")
        # pl <- matrixplot(sc1.matrix)
        # assign(nm, pl)
        # rm(pl)
        
        # plots.nmb <- cbind(plots.nmb, pl=as.character(nm))
        
        # Boxplot the climate projection----
        mx <- max(coredata(Scen1M), na.rm=TRUE)+400
        
        boxplot(coredata(Scen1M)~sc1.months, col="lightblue", 
                ylim=c(0, mx),
                main=paste("Monthly pcp from ", nameSc1St, "\n (projected CFSR Data) in ", 
                           station[i], "from scenario \n", climfolder[k], sep=""),
                ylab="Precipitation [mm]")
        top <- mx
        dis <- round(mx/14,2)
        
        # add stats to plot
        text(2, top-dis, "Min: ",pos=2, cex=.8)
        text(2, top-dis, paste(smry(Scen1M)[1,2], "mm"), pos=4, cex=.8)
        text(2, top-2*dis, "1st Qu.: ", pos=2, cex=.8)
        text(2, top-2*dis, paste(round(smry(Scen1M)[2,2],0), "mm"), pos=4, cex=.8)
        text(2, top-3*dis, "Median: ", pos=2, cex=.8) 
        text(2, top-3*dis, paste(round(smry(Scen1M)[3,2], 0), "mm"), pos=4, cex=.8)
        text(2, top-4*dis, "Mean: ", pos=2, cex=.8) 
        text(2, top-4*dis, paste(round(smry(Scen1M)[4,2], 0), "mm"), pos=4, cex=.8)
        text(2, top-5*dis, "3rd Qu.: ", pos=2, cex=.8) 
        text(2, top-5*dis, paste(round(smry(Scen1M)[5,2], 0), "mm"), pos=4, cex=.8)
        text(2, top-6*dis, "Max: ", pos=2, cex=.8) 
        text(2, top-6*dis, paste(round(smry(Scen1M)[6,2], 0), "mm"), pos=4, cex=.8)
      }
      dev.off()
    }
    
  }
  
}








