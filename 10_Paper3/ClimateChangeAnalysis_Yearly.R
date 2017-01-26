#**************************************
# Climate change analysis for yearly data
#
#
#***************************************

library(tools)
library(zoo)
library(hydroGOF)
library(hydroTSM)
library(Cairo)
library(grDevices)
library(lattice)
library(gridExtra)
library(RColorBrewer)
# library(fBasics)

ini <- "~/R/Data/BNB/02_ClimateScenarios"

# set initial folder----
setwd(ini)
watershed <- list.dirs(full.names = FALSE, recursive = FALSE)

# define other necessary variables----
start <- as.Date("1979-01-01", format="%Y-%m-%d")
startScen1 <- as.Date("2046-01-01", format="%Y-%m-%d")

for(i in seq_along(watershed)){
  n <- watershed[i]
  nameWS <- watershed[i]
  filesCFSRcur <- list.files(path=paste(getwd(), n, sep="/"),pattern="^[p]")
  scenarioFld <- list.dirs(paste(getwd(), n, "climate", sep="/"), full.names=FALSE,
                           recursive=FALSE)
  par(mfrow=c(4,2))
  
  for(j in seq_along(filesCFSRcur)){
    currCFSR <- read.csv(paste(getwd(),n,filesCFSRcur[j],sep="/"), 
                         na.strings=c("NA","-99", "-99.000"), 
                         stringsAsFactors=FALSE,header=TRUE)
    
    date <- seq(start, as.Date(start + nrow(currCFSR)-1, format="%Y-%m-%d"), by="days")
    yrs <- seq(as.numeric(format(min(date),"%Y")),as.numeric(format(max(date),"%Y")),by=1)
    pcpname <- file_path_sans_ext(filesCFSRcur[j])
    currCFSR <- cbind(date,currCFSR)
    currC.z <- read.zoo(currCFSR)
    currC.z <- window(currC.z, from=start(index(currC.z)), to=as.Date("2013-12-31"))
    
    # Annual data aggregation
    currPM <- daily2monthly(currC.z, FUN=sum)
    currPY <- daily2annual(currC.z, FUN=sum)
    cPm.ts <- ts(currPM, frequency=12, start=c(1979,1))
    cPm.ts <- window(cPm.ts, from=c(1979,1), end=c(2013,12))
    
    # create a dataframe from the ts-----
    pcp.ma.curr <- matrix(cPm.ts, ncol=12, byrow=TRUE)
    colnames(pcp.ma.curr) <- month.abb
    rownames(pcp.ma.curr) <- seq(min(yrs), max(yrs)-1,1)
    pcp.df <- as.data.frame(pcp.ma.curr)
    
    for(k in seq_along(scenarioFld)){
      pcpdir <- paste(getwd(),watershed[i],"climate",scenarioFld[k],"pcp",sep="/")
      
      pcpfls <- list.files(pcpdir, pattern="^[P]")
      
      d <- NULL
      nm <- NULL
      par(mfrow=c(4,2))
      boxplot(cPm.ts~cycle(cPm.ts), col="brown", ylim=c(0,1000), names=month.abb,
              main=paste("Current CFSR rainfall data in ", n, sep=""))
      
      txt <- summary(currPM)[,2]
      
      text(0.5,900, pos=4, txt[1], cex=.8)
      text(0.5,850, pos=4, txt[2], cex=.8)
      text(0.5,800, pos=4, txt[3], cex=.8)
      text(0.5,750, pos=4, txt[4], cex=.8)
      text(0.5,700, pos=4, txt[5], cex=.8)
      text(0.5,650, pos=4, txt[6], cex=.8)
      text(0.5,500, pos=4, paste("Mean ann. rainfall: ", round(mean(currPY),0), sep=""), cex=.8)
      
      
      
      for(l in 1:length(pcpfls)){
        pcp <- read.csv(paste(pcpdir,pcpfls[l],sep="/"), header=TRUE, 
                        na.strings=c("NA", "-99","-99.000"),
                        stringsAsFactors=FALSE)
        
        stName <- file_path_sans_ext(pcpfls[l])
        
        dateSc <- seq(startScen1, startScen1+nrow(pcp)-1, by="days")
        pcp <- cbind(dateSc, pcp)
        names(pcp) <- c("date", "pcp")
        pcp.z <- read.zoo(pcp)
        
        pcp.M <- hydroTSM::daily2monthly(pcp.z, FUN=sum)
        pcp.Y <- hydroTSM::daily2annual(pcp.z, FUN=sum)
        yrs.cc <- seq(as.numeric(format(min(index(pcp.Y)),"%Y")), 
                      as.numeric(format(max(index(pcp.Y)),"%Y")),1)
        # boxplot for data----
        
        pcp.ts <- ts(coredata(pcp.M), frequency=12, start=c(2046,1))
        
        boxplot(pcp.ts~cycle(pcp.ts), col="lightblue", ylim=c(0,1000), names=month.abb,
                main=paste("Projected climate change rainfall for ", stName, sep=""))
        
        txt1 <- summary(pcp.M)[,2]
        
        text(0.5,900, pos=4, txt1[1], cex=.8)
        text(0.5,850, pos=4, txt1[2], cex=.8)
        text(0.5,800, pos=4, txt1[3], cex=.8)
        text(0.5,750, pos=4, txt1[4], cex=.8)
        text(0.5,700, pos=4, txt1[5], cex=.8)
        text(0.5,650, pos=4, txt1[6], cex=.8)
        text(0.5,500, pos=4, paste("Mean ann. rainfall: ", round(mean(pcp.Y),0), 
                                   " (", 0-round(mean(pcp.Y)*100/mean(currPY),1),"%)",
                                   sep=""), cex=.8)
       
        
        nm <- cbind(nm,stName)
        d <- cbind(d,coredata(pcp.Y))
        
      }
      
      
      rownames(d) <- yrsSc
      colnames(d) <- nm
      rM <- rowMeans(d)
      d <- cbind(d, rM)
      
      
      
      require(fBasics)
      par(mfrow=c(1,1))
      yrsSc <- seq(as.numeric(format(min(dateSc),"%Y")),
                 as.numeric(format(max(dateSc), "%Y")), by=1)
      
      # generate stats for d----
      d1 <- d[,1:8]
      bsStat <- basicStats(d1)
      yrl.mean <- mean(colStats(d1, FUN=mean))
      yrl.stDv <- mean(colStdevs(d1))
      yrl.var <- mean(colVars(d1))
      yrl.min <- mean(colStats(d1,FUN=min))
      yrl.max <- mean(colStats(d1, FUN=max))
      ylr.
      
      d.st <- cbind(yrl.mean, yrl.stDv, yrl.var)
      

      colorsPl <- brewer.pal(ncol(d), "Set1")
      colorsGr <- brewer.pal(ncol(d), "Greys")

      # par(mfrow=c(4,2))
      require(gplots)

      # col=colorsPl[m]
      for(m in 1:ncol(d)){
        plot(d[,m], type="l", col=colorsGr[m] , axes=FALSE, xlab="", ylab="",
             ylim=c(0,round(max(d[,m]),0)+50))                    # round(max(d[,m]),0)+50)
        par(new=TRUE)
      }
      lines(d[,max(ncol(d))], col="brown", lwd=2)
      # legend(1,400, legend=nm, col=colorsGr, lty = 1, bty="n", cex=.7, ncol=3)
      box()
      axis(2,seq(0,round(max(d[,m]),0)+100,100),las=2, cex.axis=.8)
      axis(1,at=seq(1,length(yrsSc),1),labels=yrsSc, cex.axis=.8)
      title(main=paste("Mean yearly precipitation from the ", scenarioFld[k],
                 " model", sep=""))
      
      textplot(bsStat, halign="left", valign="top", cex=.7, cmar=.5)
      
      # text(11,350, paste("Mean ann. pcp: ",
      #                    round(mean(rowStats(d[,1:8], FUN="mean")),0), 
      #                    " mm", sep=""), cex=.7)
      # text(11, 300, paste("Standard dev: ", 
      #                     round(mean(rowStdevs(d[,1:8])),0), " mm", sep=""), cex=.7)
      # text(11, 250, paste(""))
      detach("package:fBasics")


      par(new=FALSE)
    }
  }
  # dev.off()
}


