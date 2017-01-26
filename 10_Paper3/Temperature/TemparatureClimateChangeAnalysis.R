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
  tmpCFSRcur <- list.files(path=paste(getwd(), n, sep="/"), pattern="^[t]")
  scenarioFld <- list.dirs(paste(getwd(), n, "climate", sep="/"), full.names=FALSE,
                           recursive=FALSE)
  par(mfrow=c(4,2))
  
  for(j in seq_along(tmpCFSRcur)){
    currTmp <- read.csv(paste(getwd(),n,tmpCFSRcur[j],sep="/"),
                        na.strings=c("NA","-99", "-99.000"),stringsAsFactors=FALSE,header=TRUE)
    
    dateCur <- seq(start, as.Date(start + nrow(currTmp)-1, format="%Y-%m-%d"), by="days")
    yrs <- seq(as.numeric(format(min(dateCur),"%Y")),as.numeric(format(max(dateCur),"%Y")),by=1)
    tcpname <- file_path_sans_ext(tmpCFSRcur[j])
    currTmp <- cbind(dateCur,currTmp)
    currT.z <- read.zoo(currTmp)
    currT.z <- window(currT.z, from=start(index(currT.z)), to=as.Date("2013-12-31"))
    

    # monthly and annual data aggregation----
    currTM <- daily2monthly(currT.z, FUN=mean)
    currTy <- daily2annual(currT.z, FUN=mean)
    
    # Create ts for max and min tmp ----
    Tmx.ts <- ts(coredata(currTM$tmpMx), frequency=12, start=c(1979,1))
    Tmn.ts <- ts(coredata(currTM$tmpMn), frequency=12, start=c(1979,1))
    
    plot.ts(Tmx.ts, ylim=c(10,36),
            ylab="", xlab="", axes=FALSE, col="brown")
    lines(filter(Tmx.ts, filter=rep(1/25,25)), col="brown", lwd=2)
    abline(reg=lm(Tmx.ts~time(Tmx.ts)), col="brown")
    par(new=TRUE)
    plot.ts(Tmn.ts, ylim=c(10,36),
            ylab="Temperature [°C]", main="Min & max temperature from CFSR data", col="darkblue")
    lines(filter(Tmn.ts, filter=rep(1/25,25)), col="darkblue", lwd=2)
    abline(reg=lm(Tmn.ts~time(Tmn.ts)), col="darkblue")
    legend(1980,21, legend=c("Maximum temp", "Minimum temp"),lty=1, 
           col=c("brown", "darkblue"), bty="n", cex=.8)
    
    
    # decompose ts----
    require(TTR)
    TmxDecomp <- decompose(Tmx.ts)
    TmnDecomp <- decompose(Tmn.ts)
    
    plot(TmxDecomp, col="brown")
    plot(TmnDecomp)
    
    # Seasonally adjusted ts----
    tmx.seaonAdj <- Tmx.ts - TmxDecomp$seasonal
    tmn.seaonAdj <- Tmn.ts - TmnDecomp$seasonal
    
    par(mfrow=c(2,1))
    
    plot(tmx.seaonAdj, ylab="Temperature [°C]", main="Seaonally adjusted maximum temperature",
         col="brown")
    plot(tmn.seaonAdj, ylab="Temperature [°C]", main="Seaonally adjusted minimum temperature",
         col="darkblue")
    
    
    par(mfrow=c(1,1))
    
    
    
    
    
    
    
    for(k in seq_along(scenarioFld)){
      tmpdir <- paste(getwd(),watershed[i],"climate",scenarioFld[k],"tmp",sep="/")
      
      tmpfls <- list.files(tmpdir, pattern="^[T]")
      
      
      tMx <- NULL
      tMn <- NULL
      nm <- NULL
      
      for(l in 1:length(tmpfls)){
        tmp <- read.csv(paste(tmpdir,tmpfls[l],sep="/"), header=FALSE, 
                        na.strings=c("NA", "-99","-99.000"),
                        stringsAsFactors=FALSE)
        tmp <- tmp[-1,]
        
        stName <- file_path_sans_ext(tmpfls[l])
        
        dateSc <- seq(startScen1, startScen1+nrow(tmp)-1, by="days")
        tmp <- cbind(dateSc,tmp)
        names(tmp) <- c("date", "tmpMx", "tmpMn")
        tmpMx <- tmp[,c(1,2)]
        tmpMn <- tmp[,c(1,3)]
        
        tmpMx.z <- read.zoo(tmpMx)
        tmpMn.z <- read.zoo(tmpMn)
        
        
        tmpMn.Y <- daily2annual(tmpMn.z, FUN=mean)
        tmpMx.Y <- daily2annual(tmpMx.z, FUN=mean)
        
        nm <- cbind(nm,stName)
        
        tMx <- cbind(tMx, coredata(tmpMx.Y))
        tMn <- cbind(tMn, coredata(tmpMn.Y))
        
      }
      
      yrsSc <- seq(as.numeric(format(min(dateSc),"%Y")),
                   as.numeric(format(max(dateSc), "%Y")), by=1)
      
      rownames(tMx) <- yrsSc
      rownames(tMn) <- yrsSc
    
      colnames(tMx) <- nm
      colnames(tMn) <- nm
      rmTx <- rowMeans(tMx)
      rmTn <- rowMeans(tMn)
      
      tMx <- cbind(tMx, rmTx)
      tMn <- cbind(tMn, rmTn)
      
      colorsPl <- brewer.pal(ncol(tMx), "Set1")
      colorsGr <- brewer.pal(ncol(tMx), "Greys")
      colorsPb <- brewer.pal(ncol(tMn), "PuBu")
      
      # par(mfrow=c(4,2))
      
      # col=colorsPl[m]
      for(m in 1:ncol(tMx)){
        plot(tMx[,m], type="l", col=colorsGr[m] , axes=FALSE, xlab="", ylab="",
             ylim=c(0,round(max(tMx[,m]),0)+(max(tMx[,m]/10))))                    # round(max(d[,m]),0)+50)
        par(new=TRUE)
        plot(tMn[,m], type="l", col=colorsGr[m], axes=FALSE, xlab="", ylab="",
             ylim=c(0,round(max(tMx[,m]),0)+(max(tMx[,m]/10))))
        par(new=TRUE)
      }
      lines(tMx[,max(ncol(tMx))], col="brown", lwd=2)
      lines(tMn[,max(ncol(tMn))], col="brown", lwd=2)
      legend(1,10, legend=nm, col=colorsGr, lty = 2, bty="n", cex=.7, ncol=3)
      legend(12,10,legend=c("Mean maximum temp", "Mean minimum temp"),
             col=c("brown", "brown"), lty=2, bty="n", cex=.7)
      box()
      axis(2,seq(0,round(max(tMx[,m]),0)+5,5),las=2, cex.axis=.8)
      axis(1,at=seq(1,length(yrsSc),1),labels=yrsSc, cex.axis=.8)
      title(main=paste("Mean yearly temperatures from the \n", scenarioFld[k], 
                       " model in ", n, sep=""))
      
      
      par(new=FALSE)
    }
  }
  # dev.off()
}


