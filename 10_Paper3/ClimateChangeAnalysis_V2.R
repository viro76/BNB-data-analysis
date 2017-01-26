#

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
  filesCFSRcur <- list.files(path=paste(getwd(), n, sep="/"),pattern="^[p]")
  scenarioFld <- list.dirs(paste(getwd(), n, "climate", sep="/"), full.names=FALSE,
                         recursive=FALSE)
  
  par(mfrow=c(4,2))
  
  for(j in seq_along(filesCFSRcur)){
    # setwd
    m <- read.csv(paste(getwd(), n, filesCFSRcur[j], sep="/"), header=TRUE,
                  na.strings=c("NA", "-99.000", "-99"))
    dd <- seq(as.Date(start), as.Date(start + nrow(m)-1), by="days")
    m <- cbind(dd,m)
    names(m) <- c("date", "pcp")
    nameSt <- file_path_sans_ext(filesCFSRcur[j])
    
    # Prepare the different formats
    nowD <- read.zoo(m, format="%Y-%m-%d")
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
    av <- colMeans(n.matrix)
    
    colors <- brewer.pal(length(scenarioFld), "Pastel2")
    # Prepare data frame----
    d <- data.frame()
    
    #**********************************
    # Plot the main CFSR data----
    
    # 
    # cairo_pdf(paste(getwd(), "/",watershed[i],".pdf", sep=""), width=11.69,
    #           height=8.27, onefile=TRUE)
    plot(av, type="l", col="brown", lwd=2, ylim=c(0,700),
         main=paste("Mean monthly rainfall in ", n, " watershed \n",
                    "Comparing climate change data to CFSR ", nameSt, sep=""),
         ylab="Precipitation [mm]", xlab="", axes=FALSE)
    box()
    axis(1, seq(1,12,1), labels = month.abb)
    axis(2, seq(0,700,100))
    
    #*****************************************************
    # Plot the scenarios from these folders----
    for(k in seq_along(scenarioFld)){
      filesCFSRScen1 <- list.files(paste(getwd(), n, "climate", scenarioFld[k],
                                         "pcp", sep="/"))
      
      
      
      #***********************************
      # add the projected CFSR data----
      for(l in seq_along(filesCFSRScen1)){
        q <- read.csv(paste(getwd(), n, "climate", scenarioFld[k], "pcp", 
                            filesCFSRScen1[l], sep="/"), 
                      header=TRUE, na.strings=c("-99.000", "NA", "-99"))
        ddd <- seq(as.Date(startScen1), as.Date(startScen1+nrow(q)-1), by="days")
        
        q <- cbind(ddd,q)
        names(m) <- c("date", "pcp")
        nameStScen1 <- file_path_sans_ext(filesCFSRScen1[l])
        
        # Prepare the different formats
        scen1D <- read.zoo(q, format="%Y-%m-%d")
        scen1D <- window(scen1D, start="2046-01-01", end="2064-12-31")
        scen1M <- daily2monthly(scen1D, FUN=sum, na.rm=TRUE)
        scen1Y <- daily2annual(scen1D, FUN=sum, na.rm=TRUE)
        
        # Compile different necessary parameters----
        n.yrs.sc1 <- yip(from=start(scen1M), to=end(scen1M), out.type="nmbr")
        beg.yrs.sc1 <- as.numeric(format(min(index(scen1Y)), "%Y"))
        end.yrs.sc1 <- as.numeric(format(max(index(scen1Y)), "%Y"))
        beg.mo.sc1 <- as.numeric(format(min(index(scen1M)), "%m"))
        end.mo.sc1 <- as.numeric(format(max(index(scen1M)), "%m"))
        
        sc1.ts <- ts(scen1M, freq=12, start(c(beg.yrs.sc1,beg.mo.sc1), 
                                            end=c(end.yrs.sc1, end.mo.sc1)))
        
        nmon <- format(time(scen1M), "%b")
        monts <- factor(nmon, level=unique(nmon), ordered=TRUE)
        
        sc1.matrix <- matrix(sc1.ts, ncol=12, byrow=TRUE)
        colnames(sc1.matrix) <- month.abb
        rownames(sc1.matrix) <- as.numeric(format(index(scen1Y), "%Y"))
        
        av.scen1 <- colMeans(sc1.matrix)
        par(new=TRUE)
        plot(av.scen1, type="l", col=colors[k], axes=FALSE, ylab="", xlab="")
        
      }
      f <- cbind(scenarioFld[k], colors[k])
      d <- rbind(d, f)
      
    }
    par(new=TRUE)
    plot(av, type="l", col="brown", lwd=3, ylab="", xlab="", axes=FALSE, ylim=c(0,700))
    # legend(1,600, legend=as.character(d$V1), pch=.8, 
    #        fill=as.character(d$V2), bty="n")
    
    # legend(1,600, as.character(d$V1), bty="n", text.col=as.character(d$V2), pch=.8)
    
    
  }
  # dev.off()
}


