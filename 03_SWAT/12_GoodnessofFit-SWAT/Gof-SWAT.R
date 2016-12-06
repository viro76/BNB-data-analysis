#--------------------------------------------------------------
# Goodness of fit (gof) of SWAT data with the HydroGOF package
#-------------------------------------------------------------
# Date created:      03-01-2013
# Date modified:     --
# Author:            VR
#-------------------------------------------------------------

require(hydroTSM)
require(hydroGOF)
require(zoo)

# load the necessary auxiliaries
# files need to have XXData.csv in the name: maAllData.csv
# the file should have columns: (date, discharge, sedloss, prec, airmin, airmax, airav, sim)
setwd("~/R/Data/SCRP/Simulations/")
files <- list.files(pattern="*Data.csv")
names <- c("Anjeni", "AnditTid", "Maybar")

# Import the data from the folder and give it a correct name
for(i in seq_along(files)){
   
   # read all values and save them 
   x <- read.csv(files[i])
   x$date <- as.Date(x$date, format="%d-%m-%Y")
   assign(names[i],x)
#    write.csv(x, file=paste(files[i],".csv"),row.names=F)  
   
   # create subfiles inside the loop with discharge, sediment and precipitation
   obs.disch <- read.zoo(x[,c(1:2)], sep=",", format="%Y-%m-%d")
   obs.sed <- read.zoo(x[,c(1,3)], sep=",", format="%Y-%m-%d")
   obs.prec <- read.zoo(x[,c(1,5)], sep=",", format="%Y-%m-%d")
   sim.disch <- read.zoo(x[,c(1,4)],sep=",", format="%Y-%m-%d")
   
   
   # Create a specific time frame to compare data
   obs.disch.win <- window(obs.disch, start="1985-12-31", end="1994-01-01")
   sim.disch.win <- window(sim.disch, start="1985-12-31", end="1994-01-01")
#  Create a time frame of a single year
   obs.disch.88 <- window(obs.disch, start="1987-12-31", end="1989-01-01")
   sim.disch.88 <- window(sim.disch, start="1987-12-31", end="1989-01-01")


   
   
   # Numerical comparison of all data
   g <- gof(obs.disch, sim.disch)
   # Numerical comparison of window
   gg <- gof(obs.disch.win, sim.disch.win)
   # Numerical comparison of one year
   ggg <- gof(obs.disch.88, sim.disch.88)
   
   g;gg;ggg
   
   #---------------------------------------------------------------
   # Graphical comparison of observed and simulated data
   # All data
#    pdf(paste(file="~/R/Data/SCRP/Simulations/gof.",files[i],".pdf",sep=""), 
#        paper="a4r", onefile=T, height=9, width=14)
   ggof(obs.disch, sim.disch, main=paste("Observation vs simulation in",files[i],
         sep=" "), col=c("darkblue", "brown"),
        lty=c(1,1), lwd=c(2,0.7), ylab="Q [m3]", pch=c(46,46), xlab="",
        tick.tstep="years", lab.tstep="years", lab.fmt="%Y")
   
   ggof(obs.disch, sim.disch, main=paste("Observation vs simulation in",files[i],
         sep=" "), col=c("darkblue", "brown"),
        lty=c(1,1), lwd=c(2,0.7), ylab="Q [m3]", pch=c(46,46), xlab="",
        tick.tstep="years", lab.tstep="years", lab.fmt="%Y", 
      ftype="dm", FUN="mean")
   
   ggof(obs.disch, sim.disch, main=paste("Observation vs simulation in",files[i],
         sep=" "), col=c("darkblue", "brown"),
        lty=c(1,1), lwd=c(2,0.7), ylab="Q [m3]", pch=c(46,46), xlab="",
        tick.tstep="years", lab.tstep="years", lab.fmt="%Y", 
        ftype="dma", FUN="mean")
   
   #---------------------------------------------------------------
   # Graphical comparison of windowed data
   
   ggof(obs.disch.win, sim.disch.win, 
        main=paste("Goodness of fit for overlapping of SWAT simulation",files[i], sep=" "),
        col=c("darkblue", "brown"), lty=c(1,1),lwd=c(2,1.5), pch=c(46,46),
        lab.tstep="months", lab.fmt=c("%b-%y"))
   
   
   ggof(obs.disch.win, sim.disch.win, 
        main=paste("Goodness of fit for overlapping of SWAT simulation",files[i], sep=" "),
        col=c("darkblue", "brown"), lty=c(1,3),lwd=c(2,1.5), pch=c(46,46),
        lab.tstep="months", lab.fmt=c("%b-%y"), leg.cex=0.7, 
        ftype="dm", FUN="mean")
   
   
   ggof(obs.disch.win, sim.disch.win, 
        main=paste("Goodness of fit overlapping of SWAT simulation",files[i], sep=" "),
        col=c("darkblue", "brown"), lty=c(1,3),lwd=c(2,1.5), pch=c(46,46),
        lab.tstep="months", lab.fmt=c("%b-%y"), 
        ftype="dma", FUN="mean",leg.cex=0.7)
   
   #----------------------------------------------------------------
   # Graphical comparison of one year
   ggof(obs.disch.88, sim.disch.88, main=paste("Observation vs simulation of one year in",
      files[i],sep=" "), col=c("darkblue", "brown"),lty=c(1,1), 
      lwd=c(2,1.5), ylab="Q [m3]", pch=c(19,15), xlab="",tick.tstep="months", 
      lab.tstep="months", lab.fmt="%b-%y")
      
   ggof(obs.disch.88, sim.disch.88, main=paste("Observation vs simulation of one year in",
      files[i],sep=" "), col=c("darkblue", "brown"),lty=c(1,1), 
      lwd=c(2,1.5), ylab="Q [m3]", pch=c(19,15), xlab="",tick.tstep="months", 
      lab.tstep="months", lab.fmt="%b-%y", 
      ftype="dm", FUN="mean")
   dev.off()
   
}   
   
   # plot zoo objects with ggplot2
#    autoplot.zoo(obs.disch, geom="line")
   
#------------------------------------------------------------------------------   
#------------------------------------------------------------------------------   
   # Split zoo time series into years and plot 
   mydata.s <- as.xts(obs.disch)
      ts.plot(do.call(cbind, lapply(split(mydata.s, 'years'), 
                  function(x) {
                           zoo(x, 1:NROW(x))
                              })), 
              col=terrain.colors(nyears(mydata.s)), 
               main="Discharge Maybar, all years", ylab="Discharge [m3]",xlab="")
   grid(nx=NULL, ny=NULL, col="darkgrey")
   
   # Different attempt
   # from: http://stackoverflow.com/questions/9762220/r-splitting-long-zoo-time-series-into-calendar
   tt <- time(obs.disch)
   DF <- data.frame(Date = as.Date(format(tt, "2000-%m-%d")), 
                    Year = format(tt, "%Y"), 
                    discharge = coredata(obs.disch))
   z <- read.zoo(DF, split="Year")
   plot(z, col=terrain.colors(nyears(mydata.s)))
   
  
   
#------------------------------------------------------------------------------   
#------------------------------------------------------------------------------   
   
