#****************************************
# Read NMA data from rows to columns for Monthly DATA
# 
#****************************************
library(plyr)
library(reshape2)
library(reshape)

dirs.init <- "D:/7_PhD/Data/Data BNB/1_PCP/10_ToProcess/monthly/"
setwd(dirs.init)
dirs <- list.dirs(full.names = FALSE, recursive = FALSE)

for (i in seq_along(dirs)){
  di <- dirs[i]
  setwd(paste(dirs.init, di, sep=""))
  
  files <- list.files()
  
  for(j in seq_along(files)){
    n <- read.csv(files[j], header=TRUE)
    names(n)[2:length(n)] <- month.abb
    
    dataNMA <- data.frame(values=numeric)
    da <- data.frame(yr=date)
    
    
    for(k in 1:nrow(n)){
      # extract the values and change them to one column
      y <- stack(n[k,2:13])
      y <- y[1]
      dataNMA <- rbind(dataNMA,y)
      # extract the dates
      yr <- seq(as.Date(paste(n[k,1],"1","1",sep="-"), format="%Y-%m-%d"),
                  length=12, by="months")
      yr <- as.data.frame(yr)
      da <- rbind(da, yr)
    }
    dataNMA <- cbind(da,dataNMA)
    names(dataNMA) <- c("date", "pcp")
    dataNMA$date <- format(dataNMA$date, "%m/%d/%Y")
    
    # Export data to csv and check if there is already a file with this name
    dir.out <- paste("~/R/Data/BNB/01_Comparison/", di,"/", sep="")
    if(file.exists(paste(dir.out,files[j], sep=""))){
          write.csv(dataNMA, paste(dir.out, "doublefiles/", files[j], sep=""), quote=FALSE,
                    row.names=FALSE)
    } else {
      write.csv(dataNMA, paste(dir.out,files[j], sep=""), quote=FALSE, row.names=FALSE)
    }
    
  }
}