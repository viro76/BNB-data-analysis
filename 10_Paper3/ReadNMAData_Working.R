#****************************************
# Read NMA data from rows to columns
# 
#****************************************
library(plyr)



# Test for row to column

twoyr <- read.csv("D:/7_PhD/Data/Data BNB/1_PCP/10_ToProcess/daily/AllMidabbay.csv", header=TRUE, sep=",")
# allNMA <- read.csv("C:/Modelling/Data NMA/Prec_withEmties.csv", header=TRUE)
nbrd <- read.csv("D:/7_PhD/Data/0_Original/6_Data NMA/0_Processed/nbrdys_noDate.csv", header=FALSE)     # add the number of days for each month

# Output folder
dir <- "C:/Users/Vincent Roth/Documents/R/Data/BNB/01_Comparison/MidAbbay/"

# Prepare for merging
nbrd <- read.csv("D:/7_PhD/Data/0_Original/6_Data NMA/0_Processed/nbrds.csv", header=TRUE)
twoyr$date <- paste(twoyr$Month,"1",twoyr$YEAR, sep="/")
twoyr <- merge(twoyr, nbrd, by="date")
twoyr$date <- as.Date(twoyr$date, format="%m/%d/%Y")
twoyr$NAME <- as.character(twoyr$NAME)
twoyr$YEAR <- as.numeric(twoyr$YEAR)
twoyr$Month <- as.numeric(twoyr$Month)
twoyr$Element <- as.character(twoyr$Element)
twoyr$days <- as.numeric(twoyr$days)

twoyr <- twoyr[order(twoyr$NAME, twoyr$date),]
# Remove unwanted columns----
twoyr <- twoyr[, c(1:36,38)]


# Prepare empty variables ----
monthNames <- month.abb
# Define years 
years <- unique(twoyr$YEAR)

StaNames <- unique(twoyr[, 2])
StaNames <- as.character(StaNames)

source("~/R/Scripts/17_Functions/cbindpad.R")

#********************************************
# Loop through data frame and extract rows----
dataNMA <- matrix()

for (j in seq_along(StaNames)){
  name <- StaNames[j]
  cols <- data.frame()
  bar <- subset(twoyr, NAME==StaNames[j])
  # bar$MonthN <- month.abb[bar$Month]            # Add a column with the month names
  # bar <- bar[, c(1,2,3,4,36,5:35)] 
  
  for (i in 2:nrow(bar)-1){
    nbd <- bar$days[i]
    row <- bar[i,c(6:(nbd+5))]
    date <- seq(as.Date(bar$date[i]),as.Date(paste(bar[i,4],bar[i,5],bar$days[i],sep="-")),1)
    col <- stack(row)
    col <- col[1]
    col <- cbind(date,col)
    colnames(col) <- c("date", StaNames[j])
    
    cols <- rbind(cols,col)
  }
  # dataNMA <- cbindPad(dataNMA, cols)
  # Add NA to dataframe for every date----
  nma <- cols
  beg <- as.Date(paste(format(as.Date(min(nma$date)), "%Y"), "-01-01", sep=""))
  end <- as.Date(paste(format(as.Date(max(nma$date)), "%Y"), "-12-31", sep=""))
  dd <- seq(beg, end, 1)
  ddd <- data.frame(date=dd, nb=NA)
    
  mrg <- merge(ddd, nma, by="date", all.x=TRUE)
  mrg <- mrg[,c(1,3)]
    
  write.csv(mrg, file=paste(dir,"NMA_",name, ".csv",sep=""), 
            quote=FALSE, row.names=FALSE)
  
}

