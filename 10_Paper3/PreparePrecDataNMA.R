#**********************************************
# Read in NMA precipitation and temperature data and manipulate it
# into the required form for SWAT and for statistics for SWAT.
# Each stations ends up in columns with start date at top
#*********************************************
# date: 20/02/2016
# author: VR
#*********************************************

rawdata <- read.csv("C:/Modelling/Data NMA/PRECIPITATION.csv", header=TRUE, sep=",")
rawdata$Element <- as.character(rawdata$Element)
rawdata$NAME <- as.character(rawdata$NAME)



# separate precipitation and temperature from the file----
precip <- subset(rawdata, Element =="PRECIP")
temp <- subset(rawdata, Element =="TMPMAX" | Element =="TMPMIN")



# Prepare for merging----
nbrd <- read.csv("C:/Modelling/Data NMA/nbrds.csv", header=TRUE)
precip$date <- paste(precip$Month,"1",precip$YEAR, sep="/")
# precip <- merge(precip, nbrd, by="date")
precip <- merge(precip, nbrd, by="date")
precip$date <- as.Date(precip$date, format="%m/%d/%Y")
precip$NAME <- as.character(precip$NAME)
precip$YEAR <- as.numeric(precip$YEAR)
precip$Month <- as.numeric(precip$Month)
precip$Element <- as.character(precip$Element)
precip$days <- as.numeric(precip$days)

precip <- precip[order(precip$NAME, precip$date),]
# Remove unwanted columns----
precip <- precip[, c(1:36,38)]


# Prepare empty variables ----
monthNames <- month.abb
# Define years 
years <- unique(precip$YEAR)

StaNames <- unique(precip[, 2])
StaNames <- as.character(StaNames)

source("~/R/Scripts/17_Functions/cbindpad.R")

#********************************************
# Loop through data frame and extract rows----
dataNMA <- matrix()

for (j in seq_along(StaNames)){
  cols <- data.frame()
  bar <- subset(precip, NAME==StaNames[j])
  # bar$MonthN <- month.abb[bar$Month]            # Add a column with the month names
  # bar <- bar[, c(1,2,3,4,36,5:35)]

  for (i in 1:nrow(bar)){
    nbd <- bar$days[i]
    row <- bar[i,c(6:(nbd+5))]
    date <- seq(as.Date(bar$date[i]),as.Date(paste(bar[i,4],bar[i,5],bar$days[i],sep="-")),1)
    col <- stack(row)
    col <- col[1]
    col <- cbind(date,col)
    colnames(col) <- c("date", StaNames[j])

    cols <- rbind(cols,col)
  }
  dataNMA <- cbindPad(dataNMA, cols)

}
dataNMA <- dataNMA[,c(2,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)]
write.csv(dataNMA, "C:/Modelling/Data NMA/dataNMA_inCols.csv", quote=FALSE, row.names=FALSE)

#***********************************************
# Write every station to one file for statistics calculations-----
dates <- seq(as.Date("1987-01-01"), as.Date("2006-12-31"), 1)
df.date <- data.frame(date=dates, num=NA, row.names=NULL)
for (j in seq_along(StaNames)){
  cols <- data.frame()
  bar <- subset(precip, NAME==StaNames[j])
  
  for (i in 1:nrow(bar)){
    nbd <- bar$days[i]
    row <- bar[i, c(6:(nbd+5))]
    date <- seq(as.Date(bar$date[i]),as.Date(paste(bar[i,4],bar[i,5],bar$days[i],sep="-")),1)
    col <- stack(row)
    col <- col[1]
    col <- cbind(date,col)
    colnames(col) <- c("date", StaNames[j])
    cols <- rbind(cols, col)
    
  }
  
  # merge dates and cols to keep dates with NAs----
  cols <- merge(df.date, cols, by="date", all.x = TRUE)
  # Change NAs to -99
  cols[is.na(cols)] <- -99
  
  x <- assign(StaNames[j], cols[,3])
  y <- 19870101
  x <- append(y,x)
  write.table(x, paste("C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin/climate/p",StaNames[j],".txt",sep=""),
            row.names=FALSE, col.names = FALSE, sep=",")
}



#**************************************************************************************************
#                                               TEMPERATURE                                       *
#**************************************************************************************************
# Do the same thing for temperature----

# Prepare for merging----
nbrd <- read.csv("C:/Modelling/Data NMA/nbrds.csv", header=TRUE)
temp$date <- paste(temp$Month,"1",temp$YEAR, sep="/")
# temp <- merge(temp, nbrd, by="date")
temp <- merge(temp, nbrd, by="date")
temp$date <- as.Date(temp$date, format="%m/%d/%Y")
# temp$NAME <- as.character(temp$NAME)
temp$YEAR <- as.numeric(temp$YEAR)
temp$Month <- as.numeric(temp$Month)
# temp$Element <- as.character(temp$Element)
temp$days <- as.numeric(temp$days)

temp <- temp[order(temp$NAME, temp$Element, temp$date),]
# Remove unwanted columns----
temp <- temp[, c(1:36,38)]

# create max and min
tempMax <- subset(temp, Element=="TMPMAX")
tempMin <- subset(temp, Element=="TMPMIN")

# Prepare empty variables ----
monthNames <- month.abb
# Define years 
years <- unique(temp$YEAR)

StaNames <- unique(temp[, 2])
StaNames <- as.character(StaNames)

source("~/R/Scripts/17_Functions/cbindpad.R")

# #********************************************
# # Loop through data frame and extract rows----
# tempNMA <- matrix()
# 
# for (j in seq_along(StaNames)){
#   cols <- data.frame()
#   barMax <- subset(tempMax, NAME==StaNames[j], stringsAsFactors=FALSE)
#   barMin <- subset(tempMin, NAME==StaNames[j])
# 
#   for (i in 1:nrow(barMax)){
#     nbd <- barMax$days[i]
#     rowMax <- barMax[i,c(6:(nbd+5))]
#     rowMin <- barMin[i,c(6:(nbd+5))]
#     date <- seq(as.Date(barMax$date[i]),as.Date(paste(barMax[i,4],barMax[i,5],barMax$days[i],sep="-")),1)
#     colMax <- stack(rowMax)
#     colMin <- stack(rowMin)
#     colMax <- colMax[1]
#     colMin <- colMin[1]
#     col <- cbind(date,colMax,colMin)
#     colnames(col) <- c("date", "Max","Min")
#     
#     cols <- rbind(cols,col)
#   }
#   tempNMA <- cbindPad(tempNMA, cols)
#   
# }
# tempNMA <- tempNMA[,c(2,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33,34)]
# write.csv(tempNMA, "C:/Modelling/Data NMA/tempNMA_inCols.csv", quote=FALSE, row.names=FALSE)



#***************************************************************************************************
# Write every station to one file for statistics calculations-----
dates <- seq(as.Date("1987-01-01"), as.Date("2006-12-31"), 1)
df.date <- data.frame(date=dates, num=NA, row.names=NULL)

for (j in seq_along(StaNames)){
  cols <- data.frame()
  barMax <- subset(tempMax, NAME==StaNames[j])
  barMin <- subset(tempMin, NAME==StaNames[j])
  
  for (i in 1:nrow(barMax)){
    nbd <- barMax$days[i]
    rowMax <- barMax[i,c(6:(nbd+5))]
    rowMin <- barMin[i,c(6:(nbd+5))]
    date <- seq(as.Date(barMax$date[i]),as.Date(paste(barMax[i,4],barMax[i,5],barMax$days[i],sep="-")),1)
    colMax <- stack(rowMax)
    colMin <- stack(rowMin)
    colMax <- colMax[1]
    colMin <- colMin[1]
    col <- cbind(date,colMax,colMin)
    colnames(col) <- c("date", "Max","Min")
    
    cols <- rbind(cols,col)
    
  }
  
  # merge dates and cols to keep dates with NAs----
  cols <- merge(df.date, cols, by="date", all.x = TRUE)
  # Change NAs to -99
  cols[is.na(cols)] <- -99
  
  x <- assign(StaNames[j], cols[,c(3,4)])
  y <- 19870101
  x <- rbind(y,x)
  write.table(x, paste("C:/Modelling/ArcSWAT_Examples/5_BlueNileBasin/climate/t",StaNames[j],".txt",sep=""),
              row.names=FALSE, col.names = FALSE, sep=",")
  
}








