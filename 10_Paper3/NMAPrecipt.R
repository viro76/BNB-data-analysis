# ********************************************** 
# Read in NMA precipitation data
#  and manipulate it into a certain format.
# *********************************************
library(funModeling)


rawdata <- read.csv("C:/Modelling/Data NMA/PRECIPITATION.csv", 
  header = TRUE, sep = ",")
df_status(rawdata)

# Take every row with month 1 and stack it
StaNames <- unique(rawdata[, 1])
StaNames <- as.character(StaNames)

for (k in seq_along(StaNames)) {
  bar <- subset(rawdata, NAME == StaNames[k])
  assign("Station", bar)
}

precip <- subset(Station, Element == "PRECIP")
year <- unique(precip[, 3])

trnspPr <- data.frame()

for (i in seq_along(year)) {
  yr <- subset(precip, YEAR == year[i])
  
  for (j in 1:12) {
    line <- precip[j, c(5:35)]
    line <- t(line)
    line <- unlist(lapply(line, "[[", 1))
    trnspPr <- rbind(trnspPr, line)
  }
}

