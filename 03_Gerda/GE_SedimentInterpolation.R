#***********************************************
# Gerda Hydrology and sedimentology
# 
#***********************************************

library(chron)
library(dplyr)

setwd("~/R/Data/Gerda/sediment/")

ge14 <- "ye14RSRD"


ge <- read.csv(paste(ge14,".csv", sep=""), header=TRUE)
# ge$DATE <- as.Date(ge$DATE, format="%d/%m/%Y")
# OR
ge$DATE <- as.Date(ge$DATE, format="%m/%d/%Y")

# define the time
ge$DateTimeSt <- strptime(paste(ge$DATE, ge$START), "%Y-%m-%d %H:%M")
ge$DateTimeEn <- strptime(paste(ge$DATE, ge$END), "%Y-%m-%d %H:%M")
ge$DurationS <- (ge$DateTimeEn - ge$DateTimeSt)*60
ge$DateSmpl <- strptime(paste(ge$DATE, ge$SAMPLTIME), "%Y-%m-%d %H:%M")

#*********************************************
# Calculate discharge according to waterlevel
# H.Sutter calculated by m3/s --> change to l/s

for(i in 1:nrow(ge)){
      h <- ge$TRUEWATERL[i]
      if (h < 0.865){
         q <- (9.1251 * (h + 0.0)^3.1406)
      } else if (h > 1.954){
         q <- (16.6778 * (h-0.038)^1.5468)
      } else {
         q <- (8.3542 * (h+0.0)^2.5333)
      }
      ge$RUNOFF[i] <- q
      rm(h,i,q)
}

ge$DISCHARGE <- round(ge$RUNOFF*ge$DurationS, 4)         # This is m3 --> (m3/s * s)
ge$DISCHARGEinL <- round(ge$DISCHARGE*1000, 4)


# Sediment
#*************************************************
# load sediment data

ye <- "ye14RSSL"
# ye <- "ye13RSSL.csv"

#*************************************
yesd <- read.csv(paste(ye, ".csv", sep=""), header=TRUE)
# Change date to data format
yesd$DATE <- as.Date(yesd$DATE, format="%m/%d/%Y")
# OR
# yesd$DATE <- as.Date(yesd$DATE, format="%d.%m.%Y")

yesd$DateTime <- strptime(paste(yesd$DATE, yesd$SAMPLTIME), "%Y-%m-%d %H:%M")

# Not always necessary
yesd$DRYSOILWER <- yesd$FULLOWER - yesd$EMPYWER


# Merge both data frames to one
# Merge RSRD and RSSL by the "DATE" column while keeping all rows from RSRD
geAll <- merge(ge, yesd, by.x="DateTimeSt", by.y="DateTime", all=TRUE)

geAll <- geAll[order(geAll$DateTimeSt),]
row.names(geAll) <- NULL

geInter <- geAll[,c(1,9,10,11,12,13,19)]
row.names(geInter) <- NULL 
# write.csv(geAll, "geInter.csv")

Compile sediment in river for the time given
for(i in 1:nrow(geInter)){
   if(!is.na(geInter$DateSmpl[i])){
      geInter$SedimentinT[i] <- (geInter$DISCHARGEinL[i] * geInter$DRYSOILWER[i])/1000000
   } else {
      geInter$SedimentinT[i] <- NA
   }
}

#*****************************************************************************
#*****************************************************************************
#*****************************************************************************
# Special calculations for years which have only RSSL and no RSRD

for(i in 1:nrow(yesd)){
   h <- yesd$TRUEWATERL[i]/100
   if (h < 0.865){
      q <- (9.1251 * (h + 0.0)^3.1406)
   } else if (h > 1.954){
      q <- (16.6778 * (h-0.038)^1.5468)
   } else {
      q <- (8.3542 * (h+0.0)^2.5333)
   }
   yesd$RUNOFF[i] <- q
   rm(h,i,q)
}

yesd$DISCHARGE <- yesd$RUNOFF*600
yesd$DISCHARGEinL <- round(yesd$DISCHARGE*1000)

#************************************************

for(i in 1:nrow(yesd)){
   yesd$SedimentinT[i] <- (yesd$DISCHARGEinL[i] * yesd$DRYSOILWER[i])/1000000
}

yesd <- yesd[, c(1,2,9,10,11)]

write.csv(yesd, "ye14SoilLoss.csv", quote=FALSE, row.names=FALSE)

#*****************************************************************************
#*****************************************************************************
#*****************************************************************************
#*****************************************************************************




dailyGESed <- geInter[, c(1,9,11)]
dailyGESed <- aggregate(dailyGESed[c("DISCHARGEinL","SedimentinT")], 
                        by=list(Group.date=dailyGESed$DATE.x), FUN=sum, na.rm=TRUE)  

write.csv(dailyGESed, paste(ye, "_R.csv", sep=""), row.names=FALSE, quote=FALSE)


#***************************

for(i in 1:nrow(ge)){
   if(!is.na(ge$DateSmpl[i])){
      ajInter$SedimentinT[i] <- (ajInter$DISCHARGEinL[i] * ajInter$DRYSOILWER[i])/1000000
   } else {
      ajInter$SedimentinT[i] <- NA
   }
}

dailyAJSed <- ajInter[, c(1,6,7,8,12)]
dailyAJSed <- aggregate(dailyAJSed[c("DISCHARGEinL","SedimentinT")], 
                        by=list(Group.date=dailyAJSed$DATE.x), FUN=sum, na.rm=TRUE)  











#******************************************************************************
#***************************       ANJENI      ********************************
#******************************************************************************
#******************************************************************************
# Same for Anjeni------
aj12 <- "aj12RSRD.csv"
aj13 <- "aj13RSRD"



setwd("~/R/Data/Gerda/sediment")
#*********************
# Adapt filename here
aj <- read.csv(paste(aj13,".csv", sep=""), header=TRUE)

#***************
aj$DATE <- as.Date(aj$DATE, format="%m/%d/%Y")

# define the time
aj$DateTimeSt <- strptime(paste(aj$DATE, aj$START), "%Y-%m-%d %H:%M")
aj$DateTimeEn <- strptime(paste(aj$DATE, aj$END), "%Y-%m-%d %H:%M")
aj$DurationS <- (aj$DateTimeEn - aj$DateTimeSt)*60


aj$DateSmpl <- strptime(paste(aj$DATE, aj$SAMPLTIME), "%Y-%m-%d %H:%M")

for(i in 1:nrow(aj)){
   h <- aj$TRUEWATERL[i]
   if (h < 60){
      q <- (1.2 * (h + 0.0)^1.5)
   } else if (h > 119){
      q <- (70 * h-3620)
   } else {
      q <- (0.7 * h^2 -56*h + 1400)
   }
   #       print(i)
   #       print(h)
   #       print(q)
   aj$RUNOFF[i] <- q
}
rm(h,i,q)

aj$DISCHARGE <- round(aj$RUNOFF*aj$DurationS/1000, 3)
aj$DISCHARGEinL <- round(aj$RUNOFF*aj$DurationS, 3)

# Write it to a file
write.csv(aj, paste(aj13, "_R.csv", sep=""))



#*************************************************
# load sediment data

aj12 <- "aj12RSSL.csv"

#*************************************
ajsd <- read.csv(aj12, header=TRUE)
ajsd$DATE <- as.Date(ajsd$DATE, format="%m/%d/%Y")
ajsd$DateTime <- strptime(paste(ajsd$DATE, ajsd$SAMPLTIME), "%Y-%m-%d %H:%M")
ajsd$DRYSOILWER <- ajsd$FULLOWER - ajsd$EMPTYWER


# Merge both data frames to one
# Merge RSRD and RSSL by the "DATE" column while keeping all rows from RSRD
ajAll <- merge(aj, ajsd, by.x="DateSmpl", by.y="DateTime", all.x=TRUE)

ajAll <- ajAll[order(ajAll$DateTimeSt),]
row.names(ajAll) <- NULL

ajInter <- ajAll[,c(2,3,4,8,9,10,16,13,14,15,1,22)]
row.names(ajInter) <- NULL 

# Compile sediment in river for the time given
for(i in 1:nrow(ajInter)){
   if(!is.na(ajInter$DateSmpl[i])){
      ajInter$SedimentinT[i] <- (ajInter$DISCHARGEinL[i] * ajInter$DRYSOILWER[i])/1000000
   } else {
      ajInter$SedimentinT[i] <- NA
   }
}

dailyAJSed <- ajInter[, c(1,7,13)]
dailyAJSed <- aggregate(dailyAJSed[c("DISCHARGEinL","SedimentinT")], 
                        by=list(Group.date=dailyAJSed$DATE.x), FUN=sum, na.rm=TRUE)  

write.csv(dailyAJSed, paste(aj12,"_R.csv", sep=""), row.names=FALSE, quote=FALSE)



























