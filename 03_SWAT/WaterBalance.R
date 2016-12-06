#****************************************
# Analyse the water balance in SWAT 
# Taken from: http://santiago.begueria.es/2013/10/unraveling-the-hru-water-balance-in-swat-i/
#***************************************



t <- 2:12
tt <- t-1
DStorage <- c(NA, SNOFALL[t] - SNOMELT[t] +
                 SW_END[t] - SW_END[tt] + SA_ST[t] -
                 SA_ST[tt] + DA_ST[t] - DA_ST[tt])
Flows <- PRECIP + IRR + GW_RCHG -
   ET - REVAP - (SURQ - TLOSS) -
   LATQ - GW_Q - PERC
BAL = Flows - DStorage




setwd("C:/Modelling/ArcSWAT_Examples/4_Gerda/GerdaClassic/Scenarios/WLRC_31yrs_NewDEM/TxtInOut/")
hrudata <- read.table("output.hru", skip = 9)
dim(hrudata)
header <- read.csv("headersHRU.csv")
























