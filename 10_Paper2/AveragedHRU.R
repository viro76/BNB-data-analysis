#*****************************************
# Read out parameters from SWAT and calculate
# average, max and min
#*****************************************

library(timeSeries)
require(zoo)
require(hydroGOF)
require(xts)

#*********************************************
# before calibration
fld <- "E:/ArcSWAT_Examples/1_Anjeni NNR/Scenarios/Default/TxtInOut/"


fld <- "E:/ArcSWAT_Examples/4_Gerda NNR/FAO/Scenarios/2015-11-11_FAO_NwLU_HeatUnits/TxtInOut/"
fld <- "E:/ArcSWAT_Examples/4_Gerda/GerdaNewLU/Scenarios/2015-11-11_WLRC_NwLU_HeatUnits/TxtInOut/"

# after calibration
fld <- "E:/ArcSWAT_Examples/4_Gerda NNR/FAO/Scenarios/FAO_Calibr_Daily/TxtInOut/"
fld <- "E:/ArcSWAT_Examples/4_Gerda/GerdaNewLU/Scenarios/DailyWLRC_singleCalibrated/TxtInOut/"

#AnditTid-WLRC
fld <- "E:/ArcSWAT_Examples/3_AnditTid/Scenarios/AT_WLRC_82-14_mon/TxtInOut/"


#*********************************
# HRU_SLP
hrus <- list.files(fld, pattern="*.hru")
#****************Adapt the number of files***********************************
hrus <- hrus[c(1:728)]
hrusforav <- data.frame()
SummaryHRU_SLP <- data.frame()

for(i in 1:length(hrus)){
  hru <- hrus[i]
  catch <- read.table(paste(fld,hru, sep=""), skip=3, nrow=1)
  hrusforav <- rbind(hrusforav, catch[1])
}

av <- colMeans(hrusforav)
maxi <- max(hrusforav)
mini <- min(hrusforav)

SummaryHRU_SLP <- rbind(SummaryHRU_SLP, "Average" =av, "Maximum" =maxi, "Minimum" =mini)


#**************************************************************************
# same for SLSUBBSN

hrus <- list.files(fld, pattern="*.hru")
hrus <- hrus[c(1:728)]
slsubforav <- data.frame()

for(i in 1:length(hrus)){
  hru <- hrus[i]
  catch <- read.table(paste(fld,hru, sep=""), skip=2, nrow=1)
  slsubforav <- rbind(slsubforav, catch[1])
  
}

av <- colMeans(slsubforav)
maxi <- max(slsubforav)
mini <- min(slsubforav)

SummarySLS <- rbind(av, maxi, mini)
#SummaryAll <- cbind(SummaryAll, SummarySLS)

#**************************************************************************
# same for OV_N


hrus <- list.files(fld, pattern="*.hru")
hrus <- hrus[c(1:728)]
slsubforav <- data.frame()

for(i in 1:length(hrus)){
   hru <- hrus[i]
   catch <- read.table(paste(fld,hru, sep=""), skip=4, nrow=1)
   slsubforav <- rbind(slsubforav, catch[1])
   
}

av <- colMeans(slsubforav)
maxi <- max(slsubforav)
mini <- min(slsubforav)

SummaryOV <- rbind(av, maxi, mini)
#SummaryAll <- cbind(SummaryAll, SummaryOV)


#**************************************************************************
# same for ESCO

hrus <- list.files(fld, pattern="*.hru")
hrus <- hrus[c(1:728)]
ESCOforav <- data.frame()

for(i in 1:length(hrus)){
   hru <- hrus[i]
   catch <- read.table(paste(fld,hru, sep=""), skip=9, nrow=1)
   ESCOforav <- rbind(ESCOforav, catch[1])
   
}

av <- colMeans(ESCOforav)
maxi <- max(ESCOforav)
mini <- min(ESCOforav)

SummaryESCO <- rbind(av, maxi, mini)
#SummaryAll <- cbind(SummaryAll, SummaryESCO)

#**************************************************************************
# same for EPCO

hrus <- list.files(fld, pattern="*.hru")
hrus <- hrus[c(1:728)]
EPCOforav <- data.frame()

for(i in 1:length(hrus)){
   hru <- hrus[i]
   catch <- read.table(paste(fld,hru, sep=""), skip=10, nrow=1)
   EPCOforav <- rbind(EPCOforav, catch[1])
   
}

av <- colMeans(EPCOforav)
maxi <- max(EPCOforav)
mini <- min(EPCOforav)

SummaryEPCO <- rbind(av, maxi, mini)
#SummaryAll <- cbind(SummaryAll, SummaryEPCO)



#**************************************************************************
# same for USLE_K

sols <- list.files(fld, pattern="*.sol")
sols <- sols[c(1:728)]
solsforav <- data.frame()

for(i in 1:length(sols)){
  sol <- sols[i]
  catch <- read.table(paste(fld,sol, sep=""), skip=17, nrow=1)
  catch <- catch[4]
  solsforav <- rbind(solsforav, catch[1]) 
}

av <- colMeans(solsforav)
maxi <- max(solsforav)
mini <- min(solsforav)

SummaryUSLE_K <- rbind(av, maxi, mini)



#**************************************************************************
# same for USLE_P

mgt <- list.files(fld, pattern="*.mgt")
mgt <- mgt[c(1:728)]
mgtsforav <- data.frame()

for(i in 1:length(mgt)){
  mgts <- mgt[i]
  catch <- read.table(paste(fld,mgts, sep=""), skip=11, nrow=1)
  catch <- catch[1]
  mgtsforav <- rbind(mgtsforav, catch[1]) 
}

av <- colMeans(mgtsforav)
maxi <- max(mgtsforav)
mini <- min(mgtsforav)

SummaryUSLE_P <- rbind(av, maxi, mini)


#********************************************************************************
# Add everything into one file
#SummaryAll <- cbind(SummaryAll, SummaryMgt)


#********************************************************************************
# same for GW_DELAY

gwd <- list.files(fld, pattern="*.gw")
gwd <- gwd[c(1:728)]
gwdsforav <- data.frame()

for(i in 1:length(mgt)){
   gwds <- gwd[i]
  catch <- read.table(paste(fld,gwds, sep=""), skip=3, nrow=1)
  catch <- catch[1]
  gwdsforav <- rbind(gwdsforav, catch[1]) 
}

av <- colMeans(gwdsforav)
maxi <- max(gwdsforav)
mini <- min(gwdsforav)

SummaryGW_DELAY <- rbind(av, maxi, mini)


#*******************
# same for GWQMN

GWQMN <- list.files(fld, pattern="*.gw")
GWQMN <- GWQMN[c(1:728)]
GWQMNforav <- data.frame()

for(i in 1:length(GWQMN)){
   GWQMNs <- GWQMN[i]
   catch <- read.table(paste(fld,GWQMNs, sep=""), skip=5, nrow=1)
   catch <- catch[1]
   GWQMNforav <- rbind(GWQMNforav, catch[1]) 
}

av <- colMeans(GWQMNforav)
maxi <- max(GWQMNforav)
mini <- min(GWQMNforav)

SummaryGWQMN <- rbind(av, maxi, mini)


#********************************
# Same for GW_REVAP

GW_REVAP <- list.files(fld, pattern="*.gw")
GW_REVAP <- GW_REVAP[c(1:728)]
GW_REVAPforav <- data.frame()

for(i in 1:length(GW_REVAP)){
   GW_REVAPs <- GW_REVAP[i]
   catch <- read.table(paste(fld,GW_REVAPs, sep=""), skip=6, nrow=1)
   catch <- catch[1]
   GW_REVAPforav <- rbind(GW_REVAPforav, catch[1]) 
}

av <- colMeans(GW_REVAPforav)
maxi <- max(GW_REVAPforav)
mini <- min(GW_REVAPforav)

SummaryGW_REVAP <- rbind(av, maxi, mini)


#********************************
# Same for REVAPMN

REVAPMN <- list.files(fld, pattern="*.gw")
REVAPMN <- REVAPMN[c(1:728)]
REVAPMNforav <- data.frame()

for(i in 1:length(REVAPMN)){
   REVAPMNs <- REVAPMN[i]
   catch <- read.table(paste(fld,REVAPMNs, sep=""), skip=7, nrow=1)
   catch <- catch[1]
   REVAPMNforav <- rbind(REVAPMNforav, catch[1]) 
}

av <- colMeans(REVAPMNforav)
maxi <- max(REVAPMNforav)
mini <- min(REVAPMNforav)

SummaryREVAPMN <- rbind(av, maxi, mini)

#********************************
# Same for RCHRG_DP

RCHRG_DP <- list.files(fld, pattern="*.gw")
RCHRG_DP <- RCHRG_DP[c(1:728)]
RCHRG_DPforav <- data.frame()

for(i in 1:length(RCHRG_DP)){
   RCHRG_DPs <- RCHRG_DP[i]
   catch <- read.table(paste(fld,RCHRG_DPs, sep=""), skip=8, nrow=1)
   catch <- catch[1]
   RCHRG_DPforav <- rbind(RCHRG_DPforav, catch[1]) 
}

av <- colMeans(RCHRG_DPforav)
maxi <- max(RCHRG_DPforav)
mini <- min(RCHRG_DPforav)

SummaryRCHRG_DP <- rbind(av, maxi, mini)

#******************************
# Same for CN2

CN2 <- list.files(fld, pattern="*.mgt")
CN2 <- CN2[c(1:728)]
CN2forav <- data.frame()

for(i in 1:length(CN2)){
   CN2s <- CN2[i]
   catch <- read.table(paste(fld,CN2s, sep=""), skip=10, nrow=1)
   catch <- catch[1]
   CN2forav <- rbind(CN2forav, catch[1]) 
}

av <- colMeans(CN2forav)
maxi <- max(CN2forav)
mini <- min(CN2forav)

SummaryCN2 <- rbind(av, maxi, mini)

#******************************
# Same for CH_N2

CH_N2 <- list.files(fld, pattern="*.rte")
CH_N2 <- CH_N2[c(1:12)]
CH_N2forav <- data.frame()

for(i in 1:length(CH_N2)){
   CH_N2s <- CH_N2[i]
   catch <- read.table(paste(fld,CH_N2s, sep=""), skip=5, nrow=1)
   catch <- catch[1]
   CH_N2forav <- rbind(CH_N2forav, catch[1]) 
}

av <- colMeans(CH_N2forav)
maxi <- max(CH_N2forav)
mini <- min(CH_N2forav)

SummaryCH_N2 <- rbind(av, maxi, mini)


#******************************
# Same for CH_COV1

CH_COV1 <- list.files(fld, pattern="*.rte")
CH_COV1 <- CH_COV1[c(1:12)]
CH_COV1forav <- data.frame()

for(i in 1:length(CH_COV1)){
   CH_COV1s <- CH_COV1[i]
   catch <- read.table(paste(fld,CH_COV1s, sep=""), skip=7, nrow=1)
   catch <- catch[1]
   CH_COV1forav <- rbind(CH_COV1forav, catch[1]) 
}

av <- colMeans(CH_COV1forav)
maxi <- max(CH_COV1forav)
mini <- min(CH_COV1forav)

SummaryCH_COV1 <- rbind(av, maxi, mini)

#********************************************************************************
# Same for USLE_C
plant <- read.table(paste(fld, "plant.dat", sep=""), header=FALSE, fill=TRUE)
readout <- seq(3,590,5)
usle_c <- plant$V10[readout]

av <- mean(usle_c)
maxi <- max(usle_c)
mini <- min(usle_c)

SummaryUSLE_C <- rbind(av, maxi, mini)

#********************************************************************************
# Add everything into one file
SummaryAll <- cbind(SummaryGW_DELAY, SummaryRCHRG_DP, SummaryCN2, SummaryESCO, SummaryGWQMN,
                    SummaryGW_REVAP, SummaryREVAPMN, SummaryUSLE_K, SummaryHRU_SLP, SummarySLS, SummaryUSLE_C,
                    SummaryUSLE_P, SummaryCH_COV1, SummaryEPCO, SummaryOV, SummaryCH_N2) 

names(SummaryAll) <- c("GW_DELAY","RCHRG_DP","CN2","ESCO","GWQMN","GW_REVAP","REVAPMN","USLE_K","HRU_SLP",
                       "SLSUBBSN","USLE_C","USLE_P","CH_COV1","EPCO","OV_N","CH_N2")

SummaryAll

# rm(SummaryAll)
# WLRC <- rbind(SummaryAll, SummaryAll2)
