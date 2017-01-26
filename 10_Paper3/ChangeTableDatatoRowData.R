
require(stats)

setwd("C:/Modelling/Data NMA/SortData")

filesp <- list.files(path=".", pattern="Ke.*\\.csv$")


for (i in seq_along(filesp)){
  ke <- read.csv(filesp[i], header=FALSE, sep=",", na.strings="NA")
  k2 <- stack(ke)
  k2 <- k2[1]
  names(k2) <- "cms"
  
  write.csv(k2, paste("st_", filesp[i], sep=""), quote=FALSE, row.names=FALSE)
  rm(ke, k2)
}

#*****************************************************************************
# Here you have to remove all the NAs before you can add date


tab <- data.frame()

filest <- list.files(path=".", pattern="st.*\\.csv$")
for (j in seq_along(filest)){
  st <- read.csv(filest[j], header=TRUE, sep=",", na.strings="NA")
  tab <- rbind(tab,st)
  
}

write.csv(tab, "KessieAllData.csv", quote=FALSE, row.names=FALSE)


