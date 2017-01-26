
indir <- "D:/7_PhD/Data/1_CompareRainfall/2_Version July 16/00_ToUseCFSRRainfall/00_Climate for Simulation/pcp"
dirs4p <- "D:/7_PhD/Data/Data BNB/1_PCP/3_CFSR/1_CFSR-Data"

folds <- list.dirs(dirs4p, full.names=FALSE, recursive=FALSE)
folds <- folds[2:5]

outdir <- "D:/7_PhD/Data/1_CompareRainfall/2_Version July 16/00_ToUseCFSRRainfall/00_Climate for Simulation/"

  

letters <- c("r", "s", "t", "w")

setwd(indir)
files <- list.files(full.names=FALSE, recursive=FALSE)
files <- files[1:82]
write.csv(files,paste(outdir, "stations.csv", sep=""), quote=FALSE, row.names=FALSE)


for(j in seq_along(letters)){
  files.n <- gsub("p", letters[j], files)
  
  for(i in seq_along(files.n)){
    n <- read.csv(paste(dirs4p, "/", folds[j], "/", files.n[i], sep="")) #  , header=FALSE
    write.csv(n, paste(outdir, folds[j], "/", files.n[i], sep=""), 
              quote=FALSE, row.names=FALSE)

  }
  
}


# find only the stations needed
useSt <- file_path_sans_ext(files)
str(useSt)
orig <- read.csv(paste(outdir, "OrigCFSR.csv", sep=""), header=TRUE)
names(orig) <- c("ID", "y", "LAT", "LONG", "ELEVATION")

origNew <- merge(orig, useSt, by="y", all.y=TRUE)
names(origNew) <- c("NAME", "ID", "LAT", "LONG", "ELEVATION")
origNew <- origNew[,c(2,1,3,4,5)]
origNew$ID <- seq(1,nrow(origNew),1)
write.csv(origNew, paste(outdir, "AllCFSR.csv"), quote=FALSE, row.names=FALSE)




#*************************************
# Add temp data to the folder
for(j in seq_along(watershed)){
  indir <- paste("C:/Users/Vincent Roth/Documents/R/Data/BNB/02_ClimateScenarios/",
                 watershed[j], sep="")
  files <- list.files(indir, pattern="^[p]")

  for(i in seq_along(files)){
    name <- files[i]
    name <- gsub("p", "t", name)
    o <- read.csv(paste(tdir,name, sep=""), header=FALSE)
    names(o) <- c("tmpMx", "tmpMn")
    o <- o[-1,]
    write.csv(o, paste(indir,name,sep="/"), quote=FALSE, row.names=FALSE)
  }
}







