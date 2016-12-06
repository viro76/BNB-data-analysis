allDatFAO <- read.csv("~/R/Data/02_CDEDatablog/data/FAO_disch_Minch.txt", sep="\t", na.strings = "NA")
allDatWLRC <- read.csv("~/R/Data/02_CDEDatablog/data/WLRC_disch_Minch.txt", sep="\t", na.strings = "NA")


observationFAO <- ts(allDatFAO$observed, start=c(1986,1), freq=12)
bestsimFAO <- ts(allDatFAO$Best_Sim, start=c(1986,1), freq=12)


simsobFAO <- cbind(observationFAO,bestsimFAO)

uprFAO <- ts(allDatFAO$U95PPU, start=c(1986,1), freq=12)
lwrFAO <- ts(allDatFAO$L95PPU, start=c(1986,1), freq=12)


observationWLRC <- ts(allDatWLRC$observed, start=c(1986,1), freq=12)
bestsimWLRC <- ts(allDatWLRC$Best_Sim, start=c(1986,1), freq=12)


simsobWLRC <- cbind(observationWLRC,bestsimWLRC)

uprWLRC <- ts(allDatWLRC$U95PPU, start=c(1986,1), freq=12)
lwrWLRC <- ts(allDatWLRC$L95PPU, start=c(1986,1), freq=12)



# Add all the data together
ppuFAO <- cbind(observationFAO, bestsimFAO, uprFAO, lwrFAO, bestsimWLRC, uprWLRC, lwrWLRC)
ppuFAO



#******************************************
# add labels, legend in vertical, and show missing data
dygraph(ppuFAO, main="Results for Minchet catchment\n(1986-2014)", 
        ylab="Discharge [m3/s]", group = "discharge") %>%
   dyAxis("x", drawGrid=TRUE, rangePad = 10) %>%
   dySeries(c("lwrFAO", "bestsimFAO", "uprFAO"), label="Best sim FAO", strokeWidth=3, strokePattern = "dashed") %>%
   dyOptions(colors=RColorBrewer::brewer.pal(3, "Set1"), strokeWidth=1.2,
             connectSeparatedPoints=FALSE, titleHeight=32) %>%
   # Add the WLRC data on the same graph
   dySeries(c("lwrWLRC", "bestsimWLRC", "uprWLRC"), label="Best sim WLRC", strokeWidth=3, strokePattern = "dotted") %>%
   dyOptions(colors=RColorBrewer::brewer.pal(3, "Set1"), strokeWidth=1.2,
             connectSeparatedPoints=FALSE, titleHeight=32) %>%
      # Finish the graph
   dyShading(from="1998-12-31", to="2009-12-31", color="#FFF5FF") %>%
   dyShading(from="2010-12-31", to="2011-12-31", color="#FFF5FF") %>%
   dyRangeSelector(height=25) %>%
   dyLegend(show = "auto", labelsSeparateLines = TRUE) %>%
   dyEvent("1999-01-01", labelLoc = "top", color="darkred", label="Calibration-validation line") %>%
   dyAnnotation("2005-06-01", text = "Missing data", width = 100, height=15, attachAtBottom = TRUE)



#**************************************
# do the same for the WLRC data











