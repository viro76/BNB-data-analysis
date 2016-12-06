function (sim, obs, na.rm = TRUE, dates, date.fmt = "%Y-%m-%d", 
          pt.style = "ts", ftype = "o", FUN, gof.leg = TRUE, digits = 2, 
          legend, leg.cex = 1, tick.tstep = "auto", lab.tstep = "auto", 
          lab.fmt = NULL, cal.ini = NA, val.ini = NA, main, xlab = "Time", 
          ylab = c("Q, [m3/s]"), col = c("blue", "brown"), cex = c(0.5, 
                           0.5), cex.axis = 1.2, cex.lab = 1.2, lwd = c(1, 1), 
                           lty = c(1,3),pch = c(1, 9), ...) 
{
   valid.class <- c("xts", "zoo", "numeric", "integer")
   
   if (length(which(!is.na(match(class(sim), valid.class)))) <= 0)
      stop("Invalid argument: 'class(sim)' must be in c('xts', 'zoo', 'numeric', 'integer')")
   
   if (length(which(!is.na(match(class(obs), valid.class)))) <= 0) 
      stop("Invalid argument: 'class(obs)' must be in c('xts', 'zoo', 'numeric', 'integer')")
   
   if (length(sim) != length(obs)) 
      stop("Invalid argument: 'obs' and 'sim' must have the same length ! (",
           length(obs), "vs", length(sim), ")")
   sim.name <- deparse(substitute(sim))
   obs.name <- deparse(substitute(obs))
   
   if (missing(legend)) 
      legend <- c(sim.name, obs.name)
   
   if (zoo::is.zoo(obs) & zoo::is.zoo(sim)) {
      if (all.equal(time(obs), time(sim)) != TRUE) 
         stop("Invalid argument: 'obs' and 'sim' have different time stamps !")
      }
      
   if (!missing(dates)) {
      if (length(dates) != length(sim)) 
         stop("Invalid argument: 'dates' and 'sim' must have the same length")
      
      if (is.na(match(class(dates), c("character", "factor", 
                                      "Date", "POSIXct")))) 
         stop("Invalid argument: 'class(dates)' must be in c('character', 'factor', 'Date', 'POSIXct')")
      
      if (class(dates)[1] %in% c("factor", "character")) {
         ifelse(grepl("%H", date.fmt, fixed = TRUE) | grepl("%M", 
         date.fmt, fixed = TRUE) | grepl("%S", date.fmt, 
         fixed = TRUE) | grepl("%I", date.fmt, fixed = TRUE) | 
                   grepl("%p", date.fmt, fixed = TRUE) | grepl("%X", 
                  date.fmt, fixed = TRUE), subdaily <- TRUE, subdaily <- FALSE)
         
         ifelse(subdaily, dates <- as.POSIXct(dates, format = date.fmt), 
                dates <- as.Date(dates, format = date.fmt))
      }
      
      if (zoo::is.zoo(obs)) 
         time(obs) <- dates
      
      if (zoo::is.zoo(sim)) 
         time(sim) <- dates
   }
   else if (!zoo::is.zoo(obs)) 
      message("[ Note: You did not provide dates, so only a numeric index will be used in the time axis ]")
   
   if (!zoo::is.zoo(obs) & !missing(dates)) {
      obs <- hydroTSM::vector2zoo(x = obs, dates = dates, date.fmt = date.fmt)
   }
   else if (zoo::is.zoo(obs) & missing(dates)) {
      
      if (class(time(obs))[1] %in% c("Date", "POSIXct")) {
         dates <- time(obs)
      }
      else if (class(time(obs))[1] == "character") 
         dates <- as.Date(time(obs), format = "%Y")
   }
   
   if (!zoo::is.zoo(sim) & !missing(dates)) {
      sim <- hydroTSM::vector2zoo(x = sim, dates = dates, date.fmt = date.fmt)
   }
   else if (zoo::is.zoo(sim) & zoo::is.zoo(obs) & missing(dates)) {
      if (class(time(sim))[1] %in% c("Date", "POSIXct")) {
         dates <- time(obs)
      }
      else if (class(time(sim))[1] == "character") {
         dates <- as.Date(time(sim), format = "%Y")
      }
   }
   
   if (is.na(match(ftype, c("o", "dm", "ma", "dma")))) 
      stop("Invalid argument: 'ftype' must be in c('o', 'dm', 'ma, 'dma')")
   
   if (!zoo::is.zoo(sim) & !zoo::is.zoo(sim)) {
      if (!is.na(match(ftype, c("dm", "ma", "dma")))) 
         message("[ Note: 'sim' & 'obs' are not zoo objects => 'ftype' was changed to 'o' ]")
      ftype <- "o"
   }
   else if (zoo::is.zoo(sim)) 
      sim.freq <- xts::periodicity(sim)$scale
   
   if (!is.na(match(ftype, c("dm", "ma", "dma"))) & missing(FUN)) 
      stop("Missing argument: 'FUN' must be provided when 'ftype' is in c('dm', 'ma, 'dma')")
   
   if (missing(main)) 
      main <- "Observations vs Simulations"
   
# ----------------------------------------------------------------------------
# First type of plot 
   if (ftype == "o") {
      plot2(x = sim, y = obs, plot.type = "single", main = main, 
            col = col, lwd = lwd, lty = lty, pch = pch, xlab = xlab, 
            ylab = ylab, pt.style = pt.style, add = FALSE, tick.tstep, 
            lab.tstep, lab.fmt = lab.fmt, cex = cex, cex.axis = cex.axis, 
            cex.lab = cex.lab, gof.leg = gof.leg, gof.digits = digits, 
            legend = legend, leg.cex = leg.cex, cal.ini = cal.ini, 
            val.ini = val.ini, date.fmt = date.fmt, ...)
   }
   
# ----------------------------------------------------------------------------
# Second type of plot    
   
   else if (ftype == "dm") {
      if (sim.freq != "daily") {
         stop("Invalid argument: 'sim' has to have a 'daily' sampling frequency")
      }
      else {
         obs.monthly <- hydroTSM::daily2monthly(obs, FUN, 
                                                na.rm)
         sim.monthly <- hydroTSM::daily2monthly(sim, FUN, 
                                                na.rm)
         def.par <- par(no.readonly = TRUE)
         on.exit(par(def.par))
         if (gof.leg) {
            layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 
                            2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4), ncol = 11, 
                          byrow = TRUE))
         }
         else {
            par(mfrow = c(2, 1))
         }
         par(mar = c(5, 4, 4, 0) + 0.1)
         plot2(x = sim, y = obs, plot.type = "single", main = paste("Daily", 
                                                                    main, sep = " "), tick.tstep = tick.tstep, lab.tstep = lab.tstep, 
               lab.fmt = lab.fmt, cex = cex, cex.axis = cex.axis, 
               cex.lab = cex.lab, col = col, lwd = lwd, lty = lty, 
               pch = pch, xlab = xlab, ylab = ylab, pt.style = "ts", 
               add = TRUE, gof.leg = gof.leg, gof.digits = digits, 
               legend = legend, leg.cex = leg.cex, cal.ini = cal.ini, 
               val.ini = val.ini, date.fmt = date.fmt, ...)
         par(mar = c(5, 4, 4, 0) + 0.1)
         plot2(x = sim.monthly, y = obs.monthly, plot.type = "single", 
               main = paste("Monthly", main, sep = " "), tick.tstep = tick.tstep, 
               lab.tstep = lab.tstep, lab.fmt = lab.fmt, cex = cex, 
               cex.axis = cex.axis, cex.lab = cex.lab, col = col, 
               lwd = lwd, lty = lty, pch = pch, xlab = xlab, 
               ylab = ylab, pt.style = "ts", add = TRUE, gof.leg = gof.leg, 
               gof.digits = digits, legend = legend, leg.cex = leg.cex, 
               cal.ini = cal.ini, val.ini = val.ini, date.fmt = date.fmt, 
               ...)
      }
   }
   
# ----------------------------------------------------------------------------
# Third type of plot    
   else if (ftype == "ma") {
      if (is.na(match(sim.freq, c("daily", "monthly")))) {
         stop("Invalid argument: the sampling frequency of 'sim' has to be in c('daily', 'monthly'")
      }
      else {
         if (sim.freq == "daily") {
            obs <- hydroTSM::daily2monthly(obs, FUN, na.rm)
            sim <- hydroTSM::daily2monthly(sim, FUN, na.rm)
         }
         obs.annual <- hydroTSM::monthly2annual(obs, FUN, 
                                                na.rm, out.fmt = "%Y-%m-%d")
         sim.annual <- hydroTSM::monthly2annual(sim, FUN, 
                                                na.rm, out.fmt = "%Y-%m-%d")
         def.par <- par(no.readonly = TRUE)
         on.exit(par(def.par))
         if (gof.leg) {
            layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 
                            2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4), ncol = 11, 
                          byrow = TRUE))
         }
         else {
            par(mfrow = c(2, 1))
         }
         par(mar = c(5, 4, 4, 0) + 0.1)
         plot2(x = sim, y = obs, plot.type = "single", main = paste("Monthly", 
               main, sep = " "), tick.tstep = tick.tstep, lab.tstep = lab.tstep, 
               lab.fmt = lab.fmt, cex = cex, cex.axis = cex.axis, 
               cex.lab = cex.lab, col = col, lwd = lwd, lty = lty, 
               pch = pch, xlab = xlab, ylab = ylab, pt.style = "ts", 
               add = TRUE, gof.leg = gof.leg, gof.digits = digits, 
               legend = legend, leg.cex = leg.cex, cal.ini = cal.ini, 
               val.ini = val.ini, date.fmt = date.fmt, ...)
         par(mar = c(5, 4, 4, 0) + 0.1)
         plot2(x = sim.annual, y = obs.annual, plot.type = "single", 
               main = paste("Annual", main, sep = " "), tick.tstep = "years", 
               lab.tstep = "years", cex = cex, cex.axis = cex.axis, 
               cex.lab = cex.lab, lab.fmt = lab.fmt, col = col, 
               lwd = lwd, lty = lty, pch = pch, xlab = xlab, 
               ylab = ylab, pt.style = pt.style, add = TRUE, 
               gof.leg = gof.leg, gof.digits = digits, legend = legend, 
               leg.cex = leg.cex, cal.ini = cal.ini, val.ini = val.ini, 
               date.fmt = date.fmt, ...)
      }
   }

   
# ----------------------------------------------------------------------------
# Fourth type of plot  
  else if (ftype == "dma") {
      if (sim.freq != "daily") {
         stop("Invalid argument: 'sim' has to have a 'daily' sampling frequency")
      }
      else {
         obs.monthly <- hydroTSM::daily2monthly(obs, FUN, 
                                                na.rm)
         sim.monthly <- hydroTSM::daily2monthly(sim, FUN, 
                                                na.rm)
         obs.annual <- hydroTSM::daily2annual(obs, FUN, na.rm, 
                                              out.fmt = "%Y-%m-%d")
         sim.annual <- hydroTSM::daily2annual(sim, FUN, na.rm, 
                                              out.fmt = "%Y-%m-%d")
         def.par <- par(no.readonly = TRUE)
         on.exit(par(def.par))
         if (gof.leg) {
            layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 
                            2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 5, 5, 5, 
                            5, 5, 5, 5, 5, 5, 6, 6), ncol = 11, byrow = TRUE))
         }
         else {
            par(mfrow = c(3, 1))
         }
         par(mar = c(5, 4, 4, 0) + 0.1)
         plot2(x = sim, y = obs, plot.type = "single", main = paste("Daily", 
               main, sep = " "), tick.tstep = tick.tstep, lab.tstep = lab.tstep, 
               lab.fmt = lab.fmt, cex = cex, cex.axis = cex.axis, 
               cex.lab = cex.lab, col = col, lwd = lwd, lty = lty, 
               pch = pch, xlab = xlab, ylab = ylab, pt.style = "ts", 
               add = TRUE, gof.leg = gof.leg, gof.digits = digits, 
               legend = legend, leg.cex = leg.cex, cal.ini = cal.ini, 
               val.ini = val.ini, date.fmt = date.fmt, ...)
         par(mar = c(5, 4, 4, 0) + 0.1)
         plot2(x = sim.monthly, y = obs.monthly, plot.type = "single", 
               main = paste("Monthly", main, sep = " "), tick.tstep = tick.tstep, 
               lab.tstep = lab.tstep, lab.fmt = lab.fmt, cex = cex, 
               cex.axis = cex.axis, cex.lab = cex.lab, col = col, 
               lwd = lwd, lty = lty, pch = pch, xlab = xlab, 
               ylab = ylab, pt.style = "ts", add = TRUE, gof.leg = gof.leg, 
               gof.digits = digits, legend = legend, leg.cex = leg.cex, 
               cal.ini = cal.ini, val.ini = val.ini, date.fmt = date.fmt, 
               ...)
         par(mar = c(5, 4, 4, 0) + 0.1)
         plot2(x = sim.annual, y = obs.annual, plot.type = "single", 
               main = paste("Annual", main, sep = " "), tick.tstep = "years", 
               lab.tstep = "years", lab.fmt = lab.fmt, cex = cex, 
               cex.axis = cex.axis, cex.lab = cex.lab, col = col, 
               lwd = lwd, lty = lty, pch = pch, xlab = xlab, 
               ylab = ylab, pt.style = pt.style, add = TRUE, 
               gof.leg = gof.leg, gof.digits = digits, legend = legend, 
               leg.cex = leg.cex, cal.ini = cal.ini, val.ini = val.ini, 
               date.fmt = date.fmt, ...)
      }
   }
}
