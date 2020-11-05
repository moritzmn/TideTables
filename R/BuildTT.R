#' Builds a TideTable model
#' @description Builds a TideTable model of class "tidetable".
#' @param data the data frame with observation date, observation time and height. 
#' @param otz time zone of the observations. Default is 1 (UTC + 1)
#' @param start_date The start date
#' @param start_time The start time
#' @param end_date The end date
#' @param end_time The end time
#' @return Returns a object of class "tidetable"
#' @export

BuildTT <- function(data, otz = 1, start_date, start_time, end_date, end_time) {
  
  #Constants
  chron.origin <- chron(dates. = "1900/01/01", 
                        times. = "00:00:00",
                        format = c(dates = "y/m/d", times = "h:m:s"),
                        out.format = c(dates = "y/m/d", times = "h:m:s"))
  tmoon.0      <- chron(dates. = "1949/12/31", 
                        times. = "21:08:00",
                        format = c(dates = "y/m/d", times = "h:m:s"),
                        out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin
  
  tplus        <- tmoon.0 + 24.2491 / 1440.00
  tperiode.m2  <- 360 / 28.9841042
  tmean.moon   <- tperiode.m2 * 2
  tm24         <- tmean.moon / 24
  numm         <- NULL
  
  #Reading the data
  chron.beob      <- chron(dates. = as.character(data$observation_date),
                           times. = as.character(data$observation_time),
                           format = c(dates = "y/m/d", times = "h:m:s"),
                           out.format = c(dates = "y/m/d", times = "h:m:s"))
  
  diff.days       <- (chron.beob - chron.origin) - otz / 24 
  high.low        <- data$high_or_low_water
  
  #Analysis date and times as chron
  asdate.time <- chron(dates. = asdate,
                       times. = astime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin
  
  aedate.time <- chron(dates. = aedate,
                       times. = aetime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin
  
  
  
  #Computation of tmhwi, when not supplied by user
  mhist.table     <- data.table(d_days = diff.days, high_low = high.low, height = dataInput$height)
  
  if (unlist(strsplit(hwi, ":"))[1] == "99") {
    #Compute tmhwi here
    tmhwi           <- EstimateTmhwi(input = mhist.table, strict = sharp_hwi) / 24
  } else {
    tmhwi <- as.numeric(unlist(strsplit(hwi, ":"))[1]) / 24 + as.numeric(unlist(strsplit(hwi, ":"))[2]) / 1440
  }
  
  nummk4          <- NumCulm(t = diff.days, tmhwi = tmhwi)
  tmmt.numm       <- nummk4$numm * tm24 + tplus
  stunden.transit <- (diff.days - tmmt.numm) * 24
  
  design.frame  <- data.table(numm            = nummk4$numm,
                              k               = nummk4$k4,
                              stunden.transit = stunden.transit,
                              height          = dataInput$height,
                              high.low        = high.low)
  
  #removing rows where k+high.low is odd
  design.frame <- design.frame[((k + high.low) %% 2) != 1]
  #Dropping the high.low column
  design.frame[, high.low := NULL]
  
  #NumCulm Analysis
  astart.nummculm  <- NumCulm(t = asdate.time, tmhwi = tmhwi)
  aend.nummculm    <- NumCulm(t = aedate.time, tmhwi = tmhwi)
  
  #Computing Funcs for all cases
  tdiff.analyse    <- -astart.nummculm$numm + aend.nummculm$numm + 1
  #Compute Funcs for theoretical xi to get the length of the column vector
  omega_t          <- FindOmega(tdiff = tdiff.analyse)
  func_t           <- ComputeAfunc(omega = omega_t, xi = aend.nummculm$numm)
  matrix.cols      <- length(func_t[[3]])
  
  design.frame     <- design.frame[(numm >= astart.nummculm$numm) & (numm <= aend.nummculm$numm)]
  
  lm.fitting          <- list()
  lm.fits             <- list()
  fitting.coef        <- list()
  i.analyse           <- matrix(nrow = 4, ncol = 2)
  colnames(i.analyse) <- c("stunden.transit", "height")
  rownames(i.analyse) <- 1 : 4
  temp.design         <- list()
  design.list         <- list()
  
  for(k4 in 1 : 4) {    
    predictor     <- design.frame[k == k4, numm]
    
    design.matrix <- matrix(nrow = length(predictor), ncol = matrix.cols)
    
    for(i in 1 : nrow(design.matrix)) {
      design.matrix[i, 1 : matrix.cols] <- ComputeAfunc(xi = predictor[i], omega = omega_t)[[3]]
    }
    
    for(l in c("stunden.transit", "height")) {
      predictant <- design.frame[k == k4, ..l]
      mean_p     <- predictant[, mean(get(l))]
      sd_p       <- predictant[, 3 * sd(get(l))]
      
      temp.design[[l]] <- data.table(design.matrix, predictant, predictor)
      temp.design[[l]] <- temp.design[[l]][(get(l) >= mean_p -  sd_p) & (get(l) <= mean_p + sd_p)]
      lm.fitting[[l]]  <- lm.fit(x = as.matrix(temp.design[[l]][, !c(..l, "predictor")]),
                                 y = temp.design[[l]][, get(l)])
      i.analyse[k4, l] <- nrow(temp.design[[l]])
    }
    lm.fits[[k4]]      <- lm.fitting
    design.list[[k4]]  <- temp.design
    fitting.coef[[k4]] <- lapply(lm.fitting, function(x) {
      c <- as.vector(coef(x))
      c[is.na(c)] <- 0 
      return(c)
      
    })
  }
  
  #return object and class setting
  tt_object <- list("tdiff" = tdiff.analyse,
                    "omega_t" = omega_t,
                    "tm24" = tm24,
                    "tplus" = tplus,
                    "tmhwi" = tmhwi,
                    "fitting.coef" = fitting.coef)
  
  class(tt_object) <- "tidetable"
  
  return(tt_object)
  
}