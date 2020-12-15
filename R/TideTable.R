#' Computes a tide table
#' 
#' @description Takes a data frame as input with date time, high water and height information and returns a tide table
#' @references Horn, W. (1960) Some Recent Approaches to Tidal Problems. Int. Hydrogr. Rev. 37(2), 65-84
#' @references  Godin, Gabriel (1972) The Analysis of Tides. Toronto, 264pp
#' @references \url{https://www.bsh.de/DE/PUBLIKATIONEN/_Anlagen/Downloads/Meer_und_Umwelt/Berichte-des-BSH/Berichte-des-BSH_50_de.pdf?__blob=publicationFile&v=13}
#' @references \url{https://doi.org/10.5194/os-15-1363-2019}
#' @param dataInput A data frame with the columns observation_date, observation_time, high_or_low_water and height. See attached data for correct formats.
#' @param otz The time zone of the observations
#' @param hwi The average of all intervals between the Moon's transit (upper or lower) over the Greenwich meridian and the following high or low waters for all phases of the Moon is known as mean high water lunitidal interval and is abbreviated to high              water interval (hwi). Please only supply a value, when you are sure. Otherwise leave the default value "99:99" untouched.            hwi is then computed for you.
#' @param sharp_hwi Default is TRUE, which results in a sharp hwi computation. Set on FALSE if you analyze shorter time intervals and EstimateTmhwi function returns NA.
#' @param asdate A string indication the date you want the analysis to start with. Format: "yyyy/mm/dd".
#' @param astime A string indicating the time you want the analysis to start with. Format: "hh:mm:ss"
#' @param aedate A string indication the date you want the analysis to end with. Format: "yyyy/mm/dd".
#' @param aetime A string indicating the time you want the analysis to end with. Format: "hh:mm:ss"
#' @param ssdate Synthesis start date. This indicates the date you want your tide table to start with. Format: See above
#' @param sstime Synthesis start time. The starting time for your tide table. Format: See above
#' @param sedate Synthesis end date. Format: See above
#' @param setime Synthesis end time. Format: See above
#' @param stz Dummy for later extension to modify target time zone.
#' @return Returns a list with elements of the analysis, fitting and the tide table for given data 
#' \item{c.table}{The complete synthesis data as a data.table object}
#' \item{tide.table}{The tide table as a data.table object}
#' \item{lm.coeff}{Coefficients for the eight fitted linear models used in the synthesis}
#' \item{diff.analyse}{Time in days spanning the analysis}
#' \item{i.analyse}{How many different cases where used in the analysis}
#' @examples 
#' TideTable(dataInput = observation, asdate = "1991/01/01", 
#' astime = "12:00:00", 
#' aedate = "1992/01/01", aetime = "12:00:00", ssdate = "1991/01/01", 
#' sstime = "00:00:00", sedate = "1991/01/31", setime = "21:00:00")
#' 
#' @import data.table
#' @import chron
#' @importFrom stats coef
#' @importFrom stats coefficients
#' @importFrom stats lm.fit
#' @importFrom stats sd
#' @export
TideTable <- function(dataInput, otz = 1, hwi = "99:99", sharp_hwi = TRUE, asdate, astime, aedate, aetime, ssdate, sstime, sedate, setime, stz = 1) {
  
  chron.origin <- chron(dates. = "1900/01/01", 
                        times. = "00:00:00",
                        format = c(dates = "y/m/d", times = "h:m:s"),
                        out.format = c(dates = "y/m/d", times = "h:m:s"))
  tperiode.m2  <- 360 / 28.9841042
  tmean.moon   <- tperiode.m2 * 2
  tm24         <- tmean.moon / 24
  tmoon.0      <- chron(dates. = "1949/12/31", 
                        times. = "21:08:00",
                        format = c(dates = "y/m/d", times = "h:m:s"),
                        out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin
  
  tplus        <- as.numeric(tmoon.0 + 24.2491 / 1440.00)
  numm         <- NULL
  

  
  chron.beob      <- chron(dates. = as.character(dataInput[["observation_date"]]),
                           times. = as.character(dataInput[["observation_time"]]),
                           format = c(dates = "y/m/d", times = "h:m:s"),
                           out.format = c(dates = "y/m/d", times = "h:m:s"))
  
  diff.days       <- as.numeric((chron.beob - chron.origin) - otz / 24) 
  high.low        <- dataInput[["high_or_low_water"]]
  
  #Computation of tmhwi, when not supplied by user
  
  mhist.table     <- data.table(d_days = diff.days, 
                                high_low = high.low, 
                                height = dataInput[["height"]])
  
  if (unlist(strsplit(hwi, ":"))[1] == "99") {
    #Compute tmhwi here
    tmhwi           <- EstimateTmhwi(input = mhist.table, strict = sharp_hwi) / 24
  } else {
    tmhwi <- as.numeric(unlist(strsplit(hwi, ":"))[1]) / 24 + as.numeric(unlist(strsplit(hwi, ":"))[2]) / 1440
  }
  
  nummk4          <- NumCulm(t = diff.days, tmhwi = tmhwi)
  tmmt.numm       <- nummk4[["numm"]] * tm24 + tplus
  stunden.transit <- (diff.days - tmmt.numm) * 24
  
  design.frame  <- data.table(numm            = nummk4[["numm"]],
                              k               = nummk4[["k4"]],
                              stunden.transit = stunden.transit,
                              height          = dataInput[["height"]],
                              high.low        = high.low)
  
  #removing rows where k+high.low is odd
  design.frame <- design.frame[((k + high.low) %% 2) != 1]
  #Dropping the high.low column
  design.frame[, high.low := NULL]
  
  asdate.time <- chron(dates. = asdate,
                       times. = astime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin
  
  aedate.time <- chron(dates. = aedate,
                       times. = aetime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin
  
  #Synthesis period
  ssdate.time <- chron(dates. = ssdate,
                       times. = sstime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin
  sedate.time <- chron(dates. = sedate,
                       times. = setime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin
  #NumCulm Synthesis
  start.nummculm  <- NumCulm(t = ssdate.time, tmhwi = tmhwi)
  end.nummculm    <- NumCulm(t = sedate.time, tmhwi = tmhwi)
  
  #NumCulm Analysis
  astart.nummculm  <- NumCulm(t = asdate.time, tmhwi = tmhwi)
  aend.nummculm    <- NumCulm(t = aedate.time, tmhwi = tmhwi)
  
  #Computing Funcs for all cases
  
  tdiff.analyse    <- aend.nummculm[["numm"]] - astart.nummculm[["numm"]] + 1
  #Compute Funcs for theoretical xi to get the length of the column vector
  omega_t          <- FindOmega(tdiff = tdiff.analyse)
  func_t           <- ComputeAfunc(omega = omega_t, xi = aend.nummculm[["numm"]])
  matrix.cols      <- length(func_t[[3]])
  
  #Subsetting design.frame
  
  design.frame     <- design.frame[(numm >= astart.nummculm[["numm"]]) & (numm <= aend.nummculm[["numm"]])]
  
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
      # predictant <- design.frame[k == k4, ..l]
      predictant <- design.frame[k == k4, .SD, .SDcols = l]
      mean_p     <- predictant[, mean(get(l))]
      sd_p       <- predictant[, 3 * sd(get(l))]
      
      temp.design[[l]] <- data.table(design.matrix, predictant, predictor)
      temp.design[[l]] <- temp.design[[l]][(get(l) >= mean_p -  sd_p) & (get(l) <= mean_p + sd_p)]
      # lm.fitting[[l]]  <- lm.fit(x = as.matrix(temp.design[[l]][, !c(..l, "predictor")]),
      #                            y = temp.design[[l]][, get(l)])
      
      lm.fitting[[l]]  <- lm.fit(x = as.matrix(temp.design[[l]][, .SD, .SDcols = !c(l, "predictor")]),
                                 y = temp.design[[l]][, get(l)])
      
      i.analyse[k4, l] <- nrow(temp.design[[l]])
    }
    lm.fits[[k4]]      <- lm.fitting
    # design.list[[k4]]  <- temp.design
    fitting.coef[[k4]] <- lapply(lm.fitting, function(x) {
      c_o <- as.vector(coef(x))
      c_o[is.na(c_o)] <- 0 
      return(c_o)
      
    })
  }
  #Synthesis
  time1        <- vector(mode = "double")
  height       <- vector(mode = "double")
  afunc        <- vector(mode = "double")
  coeff        <- vector(mode = "double")
  st.transit   <- vector(mode = "double")
  
  time.height  <- matrix(0.0, ncol = 6,
                         nrow = ((end.nummculm[["numm"]] - start.nummculm[["numm"]] + 1) * 4))
  
  m  <- 0L
  ii <- 0L
  for (i in start.nummculm[["numm"]] : end.nummculm[["numm"]]) {
    ii <- ii + 1L
    afunc <- ComputeAfunc(xi = i, omega = omega_t)[[3]]
    for (k in 1 : 4) {
      m <- m + 1L
      for (l in c("stunden.transit", "height")) {        
        coeff <- fitting.coef[[k]][[l]]
        summe <- coeff %*% afunc
        
        if (l == "stunden.transit") {
          st.transit <- summe
          tmmt.numm     <- i * tm24 + tplus
          time1      <- (tmmt.numm + summe / 24 + otz / 24)
          
        }
        else {
          height <- summe
        }   
      }
      if (k == 1 | k == 3){
        ihn <- 1
      } else {
        ihn <- 0
      }
      if (k == 1 | k == 2) {
        trans <- 1
      } else {
        trans <- 0
      }
      
      time.height[m, ] <- c(time1, ihn, trans, height,
                            st.transit, ii)
    }
  }
  date_time       <- NULL
  prediction_date <- NULL
  prediction_time <- NULL
  V1              <- NULL
  
  time.height <- as.data.table(time.height)
  
  time.height[ ,date_time := format(chron(dates. = V1, 
                                 origin. = c(month = 1, day = 1, year = 1900)),
                                 "%Y/%m/%d %H:%M:%S" )]
  time.height[, c("prediction_date", "prediction_time") := tstrsplit(date_time, split = " ")]
  
  setnames(time.height, c("V6","V5","V4","V3","V2", "V1"), 
           c("i", "st.transit", "height", "upper_or_lower_transit",
             "high_or_low_water", "transit"))
  
  time.height[, date_time := NULL]
  setcolorder(time.height, c("transit",
                             "prediction_date",
                             "prediction_time",
                             "high_or_low_water",
                             "upper_or_lower_transit",
                             "height",
                             "st.transit",
                             "i"))
  tide.table <- time.height[, c("prediction_date",
                               "prediction_time",
                               "high_or_low_water",
                               "height")]
  #we return a list containing c.table, tide table, diff.analyse, i.analyse and lm.coeff
  report              <- list(
    "c.table"      = time.height,
    "tide.table"   = tide.table,
    "diff.analyse" = tdiff.analyse,
    "i.analyse"    = i.analyse,
    "lm.coeff"     = fitting.coef,
    "tmhwi"        = tmhwi
  )

  return(report)
}

