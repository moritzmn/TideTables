#' Builds a TideTable model
#' @description Builds a TideTable model of class "tidetable".
#' @references \url{https://www.bsh.de/DE/PUBLIKATIONEN/_Anlagen/Downloads/Meer_und_Umwelt/Berichte-des-BSH/Berichte-des-BSH_50_de.pdf?__blob=publicationFile&v=13/}
#' @references \url{https://doi.org/10.5194/os-15-1363-2019}
#' @param dataInput the data frame with observation date, observation time and height. 
#' @param otz time zone of the observations. Default is 1 (UTC + 1)
#' @param asdate The start date.Format: "yyyy/mm/dd"
#' @param astime The start time. Format: "hh:mm:ss"
#' @param aedate The end date. Format: "yyyy/mm/dd" 
#' @param aetime The end time. Format: "hh:mm:ss"
#' @param hwi The high water interval. Format: "hh::mm"
#' @param sharp_hwi should the hwi computation be sharp? Default is TRUE
#' @return Returns a object of class "tidetable" which contains following elements:
#' \item{fitting.coeff}{Coefficients for the eight fitted linear models used in the synthesis}
#' \item{diff.analyse}{Time in days spanning the analysis}
#' \item{omega_t}{Return value of FindOmega()}
#' \item{tm24}{Internal constant}
#' \item{tplus}{Internal constant}
#' \item{tmhwi}{Mean high water interval}
#' @examples 
#' BuildTT(dataInput = observation, asdate = "1991/01/01", 
#' astime ="12:00:00", aedate = "1992/01/01", aetime = "12:00:00")
#' @export

BuildTT <- function(dataInput, otz = 1, asdate, astime, aedate, aetime, hwi = "99:99", sharp_hwi = TRUE) {
  
  #Constants
  chron.origin <- chron(dates. = "1900/01/01", 
                        times. = "00:00:00",
                        format = c(dates = "y/m/d", times = "h:m:s"),
                        out.format = c(dates = "y/m/d", times = "h:m:s"))
  tmoon.0      <- chron(dates. = "1949/12/31", 
                        times. = "21:08:00",
                        format = c(dates = "y/m/d", times = "h:m:s"),
                        out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin
  
  tplus        <- as.numeric(tmoon.0 + 24.2491 / 1440.00)
  tperiode.m2  <- 360 / 28.9841042
  tmean.moon   <- tperiode.m2 * 2
  tm24         <- tmean.moon / 24
  numm         <- NULL
  k <- NULL
  
  
  #Reading the data
  chron.beob      <- chron(dates. = as.character(dataInput[["observation_date"]]),
                           times. = as.character(dataInput[["observation_time"]]),
                           format = c(dates = "y/m/d", times = "h:m:s"),
                           out.format = c(dates = "y/m/d", times = "h:m:s"))
  
  diff.days       <- as.numeric((chron.beob - chron.origin) - otz / 24 )
  high.low        <- dataInput[["high_or_low_water"]]
  
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
  
  #NumCulm Analysis
  astart.nummculm  <- NumCulm(t = asdate.time, tmhwi = tmhwi)
  aend.nummculm    <- NumCulm(t = aedate.time, tmhwi = tmhwi)
  
  #Computing Funcs for all cases
  tdiff.analyse    <- aend.nummculm[["numm"]] - astart.nummculm[["numm"]] + 1
  #Compute Funcs for theoretical xi to get the length of the column vector
  omega_t          <- FindOmega(tdiff = tdiff.analyse)
  func_t           <- ComputeAfunc(omega = omega_t, xi = aend.nummculm[["numm"]])
  matrix.cols      <- length(func_t[[3]])
  
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
    #design.list[[k4]]  <- temp.design
    fitting.coef[[k4]] <- lapply(lm.fitting, function(x) {
      c_o <- as.vector(coef(x))
      c_o[is.na(c_o)] <- 0 
      return(c_o)
      
    })
  }
  
  #return object and class setting
  tt_object <- list("diff.analyse" = tdiff.analyse,
                    "omega_t" = omega_t,
                    "tm24" = tm24,
                    "tplus" = tplus,
                    "tmhwi" = tmhwi,
                    "fitting.coef" = fitting.coef,
                    "otz" = otz)
  
  class(tt_object) <- "tidetable"
  
  return(tt_object)
  
}