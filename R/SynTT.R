#' Synthesizes a Tide Table
#' @description Synthesiszes a tide table
#' @param model the model you built with BuildTT()
#' @param ssdate Start date of the Synthesis
#' @param sstime End date of the Synthesis
#' @param sedate Start time of the Synthesis
#' @param setime End time of the Synthesis
#' @param stz The target time zone. Defaults to UTC +1 
#' @return Returns the tide table
#' @export


SynTT <- function(model = NULL, ssdate, sstime, sedate, setime, stz = 1) {
  
  stopifnot(class(model) == "tidetable")
  
  chron.origin <- chron(dates. = "1900/01/01", 
                        times. = "00:00:00",
                        format = c(dates = "y/m/d", times = "h:m:s"),
                        out.format = c(dates = "y/m/d", times = "h:m:s"))
  #retrieving objects
  omega_t <- model[["omega_t"]]
  tm24    <- model[["tm24"]]
  tplus   <- model[["tplus"]]
  tmhwi   <- model[["tmhwi"]]
  fitting.coef <- model[["fitting.coef"]]
  
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
  
  time1        <- vector(mode = "double")
  height       <- vector(mode = "double")
  afunc        <- vector(mode = "double")
  coeff        <- vector(mode = "double")
  st.transit   <- vector(mode = "double")
  # time.height  <- data.table(matrix(0.0, 
  #                                   ncol = 6,
  #                                   nrow = ((end.nummculm$numm - start.nummculm$numm + 1) * 4)))
  
  time.height  <- matrix(0.0, ncol = 6,
                         nrow = ((end.nummculm$numm - start.nummculm$numm + 1) * 4))
  
  m  <- 0L
  ii <- 0L
  stz24 <- stz / 24
  for (i in start.nummculm$numm : end.nummculm$numm) {
    ii <- ii + 1L
    afunc <- ComputeAfunc(xi = i, omega = omega_t)[[3]] #optimize?!
    for (k in 1 : 4) {
      m <- m + 1L
      for (l in c("stunden.transit", "height")) {        
        coeff <- fitting.coef[[k]][[l]]
        summe <- coeff %*% afunc
        
        if (l == "stunden.transit") {
          st.transit[m] <- summe
          tmmt.numm     <- i * tm24 + tplus
          time1[m]      <- (tmmt.numm + summe / 24 + stz24)
          
        }
        else {
          height[m] <- summe
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
      
      time.height[m, ] <- c(time1[m], ihn, trans, height[m],
                            st.transit[m], ii)
      
      # set(time.height, i = m, j = 1L, value = time1[m])
      # set(time.height, i = m, j = 2L, value = ihn)
      # set(time.height, i = m, j = 3L, value = trans)
      # set(time.height, i = m, j = 4L, value = height[m])
      # set(time.height, i = m, j = 5L, value = st.transit[m])
      # set(time.height, i = m, j = 6L, value = ii)
      
    }
  }
  date_time       <- NULL
  prediction_date <- NULL
  prediction_time <- NULL
  V1              <- NULL
  time.height <- as.data.table(time.height)
  
  time.height[ , date_time := format(chron(dates. = V1, 
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
  time.height <- time.height[, c("prediction_date",
                                "prediction_time",
                                "high_or_low_water",
                                "upper_or_lower_transit",
                                "height")]
  return(time.height)
  
  
}