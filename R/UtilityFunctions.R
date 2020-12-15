#' Returns predictor vector for design matrix
#' @description Returns predictor vector for design matrix from 39 astronomical angular velocities.
#' @param xi Transit index
#' @param tdiff Length of input time series.
#' @return A list with the selected angular velocities, their ranks and the predictor vector (Values between -1, 1).
#' @export  

Funcs <-  function(tdiff, xi) {
  
  .Deprecated(package = "TideTables", msg = "This function is deprecated and will be removed in later releases.")
  
    #Vector with angular velocities
    rad <- 0.017453292519943
    xi  <- rad * xi
    
    omegas <- vector()
    omegas[ 1]<-0.0548098
    omegas[ 2]<-0.2306165
    omegas[ 3]<-1.0201944
    omegas[ 4]<-1.8097724
    omegas[ 5]<-2.0403886
    omegas[ 6]<-11.5978420
    omegas[ 7]<-11.7131503
    omegas[ 8]<-12.3874200
    omegas[ 9]<-12.6180365
    omegas[10]<-12.7881545
    omegas[11]<-13.5229227
    omegas[12]<-13.6382309
    omegas[13]<-13.6930407
    omegas[14]<-13.7535391
    omegas[15]<-15.9640460
    omegas[16]<-24.2158785
    omegas[17]<-25.1812631
    omegas[18]<-25.2360729
    omegas[19]<-26.2562673
    omegas[20]<-27.0458453
    omegas[21]<-27.1611535
    omegas[22]<-27.2764618
    omegas[23]<-27.3312716
    omegas[24]<-36.9492232
    omegas[25]<-38.7589956
    omegas[26]<-38.8743038
    omegas[27]<-38.9896120
    omegas[28]<-40.7993844
    omegas[29]<-49.4519514
    omegas[30]<-50.4721458
    omegas[31]<-52.5125347
    omegas[32]<-52.5673444
    omegas[33]<-54.5529235
    omegas[34]<-62.1852961
    omegas[35]<-63.9950685
    omegas[36]<-64.2256849
    omegas[37]<-75.7082187
    omegas[38]<-77.7486076
    omegas[39]<-100.9442917
    
    omega_pos <- omegas
    
    iranks <- vector()
    iranks[ 1]<-6
    iranks[ 2]<-13
    iranks[ 3]<-7
    iranks[ 4]<-31
    iranks[ 5]<-17
    iranks[ 6]<-14
    iranks[ 7]<-8
    iranks[ 8]<-34
    iranks[ 9]<-19
    iranks[10]<-39
    iranks[11]<-3
    iranks[12]<-4
    iranks[13]<-38
    iranks[14]<-21
    iranks[15]<-36
    iranks[16]<-11
    iranks[17]<-35
    iranks[18]<-1
    iranks[19]<-12
    iranks[20]<-33
    iranks[21]<-15
    iranks[22]<-2
    iranks[23]<-27
    iranks[24]<-10
    iranks[25]<-16
    iranks[26]<-24
    iranks[27]<-22
    iranks[28]<-23
    iranks[29]<-29
    iranks[30]<-5
    iranks[31]<-9
    iranks[32]<-37
    iranks[33]<-30
    iranks[34]<-25
    iranks[35]<-28
    iranks[36]<-26
    iranks[37]<-20
    iranks[38]<-18
    iranks[39]<-32
    
    omega_pos_rank <- iranks
    omega_sel      <- vector()
    omega_sel_rank <- vector()
    
    while (length(omega_pos) > 0) {
      #Find most relevant velocities
      maxrank <- which.min(omega_pos_rank)
      omega_pos_maxrank <- omega_pos[maxrank]
      if( omega_pos_maxrank * tdiff > 360){
        omega_sel       <- c(omega_sel,  omega_pos_maxrank)
        omega_sel_rank  <- c(omega_sel_rank, omega_pos_rank[maxrank])
        del_index       <- which((abs(omega_pos -  omega_pos_maxrank) * tdiff) < 360)
        omega_pos       <- omega_pos[-del_index]
        omega_pos_rank  <- omega_pos_rank[-del_index]
      } else {
        omega_pos      <- omega_pos[-maxrank]
        omega_pos_rank <- omega_pos_rank[-maxrank]
      }
    }
    omega_sel <- omega_sel[order(omega_sel)]
    
    #Computing the afuncs for the selected omegas
    afunc <- vector(mode = "numeric", length = 2 * length(omega_sel) + 1)
    afunc[1] <- 1.00000
    
    for(i in seq(2, 2 * length(omega_sel) + 1, 2)) {
      oxi          <- omega_sel[i / 2] * xi
      afunc[i]     <- cos(oxi)
      afunc[i + 1] <- sin(oxi)
    }
    
    return(list(omega_sel, omega_sel_rank, afunc))
}

#' Returns omegas and their ranks.
#' @description Returns omegas and their ranks from 39 astronomical angular velocities.
#' @param tdiff Length of input time series.
#' @return A list with the selected angular velocities and their ranks.
#' @export 

FindOmega <- function(tdiff) {
  #Vector with angular velocities
  #rad <- 0.017453292519943
  #xi  <- rad * xi
  
  omegas <- vector(mode = "numeric", length = 39)
  omegas[ 1]<-0.0548098
  omegas[ 2]<-0.2306165
  omegas[ 3]<-1.0201944
  omegas[ 4]<-1.8097724
  omegas[ 5]<-2.0403886
  omegas[ 6]<-11.5978420
  omegas[ 7]<-11.7131503
  omegas[ 8]<-12.3874200
  omegas[ 9]<-12.6180365
  omegas[10]<-12.7881545
  omegas[11]<-13.5229227
  omegas[12]<-13.6382309
  omegas[13]<-13.6930407
  omegas[14]<-13.7535391
  omegas[15]<-15.9640460
  omegas[16]<-24.2158785
  omegas[17]<-25.1812631
  omegas[18]<-25.2360729
  omegas[19]<-26.2562673
  omegas[20]<-27.0458453
  omegas[21]<-27.1611535
  omegas[22]<-27.2764618
  omegas[23]<-27.3312716
  omegas[24]<-36.9492232
  omegas[25]<-38.7589956
  omegas[26]<-38.8743038
  omegas[27]<-38.9896120
  omegas[28]<-40.7993844
  omegas[29]<-49.4519514
  omegas[30]<-50.4721458
  omegas[31]<-52.5125347
  omegas[32]<-52.5673444
  omegas[33]<-54.5529235
  omegas[34]<-62.1852961
  omegas[35]<-63.9950685
  omegas[36]<-64.2256849
  omegas[37]<-75.7082187
  omegas[38]<-77.7486076
  omegas[39]<-100.9442917
  
  omega_pos <- omegas
  
  iranks <- vector(mode = "logical", length = 39)
  iranks[ 1]<-6
  iranks[ 2]<-13
  iranks[ 3]<-7
  iranks[ 4]<-31
  iranks[ 5]<-17
  iranks[ 6]<-14
  iranks[ 7]<-8
  iranks[ 8]<-34
  iranks[ 9]<-19
  iranks[10]<-39
  iranks[11]<-3
  iranks[12]<-4
  iranks[13]<-38
  iranks[14]<-21
  iranks[15]<-36
  iranks[16]<-11
  iranks[17]<-35
  iranks[18]<-1
  iranks[19]<-12
  iranks[20]<-33
  iranks[21]<-15
  iranks[22]<-2
  iranks[23]<-27
  iranks[24]<-10
  iranks[25]<-16
  iranks[26]<-24
  iranks[27]<-22
  iranks[28]<-23
  iranks[29]<-29
  iranks[30]<-5
  iranks[31]<-9
  iranks[32]<-37
  iranks[33]<-30
  iranks[34]<-25
  iranks[35]<-28
  iranks[36]<-26
  iranks[37]<-20
  iranks[38]<-18
  iranks[39]<-32
  
  omega_pos_rank <- iranks
  omega_sel      <- vector()
  omega_sel_rank <- vector()
  
  while (length(omega_pos) > 0) {
    #Find most relevant velocities
    maxrank <- which.min(omega_pos_rank)
    omega_pos_maxrank <- omega_pos[maxrank]
    if( omega_pos_maxrank * tdiff > 360){
      omega_sel       <- c(omega_sel,  omega_pos_maxrank)
      omega_sel_rank  <- c(omega_sel_rank, omega_pos_rank[maxrank])
      del_index       <- which((abs(omega_pos -  omega_pos_maxrank) * tdiff) < 360)
      omega_pos       <- omega_pos[-del_index]
      omega_pos_rank  <- omega_pos_rank[-del_index]
    } else {
      omega_pos      <- omega_pos[-maxrank]
      omega_pos_rank <- omega_pos_rank[-maxrank]
    }
  }
  omega_sel <- omega_sel[order(omega_sel)]
  
  return(list(omega_sel, omega_sel_rank))
  
}

#' Returns predictor vector for design matrix
#' @description Returns predictor vector for design matrix from 39 astronomical angular velocities.
#' @param xi Transit index
#' @param omega The return value of FindOmega().
#' @return A list with the selected angular velocities, their ranks and the predictor vector (values between -1, 1).
#' @export 

ComputeAfunc <- function(omega = NULL, xi = NULL) {
  
  rad <- 0.017453292519943
  xi  <- rad * xi
  omega_sel <- omega[[1]]
  o_length  <- length(omega_sel)
  #Computing the afuncs for the selected omegas
  afunc <- vector(mode = "numeric", length = 2 * o_length + 1)
  afunc[1] <- 1.00000
  
  for(i in seq.int(2, 2 * o_length + 1, 2)) {
    oxi          <- omega_sel[i / 2] * xi
    afunc[i]     <- cos(oxi)
    afunc[i + 1] <- sin(oxi)
  }
  omega[[3]] <- afunc
  return(omega)
}

#' Calculates numm and k4
#' @description Calculates transit number (numm) and high (1, 3) or low (2, 4) water number (k4).
#' @param t Time in days after 1900/01/01 00:00:00 UTC.
#' @param tmhwi Mean high water interval (Greenwich meridian).
#' @return Returns a list containing numm and k4.

NumCulm <- function(t, tmhwi){
  nummk4          <- list()
  tperiode.m2     <- 360 / 28.9841042
  tt              <- t - tmhwi
  chron.origin    <- chron(dates.  = "1900/01/01",
                           times.  = "00:00:00",
                           format = c(dates = "y/m/d", times = "h:m:s"),
                           out.format = c(dates = "y/m/d", times = "h:m:s"))
  
  tmoon.0         <- chron(dates.  = "1949/12/31",
                           times.  = "21:08:00",
                           format = c(dates = "y/m/d", times = "h:m:s"),
                           out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin
                           
  ttdiff          <- tt - tmoon.0
  ttdiff          <- ttdiff * 24 + tperiode.m2 / 4
  tm2             <- ttdiff / tperiode.m2 
  nummk4$numm     <- as.numeric(floor(tm2 / 2))
  nummk4$k4       <- as.numeric(1 + (floor(tm2 * 2 ) %% 4))
  
  return(nummk4)
}

#' Calculates tmhwi
#' @description This functions computes an estimate for the mean high water interval (tmhwi) in UTC
#' @param input Should be a data.table object with three columns d_days, high_low and height, where 
#' d_days is a vector of fraction of days since 1900/01/01 00:00:00, high_low indicating a high water(1)
#' or a low water(0), height is the corresponding height
#' @param strict If strict is true (default), the computations are more sharp.
#' @return Returns the mean high water interval in UTC
#' @importFrom chron chron
#' @export
EstimateTmhwi <- function(input, strict = TRUE){
  
  input <- input[high_low == 1]
  
  tperiode.m2  <- 360 / 28.9841042
  tmean.moon   <- tperiode.m2 * 2
  tm24         <- tmean.moon / 24
  or           <- chron(dates. = "1900/01/01", 
                                        times. = "00:00:00",
                                        format = c(dates = "y/m/d", times = "h:m:s"),
                                        out.format = c(dates = "y/m/d", times = "h:m:s"))
  tmoon.0      <- chron(dates. = "1949/12/31", 
                        times. = "21:08:00",
                        format = c(dates = "y/m/d", times = "h:m:s"),
                        out.format = c(dates = "y/m/d", times = "h:m:s")) - or
  
  tplus        <- as.numeric(tmoon.0 + 24.2491 / 1440.00)
  
  numm      <- NULL
  tmmt_numm <- NULL
  height    <- NULL
  high_low  <- NULL
  mean_h    <- NULL
  sd.h      <- NULL
  mhist     <- NULL
  n_mhist   <- NULL
  tmmt_numm <- NULL
  d_days    <- NULL
  
  input[, numm := floor((d_days - tplus) / tm24)]
  input[, tmmt_numm := numm * tm24 + tplus]
  input[, mhist := as.numeric(floor(96 * (d_days - tmmt_numm) / tm24) + 1)]
  
  input[, n_mhist := .N, by = mhist]
  input[, mean_h := mean(height), by = mhist]
  input[, sd.h := 3*sd(height), by = mhist]
  input <- input[(height <= mean_h + sd.h) & (height >= mean_h - sd.h)]
  
  input[, mean_h := mean(height), by = mhist]
  input[, n_mhist := .N, by = mhist]
  
  setkey(input, "mhist") #why here
  
  input <- unique(input, by = "mhist")[order(mhist)]
  check.matrix <- matrix(data = NA_real_, nrow = 96, ncol = 3)
  
  for(i in 1L : 96L){
    mh <- i - 1
    if(length(input[mhist == ((mh + 1) %% 96) + 1, n_mhist] - input[mhist == i, n_mhist]) > 0) {
      check.matrix[i, 1] <- input[mhist == ((mh + 1) %% 96) + 1, n_mhist] - input[mhist == i, n_mhist]
      check.matrix[i, 2] <- input[mhist == i, mean_h]
      check.matrix[i, 3] <- input[mhist == i, n_mhist]
    } else {
      check.matrix[i, 1] <- 99999
      check.matrix[i, 2] <- 99999
      check.matrix[i, 3] <- 99999
    }
  }
  
  mmax     <- 0
  mhistmax <- vector(mode = "numeric")
  mhist_m  <- matrix(data = NA_real_, nrow = 3, ncol = 96)
  for(i in 1L : 96L) {
    mh <- i - 1
    if(
      #(check.matrix[((mh - 4) %% 96) + 1, 1] > 0) & 
      (check.matrix[((mh - 3) %% 96) + 1, 1] > 0) 
       & (check.matrix[((mh - 2) %% 96) + 1, 1] > 0)
       & (check.matrix[((mh - 1) %% 96) + 1, 1] >= 0)
       & (check.matrix[((mh)) + 1, 1] <= 0)
       & (check.matrix[((mh + 1) %% 96) + 1, 1] < 0)
       & (check.matrix[((mh + 2) %% 96) + 1, 1] < 0)
       #& (check.matrix[((mh + 3) %% 96) + 1, 1] < 0)
       ) {
      if(strict) {
        if(check.matrix[i, 2] > check.matrix[(mh - 1%%i) + 1, 2] &
           check.matrix[i, 2] >= check.matrix[(mh + 1%%i) + 1, 2]){
          mmax           <- mmax + 1
          mhistmax[mmax] <- i
        }
      } else {
        mmax           <- mmax + 1
        mhistmax[mmax] <- i
        mhist_m[,mmax] <- c((mh - 1%%i) + 1, i, (mh + 1%%i) + 1)
          }
    }
  }
  if(strict) {
    tmhwi <- as.numeric(input[mhist %in% mhistmax][order(mean_h, decreasing = TRUE)][1][, mhist]) 
  } else {
    prod <- vector(mode = "numeric")
    mhist_m <- mhist_m[, !is.na((colSums(mhist_m))), drop = FALSE]
    for(i in 1: ncol(mhist_m)) {
      temp <- input[mhist %in% mhist_m[,i]][, c("mhist", "n_mhist", "mean_h")]
      prod[i] <- temp[, sum(n_mhist*mean_h)/sum(n_mhist)]
    }
    ind <- mhist_m[2, which.max(prod)]
    tmhwi <- as.numeric(input[mhist == ind, mhist])
    
  }
  tmhwi <- tm24 / 96 * (tmhwi - 0.5)
  tmhwi <- 24 * tmhwi
  return(tmhwi)
}
