#' Sample file of high and low water times and heights
#' 
#' @description A sample dataset containing observation date, time and height of high and low water
#' @format A data frame with 26819 rows and 4 variables
#' \describe{
#'   \item{observation_date}{date of tide, "yyyy/mm/dd" format, character}
#'   \item{observation_time}{time of tide, "hh:mm:ss" format, character}
#'   \item{high_or_low_water}{indication whether high (1) or low water (0) was present given date and time, integer}
#'   \item{height}{height of the tide, numeric}
#'}
"observation"