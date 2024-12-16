#' Enforce Monotonic Decrease in Values
#' 
#' @description
#' Corrects a numeric vector to ensure values only decrease monotonically. 
#' Particularly useful for live/death fraction data where biological death 
#' can only increase over time (thus live fraction can only decrease).
#'
#' @param x Numeric vector of values to be corrected
#'
#' @return Numeric vector of same length as input where each value is less than 
#' or equal to all previous values
#'
#' @examples
#' x <- c(0.95, 0.92, 0.94, 0.90, 0.91)
#' enforce_decrease(x)
#' # Returns: c(0.95, 0.92, 0.92, 0.90, 0.90)
#'
#' @export
enforce_decrease <- function(x) {
  pmin(x, cummin(x))
}