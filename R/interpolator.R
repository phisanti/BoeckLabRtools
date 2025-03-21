#' Interpolate Variable
#'
#' This function interpolates a variable based on given time points and values.
#' It can handle regular and irregular time steps, fill extreme values, and return interpolated time points if desired.
#'
#' @param time A numeric vector of time points.
#' @param values A numeric vector of values corresponding to the time points.
#' @param timepoints A numeric or difftime vector specifying the desired time steps for interpolation.
#' @param fill_extremes A logical value indicating whether to fill extreme values (default: FALSE).
#' @param return_time A logical value indicating whether to return interpolated time points (default: TRUE).
#'
#' @return If `return_time` is TRUE, a list with two elements: `interpolated_time` (interpolated time points) and `interpolated_val` (interpolated values).
#'         If `return_time` is FALSE, a numeric vector of interpolated values.
#'
#' @examples
#' time <- c(0, 1, 2, 3, 4, 5)
#' values <- c(0, 1, 2, 3, 4, 5)
#' interpolate_variable(time, values, timepoints = 0.5)
#'
#' time <- c(0, 1, 2, 4, 8)
#' values <- c(0, 1, 2, 4, 8)
#' interpolate_variable(time, values, timepoints = c(0, 1, 2, 1), fill_extremes = TRUE)
#'
#' @export
interpolate_variable <- function(time, values, timepoints, fill_extremes = FALSE, return_time = TRUE) {
  # Initial validations remain the same
  if (!is.numeric(values)) stop("values must be numeric")
  if (length(time) != length(values)) stop("time and values must have same length")
  if (length(time) < 2) stop("at least 2 points required for interpolation")
  if (length(timepoints) > length(time)) stop("timepoints must be smaller or equal length than time")
  if (!is.numeric(timepoints) && !inherits(timepoints, "difftime")) stop("timepoints must be numeric or difftime")
  if (any(duplicated(time))) warning("Duplicate time points found. Check if data is properly grouped")
  time_num <- as.numeric(time)
  if (any(is.na(time_num))) warning("NAs found in time values")

  # Handle timepointss
  time_range <- if (length(timepoints) > 1 && length(timepoints) < length(time)) {
    # Generate regular sequence
    regular_seq <- seq(min(time_num), max(time_num), length.out = length(time))
    # Merge key timepoints with regular sequence
    all_points <- sort(unique(c(timepoints, regular_seq)))
    # Keep only the number of points matching the input length
    indices <- round(seq(1, length(all_points), length.out = length(time)))
    all_points[indices]
  } else if (length(timepoints) > 1) {
    timepoints
  } else {
    seq(min(time_num), max(time_num), by = timepoints)
  }

  # Deal with extrapolation
  if (fill_extremes) {
    rule <- 2
  } else {
    rule <- 1
  }
  # interpolate and return
  interpolated <- approx(x = time_num, y = values, xout = time_range, rule = rule)
  if (return_time) {
    list(interpolated_time = interpolated$x, interpolated_val = interpolated$y)
  } else {
    interpolated$y
  }
}
