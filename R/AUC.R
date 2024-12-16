#' Calculate the Area Under the Curve (AUC)
#'
#' This function calculates the Area Under the Curve (AUC) using the trapezoidal rule.
#' It can interpolate the input data to a specified number of steps and normalize the result to 1.
#'
#' @param y Numeric vector of y-values.
#' @param x Numeric vector of x-values.
#' @param steps Optional. Number of steps to interpolate the data. If NULL, no interpolation is performed.
#' @param norm_to_1 Logical. If TRUE (default), the calculated AUC is normalized to 1.
#'
#' @return The calculated Area Under the Curve (AUC).
#'
#' @examples
#' # Basic usage
#' x <- c(0, 1, 2, 3, 4, 5)
#' y <- c(0, 1, 2, 3, 4, 5)
#' calc_auc(y, x)
#'
#' # With interpolation
#' x <- c(0, 2, 4)
#' y <- c(0, 2, 4)
#' calc_auc(y, x, steps = 100)
#'
#' # Without normalization
#' x <- c(0, 1, 2, 3, 4, 5)
#' y <- c(0, 1, 2, 3, 4, 5)
#' calc_auc(y, x, norm_to_1 = FALSE)
#'
#' @export
calc_auc <- function(y, x, steps = NULL, norm_to_1 = TRUE) {
  # Input check
  if (!is.numeric(x) || !is.numeric(y)) {
    x_class <- class(x)
    y_class <- class(y)
    stop(paste("Both x and y must be numeric. X class = ", x_class, "Y class = ", y_class))
  }
  if (!is.null(steps) && (!is.numeric(steps) || steps <= 0 || steps != as.integer(steps))) {
    stop("Steps must be a positive integer or NULL.")
  }

  # Check X order
  if (any(diff(x) < 0)) {
    warning("The x values were not ordered. They have been sorted.")
    ordered_indices <- order(x)
    x <- x[ordered_indices]
    y <- y[ordered_indices]
  }

  # Interpolate values
  if (!is.null(steps)) {
    x_interp <- seq(min(x), max(x), length.out = steps)
    y_interp <- approx(x, y, xout = x_interp)$y
    x <- x_interp
    y <- y_interp
  }

  # Trapz rule to calculate the integral (AUC)
  integral <- sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
  if (norm_to_1) integral  <- integral / (max(x) - min(x))

  return(integral)
}