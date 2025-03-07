% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolator.R
\name{interpolate_variable}
\alias{interpolate_variable}
\title{Interpolate Variable}
\usage{
interpolate_variable(
  time,
  values,
  timestep,
  fill_extremes = FALSE,
  return_time = TRUE
)
}
\arguments{
\item{time}{A numeric vector of time points.}

\item{values}{A numeric vector of values corresponding to the time points.}

\item{timestep}{A numeric or difftime vector specifying the desired time steps for interpolation.}

\item{fill_extremes}{A logical value indicating whether to fill extreme values (default: FALSE).}

\item{return_time}{A logical value indicating whether to return interpolated time points (default: TRUE).}
}
\value{
If \code{return_time} is TRUE, a list with two elements: \code{interpolated_time} (interpolated time points) and \code{interpolated_val} (interpolated values).
If \code{return_time} is FALSE, a numeric vector of interpolated values.
}
\description{
This function interpolates a variable based on given time points and values.
It can handle regular and irregular time steps, fill extreme values, and return interpolated time points if desired.
}
\examples{
time <- c(0, 1, 2, 3, 4, 5)
values <- c(0, 1, 2, 3, 4, 5)
interpolate_variable(time, values, timestep = 0.5)

time <- c(0, 1, 2, 4, 8)
values <- c(0, 1, 2, 4, 8)
interpolate_variable(time, values, timestep = c(0, 1, 2, 1), fill_extremes = TRUE)

}
