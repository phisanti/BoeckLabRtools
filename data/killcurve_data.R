#' Kill curve data
#'
#' A dataset containing kill curve data for demonstrating the package functionality.
#'
#' @format A data.table with the following columns:
#' \describe{
#'   \item{file}{Character vector representing the file name}
#'   \item{frame}{Integer vector representing the frame number}
#'   \item{channel}{Integer vector representing the channel number}
#'   \item{date_time}{POSIXct vector representing the date and time}
#'   \item{timestep}{Numeric vector representing the time step}
#'   \item{object_class}{Character vector representing the object class}
#'   \item{total_count}{Integer vector representing the total count}
#'   \item{pi_class_neg}{Integer vector representing the PI negative class count}
#'   \item{pi_class_pos}{Integer vector representing the PI positive class count}
#'   \item{area_pineg}{Numeric vector representing the area of PI negative objects}
#'   \item{area_pipos}{Integer vector representing the area of PI positive objects}
#'   \item{well}{Character vector representing the well ID}
#'   \item{field}{Character vector representing the field ID}
#'   \item{row}{Character vector representing the row label}
#'   \item{col}{Integer vector representing the column number}
#' }
"killcurves"