#' Plate Schema Data
#'
#' A dataset containing plate schema information for demonstrating the package functionality.
#' The data represents a casted plate layout with rows and columns.
#' @docType data
#' @name plate_schema
#' @usage data(plate_schema)
#' 
#' @format A data.table with the following columns:
#' \describe{
#'   \item{row}{Character vector representing the row identifier (e.g., "C", "D", "E", etc.)}
#'   \item{3}{Character vector representing the content of column 3 (e.g., "IPM", "TGC", etc.)}
#'   \item{4}{Character vector representing the content of column 4 (e.g., "TGC", "AMK", etc.)}
#'   \item{5}{Character vector representing the content of column 5 (e.g., "TGC", "AMK", etc.)}
#'   \item{6}{Character vector representing the content of column 6 (e.g., "AMK", "TGC", etc.)}
#'   \item{7}{Character vector representing the content of column 7 (e.g., "AMK", "TGC", etc.)}
#'   \item{8}{Character vector representing the content of column 8 (e.g., "TGC", "TGC", etc.)}
#'   \item{9}{Character vector representing the content of column 9 (e.g., "TGC", "IPM", etc.)}
#'   \item{10}{Character vector representing the content of column 10 (e.g., "MXF", "CLR", etc.)}
#'   \item{11}{Character vector representing the content of column 11 (e.g., "MXF", "CLR", etc.)}
#'   \item{12}{Character vector representing the content of column 12 (e.g., "TGC", "AMK", etc.)}
#' }
"plate_schema"

#' Kill curve data
#'
#' A dataset containing kill curve data for demonstrating the package functionality.
#' 
#' @docType data
#' @name killcurves
#' @usage data(killcurves)
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
#' }
"killcurves"