#' Plate schema for antimicrobial treatments
#'
#' A dataset containing the mapping of wells to antimicrobial treatments
#' in a 96-well plate format. Used in the killcurve analysis examples.
#'
#' @format A data frame with 6 rows and 11 columns:
#' \describe{
#'   \item{row}{Well row identifier (letters C-H)}
#'   \item{3-12}{Columns with antimicrobial treatment codes for each well position}
#' }
#' @examples
#' data(plate_schema)
#' head(plate_schema)
"plate_schema" 