#' Merge plate information with well information
#'
#' This function merges a dataset containing plate data information with well information based on the specified plate format.
#'
#' @param d A data.table containing the well information. It must have columns named "row", "col", and "well" (all lowercase).
#' @param well_info A data.frame or data.table containing the well information. The format depends on the `plate_format` argument.
#' @param info_name A character string specifying the name of the column containing the well information. Default is "well_info".
#' @param plate_format A character string specifying the format of the well information. Can be either "plate_view" (default) or "transposed".
#' @param wells_to_ignore A character vector specifying the wells to ignore. Default is c("EMPTY", "BW").
#' @param force_lower A logical value indicating whether to force column names to lowercase. Default is TRUE.
#' @param inplace A logical value indicating whether to modify the input data.table in place. Default is TRUE. This means the data.table given 
#' as input will be directly modified without using the assignment operator '<-'.
#' 
#' @return A data.table with the merged drug data and well information.
#'
#' @examples
#' # Example data
#' library(data.table)
#' d <- read.table(text = "
#' row | col | well | value
#' A   | 1   | A1   | 10
#' A   | 2   | A2   | 20
#' B   | 1   | B1   | 30
#' B   | 2   | B2   | 40", 
#' header = TRUE, sep = "|", stringsAsFactors = FALSE, strip.white = TRUE)
#' d <- data.table(d)
#' well_info <- read.table(text = "
#' row | 1     | 2
#' A   | Drug1 | Drug2
#' B   | Drug3 | Drug4", 
#' header = TRUE, sep = "|", stringsAsFactors = FALSE, strip.white = TRUE)
#' well_info <- data.table::data.table(well_info)
#' # Merge drug data with well information
#' merged_data <- merge_plate_data(d, well_info)
#'
#' # Merge drug data with transposed well information
#' well_info_transposed <- read.table(text = "
#' col | row | info
#' 1   | A | Drug1
#' 2   | B | Drug2
#' 1   | A | Drug3
#' 2   | B | Drug4", 
#' header = TRUE, sep = "|", stringsAsFactors = FALSE, strip.white = TRUE)
#' well_info_transposed <- data.table::data.table(well_info_transposed)
#' well_info_transposed[, well := paste0(row, col)]
#' merged_data_transposed <- merge_plate_data(d, well_info_transposed, plate_format = "transposed")
#' 
#' @export
merge_plate_data <- function(d, well_info, info_name = 'well_info', plate_format = "plate_view", wells_to_ignore = c("EMPTY", "BW"), force_lower = TRUE, inplace = TRUE) {
  well <- NULL
  # Check if well_info is a path to a CSV file
  if (is.character(well_info) && length(well_info) == 1 && file.exists(well_info)) {
    well_info <- data.table::fread(well_info, header = TRUE)
  }
  
  # Check plate_format and column names
  if (plate_format == "plate_view") {
    if (colnames(well_info)[1] != "row") {
      if (force_lower) {
        warning("Column 1 should be named 'row' in plate_view format. Converting to lowercase.")
        colnames(well_info)[1] <- tolower(colnames(well_info)[1])
      } else {
        stop("Column 1 must be named 'row' in plate_view format")
      }
    }
    well_info <- data.table::melt(well_info, id.vars = "row", variable.name = "col", value.name = info_name)
  } else if (plate_format == "transposed") {
    if (!all(colnames(well_info)[1:2] %in% c("col", "row"))) {
      if (force_lower) {
        warning("Column 1 should be named 'col' and column 2 should be named 'row' in transposed format. Converting to lowercase.")
        colnames(well_info)[1:2] <- tolower(colnames(well_info)[1:2])
      } else {
        stop("Column 1 must be named 'col' and column 2 must be named 'row' in transposed format")
      }
    }
  }
  
  # Check column names in d
  if (!all(c("row", "col", "well") %in% tolower(colnames(d)))) {
    if (force_lower) {
      warning("Column names in d should be lowercase and include 'row', 'col' and 'well'. Converting to lowercase.")
      colnames(d) <- tolower(colnames(d))
    } else {
      stop("Column names in d must be lowercase and include 'row', 'col' and 'well'")
    }
  }

  # Add info and merge datasets
  well_info[, ":="(well = paste0(row, col), col = as.integer(col))]
  filt_rows <- !well_info[[info_name]] %in% wells_to_ignore
  well_info <- well_info[filt_rows]
  data.table::setorder(well_info, well, col, row)
  data.table::setorder(d, well)

  if (inplace) {
    d[well_info, (info_name) := get(info_name), on = c("well", "col", "row")]
    return(invisible(d))
  } else {
    return(well_info[, .SD, .SDcols = c("well", info_name)])
  }
}
