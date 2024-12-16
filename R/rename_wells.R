#' Rename Well Identifiers
#'
#' This function renames well identifiers to handle NIKON file naming conventions for 1536-well plates.
#' In cases where the row goes beyond A-Z, NIKON uses the same letters of the alphabet with the appendix "_0001_" (e.g., A_****_0001***).
#' For improved data handling, these cases are changed to ZA, ZB, and so on.
#'
#' @param well Character vector of well identifiers to be renamed.
#' @param file Character vector of file names associated with each well.
#' @param omitted_rows Character vector of row identifiers to be omitted and renamed with a "Z" prefix (default: c("A", "B")).
#'
#' @return A character vector of renamed well identifiers.
#'
#' @examples
#' wells <- c("A01", "B02", "C03", "A01_0001_", "B02_0001_")
#' files <- c("file1.txt", "file2.txt", "file3.txt", "file4_0001_.txt", "file5_0001_.txt")
#' rename_well(wells, files)
#'
#' @export
rename_well <- function(well, file, omitted_rows = c("A", "B")) {
  mapply(function(w, f) {
    row <- substr(w, 1, 1)
    col <- substr(w, 2, nchar(w))
    
    if (row %in% omitted_rows) {
      row <- paste0("Z", row)
      col <- as.integer(substr(col, 2, nchar(col)))
    } else if (nchar(w) == 4) {
      row <- paste0("Z", row)
      col <- as.integer(substr(col, 2, nchar(col)))
    } else if (grepl("_0001_", f)) {
      row <- paste0("Z", row)
      col <- as.integer(substr(col, 2, nchar(col)))
    } else {
      col <- as.integer(col)
    }
    
    if (is.na(col)) col <- 1  # Handle cases where col becomes NA
    col <- max(1, min(col, 50))
    
    paste0(row, col)
  }, well, file, USE.NAMES = FALSE)
}