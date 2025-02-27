#' Fill Missing Combinations in Data.Table
#' 
#' @description This function is designed to fill the 0-count cases for object classes missed in certain conditions. 
#' Creates complete combinations of specified key columns and fills missing rows with zeros in count columns
#' 
#' @param dt A data.table object containing the source data
#' @param key_cols Character vector of column names to use as keys
#' @param fill_col Character string specifying which key column to expand with required values
#' @param cols_0count Character vector of column names that should be filled with zeros when missing
#' @param required_values Vector of values that must exist for fill_col
#' 
#' @return A data.table with complete combinations and filled rows
#' 
#' @details 
#' The function ensures all combinations of key columns with the required values exist in the output.
#' All required_values must already be present in the original dataset.
#' Missing values in cols_0count will be filled with zeros.
#' 
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   file = c("video1", "video1", "video1", "video2", "video2"),
#'   frame = c(1, 2, 3, 1, 2),
#'   object_class = c("car", "person", "car", "car", "bicycle"),
#'   count = c(3, 1, 2, 1, 1),
#'   score = c(0.9, 0.8, 0.7, 0.95, 0.85)
#' )
#' result <- fill_missing_classes(
#'   dt = dt,
#'   key_cols = c("file", "frame", "object_class"),
#'   fill_col = "object_class",
#'   cols_0count = c("count", "score"),
#'   required_values = c("car", "person", "bicycle")
#' )
#' 
#' @export

fill_missing_classes <- function(dt, key_cols, fill_col, cols_0count, required_values) {
    
    # init data.table vars
    dummy <- NULL
    # Checks
    if (!data.table::is.data.table(dt)) stop("Input must be a data.table")
    missing_cols <- setdiff(key_cols, names(dt))
    if (length(missing_cols) > 0) stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
    existing_values <- unique(dt[[fill_col]])
    if (!all(required_values %in% existing_values)) {
        stop("All required_values must exist in the source data.table. Missing values: ",
             paste(setdiff(required_values, existing_values), collapse = ", "))
    }
    # Create complete combinations
    dt_local <- data.table::copy(dt[, key_cols, with = FALSE])
    dt_local <- unique(dt_local)
    dt_local[, (required_values) := 0]
    dt_local <- data.table::melt.data.table(dt_local, id.vars = key_cols, value.name = "dummy", variable.name = fill_col)
    dt_local[, dummy := NULL]
    # Merge and fill
    dt_filled <- data.table::merge.data.table(dt, dt_local, by = c(key_cols, fill_col), all = TRUE)
    dt_filled[, 
        (cols_0count) := lapply(.SD, function(x) data.table::fifelse(is.na(x), 0, x)), 
        .SDcols = cols_0count]

    return(dt_filled)
}