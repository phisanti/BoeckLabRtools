#' Fill Missing Combinations in Data.Table
#' 
#' @description Creates complete combinations of specified key columns and fills missing rows
#' 
#' @param dt A data.table object containing the source data
#' @param key_cols Character vector of column names to use as keys
#' @param fill_col Character string specifying which key column to fill
#' @param required_values Vector of values that must exist for fill_col
#' 
#' @return A data.table with complete combinations and filled rows
#' 
#' @details 
#' The function ensures all combinations of key columns exist in the output.
#' All required_values must be present in the original dataset.
#' 
#' @examples
#' dt <- data.table(
#'   file = rep("f1", 3),
#'   frame = 1:3,
#'   object_class = c("A", "B", "A")
#' )
#' result <- fill_missing_classes(
#'   dt,
#'   key_cols = c("file", "frame", "object_class"),
#'   fill_col = "object_class",
#'   required_values = c("A", "B")
#' )
#' 
#' @export
fill_missing_classes <- function(dt, key_cols, fill_col, required_values, in_place = FALSE) {
    data.table::is.data.table(dt) || stop("Input must be a data.table")
    
    if (nrow(dt) == 0L) stop("Empty data.table provided")
    
    missing_cols <- setdiff(key_cols, names(dt))
    if (length(missing_cols)) stop("Missing columns: ", toString(missing_cols))
    
    existing_values <- unique(stats::na.omit(dt[[fill_col]]))
    missing_required <- setdiff(required_values, existing_values)
    if (length(missing_required)) stop("Missing required values: ", toString(missing_required))
    
    unique_vals <- lapply(key_cols[key_cols != fill_col], function(col) unique(stats::na.omit(dt[[col]])))
    names(unique_vals) <- key_cols[key_cols != fill_col]
    unique_vals[[fill_col]] <- required_values
    
    complete_dt <- do.call(data.table::CJ, unique_vals)
    
    if (in_place) {
        dt[complete_dt, on = key_cols]
    } else {
        merged_dt <- merge(dt, complete_dt, by = key_cols, all = TRUE)
        data.table::setkeyv(merged_dt, key_cols)
        return(merged_dt)
    }
}
