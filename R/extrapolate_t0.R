#' Extrapolate T0 Data
#'
#' This function extrapolates the correct T0 given plate well data, where the time 0 is lagged due to
#' the fact that imaging starts at an earlier timepoint in the first wells than on the later wells.
#' It extrapolates the average values of the wells with timepoint < min_t to the rest of the plate.
#'
#' @param d A data.table containing the plate well data.
#' @param min_t A numeric value specifying the minimum timepoint threshold.
#' @param cols A character vector specifying the columns to be extrapolated.
#'
#' @return A data.table with extrapolated T0 data.
#' @importFrom data.table :=
#' @export 
extrapolate_t0_data <- function(d, min_t, cols) {
  inputated <- timestep <- rel_plate_time <- object_class <- frame <- file <- well <- field <- channel <- date_time <- experiment <- NULL
  # Check input types
  if (!data.table::is.data.table(d)) stop("d must be a data.table")
  if (!is.numeric(min_t)) stop("min_t must be numeric")
  if (!is.character(cols)) stop("cols must be a character vector")

  # Check required columns
  d <- data.table::copy(d)
  required_cols <- c("object_class", "frame", "rel_plate_time", "file", "well", "field")
  missing_cols <- setdiff(required_cols, colnames(d))
  if (length(missing_cols) > 0) stop(paste("d must contain the required columns:", paste(missing_cols, collapse = ", ")))

  # Filter data where rel_plate_time < min_t
  filtered_data <- d[rel_plate_time < min_t & frame == 0]

  # Calculate average values for each object_class within each file, well, and field
  avg_values <- filtered_data[, lapply(.SD, mean, na.rm = TRUE), by = .(object_class), .SDcols = cols]
  unique_combinations <- unique(d[, .(file, experiment, well, field, row, col, object_class)])

  # Merge the unique combinations with the imputed values
  new_rows <- merge(unique_combinations, avg_values, by = c("object_class"), all.x = TRUE)
  new_rows <- new_rows[!file %in% filtered_data$file]
  new_rows[, frame := -1]
  new_rows[, channel := 0]
  new_rows[, rel_plate_time := as.difftime(0, units = "hours")]
  new_rows[, date_time := min(d$date_time)]
  new_rows[, inputated := TRUE]
  d[, inputated := FALSE]

  # Combine original data with new rows
  result <- data.table::rbindlist(list(d, new_rows), fill = TRUE)
  data.table::setorder(result, file, rel_plate_time, object_class)
  result[, timestep := data.table::fifelse(is.na(timestep),
                                           as.numeric(difftime(date_time, 
                                                               data.table::first(date_time[frame == 0]), unit = "hours")),
                                           timestep), by = file]

  return(result)
}