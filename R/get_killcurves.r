#' Generate Kill Curves from Cell Data
#'
#' @description
#' Processes summary imaging data from the ASCT to calculate and interpolate kill curves based on PI (propidium iodide) staining.
#' This function extracts metadata from filenames, calculates live/dead cell fractions, and generates 
#' normalized and interpolated kill curves.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates input data and parameters
#' 2. Extracts experiment, well, and field information from filenames
#' 3. Fills missing data points if needed
#' 4. Calculates PI-positive fractions (indicating dead cells)
#' 5. Derives live cell fractions
#' 6. Interpolates live cell fractions over time
#' 7. Normalizes live cell fractions
#' 8. Enforces monotonic decrease in live cell fraction (biological constraint)
#'
#' @param d A data.table containing the cell tracking data with required columns:
#'   timestep, frame, file, object_class, pi_class_pos, total_count
#' @param file_info_pattern A list with components:
#'   \itemize{
#'     \item split: Character used to split filenames (default: "_")
#'     \item keep: Vector of indices of split elements to keep (default: c(1, 4, 5))
#'   }
#' @param fill_missing_counts A list with components:
#'   \itemize{
#'     \item key_cols: Columns to use as keys when filling missing data 
#'       (default: c("file", "frame", "channel", "experiment", "well", "field", "row", "col", "well_info"))
#'     \item count_cols: Count columns to fill with zeros 
#'       (default: c("total_count", "pi_class_neg", "pi_class_pos", "area_pineg", "area_pipos", "area_total"))
#'   }
#' @param group_killcurves_by Vector of column names to group by when calculating kill curves 
#'   (default: c("file"))
#' @param max_time Maximum time point to process in hours (default: 72)
#' @param n_frames Maximum number of frames to process (default: 29)
#'
#' @return A data.table containing processed single-cell data with calculated kill curves.
#'   Key added columns include:
#'   \itemize{
#'     \item pi_frac: Fraction of PI-positive (dead) cells
#'     \item live_frac: Fraction of living cells (1 - pi_frac)
#'     \item interpolated_t: Interpolated time points
#'     \item interpolated_live_frac: Interpolated live cell fractions
#'     \item norm_live_frac: Normalized live cell fractions
#'   }
#'
#' @examples
#' \dontrun{
#' # Assuming 'cell_data' is a data.table with the required columns
#' kill_curves <- get_killcurves(
#'   d = cell_data,
#'   max_time = 48,
#'   n_frames = 25
#' )
#' }
#'
#' @export
#' @importFrom data.table is.data.table tstrsplit fifelse :=
get_killcurves <- function(d,
                        well_info = list(well_data_or_path,
                                        wells_to_ignore), 
                        file_info_pattern = list(split = "_", 
                                              keep = c(1, 4, 5)),
                        fill_missing_counts = list(key_cols = c("file", "frame", "channel", "experiment", "well", "field", "row", "col"),
                                                 count_cols = c("total_count", "pi_class_neg", "pi_class_pos", "area_pineg" , "area_pipos", "area_total")),
                        group_killcurves_by = c("file"),
                        max_time = 72,
                        n_frames = 29) {
    
    # Check that input is a data.table
    if (!data.table::is.data.table(d)) {
        stop("Input 'd' must be a data.table object")
    }
    
    # Check for required columns
    required_cols <- c("timestep", "frame", "file", "object_class", "pi_class_pos", "total_count")
    missing_cols <- required_cols[!required_cols %in% colnames(d)]
    if (length(missing_cols) > 0) {
        stop("Missing required columns in input data: ", paste(missing_cols, collapse = ", "))
    }
    
    # Check that numerical columns are indeed numeric
    numeric_cols <- c("timestep", "frame", "pi_class_pos", "total_count")
    for (col in numeric_cols) {
        if (!is.numeric(d[[col]])) {
            stop(paste0("Column '", col, "' must be numeric"))
        }
    }
        
    # Check that time values are increasing
    if (!all(diff(sort(unique(d$timestep))) >= 0)) {
        warning("Time values are not strictly increasing")
    }
    
    # Check that max time is equal or below the max time of the movie
    colnames <- colnames(d)
    if (!is.null(max_time) && !is.na(max_time)) {
        data_max_t <- max(d$timestep)  
        stopifnot(max_time <= data_max_t, paste0("The variable max_t = ", max_time, " is higher than the max time of the movie =", data_max_t))
    } else {
        max_time <- max(d$timestep)
    }
    
    if (!is.null(n_frames) && !is.na(n_frames)) {
        data_max_frame <- max(d$frame)
        stopifnot(n_frames <= data_max_frame, paste0("The variable n_frames = ", n_frames, " is higher than the max frame of the movie =", data_max_frame))
    } else {
        n_frames <- max(d$frame)
    }
    
    if (!all(group_killcurves_by %in% colnames)) {
        missing_cols <- group_killcurves_by[!group_killcurves_by %in% colnames]
        stop("The columns in group_killcurves_by are not present in the dataframe. Missing columns: ", paste(missing_cols, collapse = ", "))
    }
    
    d[, c("experiment", "well", "field") := data.table::tstrsplit(file, file_info_pattern$split, keep = file_info_pattern$keep)]
    d[, c("row", "col") := data.table::tstrsplit(well, "(?<=\\D)(?=\\d)", perl=TRUE)]
    d[, col := as.integer(col)]
    
    # Add well info (use inplace well info)
    if (!is.null(well_info)) {
        add_well_info(d, well_info$well_data_or_path, wells_to_ignore = well_info$wells_to_ignore)
    }
 
    # If fill_missing_counts present, check that the colnames are present
    if (!is.null(fill_missing_counts)) {
        if (!all(fill_missing_counts$count_cols %in% colnames)) {
            missing_cols <- fill_missing_counts$count_cols[!fill_missing_counts$count_cols %in% colnames]
            stop("The columns in fill_missing_counts$count_cols are not present in the dataframe. Missing columns: ", 
                paste(missing_cols, collapse = ", "))
        }
        if (!all(fill_missing_counts$key_cols %in% colnames)) {
            missing_cols <- fill_missing_counts$key_cols[!fill_missing_counts$key_cols %in% colnames]
            stop("The columns in fill_missing_counts$key_cols are not present in the dataframe. Missing columns: ", 
                paste(missing_cols, collapse = ", "))
        }

        d <- fill_missing_classes(
            dt = d,
            key_cols = fill_missing_counts$key_cols,
            fill_col = "object_class",
            cols_0count = fill_missing_counts$count_cols,
            required_values = unique(d$object_class)
        )
        
        # Fill missing classes:
        fill_unique_val <- c("date_time", "timestep", "abslag_in_s")
        # Check that unique values are also there, otherwise, throw useful message
        if (!all(fill_unique_val %in% colnames)) {
            missing_cols <- fill_unique_val[!fill_unique_val %in% colnames]
            stop("Required columns for fill_unique_val are missing: ", paste(missing_cols, collapse = ", "))
        }
        
        d[, (fill_unique_val) := lapply(.SD, function(x) data.table::fifelse(is.na(x), unique(x)[!is.na(unique(x))], x)), 
            .SDcols = fill_unique_val,
            by = c(fill_missing_counts$key_cols)]
    }

    # Make kill curves and compare both dataframes
    d[, valid_cell := data.table::fifelse(object_class %in% c('single-cell'), TRUE, FALSE)]
    d_sc <- d[object_class == "single-cell"]
    d_sc[, pi_frac := sum(unique(pi_class_pos), na.rm = TRUE)/sum(unique(total_count), na.rm = TRUE), 
        by = .(file, valid_cell, frame, object_class)]
    d_sc[, live_frac := 1 - pi_frac]
    
    # Define data_max_t for interpolation
    data_max_t <- max(d$timestep)
    
    d_sc[, c("interpolated_t", "interpolated_live_frac") := 
        interpolate_variable(timestep, values = live_frac, timestep = seq(0, to = max_time, length.out = data_max_t), 
                           return_time = TRUE, fill_extremes = TRUE), 
        by = group_killcurves_by]
    d_sc[, norm_live_frac := live_frac/max(live_frac, na.rm = TRUE), 
        by = group_killcurves_by]

    d_sc[, interpolated_live_frac := enforce_decrease(interpolated_live_frac), by = .(file)]

    return(d_sc)
}
