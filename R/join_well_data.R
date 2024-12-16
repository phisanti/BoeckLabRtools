#' Combine summary files within a single directory
#'
#' This function searches for files matching the specified pattern in the provided directory path,
#' reads the data from each file, adds a 'file' column with the base name of the file, and combines the data
#' into a single data.table. The combined data is then written to a file with the specified prefix and '_combined_summary.csv' suffix in the directory.
#'
#' @param path A character string specifying the directory path to search for summary files.
#' @param pattern A character string specifying the pattern to match for summary files. Default is '_summary.csv'.
#' @param output_prefix A character string specifying the prefix to be added to the output file name. Default is NULL (no prefix).
#'
#' @return NULL
#'
#' @importFrom data.table :=
#' @importFrom magrittr %>%
combine_summaries <- function(path, pattern = "_summary.csv", output_prefix = NULL) {
  
  summary_files <- list.files(path, pattern = pattern, full.names = TRUE)
  
  if (length(summary_files) == 0) {
    warning("No summary files found in directory: ", path)
    return(NULL)
  }
  
  d <- future.apply::future_lapply(summary_files, function(x) {
    tryCatch(
      {
        tmp <- data.table::fread(x)
        tmp[, file := basename(x)]
        return(tmp)
      },
      error = function(e) {
        warning("Error reading file: ", x, " - ", e$message)
        return(NULL)
      }
    )
  }, future.seed = TRUE) %>%
    data.table::rbindlist(fill = TRUE)
  
  # Create output path
  output_path <- dirname(path)
  if (is.null(output_prefix)) {
    output_prefix <- dirname(dirname(path))
  }
  output_file <- file.path(output_path, paste0(output_prefix, "_combined_summary.csv"))
  
  # Write combined data
  data.table::fwrite(d, file = output_file)
  
  message("Combined summary file created at: ", output_file)
  
  return(NULL)
}

#' Combine summary files from multiple directories
#'
#' This function applies the `combine_summaries` function to each directory path provided in `results_file_paths`.
#'
#' @param results_file_paths A character vector of directory paths to search for summary files.
#' @param pattern A character string specifying the pattern to match for summary files. Default is '_summary.csv'.
#' @param output_prefix A character string specifying the prefix to be added to the output file name. Default is NULL (no prefix).
#' @param parallel A logical value indicating whether to use parallel processing. Default is FALSE.
#' @param cores An integer specifying the number of cores to use for parallel processing. Default is NULL (automatically determined).
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' results_dirs <- c("path/to/results1", "path/to/results2")
#' combine_summary_files(results_dirs, pattern = "_summary.csv", output_prefix = "experiment_1", parallel = TRUE, cores = 4)
#' }
#' 
#' @export
combine_summary_files <- function(results_file_paths, pattern = "_summary.csv", output_prefix = NULL, parallel = FALSE, cores = NULL) {
  
  # Check if results_file_paths is a character vector
  if (!is.character(results_file_paths)) {
    stop("results_file_paths must be a character vector.")
  }
  
  # Check if all provided paths exist
  invalid_paths <- results_file_paths[!dir.exists(results_file_paths)]
  if (length(invalid_paths) > 0) {
    warning("The following paths do not exist and will be skipped: ", 
            paste(invalid_paths, collapse = ", "))
    results_file_paths <- results_file_paths[dir.exists(results_file_paths)]
  }
  
  # Set up parallel processing if specified
  if (parallel) {
    if (is.null(cores)) {
      cores <- parallel::detectCores() - 1
    } else {
      cores <- min(cores, parallel::detectCores() - 1)
    }
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl))
    future::plan(future::cluster, workers = cl)
  }
  
  # Combine summary files from each directory
  lapply(results_file_paths, function(path) {
    BoeckLabRtools:::combine_summaries(path, pattern, output_prefix)
  })
  
  return(NULL)
}
