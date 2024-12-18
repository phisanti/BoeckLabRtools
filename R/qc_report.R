#' Render the QC report
#' @description This function renders the QC report using the template in `R/qc_report_template.Rmd`.
#' @param path_to_data The path to the data file to be used in the report.
#' @param report_name The name of the report file. If not provided, the report will be saved with the name `path_to_data`_qc_report.pdf.
#' @param template_file The path to the R Markdown template file. If not provided, the default template in the package will be used.
#' @export 
render_QC <- function(path_to_data, report_name=NULL, template_file=NULL) {
  
  if (is.null(report_name)) {
    base_name <- basename(path_to_data)
    base_name <- sub("\\.[^.]*$", "", base_name)
    report_name <- paste(base_name, "qc_report.pdf", sep = "_")
  }
  if (is.null(template_file)) {
      template_file <- system.file("rmd", "qc_report.Rmd", package = "BoeckLabRtools")
  }
  # Input checks
  if (!file.exists(path_to_data)) {
    stop(paste("The data file does not exist:", path_to_data))
  }
  if (!file.exists(template_file)) {
    stop(paste("The template file does not exist:", template_file))
  }
  
  # Set working directory with I/O paths here, otherwise it will be set to the package directory
  working_dir <- getwd()
  report_name <- file.path(working_dir, report_name)
  path_to_data <- file.path(working_dir, path_to_data)
  print(path_to_data)
  # Render the report
  rmarkdown::render(template_file, output_file = report_name, params = list(file = path_to_data, working_dir = working_dir))
}