#' Render the QC report
#' @description This function renders the QC report using the template in `R/qc_report_template.Rmd`.
#' @param path_to_data The path to the data file to be used in the report.
#' @param well_field The location of the well and field in the filename pattern. The filemane of each movie should structure with `_` 
#' separating each information bit. For example, for the filename "ASCT.xx.20250101_LoopAt_C3_p01__Channel_40x BF,40x PI_summary.csv", 
#' the well is located in position 3 and the field is located in position 4. The default is c(3, 4).
#' @param report_name The name of the report file. If not provided, the report will be saved with the name `path_to_data`_qc_report.pdf.
#' @param template_file The path to the R Markdown template file. If not provided, the default template in the package will be used.
#' @export 
render_QC <- function(path_to_data, well_field = c(3, 4), working_dir=NULL, report_name=NULL, template_file=NULL) {
  
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
  if (is.null(working_dir)) {
    working_dir <- getwd()
  }
  setwd(working_dir)
  report_name <- file.path(working_dir, report_name)
  path_to_data <- file.path(working_dir, path_to_data)
  print(path_to_data)
  # Render the report
  rmarkdown::render(template_file, output_file = report_name, params = list(file = path_to_data, well_field = well_field))
}