% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_well_info.R
\name{merge_plate_data}
\alias{merge_plate_data}
\title{Merge plate information with well information}
\usage{
merge_plate_data(
  d,
  well_info,
  info_name = "well_info",
  plate_format = "plate_view",
  wells_to_ignore = c("EMPTY", "BW"),
  force_lower = TRUE,
  inplace = TRUE
)
}
\arguments{
\item{d}{A data.table containing the well information. It must have columns named "row", "col", and "well" (all lowercase).}

\item{well_info}{A data.frame or data.table containing the well information. The format depends on the \code{plate_format} argument.}

\item{info_name}{A character string specifying the name of the column containing the well information. Default is "well_info".}

\item{plate_format}{A character string specifying the format of the well information. Can be either "plate_view" (default) or "transposed".}

\item{wells_to_ignore}{A character vector specifying the wells to ignore. Default is c("EMPTY", "BW").}

\item{force_lower}{A logical value indicating whether to force column names to lowercase. Default is TRUE.}

\item{inplace}{A logical value indicating whether to modify the input data.table in place. Default is TRUE. This means the data.table given
as input will be directly modified without using the assignment operator '<-'.}
}
\value{
A data.table with the merged drug data and well information.
}
\description{
This function merges a dataset containing plate data information with well information based on the specified plate format.
}
\examples{
# Example data
library(data.table)
d <- read.table(text = "
row | col | well | value
A   | 1   | A1   | 10
A   | 2   | A2   | 20
B   | 1   | B1   | 30
B   | 2   | B2   | 40", 
header = TRUE, sep = "|", stringsAsFactors = FALSE, strip.white = TRUE)
d <- data.table(d)
well_info <- read.table(text = "
row | 1     | 2
A   | Drug1 | Drug2
B   | Drug3 | Drug4", 
header = TRUE, sep = "|", stringsAsFactors = FALSE, strip.white = TRUE)
well_info <- data.table::data.table(well_info)
# Merge drug data with well information
merged_data <- merge_plate_data(d, well_info)

# Merge drug data with transposed well information
well_info_transposed <- read.table(text = "
col | row | info
1   | A | Drug1
2   | B | Drug2
1   | A | Drug3
2   | B | Drug4", 
header = TRUE, sep = "|", stringsAsFactors = FALSE, strip.white = TRUE)
well_info_transposed <- data.table::data.table(well_info_transposed)
well_info_transposed[, well := paste0(row, col)]
merged_data_transposed <- merge_plate_data(d, well_info_transposed, plate_format = "transposed")

}
