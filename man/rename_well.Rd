% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_wells.R
\name{rename_well}
\alias{rename_well}
\title{Rename Well Identifiers}
\usage{
rename_well(well, file, omitted_rows = c("A", "B"))
}
\arguments{
\item{well}{Character vector of well identifiers to be renamed.}

\item{file}{Character vector of file names associated with each well.}

\item{omitted_rows}{Character vector of row identifiers to be omitted and renamed with a "Z" prefix (default: c("A", "B")).}
}
\value{
A character vector of renamed well identifiers.
}
\description{
This function renames well identifiers to handle NIKON file naming conventions for 1536-well plates.
In cases where the row goes beyond A-Z, NIKON uses the same letters of the alphabet with the appendix "\emph{0001}" (e.g., A_*\emph{\strong{_0001}}).
For improved data handling, these cases are changed to ZA, ZB, and so on.
}
\examples{
wells <- c("A01", "B02", "C03", "A01_0001_", "B02_0001_")
files <- c("file1.txt", "file2.txt", "file3.txt", "file4_0001_.txt", "file5_0001_.txt")
rename_well(wells, files)

}
