## Script to prepare data
library(usethis)

# Create the plate_schema data
plate_schema <- data.frame(
  row = c("C", "D", "E", "F", "G", "H"),
  `3` = c("IPM", "IPM", "IPM", "IPM", "MXF", "TGC"),
  `4` = c("TGC", "TGC", "TGC", "AMK", "AMK", "AMK"),
  `5` = c("TGC", "TGC", "TGC", "AMK", "TGC", "AMK"),
  `6` = c("AMK", "AMK", "AMK", "TGC", "TGC", "TGC"),
  `7` = c("AMK", "AMK", "TGC", "TGC", "TGC", "TGC"),
  `8` = c("AMK", "AMK", "TGC", "TGC", "TGC", "TGC"),
  `9` = c("TGC", "TGC", "TGC", "TGC", "TGC", "TGC"),
  `10` = c("TGC", "IPM", "IPM", "IPM", "TGC", "IPM"),
  `11` = c("MXF", "CLR", "CLR", "TGC", "AMK", "AMK"),
  `12` = c("MXF", "CLR", "TGC", "AMK", "TGC", "AMK"),
  stringsAsFactors = FALSE
)

# Save the data
usethis::use_data(plate_schema, overwrite = TRUE) 