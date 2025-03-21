## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load_libraries-----------------------------------------------------------
library(BoeckLabRtools)
library(data.table)
library(ggplot2)
library(magrittr)
d <- data.table::as.data.table(BoeckLabRtools::killcurves)

## ----extract_well_info--------------------------------------------------------
d[, c("well", "field") := tstrsplit(file, "_", keep = c(2, 4))]
d[, c("row", "col") := data.table::tstrsplit(well, "(?<=\\D)(?=\\d)", perl = TRUE)]
d[, col := as.integer(col)]

## ----fill_time_cols-----------------------------------------------------------

d <- fill_missing_classes(
  dt = d,
  key_cols = c("file", "frame", "row", "col", "well", "field"),
  fill_col = "object_class",
  cols_0count = c("total_count", "pi_class_neg", "pi_class_pos", "area_pineg", "area_pipos"),
  required_values = unique(d$object_class)
)

## -----------------------------------------------------------------------------
fill_cols_uniq_val <- c("date_time", "timestep")
d[, (fill_cols_uniq_val) := lapply(.SD, function(x) fifelse(is.na(x), unique(x)[2], x)),
  .SDcols = fill_cols_uniq_val, 
  by = .(file, frame)]

## ----table2, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'--------
plate_schema <- data.table(BoeckLabRtools::plate_schema) # This is the example schema for our dataset
setnames(plate_schema, c("row", as.character(3:12)))
knitr::kable(plate_schema, 
             caption = "Plate Schema with Treatment Information",
             format = "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                           full_width = FALSE) %>%
  kableExtra::row_spec(0, bold = TRUE, background = "#E6F0FF") %>%
  kableExtra::column_spec(1, bold = TRUE, background = "#F0F0F0") %>%
  kableExtra::add_footnote("Treatment abbreviations: IPM - Imipenem, TGC - Tigecycline, AMK - Amikacin, MXF - Moxifloxacin, CLR - Clarithromycin", 
                          notation = "symbol")



## ----add_treatment_info-------------------------------------------------------

d <- add_well_info(d, 
                   plate_schema, # In your case plate schema can be a path to the file
                   info_name = "treatment",
                   plate_format = "plate_view"
                   )

## ----calculate_live_fraction--------------------------------------------------

d[, valid_cell := data.table::fifelse(object_class %in% c('single-cell'), TRUE, FALSE)]
d_sc <- d[object_class == "single-cell"]
d_sc[, pi_frac := pi_class_pos/total_count, 
        by = .(file, valid_cell, frame, object_class)]
d_sc[, live_frac := 1 - pi_frac]


## ----interpolate_time_points--------------------------------------------------

d_sc[, c("interpol_time", "interpol_live_frac") := interpolate_variable(time = timestep, 
                                                                     values = live_frac, 
                                                                     timepoints = c(0, 3, 6, 9, 12), 
                                                                     fill_extremes = TRUE, 
                                                                     return_time = TRUE),
          by = .(file)]

d_sc[, interpol_live_frac := enforce_decrease(interpol_live_frac), 
  by = .(file)]


## ----plot_killcurves, fig.width=10, fig.height=8------------------------------
# plot
d_sc %>%
  ggplot(., aes(x = timestep, y =live_frac, group = file)) +
  geom_point(aes(col = well), show.legend = FALSE) +
  facet_wrap(treatment ~ .) +
  theme_BoeckLab()


## ----calculate_auc------------------------------------------------------------

auc_data <- d_sc[, .(auc = calc_auc(x = interpol_time, y = interpol_live_frac)), 
              by = .(file, well, field, row, col, treatment)]


## ----summarize_by_treatment, fig.width=8, fig.height=6------------------------

auc_data %>%
  .[, .(auc = mean(auc), sd_auc = sd(auc)), 
    by = .(treatment)] %>%
  ggplot(., aes(x = treatment, y = auc, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = auc - sd_auc, ymax = auc + sd_auc), 
                position = position_dodge(width = 0.9), width = 0.25) +
  theme_BoeckLab()

