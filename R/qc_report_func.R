#' Create Object Class Distribution Plots
#'
#' This function generates a grid of heatmaps visualizing the spatial distribution of object classes across the plate at the initial time point (t0).
#' The visualization aggregates data by summing the counts for each object class across all fields within each well.
#'
#' @param d A data.table containing the data to be plotted. It should have columns 'frame', 'row', 'col', 'object_class', and 'total_count'.
#'
#' @return A grid of heatmaps showing the object class distribution at t0.
create_object_class_plots <- function(d) {
    total_count <- object_class <- NULL
  d_summary <- d[frame == 0, 
            .(count = sum(total_count)), 
            by = .(row, col, object_class)]
  object_classes <- unique(d_summary$object_class)
  
  plot_list <- purrr::map(object_classes, ~{
    ggplot2::ggplot(d_summary[object_class == .x], ggplot2::aes(x = col, y = row, fill = count)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1) +
      ggplot2::scale_y_discrete(limits = rev(levels(factor(d_summary$row)))) +
      ggplot2::scale_x_continuous(breaks = 1:max(d_summary$col), labels = 1:max(d_summary$col)) +
      ggplot2::theme_void() +
      ggplot2::labs(x = "Column", y = "Row", fill = "Count", title = .x) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        axis.text.y = ggplot2::element_text(size = 6),
        axis.title = ggplot2::element_text(size = 10)
      )
  })
  
    grid_plot <- gridExtra::grid.arrange(
    plot_list[[1]], plot_list[[2]],
    plot_list[[3]], plot_list[[4]],
    plot_list[[5]],
    ncol = 2,
    top = grid::textGrob(expression(paste("Object Classes Distribution at t"[0])), 
                    gp = grid::gpar(fontsize = 16, font = 2))
    )
  return(grid_plot)
}


#' Create Growth Plot
#'
#' This function generates a heatmap providing a comprehensive overview of the growth patterns observed across all wells throughout the experiment.
#' The growth ratio is calculated for each well by computing the ratio of the maximum total area to the minimum total area across all time points.
#' The growth ratio is log-transformed to better visualize the wide range of growth patterns.
#'
#' @param d A data.table containing the data to be plotted. It should have columns 'well', 'frame', 'area_pineg', 'area_pipos', 'row', and 'col'.
#'
#' @return A heatmap showing the growth approximation across the plate.
create_growth_plot <- function(d) {
    area_pineg <- area_pipos <- total_area <- well <- NULL
  area_summary <- d[, 
    .(total_area = sum(area_pineg + area_pipos)), 
    by = .(well, frame)]
  growth_ratio <- area_summary[, 
        .(growth_ratio = max(total_area) / min(total_area)), 
        by = well]
  growth_ratio <- merge(growth_ratio, unique(d[, .(well, row, col)]), by = "well")
  
  ggplot2::ggplot(growth_ratio, ggplot2::aes(x = col, y = row, fill = log(growth_ratio))) +
    ggplot2::geom_tile() +
    viridis::scale_fill_viridis(option = "plasma", direction = -1) +
    ggplot2::scale_y_discrete(limits = rev(levels(factor(growth_ratio$row)))) +
    ggplot2::scale_x_continuous(breaks = 1:max(growth_ratio$col), labels = 1:max(growth_ratio$col)) +
    ggplot2::theme_void() +
    theme_plateview(col_var = growth_ratio$col, row_var = growth_ratio$row,
                  fill_lab = expression(Log(Area[t[0]] / Area[t[max]])), title = "Growth Approximation")
}


#' Create Consistency Plot
#'
#' This function generates a heatmap demonstrating the consistency of classification in the bright field across different conditions.
#' For each well and object class, a linear regression model is fitted to the object count over time. 
#' The slope of this model, normalized by the initial count and expressed as a percentage, represents the rate of change in classification.
#' The plot is faceted by object class, allowing comparison of classification consistency across different types of objects.
#'
#' @param d A data.table containing the data to be plotted. It should have columns 'well', 'object_class', 'total_count', 'frame', 'row', and 'col'.
#'
#' @return A heatmap showing the classification consistency across the plate, faceted by object class.
create_consistency_plot <- function(d) {
    well <- object_class <- percent_change_capped <- percent_change_per_frame <- NULL
  classification_consistency <- d[, {
    model <- stats::lm(total_count ~ frame)
    coef <- stats::coef(model)
    initial_count <- coef["(Intercept)"]
    slope <- coef["frame"]
    .(percent_change_per_frame = (slope / initial_count) * 100)
  }, by = .(well, object_class)]
  
  classification_consistency <- merge(classification_consistency, unique(d[, .(well, row, col)]), by = "well")
  
  q5_q95 <- stats::quantile(classification_consistency$percent_change_per_frame, probs = c(0.05, 0.95), na.rm = TRUE)
  classification_consistency[, percent_change_capped := pmin(pmax(percent_change_per_frame, q5_q95[1]), q5_q95[2])]
  
  ggplot2::ggplot(classification_consistency, ggplot2::aes(x = col, y = row, fill = percent_change_capped)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_distiller(palette = "Spectral", direction = 1) +
    ggplot2::scale_y_discrete(limits = rev(levels(factor(classification_consistency$row)))) +
    ggplot2::scale_x_continuous(breaks = 1:max(classification_consistency$col), labels = 1:max(classification_consistency$col)) +
    ggplot2::facet_wrap(~ object_class, ncol = 2) +
    ggplot2::theme_void() +
    ggplot2::labs(x = "Column", y = "Row", fill = "% Change/Frame", 
         title = "Classification Consistency (% Change per Frame)",
         subtitle = sprintf("Color scale capped at 5th (%.2f) and 95th (%.2f) percentiles", q5_q95[1], q5_q95[2])) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      axis.text.y = ggplot2::element_text(size = 6),
      axis.title = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 12, face = "bold")
    )
}

create_pi_consistency_plot <- function(d) {
    pi_frac <- total_count <- pi_class_pos <- object_class <- live_frac <- sd_live_frac <- total_sd <- well <-  NULL
  d[, pi_frac := sum(unique(pi_class_pos))/sum(unique(total_count)), 
    by = .(file, well, frame, object_class)]
  d[, live_frac := 1 - pi_frac]
  
  d_susmmary <- d[object_class %in% c('single-cell'), 
    .(live_frac = mean(live_frac),
      sd_live_frac = stats::sd(live_frac)), 
    by = .(well, row, col, frame)][, .(total_sd = sum(sd_live_frac)),
      by = .(well, row, col)] 

    ggplot2::ggplot(d_susmmary, ggplot2::aes(x = col, y = row, fill = total_sd)) +
    ggplot2::geom_tile() +
    viridis::scale_fill_viridis(option = "plasma", direction = -1) +
    ggplot2::scale_y_discrete(limits = rev(levels(factor(d_susmmary$row)))) +
    ggplot2::scale_x_continuous(breaks = 1:max(d_susmmary$col), labels = 1:max(d_susmmary$col)) +
    ggplot2::theme_void() +
    ggplot2::labs(x = "Column", y = "Row", fill = expression(sum(sigma[t], t == 1, t[max])), title = "PI Classification Consistency (Standard Deviation)") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.title = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 12, face = "bold")
    )
}


#' Create PI Consistency Plot
#'
#' This function generates a heatmap visualizing the consistency of Propidium Iodide (PI) classification across different wells and time points, focusing on single-cell objects.
#' For each well, frame, and object class, the fraction of PI-positive cells is calculated. The complement of the PI fraction represents the live cell fraction.
#' The standard deviation of the live fraction across all time points is computed for each well, serving as a measure of classification consistency over time.
#'
#' @param d A data.table containing the data to be plotted. It should have columns 'file', 'well', 'frame', 'object_class', 'pi_class_pos', 'total_count', 'row', and 'col'.
#'
#' @return A heatmap showing the PI classification consistency across the plate.
create_time_delay_plot <- function(d) {
    date_time <- delay_hours <- well <- NULL
  ref_time <- d[, min(date_time)]
  time_delay <- d[frame == 0, {
    .(delay_hours = difftime(min(date_time), ref_time, units = "hours"),
      start_time = min(date_time),
      ref_time = ref_time)
  }, by = .(well, row, col)]
  
  ggplot2::ggplot(time_delay, ggplot2::aes(x = col, y = row, fill = delay_hours)) +
    ggplot2::geom_tile() +
    viridis::scale_fill_viridis(option = "plasma", direction = 1) +
    ggplot2::scale_y_discrete(limits = rev(levels(factor(time_delay$row)))) +
    ggplot2::scale_x_continuous(breaks = 1:max(time_delay$col), labels = 1:max(time_delay$col)) +
    ggplot2::theme_void() +
    ggplot2::labs(x = "Column", y = "Row", fill = "Delay (hours)", title = "Start Recording Time Delay") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.title = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 12, face = "bold")
    )
}