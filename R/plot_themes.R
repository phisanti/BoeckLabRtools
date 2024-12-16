#' Plate View Theme for ggplot2
#'
#' This function provides a consistent theme for plate view plots in ggplot2.
#' It sets the scales, labels, and theme elements for a plate view visualization.
#'
#' @param col_var The variable representing the plate columns.
#' @param row_var The variable representing the plate rows.
#' @param col_lab The label for the x-axis (plate columns). Default is "Column".
#' @param row_lab The label for the y-axis (plate rows). Default is "Row".
#' @param fill_lab The label for the fill aesthetic. Default is NULL.
#' @param title The title of the plot. Default is NULL.
#' @param subtitle The subtitle of the plot. Default is NULL.
#'
#' @return A list of ggplot2 theme elements to be added to a ggplot object.
#'
#' @examples
#' library(ggplot2)
#' # Create dummy data
#' data <- data.frame(
#'   row = rep(LETTERS[1:8], each = 12),
#'   col = rep(1:12, 8),
#'   value = runif(96)
#' )
#' ggplot(data, aes(x = col, y = row, fill = value)) +
#'   geom_tile() +
#'   theme_plateview(col_var = data$col, row_var = data$row,
#'                   fill_lab = "Value", title = "Plate View Plot")
#'
#' @export
theme_plateview <- function(col_var, row_var, col_lab = "Column", row_lab = "Row", 
                            fill_lab = NULL, title = NULL, subtitle = NULL) {
  list(
    ggplot2::scale_y_discrete(limits = rev(levels(factor(row_var)))),
    ggplot2::scale_x_continuous(breaks = 1:max(col_var), labels = 1:max(col_var)),
    ggplot2::theme_void(),
    ggplot2::labs(x = col_lab, y = row_lab, fill = fill_lab, title = title, subtitle = NULL),
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      axis.text.y = ggplot2::element_text(size = 6),
      axis.title = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 12, face = "bold")
    )
  )
}


