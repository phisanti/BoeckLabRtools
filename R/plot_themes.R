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
                            fill_lab = NULL, title = NULL, subtitle = NULL, 
                            base_size = 12, base_family = "") {
  list(
    ggplot2::scale_y_discrete(limits = rev(levels(factor(row_var)))),
    ggplot2::scale_x_continuous(breaks = 1:max(col_var), labels = 1:max(col_var)),
    ggplot2::theme_void(),
    ggplot2::labs(x = col_lab, y = row_lab, fill = fill_lab, title = title, subtitle = NULL),
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = base_size * 0.5, family = base_family),
      axis.text.y = ggplot2::element_text(size = base_size * 0.5, family = base_family),
      axis.title.x = ggplot2::element_text(size = base_size * 0.8, family = base_family),
      axis.title.y = ggplot2::element_text(size = base_size * 0.8, angle = 90, vjust = 0.5, family = base_family),
      strip.text = ggplot2::element_text(size = base_size, face = "bold", family = base_family),
      legend.text = ggplot2::element_text(size = base_size * 0.8, family = base_family)
    )
  )
}


#' Foundation Theme
#'
#' This theme is designed to be a foundation from which to build new
#' themes, and not meant to be used directly. \code{theme_foundation()}
#' is a complete theme with only minimal number of elements defined.
#' It is easier to create new themes by extending this one rather
#' than \code{\link[ggplot2]{theme_gray}()} or \code{\link[ggplot2]{theme_bw}()},
#' because those themes define elements deep in the hierarchy.
#'
#' This theme takes \code{\link[ggplot2]{theme_gray}()} and sets all
#' \code{colour} and \code{fill} values to \code{NULL}, except for the top-level
#' elements (\code{line}, \code{rect}, and \code{title}), which have
#' \code{colour = "black"}, and \code{fill = "white"}. This leaves the spacing
#' and-non colour defaults of the default \pkg{ggplot2} themes in place.
#'
#' This function is adapted from the `ggthemes` package.
#' See the original source at \url{https://github.com/jrnold/ggthemes/blob/main/R/theme-foundation.R}.
#' 
#' @inheritParams ggplot2::theme_grey
#' @importFrom ggplot2 theme_grey
theme_foundation <- function(base_size = 12, base_family = "") {
  thm <- ggplot2::theme_grey(base_size = base_size, base_family = base_family)
  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }
    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }
  thm + ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA),
    legend.background = ggplot2::element_rect(colour = NA),
    line = ggplot2::element_line(colour = "black"),
    rect = ggplot2::element_rect(fill = "white", colour = "black"),
    text = ggplot2::element_text(colour = "black")
  )
}

#' @title Custom ggplot2 Theme for BoeckLab
#'
#' @description A custom ggplot2 theme inspired by the publication style of the BoeckLab. This theme provides a clean and professional look for plots, with options to customize font size, font family, axis text angle, borders, legend position, facet spacing, and plot margins.
#'
#' @param base_size Numeric. The base font size for text elements. Default is 14.
#' @param base_family Character. The base font family for text elements. Default is "Arial".
#' @param x.text.angle Numeric. The angle of the X-axis text labels. Default is 0 (horizontal).
#' @param border Logical. Whether to add a border around the plot. Default is FALSE.
#' @param legend Character. The position of the legend. One of "top", "bottom", "left", "right", or "none". Default is "top".
#' @param spacing Numeric. The spacing between facets in a multi-faceted plot. Default is 0.5.
#' @param margin Logical. Whether to add space between multi-faceted plots. Default is TRUE.
#' @return A ggplot2 theme object.
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' p + theme_BoeckLab()
#' @export
theme_BoeckLab <- function(base_size = 14, 
                           base_family = "Arial", 
                           x.text.angle = 0, 
                           border = FALSE,
                           margin = TRUE, 
                           legend = "top", 
                           spacing = 0.5) {
  # Security checks
  if (!is.numeric(base_size) || base_size <= 0) stop("base_size must be a positive numeric value")
  if (!is.character(base_family)) stop("base_family must be a character string")
  if (!is.numeric(x.text.angle) || x.text.angle < 0 || x.text.angle > 360) stop("x.text.angle must be a numeric value between 0 and 360")
  if (!is.logical(border)) stop("border must be a logical value")
  if (!is.logical(margin)) stop("margin must be a logical value")
  if (!is.numeric(spacing) || spacing < 0) stop("spacing must be a non-negative numeric value")
  if (!legend %in% c("top", "bottom", "left", "right", "none")) stop("legend must be one of 'top', 'bottom', 'left', 'right', or 'none'")
  if (length(legend) > 1) stop("legend must be of length = 1")

  half_line <- base_size / 2
  if (x.text.angle > 5) xhjust <- 1 else xhjust <- NULL
  if (border) {
    panel.border <- ggplot2::element_rect(fill = NA, colour = "black", size = 0.7)
    axis.line <- ggplot2::element_blank()
  } else {
    panel.border <- ggplot2::element_blank()
    axis.line <- ggplot2::element_line(colour = "black", size = 0.5)
  }
  if (margin) {
    plot.margin <- ggplot2::margin(half_line, half_line, half_line, half_line)
  } else {
    plot.margin <- ggplot2::unit(c(0.5, 0.3, 0.3, 0.3), "mm")
  }

  .theme <- theme_foundation(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2), hjust = 0.5),
      text = ggplot2::element_text(),
      panel.background = ggplot2::element_rect(colour = NA),
      plot.background = ggplot2::element_rect(colour = NA),
      panel.border = panel.border,
      axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
      axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
      axis.title.x = ggplot2::element_text(vjust = -0.2),
      axis.text = ggplot2::element_text(),
      axis.line.x = ggplot2::element_line(colour = "black", size = 1),
      axis.line.y = ggplot2::element_line(colour = "black", size = 1),
      axis.ticks = ggplot2::element_line(size = 1),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(colour = NA),
      legend.position = legend,
      legend.key.size = ggplot2::unit(0.2, "cm"),
      panel.spacing = ggplot2::unit(spacing, "cm"),
      legend.title = ggplot2::element_text(face = "italic"),
      plot.margin = plot.margin,
      strip.background = ggplot2::element_rect(colour = NA, fill = NA),
      strip.text = ggplot2::element_text(face = "bold")
    )

  if (x.text.angle != 0) {
    .theme <- .theme + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = x.text.angle, hjust = xhjust))
  }

  .theme
}