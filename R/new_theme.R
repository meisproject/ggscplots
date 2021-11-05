#' theme_classic_new
#'
#' new classic theme based on ggplot2
#'
#' @param base_size numeric font size
#' @param base_family character font family
#' @param base_line_size numeric
#' @param base_rect_size numeric
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' raw_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()
#' raw_plot
#' raw_plot + theme_classic_new()
#'
#' @importFrom ggplot2 %+replace% theme theme_classic theme_bw element_blank element_text element_line
#' @importFrom grid unit
#'
theme_classic_new <- function(base_size = 11, base_family = "", base_line_size = base_size/22,
                              base_rect_size = base_size/22)
{
  theme_classic(base_size = base_size, base_family = base_family,
                         base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(axis.ticks = element_line(colour = "black", size = 0.8),
                   axis.line = element_line(size = 0.8),
                   axis.ticks.length = unit(1.5, "mm"),
                   axis.text = element_text(colour = "black", size = 12),
                   axis.title = element_text(size = 15),
                   legend.text = element_text(colour = "black", size = 12),
                   legend.title = element_text(colour = "black", size = 12))
}


#' theme_bw_new
#'
#' new bw theme based on ggplot2
#'
#' @param base_size numeric font size
#' @param base_family character font family
#' @param base_line_size numeric
#' @param base_rect_size numeric
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' raw_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()
#' raw_plot
#' raw_plot + theme_bw_new()
#'
theme_bw_new <- function(base_size = 11, base_family = "",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  half_line <- base_size / 2

  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12, color = "black"),
      axis.ticks.length = unit(6, "pt"),
      complete = TRUE
    )
}
