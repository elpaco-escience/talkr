#' theme for the turn plot
#'
#' @param base_size int
#' @param base_family chr
#' @param ticks bool
#'
#' @importFrom ggplot2 %+replace%
#' @export
#'
theme_turnPlot <- function(base_size = 11, base_family = "serif", ticks = TRUE) {
  ggthemes::theme_tufte(
    base_size = base_size,
    base_family = base_family,
    ticks = ticks
  ) %+replace%
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(),
      strip.text.x = ggplot2::element_text(hjust = 0, margin=ggplot2::margin(l=0)),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title.position = "plot",
      complete = TRUE)
}
