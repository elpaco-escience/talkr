#' theme for the turn plot
#'
#' @param base_size int
#' @param base_family chr
#' @param ticks bool
#'
#' @export
#'
theme_turnPlot <- function(base_size = 11, base_family = "serif", ticks = TRUE) {
  ggthemes::theme_tufte(
    base_size = base_size,
    base_family = base_family,
    ticks = ticks
  ) %+replace%
    theme(
      legend.position = "none",
      axis.text.y = element_text(),
      strip.text = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title.position = "plot",
      complete = TRUE)
}
