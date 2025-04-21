#' Text evaluation of a provided Z-Score value
#'
#' This returns a text description of the given Z-score
#' Whether it is above/below average, or within the average.
#'
#' @inheritParams z_eval
#' @return Text interpretation of the given Z-Score, in the selected language.
#' @seealso [stringi::stri_length()] which this function wraps.
#' @export
#' @examples
#' z_eval(-1.52)
#' z_eval(1.23, lang = "en")
#' z_eval(0.1, lang = "cs")

z_eval <- function(z, lang = "en") {
  breaks <- c(-Inf, -2, -1.2, -0.8, 0.8, 1.2, 2, Inf)
  labels_lookup <- list(
    en = c("Extremely Below Average", "Below Average", "Lower Average", "Average", "Higher Average", "Above Average", "High Above Average"),
    cs = c("extrémní podprůměr", "podprůměr", "nižší průměr", "průměr", "vyšší průměr", "nadprůměr", "vysoký nadprůměr")
  )
  labels <- labels_lookup[[lang]]
  cut(z, breaks = breaks, labels = labels, right = FALSE)
}

