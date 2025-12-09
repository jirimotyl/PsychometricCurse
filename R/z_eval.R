#' Descriptive Evaluation of a Provided Z-Score Value
#'
#' This function returns a text description of the given Z-score, indicating whether it is above average, below average, or within the average range.
#'
#' @param z A numeric value representing the Z-score to be evaluated.
#' @param lang A character string specifying the language for the description. Currently supported languages are "en" (English), "cs" (Czech), and "de" (German). Default is "en".
#' @return A character string providing a text interpretation of the given Z-score in the selected language.
#' @export
#' @examples
#' zscore_eval(-1.52)
#' zscore_eval(1.23, lang = "en")
#' zscore_eval(0.1, lang = "cs")

zscore_eval <- function(z, lang = "en") {
  # Check if z is NA
  if (is.na(z)) {
    return(NA)
  }

  # Define the breaks for Z-score categories
  breaks <- c(-Inf, -2, -1.2, -0.8, 0.8, 1.2, 2, Inf)

  # Define the labels for each language
  labels_lookup <- list(
    en = c("Extremely Below Average", "Below Average", "Lower Average", "Average", "Higher Average", "Above Average", "High Above Average"),
    cs = c("extrémní podprůměr", "podprůměr", "nižší průměr", "průměr", "vyšší průměr", "nadprůměr", "vysoký nadprůměr"),
    de = c("Extrem unterdurchschnittlich", "Unterdurchschnittlich", "Leicht unterdurchschnittlich", "Durchschnittlich", "Leicht überdurchschnittlich", "Überdurchschnittlich", "Hoch überdurchschnittlich")
  )

  # Check if the specified language is supported
  if (!(lang %in% names(labels_lookup))) {
    stop("Unsupported language. Supported languages are: ", toString(names(labels_lookup)))
  }

  # Get the labels for the specified language
  labels <- labels_lookup[[lang]]

  # Return the description based on the Z-score
  cut(z, breaks = breaks, labels = labels, right = FALSE)
}
