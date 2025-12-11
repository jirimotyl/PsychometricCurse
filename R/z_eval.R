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
  # Check if the specified language is supported
  if (!(lang %in% lang_supported)) {
    stop("Unsupported language. Supported languages are: ", toString(lang_supported))
  }
  # Define the breaks for Z-score categories
  breaks <- c(-Inf, -2, -1.2, -0.8, 0.8, 1.2, 2, Inf)
  # Get the labels for the specified language
  labels <- zscore_labels[[lang]]
  # Return the description based on the Z-score
  cut(z, breaks = breaks, labels = labels, right = FALSE)
}
