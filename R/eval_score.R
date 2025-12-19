#' Descriptive Evaluation of a Provided Standard Score Value
#'
#' This function returns a text description of the given score, indicating whether it is above average, below average, or within the average range.
#' The score is first converted to a Z-score for evaluation using the PsychometricCurse::eval_score function.
#'
#' @param score A numeric value representing the score to be evaluated.
#' @param score_type A character string specifying the type of the input score. Must be one of the names in the package's `score_params` list. E.g. "t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", or "custom".
#' @param lang A character string specifying the language for the description. Supported languages are for example "en" (English), "cs" (Czech), and "de" (German). Default is "en".
#' @param m Optional numeric value specifying the mean for a custom distribution. Required if `score_type` is "custom".
#' @param sd Optional numeric value specifying the standard deviation for a custom distribution. Required if `score_type` is "custom".
#' @return A character string providing a text interpretation of the given score in the selected language.
#' @export
#' @examples
#' eval_score(65, score_type = "t_score", lang = "en")
#' eval_score(120, score_type = "iq", lang = "cs")
#' eval_score(75, score_type = "custom", lang = "de", m = 80, sd = 5)
eval_score <- function(score, score_type, lang = "en", m = NULL, sd = NULL) {
  # Convert the input score to a Z-score
  z <- convert_standard_score(score, score_type = score_type, target_type = "z_score", m = m, sd = sd)
  # Use the existing eval_zscore function to evaluate the Z-score
  eval_zscore(z, lang = lang)
}
