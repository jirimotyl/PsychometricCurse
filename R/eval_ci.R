#' Descriptive Evaluation of a Confidence Interval
#'
#' This function returns a text description of the given confidence interval, indicating the range of interpretation.
#' The scores are first converted to Z-scores for evaluation using the PsychometricCurse::eval_score function.
#'
#' @param ci_lower A numeric value representing the lower bound of the confidence interval.
#' @param ci_upper A numeric value representing the upper bound of the confidence interval.
#' @param score_type A character string specifying the type of the score (e.g., "z_score", "t_score", "iq", "sten", "stanine", "scaled", "percentile", "custom").
#' @param lang A character string specifying the language for the description. Default is "en".
#' @param m Optional numeric value specifying the mean for a custom distribution. Required if `score_type` is "custom".
#' @param sd Optional numeric value specifying the standard deviation for a custom distribution. Required if `score_type` is "custom".
#' @return A character string providing a text interpretation of the confidence interval in the selected language.
#' @export
#' @examples
#' eval_ci(40, 60, score_type = "t_score")
#' eval_ci(0.32, 0.95, score_type = "z_score")
#' eval_ci(25, 75, score_type = "percentile", lang = "cs")
#' eval_ci(80, 120, score_type = "custom", m = 100, sd = 10)
eval_ci <- function(ci_lower, ci_upper, score_type, lang = "en", m = NULL, sd = NULL) {
  # Check if inputs are NA
  if (is.na(ci_lower) || is.na(ci_upper)) {
    return(NA)
  }

  # Evaluate lower and upper bounds using eval_score
  lower_eval <- eval_score(ci_lower, score_type = score_type, lang = lang, m = m, sd = sd)
  upper_eval <- eval_score(ci_upper, score_type = score_type, lang = lang, m = m, sd = sd)

  # Check if the specified language is supported for interval connectors
  if (!(lang %in% names(interval_connector))) {
    stop("Unsupported language for interval connector. Supported languages are: ", toString(names(interval_connector)))
  }

  # Get the appropriate interval connector for the language
  connector <- interval_connector[[lang]]

  # Compare evaluations
  if (lower_eval == upper_eval) {
    return(lower_eval)
  } else {
    return(paste(lower_eval, connector, upper_eval))
  }
}
