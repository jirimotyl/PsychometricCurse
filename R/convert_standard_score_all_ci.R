#' Convert a Standard Score to All Possible Standard Score Types with Confidence Intervals Calculated
#'
#' Converts a given score from one standard type to all supported standard score types,
#' and calculates the confidence interval for each converted score.
#'
#' @param score A numeric value representing the score to convert.
#' @param from A character string specifying the type of the input score. Must be one of
#'        "t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", or "custom".
#' @param m Optional numeric value specifying the mean for a custom distribution.
#'        Required if `from` or any `to` is "custom".
#' @param sd Optional numeric value specifying the standard deviation for a custom distribution.
#'        Required if `from` or any `to` is "custom".
#' @param rel A numeric value between 0 and 1 representing the reliability of the measurement. Default is 0.85.
#' @param rtm A logical value indicating whether to adjust for regression towards the mean (TRUE) or not (FALSE). Default is TRUE.
#' @param ci A numeric value representing the confidence interval percentage. Default is 95.
#' @return A tibble with columns for each standard score type, their converted values, and their confidence intervals.
#' @export
#' @examples
#' convert_standard_score_all_ci(65, from = "t_score")
#' convert_standard_score_all_ci(120, from = "iq", rel = 0.9, ci = 90)
#' convert_standard_score_all_ci(75, from = "custom", m = 80, sd = 5)
#'
convert_standard_score_all_ci <- function(score, from, m = NULL, sd = NULL, rel = 0.85, rtm = TRUE, ci = 95) {
  valid_types <- c("t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", "custom")

  # Validate score
  if (!is.numeric(score)) stop("`score` must be numeric.")
  # Validate custom parameters if needed
  if ((from == "custom") && (is.null(m) || is.null(sd) || sd <= 0)) {
    stop("`m` and `sd` must be provided and valid (sd > 0) when using 'custom'.")
  }

  # Use the shared score_params
  params <- get0("score_params", envir = asNamespace("PsychometricCurse"))
  params$custom <- c(mean = m, sd = sd)

  # Convert all scores
  converted <- convert_standard_score_all(score, from, m, sd)

  # Calculate CIs for each score type
  ci_results <- lapply(valid_types, function(to) {
    if (to == "percentile" || to == "custom" && is.null(m) && is.null(sd)) {
      return(NA)
    }
    converted_score <- converted[[to]]
    if (is.na(converted_score)) return(NA)
    ci_calc(converted_score, score_type = to, rel = rel, rtm = rtm, ci = ci)
  })
  names(ci_results) <- paste0(valid_types, "_ci")

  # Combine into a single tibble
  result <- cbind(converted, do.call(cbind, lapply(ci_results, as.numeric)))
  rownames(result) <- "type"
  tibble::as_tibble(t(result), rownames = "type")
}

