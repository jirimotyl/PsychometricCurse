#' Calculation of Reliable Change Index (RCI)
#'
#' This function calculates and returns the reliable change index, indicating whether the change in scores is statistically significant. It handles both single and two-test scenarios based on the provided parameters and supports different standard score types.
#'
#' @param total_score A numeric value representing the current total score.
#' @param total_score_old A numeric value representing the previous total score.
#' @param score_type An optional character string specifying the type of standard score. Supported types are "z_score", "t_score", "sten", "stanine", "iq", and "scaled". If provided, m and sd are set automatically. Default is NULL, which requires m and sd to be specified.
#' @param m An optional numeric value representing the normative mean score. Required only if score_type is NULL.
#' @param sd An optional numeric value representing the standard deviation of the normative mean score. Required only if score_type is NULL.
#' @param rel A numeric value between 0 and 1 representing the reliability of the measurement. Default is 0.85.
#' @param rel_old An optional numeric value between 0 and 1 representing the reliability of the old (different) test. If provided, the function calculates RCI for two different tests.
#' @param rtm A logical value indicating whether to adjust for regression towards the mean (TRUE) or not (FALSE). Default is TRUE.
#' @param ci A numeric value representing the confidence interval percentage. Default is 95.
#' @param rci_method An integer value indicating the method to calculate the standard error. Default is 1. Use 1 for Lord & Novick (1968) approach, 2 for Jacobson & Truax (1991) approach.
#' @param lang A character string specifying the language for the description. Currently supported languages are "en" (English) and "cs" (Czech). Default is "en".
#' @return A list containing the evaluation result (`evaluation_result`), the tested change (`tested_change`), the standard error (of prediction or difference) with confidence interval (`standard_error_with_CI`), the regression towards the mean adjustment (`regression_towards_mean`), the confidence interval percentage (`confidence_interval`), and the RCI method used (`rci_method`). Returns NA if `total_score` or `total_score_old` is NA.
#' @export
#' @examples
#' # Example with custom m and sd
#' rci(m = 70, sd = 10, total_score = 85, total_score_old = 80)
#'
#' # Example with score_type, no need to specify m and sd
#' rci(total_score = 55, total_score_old = 50, score_type = "t_score")
#'
#' # Example with score_type and additional parameters
#' rci(total_score = 12, total_score_old = 10, rtm = FALSE, ci = 90, rci_method = 2, lang = "cs", score_type = "scaled")

rci <- function(total_score, total_score_old, score_type = NULL, m = NULL, sd = NULL, rel = 0.85, rel_old = NULL, rtm = TRUE, ci = 95, rci_method = 1, lang = "en") {
  # Early parameter validation
  if (is.na(total_score) || is.na(total_score_old)) {
    return(NA)
  }

  if (!is.null(m) && !is.numeric(m) || !is.null(sd) && !is.numeric(sd) || !is.numeric(total_score) || !is.numeric(total_score_old) ||
      !is.logical(rtm) || !is.numeric(ci)) {
    stop("m, sd, total_score, total_score_old, and ci must be numeric. rtm must be logical.")
  }

  if (ci < 0 || ci > 100) {
    stop("Confidence interval must be between 0 and 100.")
  }

  # Adjust m and sd based on score_type
  if (!is.null(score_type)) {
    score_params <- list(
      z_score = c(m = 0, sd = 1),
      t_score = c(m = 50, sd = 10),
      sten = c(m = 5.5, sd = 2),
      stanine = c(m = 5, sd = 2),
      iq = c(m = 100, sd = 15),
      scaled = c(m = 10, sd = 3)
    )

    if (score_type %in% names(score_params)) {
      m <- unname(score_params[[score_type]]["m"])
      sd <- unname(score_params[[score_type]]["sd"])
    } else {
      stop("Unsupported score_type. Use 'z_score', 't_score', 'sten', 'stanine', 'iq', or 'scaled', or leave as NULL for custom m and sd.")
    }
  } else {
    if (is.null(m) || is.null(sd)) {
      stop("m and sd must be specified if score_type is NULL.")
    }
  }

  if (!is.null(rel_old)) {
    if (!is.numeric(rel_old)) {
      stop("rel_old must be numeric.")
    }
    if (rel < 0 || rel > 1 || rel_old < 0 || rel_old > 1) {
      stop("Reliability (rel and rel_old) must be between 0 and 1.")
    }

    # Calculate true score and tested change
    if (rtm) {
      true_score <- (sqrt(rel) * (total_score - m)) - (sqrt(rel_old) * (total_score_old - m))
      tested_change <- true_score
    } else {
      tested_change <- total_score - total_score_old
    }

    # Calculate standard error
    sep <- sd * sqrt(2 - rel - rel_old)    # Standard error of prediction / difference

  } else {
    if (!is.numeric(rel)) {
      stop("rel must be numeric.")
    }
    if (rel < 0 || rel > 1) {
      stop("Reliability (rel) must be between 0 and 1.")
    }

    # Calculate true score and tested change
    if (rtm) {
      true_score <- rel * total_score_old + (1 - rel) * m
      tested_change <- total_score - true_score
    } else {
      tested_change <- total_score - total_score_old
    }

    # Calculate standard error
    if (rci_method == 1) {
      sep <- sd * sqrt(1 - rel^2)    # Standard error of prediction
    } else if (rci_method == 2) {
      sep <- sqrt(2 * (sd * sqrt(1 - rel))^2)    # Standard error of difference
    } else {
      stop("Unsupported rci_method. Use 1 for Lord & Novick (1968) or 2 for Jacobson & Truax (1991).")
    }
  }

  sep_ci <- sep * abs(qnorm((100 - ci) / 200))  # Standard error supplemented with confidence interval

  # Define the labels for each language
  labels_lookup <- list(
    en = c("Significant improvement", "Change not confirmed", "Significant decline"),
    cs = c("Signifikantní zlepšení", "Změna nepotvrzena", "Signifikantní pokles")
  )

  # Check if the specified language is supported
  if (!(lang %in% names(labels_lookup))) {
    stop("Unsupported language. Supported languages are: ", toString(names(labels_lookup)))
  }

  # Get the labels for the specified language
  labels <- labels_lookup[[lang]]

  # Determine evaluation result
  if (tested_change > sep_ci) {
    eval <- labels[1]
  } else if (tested_change <= sep_ci & tested_change >= -sep_ci) {
    eval <- labels[2]
  } else {
    eval <- labels[3]
  }

  # Return results with clear naming
  results <- list(
    evaluation_result = eval,
    tested_change = round(tested_change, 2),
    standard_error_with_CI = round(sep_ci, 2),
    regression_towards_mean = rtm,
    confidence_interval = ci,
    rci_method = rci_method
  )

  return(results)
}
