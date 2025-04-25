#' Calculation of Confidence Interval (CI)
#'
#' This function calculates and returns the lower and upper confidence interval around the provided total score.
#'
#' @param M A numeric value representing the normative mean score.
#' @param SD A numeric value representing the standard deviation of the normative mean score.
#' @param total_score A numeric value representing the total score for which the confidence interval is to be calculated.
#' @param rel A numeric value between 0 and 1 representing the reliability of the measurement. Default is 0.85.
#' @param rtm A logical value indicating whether to adjust for regression towards the mean (TRUE) or not (FALSE). Default is TRUE.
#' @param ci A numeric value representing the confidence interval percentage. Default is 95.
#' @return A numeric vector containing the lower and upper confidence interval values for the provided total score.
#' @export
#' @examples
#' ci_calc(M = 70, SD = 10, total_score = 80)
#' ci_calc(M = 70, SD = 10, total_score = 80, rel = 0.9, rtm = FALSE, ci = 90)
#' ci_calc(M = 0, SD = 1, total_score = -1.25)

ci_calc <- function(M, SD, total_score, rel = 0.85, rtm = TRUE, ci = 95) {
  # Check if inputs are valid
  if (!is.numeric(M) || !is.numeric(SD) || !is.numeric(total_score) ||
      !is.numeric(rel) || !is.logical(rtm) || !is.numeric(ci)) {
    stop("M, SD, total_score, rel, and ci must be numeric. rtm must be logical.")
  }
  if (ci < 0 || ci > 100) {
    stop("Confidence interval must be between 0 and 100.")
  }
  if (rel < 0 || rel > 1) {
    stop("Reliability (rel) must be between 0 and 1.")
  }

  # Calculate Standard Error of Measurement (SEM)
  SEM <- SD * sqrt(1 - rel)

  # Calculate Confidence Interval for SEM
  CI_SEM <- abs(qnorm((100 - ci) / 200)) * SEM

  # Calculate true score
  true_score <- rel * total_score + (1 - rel) * M

  # Calculate Standard Error of Estimate (SEE)
  SEE <- SD * sqrt(rel * (1 - rel))

  # Calculate Confidence Interval for SEE
  CI_SEE <- abs(qnorm((100 - ci) / 200)) * SEE

  # Determine the final interval based on rtm
  if (rtm) {
    interval_final <- c(true_score - CI_SEM, true_score + CI_SEM)
  } else {
    interval_final <- c(total_score - CI_SEM, total_score + CI_SEM)
  }

  return(interval_final)
}

