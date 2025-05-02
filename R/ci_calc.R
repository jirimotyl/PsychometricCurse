#' Calculation of Confidence Interval (CI)
#'
#' This function calculates and returns the lower and upper confidence interval around the provided total score.
#'
#' @param total_score A numeric value representing the total score for which the confidence interval is to be calculated.
#' @param m A numeric value representing the normative mean score.
#' @param sd A numeric value representing the standard deviation of the normative mean score.
#' @param rel A numeric value between 0 and 1 representing the reliability of the measurement. Default is 0.85.
#' @param rtm A logical value indicating whether to adjust for regression towards the mean (TRUE) or not (FALSE). Default is TRUE.
#' @param ci A numeric value representing the confidence interval percentage. Default is 95.
#' @return A numeric vector containing the lower and upper confidence interval values for the provided total score.
#' @export
#' @examples
#' ci_calc(total_score = 80, m = 70, sd = 10)
#' ci_calc(total_score = 80, m = 70, sd = 10, rel = 0.9, rtm = FALSE, ci = 90)
#' ci_calc(total_score = -1.25, m = 0, sd = 1)

ci_calc <- function(total_score, m, sd, rel = 0.85, rtm = TRUE, ci = 95) {
  # Validate inputs
  if (!is.numeric(total_score)) {
    stop("total_score must be numeric.")
  }
  if (!is.numeric(m)) {
    stop("m must be numeric.")
  }
  if (!is.numeric(sd)) {
    stop("sd must be numeric.")
  }
  if (!is.numeric(rel) || rel < 0 || rel > 1) {
    stop("rel must be numeric and between 0 and 1.")
  }
  if (!is.logical(rtm)) {
    stop("rtm must be logical.")
  }
  if (!is.numeric(ci) || ci < 0 || ci > 100) {
    stop("ci must be numeric and between 0 and 100.")
  }

  # Calculate Standard Error of Measurement (SEM)
  sem <- sd * sqrt(1 - rel)

  # Calculate Confidence Interval for SEM
  ci_sem <- abs(qnorm((100 - ci) / 200)) * sem

  # Calculate true score
  true_score <- rel * total_score + (1 - rel) * m

  # Calculate Standard Error of Estimate (SEE)
  see <- sd * sqrt(rel * (1 - rel))

  # Calculate Confidence Interval for SEE
  ci_see <- abs(qnorm((100 - ci) / 200)) * see

  # Determine the final interval based on rtm
  if (rtm) {
    interval_final <- c(true_score - ci_sem, true_score + ci_sem)
  } else {
    interval_final <- c(total_score - ci_sem, total_score + ci_sem)
  }

  return(interval_final)
}
