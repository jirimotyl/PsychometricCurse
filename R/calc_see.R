#' Calculate Standard Error of Estimate (SEE) and its Confidence Interval
#'
#' This function calculates the Standard Error of Estimate (SEE) and its confidence interval.
#' It is designed for psychometric calculations involving reliability and standard deviation.
#'
#' @param sd Standard deviation of the normative mean score.
#' @param rel Reliability of the measurement (must be between 0 and 1; default is 0.85).
#' @param ci Confidence interval percentage (default is 95).
#' @return A named list containing:
#'         - `see`: Standard Error of Estimate
#'         - `see_ci`: Confidence interval for SEE
#' @examples
#' # Example with standard deviation and reliability
#' calc_see(sd = 10, rel = 0.85, ci = 95)
#' # Example with z-score parameters (sd = 1)
#' calc_see(sd = 1, rel = 0.9, ci = 80)
calc_see <- function(sd, rel = 0.85, ci = 95) {
  # Validate inputs
  if (!is.numeric(sd) || sd <= 0) {
    stop("sd must be a positive numeric value.")
  }
  if (!is.numeric(rel) || rel < 0 || rel > 1) {
    stop("rel must be numeric and between 0 and 1.")
  }
  if (!is.numeric(ci) || ci < 0 || ci > 100) {
    stop("ci must be numeric and between 0 and 100.")
  }

  # Calculate Standard Error of Estimate (SEE)
  see <- sd * sqrt(rel * (1 - rel))

  # Calculate Confidence Interval for SEE
  see_ci <- abs(qnorm((100 - ci) / 200)) * see

  # Return results as a named list
  list(
    see = see,
    see_ci = see_ci
  )
}
