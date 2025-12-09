#' Calculate Standard Error of Measurement (SEM) and its Confidence Interval
#'
#' This function calculates the Standard Error of Measurement (SEM) and its confidence interval.
#' It is designed for psychometric calculations involving reliability and standard deviation.
#'
#' @param sd Standard deviation of the normative mean score.
#' @param rel Reliability of the measurement (must be between 0 and 1). (Default is 0.85).
#' @param ci Confidence interval percentage (default is 95).
#' @return A named list containing:
#'         - `sem`: Standard Error of Measurement
#'         - `ci_sem`: Confidence interval for SEM
#' @export
#' @examples
#' # Example with standard deviation and reliability
#' calc_sem(sd = 10, rel = 0.9, ci = 80)
#' # Example with z-score parameters (sd = 1)
#' calc_sem(sd = 1, rel = 0.85, ci = 95)
#'
calc_sem <- function(sd, rel = 0.85, ci = 95) {
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

  # Calculate Standard Error of Measurement (SEM)
  sem <- sd * sqrt(1 - rel)

  # Calculate Confidence Interval for SEM
  sem_ci <- abs(qnorm((100 - ci) / 200)) * sem

  # Return results as a named list
  list(
    sem = sem,
    sem_ci = sem_ci
  )
}
