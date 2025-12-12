#' Calculate Standard Error of Difference (SED) and its Confidence Interval
#'
#' This function calculates the Standard Error of Difference (SED) and its confidence interval,
#' based on the methodology described by Jacobson and Truax (1991).
#'
#' @param sd Standard deviation of the normative mean score.
#' @param rel Reliability of the measurement (must be between 0 and 1; default is 0.85).
#' @param ci Confidence interval percentage (default is 95).
#' @return A named list containing:
#'         - `sed`: Standard Error of Difference
#'         - `sed_ci`: Confidence interval for SED
#' @export
#' @examples
#' # Example with standard deviation and reliability
#' calc_sed(sd = 10, rel = 0.9, ci = 80)
#' # Example with z-score parameters (sd = 1)
#' calc_sed(sd = 1, rel = 0.85, ci = 95)

calc_sed <- function(sd, rel = 0.85, ci = 95) {
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

  # Calculate Standard Error of Difference (SED)
  sed <- sqrt(2 * (calc_sem(sd = sd, rel = rel, ci = ci)$sem)^2)

  # Calculate Confidence Interval for SED
  sed_ci <- abs(qnorm((100 - ci) / 200)) * sed

  # Return results as a named list
  list(
    sed = sed,
    sed_ci = sed_ci
  )
}
