#' Calculate Standard Error of Prediction (SEP) and its Confidence Interval
#'
#' This function calculates the Standard Error of Prediction (SEP) and its confidence interval,
#' based on Formula 3.8.5 from Lord and Novick (1968).
#' It is designed for Reliable Change Index (RCI) calculations involving two measurements
#' obtained from the same test, as proposed by Lord and Novick (1968).
#'
#' @param sd Standard deviation of the normative mean score.
#' @param rel Reliability of the measurement (must be between 0 and 1). Default is 0.85.
#' @param ci Confidence interval percentage. Default is 95.
#' @return A named list containing:
#'         - `sep`: Standard Error of Prediction
#'         - `sep_ci`: Confidence interval for SEP
#' @export
#' @examples
#' # Example with standard deviation and reliability
#' calc_sep(sd = 10, rel = 0.9, ci = 80)
#' # Example with z-score parameters (sd = 1)
#' calc_sep(sd = 1, rel = 0.85, ci = 95)

calc_sep <- function(sd, rel = 0.85, ci = 95) {
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

  # Calculate Standard Error of Prediction (SEP)
  sep <- sd * sqrt(1 - rel^2)

  # Calculate Confidence Interval for SEP
  sep_ci <- abs(qnorm((100 - ci) / 200)) * sep

  # Return results as a named list
  list(
    sep = sep,
    sep_ci = sep_ci
  )
}
