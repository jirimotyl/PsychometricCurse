#' Calculate True Score Estimate
#'
#' This function calculates the true score estimate using the observed score,
#' reliability of the measurement, and the normative mean. The estimate is based on the assumption of the regression towards the mean.
#' The true score is estimated using the formula: true_score = rel x score + (1 - rel) x m.
#'
#' @param score Observed score.
#' @param m Normative mean score.
#' @param rel Reliability of the measurement (must be between 0 and 1; default is 0.85).
#' @return A numeric value representing the true score estimate.
#' @export
#' @examples
#' # Example with observed score, normative mean, and reliability
#' calculate_true_score(score = 80, m = 70, rel = 0.85)
#' # Example with z-score (m = 0)
#' calculate_true_score(score = 0.8, m = 0, rel = 0.85)
calc_true_score <- function(score, m, rel = 0.85) {
  # Validate inputs
  if (!is.numeric(score)) {
    stop("score must be numeric.")
  }
  if (!is.numeric(m)) {
    stop("m must be numeric.")
  }
  if (!is.numeric(rel) || rel < 0 || rel > 1) {
    stop("rel must be numeric and between 0 and 1.")
  }

  # Calculate true score estimate
  true_score <- rel * score + (1 - rel) * m

  # Return the true score estimate
  true_score
}
