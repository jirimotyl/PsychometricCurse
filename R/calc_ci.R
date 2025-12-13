#' Calculation of Confidence Interval (CI)
#'
#' This function calculates and returns the lower and upper confidence interval around the provided score.
#' For percentiles, it converts to z-score, calculates the CI, and converts back to percentiles.
#' For other score types, it uses the provided or predefined mean and standard deviation.
#'
#' @param score A numeric value representing the score for which the confidence interval is to be calculated.
#' @param m A numeric value representing the normative mean score. Required if `score_type` is NULL or "custom".
#' @param sd A numeric value representing the standard deviation of the normative mean score. Required if `score_type` is NULL or "custom".
#' @param score_type A character string specifying the type of the score (e.g., "z_score", "t_score", "scaled", "iq", "sten", "percentile", "custom").
#'        If provided as a valid type (except "custom"), `m` and `sd` are ignored and fetched from `score_params`.
#' @param rel A numeric value between 0 and 1 representing the reliability of the measurement. Default is 0.85.
#' @param rtm A logical value indicating whether to adjust for regression towards the mean (TRUE) or not (FALSE). Default is TRUE.
#' @param ci A numeric value representing the confidence interval percentage. Default is 95.
#' @return A named numeric vector containing the lower and upper confidence interval values for the provided score.
#' @export
#' @examples
#' calc_ci(score = 80, m = 70, sd = 10)
#' calc_ci(score = 65, score_type = "t_score", rel = 0.9, rtm = FALSE, ci = 90)
#' calc_ci(score = -1.25, score_type = "z_score")
#' calc_ci(score = 75, score_type = "percentile")
#' calc_ci(score = 80, score_type = "custom", m = 70, sd = 8)
calc_ci <- function(score, m = NULL, sd = NULL, score_type = NULL, rel = 0.85, rtm = TRUE, ci = 95) {
  # Validate inputs
  if (!is.numeric(score)) stop("score must be numeric.")
  if (!is.numeric(rel) || rel < 0 || rel > 1) stop("rel must be numeric and between 0 and 1.")
  if (!is.logical(rtm)) stop("rtm must be logical.")
  if (!is.numeric(ci) || ci < 0 || ci > 100) stop("ci must be numeric and between 0 and 100.")

  # Handle percentile
  if (!is.null(score_type) && score_type == "percentile") {
    if (score < 0 || score > 100) stop("Percentile must be between 0 and 100.")
    z_score <- qnorm(score / 100)
    sem_result <- calc_sem(sd = 1, rel = rel, ci = ci)
    sem <- sem_result$sem
    ci_sem <- sem_result$sem_ci
    true_score <- calc_true_score(score = z_score, m = 0, rel = rel)
    if (rtm) {
      interval_final <- c(true_score - ci_sem, true_score + ci_sem)
    } else {
      interval_final <- c(z_score - ci_sem, z_score + ci_sem)
    }
    interval_final <- pnorm(interval_final) * 100
    return(interval_final)
  }

  # If score_type is NULL, use provided m and sd
  if (is.null(score_type)) {
    if (is.null(m) || is.null(sd)) {
      stop("If score_type is not provided, both m and sd must be provided.")
    }
    if (!is.numeric(m) || !is.numeric(sd) || sd <= 0) {
      stop("m and sd must be numeric and sd must be greater than 0.")
    }
  } else if (score_type == "custom") {
    # If score_type is "custom", use provided m and sd
    if (is.null(m) || is.null(sd) || !is.numeric(m) || !is.numeric(sd) || sd <= 0) {
      stop("For 'custom' score_type, m and sd must be provided and valid (sd > 0).")
    }
  } else {
    # If score_type is provided and not "custom" or "percentile", fetch m and sd from score_params
    params <- get0("score_params", envir = asNamespace("PsychometricCurse"))
    if (is.null(params[[score_type]])) {
      stop("Invalid score_type. See documentation for valid types.")
    }
    if (is.null(params[[score_type]]$m) || is.null(params[[score_type]]$sd)) {
      stop("score_type must have defined mean and sd in score_params.")
    }
    m <- params[[score_type]]$m
    sd <- params[[score_type]]$sd
  }

  # Calculate SEM and CI for SEM
  sem_values <- calc_sem(sd = sd, rel = rel, ci = ci)
  sem <- sem_values$sem
  sem_ci <- sem_values$sem_ci

  # Calculate true score estimate
  true_score <- calc_true_score(score = score, m = m, rel = rel)

  # Determine the final interval based on rtm
  if (rtm) {
    interval_final <- c(true_score - sem_ci, true_score + sem_ci)
  } else {
    interval_final <- c(score - sem_ci, score + sem_ci)
  }

  names(interval_final) <- c("ci_lower", "ci_upper")
  return(interval_final)
}
