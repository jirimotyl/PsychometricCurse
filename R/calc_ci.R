#' Calculation of Confidence Interval (CI)
#'
#' This function calculates and returns the lower and upper confidence interval around the provided total score.
#' For percentiles, it converts to z-score, calculates the CI, and converts back to percentiles.
#'
#' @param score A numeric value representing the total score for which the confidence interval is to be calculated.
#' @param m A numeric value representing the normative mean score. Required if `score_type` is not provided.
#' @param sd A numeric value representing the standard deviation of the normative mean score. Required if `score_type` is not provided.
#' @param score_type A character string specifying the type of the score (e.g., "t_score", "iq", "sten", "percentile"). If provided, `m` and `sd` are ignored.
#' @param rel A numeric value between 0 and 1 representing the reliability of the measurement. Default is 0.85.
#' @param rtm A logical value indicating whether to adjust for regression towards the mean (TRUE) or not (FALSE). Default is TRUE.
#' @param ci A numeric value representing the confidence interval percentage. Default is 95.
#' @return A numeric vector containing the lower and upper confidence interval values for the provided total score.
#' @export
#' @examples
#' calc_ci(score = 80, m = 70, sd = 10)
#' calc_ci(score = 80, score_type = "t_score", rel = 0.9, rtm = FALSE, ci = 90)
#' calc_ci(score = -1.25, score_type = "z_score")
#' calc_ci(score = 75, score_type = "percentile")
calc_ci <- function(score, m = NULL, sd = NULL, score_type = NULL, rel = 0.85, rtm = TRUE, ci = 95) {
  # Validate inputs
  if (!is.numeric(score)) stop("score must be numeric.")
  if (!is.numeric(rel) || rel < 0 || rel > 1) stop("rel must be numeric and between 0 and 1.")
  if (!is.logical(rtm)) stop("rtm must be logical.")
  if (!is.numeric(ci) || ci < 0 || ci > 100) stop("ci must be numeric and between 0 and 100.")

  # Handle percentile
  if (score_type == "percentile") {
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

  # Use score_params if score_type is provided
  if (!is.null(score_type)) {
    params <- get0("score_params", envir = asNamespace("PsychometricCurse"))
    if (is.null(params[[score_type]])) stop("Invalid score_type. See documentation for valid types.")
    if (is.null(params[[score_type]]$mean) || is.null(params[[score_type]]$sd)) {
      stop("score_type must have defined mean and sd in score_params.")
    }
    m <- params[[score_type]]["mean"]
    sd <- params[[score_type]]["sd"]
  } else {
    if (is.null(m) || is.null(sd)) stop("m and sd must be provided if score_type is not specified.")
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

  return(interval_final)
}
