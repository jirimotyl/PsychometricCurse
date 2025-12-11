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
    if (is.null(params[[score_type]]["mean"]) || is.null(params[[score_type]]["sd"])) {
      stop("score_type must have defined mean and sd in score_params.")
    }
    m <- params[[score_type]]["mean"]
    sd <- params[[score_type]]["sd"]
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
