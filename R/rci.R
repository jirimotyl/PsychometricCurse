#' Calculation of Reliable Change Index (RCI)
#'
#' This function calculates and returns the reliable change index, indicating whether the change in scores is statistically significant. It handles both single and two-test scenarios based on the provided parameters and supports different standard score types.
#'
#' @param score A numeric value representing the current total score.
#' @param score_old A numeric value representing the previous total score.
#' @param score_type An optional character string specifying the type of standard score. Supported types are "z_score", "t_score", "sten", "stanine", "iq", and "scaled". If provided, M and SD are set automatically. Default is NULL, which requires M and SD to be specified.
#' @param m An optional numeric value representing the normative mean score. Required only if score_type is NULL.
#' @param sd An optional numeric value representing the standard deviation of the normative mean score. Required only if score_type is NULL.
#' @param rel A numeric value between 0 and 1 representing the reliability of the measurement. Default is 0.85.
#' @param rtm A logical value indicating whether to adjust for regression towards the mean (TRUE) or not (FALSE). Default is TRUE.
#' @param ci A numeric value representing the confidence interval percentage. Default is 95.
#' @param rci_method An integer value indicating the method to calculate the standard error. Default is 1. Use 1 for Lord & Novick (1968) approach, 2 for Jacobson & Truax (1991) approach.
#' @param lang A character string specifying the language for the description. Currently supported languages are "en" (English), "cs" (Czech), and "de" (German). Default is "en".
#' @param time A logical value indicating whether a lower score represents a better result (TRUE for time-based tests). Default is FALSE.
#' @return A list containing the evaluation result (`evaluation_result`), the tested change (`tested_change`), the standard error (of prediction or difference) with confidence interval (`standard_error_with_CI`), the regression towards the mean adjustment (`regression_towards_mean`), the confidence interval percentage (`confidence_interval`), and the RCI method used (`rci_method`). Returns NA if `score` or `score_old` is NA.
#' @export
#' @examples
#' # Example with custom M and SD
#' rci(score = 85, score_old = 80, m = 70, sd = 10, )
#'
#' # Example with score_type, no need to specify M and SD
#' rci(score = 55, score_old = 50, score_type = "t_score")
#'
#' # Example with score_type and additional parameters
#' rci(score = 12, score_old = 10, score_type = "scaled", rtm = FALSE, ci = 90, rci_method = 2, lang = "cs")
#'
#' # Example with time parameter for time-based test
#' rci(score = 85, score_old = 90, score_type = "t_score", time = TRUE)

rci <- function(score, score_old, score_type = NULL, m = NULL, sd = NULL, rel = 0.85, rtm = TRUE, ci = 95, rci_method = 1, lang = "en", time = FALSE) {
  # Early parameter validation
  if (is.na(score) || is.na(score_old)) {
    return(NA)
  }

  if (!is.numeric(score) || !is.numeric(score_old) || !is.logical(rtm) || !is.numeric(ci)) {
    stop("score, score_old, and ci must be numeric. rtm must be logical.")
  }

  if (ci < 0 || ci > 100) {
    stop("Confidence interval must be between 0 and 100.")
  }

  # Load external data
  params <- get0("score_params", envir = asNamespace("PsychometricCurse"))
  lang_supported <- get0("lang_supported", envir = asNamespace("PsychometricCurse"))
  rci_labels <- get0("rci_labels", envir = asNamespace("PsychometricCurse"))

  if (!(lang %in% lang_supported)) {
    stop("Unsupported language. Supported languages are: ", toString(lang_supported))
  }

  # Handle percentile
  if (!is.null(score_type) && score_type == "percentile") {
    if (score < 0 || score > 100) stop("Percentile must be between 0 and 100.")
    z_score <- convert_standard_score(score = score, score_type = score_type, target_type = "z_score")
    m <- params[["z_score"]]$mean
    sd <- params[["z_score"]]$sd
    score <- z_score
  } else {
    # Adjust m and sd based on score_type
    if (!is.null(score_type)) {
      if (score_type == "custom") {
        if (is.null(m) || is.null(sd) || !is.numeric(m) || !is.numeric(sd) || sd <= 0) {
          stop("For 'custom' score_type, m and sd must be provided and valid (sd > 0).")
        }
      } else if (score_type %in% names(params)) {
        if (is.null(params[[score_type]])) {
          stop(paste("score_type", score_type, "must have defined mean and sd in score_params."))
        }
        m <- params[[score_type]]$mean
        sd <- params[[score_type]]$sd
      } else {
        stop("Unsupported score_type. Use one of: ", paste(names(params), collapse = ", "))
      }
    } else {
      if (is.null(m) || is.null(sd) || !is.numeric(m) || !is.numeric(sd) || sd <= 0) {
        stop("m and sd must be specified and valid (sd > 0) if score_type is NULL.")
      }
    }
  }

  if (!is.numeric(rel)) {
    stop("Reliability (rel) must be numeric.")
  }
  if (rel < 0 || rel > 1) {
    stop("Reliability (rel) must be between 0 and 1.")
  }

  # Calculate true score and tested change
  if (rtm) {
    tested_change <- score - calc_true_score(score = score_old, m = m, rel = rel)
  } else {
    tested_change <- score - score_old
  }

  # Calculate standard error
  if (rci_method == 1) {
    rci_size <- calc_sep(sd = sd, rel = rel, ci = ci)$sep_ci    # Standard error of prediction
  } else if (rci_method == 2) {
    rci_size <- calc_sed(sd = sd, rel = rel, ci = ci)$sed_ci    # Standard error of difference
  } else {
    stop("Unsupported rci_method. Use 1 for Lord & Novick (1968) or 2 for Jacobson & Truax (1991).")
  }

  # Get the labels for the specified language
  labels <- rci_labels[[lang]]

  # Determine evaluation result
  if (time) {
    if (tested_change < -rci_size) {
      eval <- labels[3]
    } else if (tested_change <= rci_size & tested_change >= -rci_size) {
      eval <- labels[2]
    } else {
      eval <- labels[1]
    }
  } else {
    if (tested_change > rci_size) {
      eval <- labels[3]
    } else if (tested_change <= rci_size & tested_change >= -rci_size) {
      eval <- labels[2]
    } else {
      eval <- labels[1]
    }
  }

  # Return results
  list(
    evaluation_result = eval,
    tested_change = round(tested_change, 2),
    standard_error_with_CI = round(rci_size, 2),
    regression_towards_mean = rtm,
    confidence_interval = ci,
    rci_method = rci_method
  )
}
