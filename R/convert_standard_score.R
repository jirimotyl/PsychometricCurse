#' Convert Between Different Standard Score Types
#'
#' Converts a score from one standard type to another. Supported score types include t_score, scaled, iq, sten, stanine, z_score, percentile, and custom distributions defined by mean (m) and standard deviation (sd).
#'
#' @param score A numeric value representing the score to convert.
#' @param score_type A character string specifying the type of the input score. Must be one of "t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", or "custom".
#' @param target_type A character string specifying the type of the output score. Must be one of "t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", or "custom".
#' @param m Optional numeric value specifying the mean for a custom distribution. Required if `score_type` or `target_type` is "custom".
#' @param sd Optional numeric value specifying the standard deviation for a custom distribution. Required if `score_type` or `target_type` is "custom".
#' @return A numeric value representing the converted score.
#' @export
#' @examples
#' convert_standard_score(65, score_type = "t_score", target_type = "iq")
#' convert_standard_score(120, score_type = "iq", target_type = "stanine")
#' convert_standard_score(75, score_type = "custom", target_type = "z_score", m = 80, sd = 5)
convert_standard_score <- function(score, score_type, target_type, m = NULL, sd = NULL) {
  valid_types <- c("t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", "custom")
  score_type <- match.arg(score_type, choices = valid_types)
  target_type <- match.arg(target_type, choices = valid_types)
  # Validate score
  if (!is.numeric(score)) stop("`score` must be numeric.")
  # Validate custom parameters
  if ((score_type == "custom" || target_type == "custom") && (is.null(m) || is.null(sd) || sd <= 0)) {
    stop("`m` and `sd` must be provided and valid (sd > 0) when using 'custom'.")
  }
  # Ensure m and sd are only used with custom types
  if (!is.null(m) || !is.null(sd)) {
    if (!(score_type == "custom" || target_type == "custom")) {
      stop("`m` and `sd` are only used when `score_type` or `target_type` is 'custom'.")
    }
  }
  # Use the shared score_params
  params <- get0("score_params", envir = asNamespace("PsychometricCurse"))
  params$custom <- c(mean = m, sd = sd)
  # Vectorized conversion
  converted_score <- sapply(score, function(s) {
    # Convert the input score to a z-score
    if (score_type == "percentile") {
      if (s < 0 || s > 100) stop("Percentile must be between 0 and 100.")
      z_score <- qnorm(s / 100)
    } else if (score_type %in% c("sten", "stanine")) {
      z_score <- (s - params[[score_type]]["mean"]) / params[[score_type]]["sd"]
    } else {
      z_score <- (s - params[[score_type]]["mean"]) / params[[score_type]]["sd"]
    }
    # Convert the z-score to the desired output score
    if (target_type == "percentile") {
      converted_s <- pmin(pmax(pnorm(z_score) * 100, 0), 100)
    } else {
      converted_s <- params[[target_type]]["mean"] + params[[target_type]]["sd"] * z_score
      # Apply rounding and clamping for sten and stanine scores
      if (target_type == "sten") {
        converted_s <- round(converted_s)
        converted_s <- pmin(pmax(converted_s, 1), 10)
      } else if (target_type == "stanine") {
        converted_s <- round(converted_s)
        converted_s <- pmin(pmax(converted_s, 1), 9)
      }
    }
    return(converted_s)
  })
  return(unname(converted_score))
}
