#' Convert a Standard Score to All Possible Standard Score Types with Confidence Intervals Calculated
#'
#' Converts a given score from one standard type to all supported standard score types,
#' and calculates the confidence interval for each converted score.
#'
#' @param score A numeric value representing the score to convert.
#' @param score_type A character string specifying the type of the input score. Must be one of
#'        "t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", or "custom".
#' @param m Optional numeric value specifying the mean for a custom distribution.
#'        Required if `score_type` is "custom" or if "custom" is included in the output.
#' @param sd Optional numeric value specifying the standard deviation for a custom distribution.
#'        Required if `score_type` is "custom" or if "custom" is included in the output.
#' @param rel A numeric value between 0 and 1 representing the reliability of the measurement. Default is 0.85.
#' @param rtm A logical value indicating whether to adjust for regression towards the mean (TRUE) or not (FALSE). Default is TRUE.
#' @param ci A numeric value representing the confidence interval percentage. Default is 95.
#' @return A tibble with columns for each standard score type, their converted values, and their confidence intervals.
#' @export
#' @examples
#' convert_standard_score_all_ci(65, score_type = "t_score")
#' convert_standard_score_all_ci(120, score_type = "iq", rel = 0.9, ci = 90)
#' convert_standard_score_all_ci(75, score_type = "custom", m = 80, sd = 5)
convert_standard_score_all_ci <- function(score, score_type, m = NULL, sd = NULL, rel = 0.85, rtm = TRUE, ci = 95) {
  # Validate score
  if (!is.numeric(score)) stop("`score` must be numeric.")
  # Validate custom parameters if needed
  if (score_type == "custom" && (is.null(m) || is.null(sd) || sd <= 0)) {
    stop("`m` and `sd` must be provided and valid (sd > 0) when `score_type` is 'custom'.")
  }

  # Fetch shared data
  valid_types <- get0("score_valid_types", envir = asNamespace("PsychometricCurse"))

  # Convert all scores (returns a tibble)
  converted <- convert_standard_score_all(score, score_type, m, sd)

  # Initialize a list to store results
  results_list <- lapply(valid_types, function(current_type) {
    # Extract the value for the current score_type from the tibble
    converted_score <- converted$value[converted$score_type == current_type]

    # Handle NA or missing values
    if (is.na(converted_score) || length(converted_score) == 0) {
      return(data.frame(
        score_type = current_type,
        value = NA,
        ci_lower = NA,
        ci_upper = NA,
        stringsAsFactors = FALSE
      ))
    }

    # Calculate CI for each score type
    if (current_type == "custom") {
      ci_result <- calc_ci(converted_score, m = m, sd = sd, score_type = current_type, rel = rel, rtm = rtm, ci = ci)
    } else {
      ci_result <- calc_ci(converted_score, score_type = current_type, rel = rel, rtm = rtm, ci = ci)
    }

    data.frame(
      score_type = current_type,
      value = converted_score,
      ci_lower = ci_result[1],
      ci_upper = ci_result[2],
      stringsAsFactors = FALSE
    )
  })

  # Combine all results into a single tibble
  result_tibble <- do.call(rbind, results_list)
  rownames(result_tibble) <- NULL

  # Return the result as a tibble
  tibble::as_tibble(result_tibble)
}
