#' Convert a Standard Score to All Possible Standard Score Types with Confidence Intervals Calculated
#'
#' Converts a given score from one standard type to all supported standard score types,
#' and calculates the confidence interval for each converted score.
#'
#' @param score A numeric value representing the score to convert.
#' @param from A character string specifying the type of the input score. Must be one of
#'        "t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", or "custom".
#' @param m Optional numeric value specifying the mean for a custom distribution.
#'        Required if `from` or any `to` is "custom".
#' @param sd Optional numeric value specifying the standard deviation for a custom distribution.
#'        Required if `from` or any `to` is "custom".
#' @param rel A numeric value between 0 and 1 representing the reliability of the measurement. Default is 0.85.
#' @param rtm A logical value indicating whether to adjust for regression towards the mean (TRUE) or not (FALSE). Default is TRUE.
#' @param ci A numeric value representing the confidence interval percentage. Default is 95.
#' @return A tibble with columns for each standard score type, their converted values, and their confidence intervals.
#' @export
#' @examples
#' convert_standard_score_all_ci(65, from = "t_score")
#' convert_standard_score_all_ci(120, from = "iq", rel = 0.9, ci = 90)
#' convert_standard_score_all_ci(75, from = "custom", m = 80, sd = 5)
#'
ciconvert_standard_score_all_ci <- function(score, from, m = NULL, sd = NULL, rel = 0.85, rtm = TRUE, ci = 95) {
  valid_types <- c("t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", "custom")

  # Validate score
  if (!is.numeric(score)) stop("`score` must be numeric.")

  # Validate custom parameters if needed
  if ((from == "custom") && (is.null(m) || is.null(sd) || sd <= 0)) {
    stop("`m` and `sd` must be provided and valid (sd > 0) when using 'custom'.")
  }

  # Use the shared score_params
  params <- get0("score_params", envir = asNamespace("PsychometricCurse"))
  params$custom <- c(mean = m, sd = sd)

  # Convert all scores (returns a tibble)
  converted <- convert_standard_score_all(score, from, m, sd)

  # Initialize a list to store results
  results_list <- list()

  # Calculate CIs for each score type and store in the list
  for (to in valid_types) {
    if (to == "percentile" || (to == "custom" && is.null(m) && is.null(sd))) {
      results_list[[to]] <- data.frame(
        score_type = to,
        value = NA,
        ci_lower = NA,
        ci_upper = NA,
        stringsAsFactors = FALSE
      )
      next
    }

    # Extract the value for the current score_type from the tibble
    converted_score <- converted$value[converted$score_type == to]

    if (length(converted_score) == 0 || is.na(converted_score)) {
      results_list[[to]] <- data.frame(
        score_type = to,
        value = NA,
        ci_lower = NA,
        ci_upper = NA,
        stringsAsFactors = FALSE
      )
      next
    }

    ci_result <- calc_ci(converted_score, score_type = to, rel = rel, rtm = rtm, ci = ci)

    results_list[[to]] <- data.frame(
      score_type = to,
      value = converted_score,
      ci_lower = ci_result[1],
      ci_upper = ci_result[2],
      stringsAsFactors = FALSE
    )
  }

  # Combine all results into a single tibble
  result_tibble <- do.call(rbind, results_list)
  rownames(result_tibble) <- NULL

  # Return the result as a tibble
  tibble::as_tibble(result_tibble)
}

