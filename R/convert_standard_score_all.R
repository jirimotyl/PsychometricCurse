#' Convert a Standard Score to All Possible Standard Score Types
#'
#' Converts a given score from one standard type to all supported standard score types,
#' returning a tibble with columns for score_type and value.
#'
#' @param score A numeric value representing the score to convert.
#' @param from A character string specifying the type of the input score. Must be one of
#'        "t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", or "custom".
#' @param m Optional numeric value specifying the mean for a custom distribution.
#'        Required if `from` or any `to` is "custom".
#' @param sd Optional numeric value specifying the standard deviation for a custom distribution.
#'        Required if `from` or any `to` is "custom".
#' @return A tibble with columns `score_type` and `value`, including the original score.
#' @export
#' @examples
#' convert_standard_score_all(65, from = "t_score")
#' convert_standard_score_all(120, from = "iq")
#' convert_standard_score_all(75, from = "custom", m = 80, sd = 5)
convert_standard_score_all <- function(score, from, m = NULL, sd = NULL) {
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

  # Helper function to convert between two types
  convert <- function(s, from, to, m, sd) {
    if (from == "percentile") {
      if (s < 0 || s > 100) stop("Percentile must be between 0 and 100.")
      z_score <- qnorm(s / 100)
    } else if (from %in% c("sten", "stanine")) {
      z_score <- (s - params[[from]]["mean"]) / params[[from]]["sd"]
    } else {
      z_score <- (s - params[[from]]["mean"]) / params[[from]]["sd"]
    }

    if (to == "percentile") {
      converted_s <- pmin(pmax(pnorm(z_score) * 100, 0), 100)
    } else {
      converted_s <- params[[to]]["mean"] + params[[to]]["sd"] * z_score
      if (to == "sten") {
        converted_s <- round(converted_s)
        converted_s <- pmin(pmax(converted_s, 1), 10)
      } else if (to == "stanine") {
        converted_s <- round(converted_s)
        converted_s <- pmin(pmax(converted_s, 1), 9)
      }
    }
    return(converted_s)
  }

  # Initialize a list to store results
  results_list <- list()

  # Fill in the original score
  results_list[[from]] <- data.frame(
    score_type = from,
    value = score,
    stringsAsFactors = FALSE
  )

  # Convert to all other types
  for (to in valid_types) {
    if (to == from) next  # Skip the original type
    if (to == "custom" && is.null(m) && is.null(sd)) {
      next  # Skip custom if m and sd are not provided
    }
    converted_score <- convert(score, from, to, m, sd)
    results_list[[to]] <- data.frame(
      score_type = to,
      value = converted_score,
      stringsAsFactors = FALSE
    )
  }

  # Combine all results into a single tibble
  result_tibble <- do.call(rbind, results_list)
  rownames(result_tibble) <- NULL

  # Return the result as a tibble
  tibble::as_tibble(result_tibble)
}
