#' Convert a Standard Score to All Possible Standard Score Types
#'
#' Converts a given score from one standard type to all supported standard score types,
#' returning a tibble with columns for score_type and value.
#'
#' @param score A numeric value representing the score to convert.
#' @param score_type A character string specifying the type of the input score. Must be one of
#'        "t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", or "custom".
#' @param m Optional numeric value specifying the mean for a custom distribution.
#'        Required if `score_type` is "custom" or if "custom" should be included in the output.
#' @param sd Optional numeric value specifying the standard deviation for a custom distribution.
#'        Required if `score_type` is "custom" or if "custom" should be included in the output.
#' @return A tibble with columns `score_type` and `value`, including the original score.
#' @export
#' @examples
#' convert_standard_score_all(65, score_type = "t_score")
#' convert_standard_score_all(120, score_type = "iq")
#' convert_standard_score_all(75, score_type = "t_score", m = 80, sd = 5)
convert_standard_score_all <- function(score, score_type, m = NULL, sd = NULL) {
  # Fetch shared data
  valid_types <- get0("score_valid_types", envir = asNamespace("PsychometricCurse"))

  # Validate score
  if (!is.numeric(score)) stop("`score` must be numeric.")

  # Initialize a list to store results
  results_list <- lapply(valid_types, function(to) {
    if (to == score_type) {
      return(data.frame(score_type = to, value = score, stringsAsFactors = FALSE))
    }
    if (to == "custom" && is.null(m) && is.null(sd)) {
      return(data.frame(score_type = to, value = NA, stringsAsFactors = FALSE))
    }
    # Use convert_standard_score for each conversion
    converted_score <- tryCatch(
      convert_standard_score(score, score_type, to, m, sd),
      error = function(e) NA
    )
    data.frame(score_type = to, value = converted_score, stringsAsFactors = FALSE)
  })

  # Combine all results into a single tibble
  result_tibble <- do.call(rbind, results_list)
  rownames(result_tibble) <- NULL

  # Return the result as a tibble
  tibble::as_tibble(result_tibble)
}

