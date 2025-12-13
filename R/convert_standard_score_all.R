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
  # Validate score
  if (!is.numeric(score)) stop("`score` must be numeric.")
  # Validate custom parameters if needed
  if (score_type == "custom" && (is.null(m) || is.null(sd) || sd <= 0)) {
    stop("`m` and `sd` must be provided and valid (sd > 0) when `score_type` is 'custom'.")
  }
  # Use the shared score_params
  params <- get0("score_params", envir = asNamespace("PsychometricCurse"))
  params$custom <- c(m = m, sd = sd)
  # Helper function to convert between two types
  convert <- function(s, from, to, m, sd) {
    tryCatch({
      if (from == "percentile") {
        if (s < 0 || s > 100) stop("Percentile must be between 0 and 100.")
        z_score <- qnorm(s / 100)
      } else if (from %in% c("sten", "stanine")) {
        z_score <- (s - params[[from]]["m"]) / params[[from]]["sd"]
      } else {
        z_score <- (s - params[[from]]["m"]) / params[[from]]["sd"]
      }
      if (to == "percentile") {
        converted_s <- pmin(pmax(pnorm(z_score) * 100, 0), 100)
      } else {
        converted_s <- params[[to]]["m"] + params[[to]]["sd"] * z_score
        if (to == "sten") {
          converted_s <- round(converted_s)
          converted_s <- pmin(pmax(converted_s, 1), 10)
        } else if (to == "stanine") {
          converted_s <- round(converted_s)
          converted_s <- pmin(pmax(converted_s, 1), 9)
        }
      }
      return(converted_s)
    }, error = function(e) {
      return(NA)
    })
  }
  # Initialize a list to store results
  results_list <- lapply(score_valid_types, function(to) {
    if (to == "custom" && (is.null(m) || is.null(sd))) {
      return(data.frame(score_type = to, value = NA, stringsAsFactors = FALSE))
    }
    if (to == score_type) {
      return(data.frame(score_type = to, value = score, stringsAsFactors = FALSE))
    }
    converted_score <- convert(score, score_type, to, m, sd)
    data.frame(score_type = to, value = converted_score, stringsAsFactors = FALSE)
  })
  # Combine all results into a single tibble
  result_tibble <- do.call(rbind, results_list)
  rownames(result_tibble) <- NULL
  # Return the result as a tibble
  tibble::as_tibble(result_tibble)
}
