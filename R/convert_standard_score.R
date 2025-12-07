#' Convert Between Different Standard Score Types
#'
#' Converts a score from one standard type to another. Supported score types include t_score, scaled, iq, sten, stanine, z_score, percentile, and custom distributions defined by mean (m) and standard deviation (sd).
#'
#' @param score A numeric value representing the score to convert.
#' @param from A character string specifying the type of the input score. Must be one of "t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", or "custom".
#' @param to A character string specifying the type of the output score. Must be one of "t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", or "custom".
#' @param m Optional numeric value specifying the mean for a custom distribution. Required if `from` or `to` is "custom".
#' @param sd Optional numeric value specifying the standard deviation for a custom distribution. Required if `from` or `to` is "custom".
#' @return A numeric value representing the converted score.
#' @export
#' @examples
#' convert_standard_score(65, from = "t_score", to = "iq")
#' convert_standard_score(120, from = "iq", to = "stanine")
#' convert_standard_score(75, from = "custom", to = "z_score", m = 80, sd = 5)

convert_standard_score <- function(score, from, to, m = NULL, sd = NULL) {
  valid_types <- c("t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", "custom")
  from <- match.arg(from, choices = valid_types)
  to <- match.arg(to, choices = valid_types)

  # Validate score
  if (!is.numeric(score)) stop("`score` must be numeric.")

  # Validate custom parameters
  if (from == "custom" || to == "custom") {
    if (is.null(m) || is.null(sd) || !is.numeric(m) || !is.numeric(sd) || sd <= 0) {
      stop("`m` and `sd` must be provided and valid (sd > 0) when using 'custom'.")
    }
  }

  # Define the parameters for each score type
  score_params <- list(
    t_score = c(mean = 50, sd = 10),
    scaled = c(mean = 10, sd = 3),
    iq = c(mean = 100, sd = 15),
    sten = c(mean = 5.5, sd = 2),
    stanine = c(mean = 5, sd = 2),
    z_score = c(mean = 0, sd = 1),
    percentile = NULL,
    custom = c(mean = m, sd = sd)
  )

  # Vectorized conversion
  converted_score <- sapply(score, function(s) {
    # Convert the input score to a z-score
    if (from == "percentile") {
      if (s < 0 || s > 100) stop("Percentile must be between 0 and 100.")
      z_score <- qnorm(s / 100)
    } else {
      params <- score_params[[from]]
      z_score <- (s - params["mean"]) / params["sd"]
    }

    # Convert the z-score to the desired output score
    if (to == "percentile") {
      converted_s <- pmin(pmax(pnorm(z_score) * 100, 0), 100)  # Clamp to [0, 100]
    } else {
      params <- score_params[[to]]
      converted_s <- params["mean"] + params["sd"] * z_score
      # Apply rounding and clamping for sten and stanine scores
      if (to == "sten") {
        converted_s <- pmin(pmax(round(converted_s), 1), 10)
      } else if (to == "stanine") {
        converted_s <- pmin(pmax(round(converted_s), 1), 9)
      }
    }
    return(converted_s)
  })

  return(converted_score)
}
