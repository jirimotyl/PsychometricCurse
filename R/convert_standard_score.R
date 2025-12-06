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
#'
#'
convert_standard_score <- function(score, from, to, m = NULL, sd = NULL) {
  valid_types <- c("t_score", "scaled", "iq", "sten", "stanine", "z_score", "percentile", "custom")
  from <- match.arg(from, choices = valid_types)
  to <- match.arg(to, choices = valid_types)

  # Define the parameters for each score type
  score_params <- list(
    t_score = c(mean = 50, sd = 10),
    scaled = c(mean = 10, sd = 3),
    iq = c(mean = 100, sd = 15),
    sten = c(mean = 5.5, sd = 2),
    stanine = c(mean = 5, sd = 2),
    z_score = c(mean = 0, sd = 1),
    percentile = NULL,  # Special case for percentiles
    custom = c(mean = m, sd = sd)
  )

  # Check if m and sd are provided and valid when using custom
  if (from == "custom" && (is.null(m) || is.null(sd) || sd <= 0)) {
    stop("m and positive sd must be provided when `from` is 'custom'.")
  }
  if (to == "custom" && (is.null(m) || is.null(sd) || sd <= 0)) {
    stop("m and positive sd must be provided when `to` is 'custom'.")
  }

  # Vectorized lookup for sten
  z_to_sten <- function(z) {
    sten <- rep(NA_real_, length(z))
    sten[z < -2.0] <- 1
    sten[z >= -2.0 & z < -1.25] <- 2
    sten[z >= -1.25 & z < -0.75] <- 3
    sten[z >= -0.75 & z < -0.25] <- 4
    sten[z >= -0.25 & z < 0.25] <- 5
    sten[z >= 0.25 & z < 0.75] <- 6
    sten[z >= 0.75 & z < 1.25] <- 7
    sten[z >= 1.25 & z < 2.0] <- 8
    sten[z >= 2.0 & z < 2.5] <- 9
    sten[z >= 2.5] <- 10
    return(sten)
  }

  # Vectorized lookup for stanine
  z_to_stanine <- function(z) {
    stanine <- rep(NA_real_, length(z))
    stanine[z < -1.75] <- 1
    stanine[z >= -1.75 & z < -1.25] <- 2
    stanine[z >= -1.25 & z < -0.75] <- 3
    stanine[z >= -0.75 & z < -0.25] <- 4
    stanine[z >= -0.25 & z < 0.25] <- 5
    stanine[z >= 0.25 & z < 0.75] <- 6
    stanine[z >= 0.75 & z < 1.25] <- 7
    stanine[z >= 1.25 & z < 1.75] <- 8
    stanine[z >= 1.75] <- 9
    return(stanine)
  }

  # Convert input to z-scores
  if (from == "percentile") {
    if (any(score < 0 | score > 100, na.rm = TRUE)) stop("Percentile must be between 0 and 100.")
    z_score <- qnorm(score / 100)
  } else {
    params <- score_params[[from]]
    z_score <- (score - params["mean"]) / params["sd"]
  }

  # Convert z-scores to target scale
  if (to == "percentile") {
    converted_s <- pnorm(z_score) * 100
  } else if (to == "sten") {
    converted_s <- z_to_sten(z_score)
    if (any(abs(z_score) > 4, na.rm = TRUE)) {
      warning("Extreme z-score(s) detected; sten conversion may not be meaningful.")
    }
  } else if (to == "stanine") {
    converted_s <- z_to_stanine(z_score)
    if (any(abs(z_score) > 4, na.rm = TRUE)) {
      warning("Extreme z-score(s) detected; stanine conversion may not be meaningful.")
    }
  } else {
    params <- score_params[[to]]
    converted_s <- params["mean"] + params["sd"] * z_score
  }

  # Return scalar for scalar input
  if (length(score) == 1) {
    return(converted_s[1])
  } else {
    return(converted_s)
  }
}
