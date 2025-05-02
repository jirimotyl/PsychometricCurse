#' Calculate Standard Scores and Confidence Intervals
#'
#' The `curse` function calculates various standard scores and their confidence intervals
#' based on a given total score, mean, and standard deviation. It supports conversion
#' between different score types, including z-score, t-score, scaled score, and percentile.
#'
#' @param total_score A numeric value representing the total score to be converted.
#' @param m A numeric value representing the mean of the custom distribution. Required for conversion.
#' @param sd A numeric value representing the standard deviation of the custom distribution. Required for conversion.
#' @param rel A numeric value representing the reliability coefficient (default is 0.85).
#' @param rtm A logical value indicating whether to account for regression towards the mean (default is TRUE).
#' @param ci A numeric value representing the confidence interval percentage (default is 95).
#' @param normative_dataset An optional dataset for normative comparisons (default is NULL).
#' @param age An optional numeric value representing the age of the individual (default is NULL).
#' @param dob An optional date value representing the date of birth of the individual (default is NULL).
#' @param doa An optional date value representing the date of assessment (default is NULL).
#' @param sex An optional character value representing the sex of the individual (default is NULL).
#' @param edu_years An optional numeric value representing the years of education (default is NULL).
#' @param edu_level An optional character value representing the education level (default is NULL).
#' @param race An optional character value representing the race of the individual (default is NULL).
#' @param laterality An optional character value representing the laterality of the individual (default is NULL).
#' @param lang A character string specifying the language for outputs (default is "en").
#'
#' @return A tibble containing the calculated scores and their confidence intervals.
#' The tibble includes the following columns:
#'   - `score_type`: The type of score (e.g., total_score, z_score, t_score, scaled_score, percentile).
#'   - `score`: The calculated scores (total_score, z_score, t_score, scaled_score, percentile).
#'   - `ci_lower`: The lower bounds of the confidence intervals for each score.
#'   - `ci_upper`: The upper bounds of the confidence intervals for each score.
#'   - `rel`: The reliability coefficient.
#'   - `rtm`: The regression towards the mean indicator.
#'   - `ci`: The confidence interval percentage.
#'
#' @examples
#' # Example usage of the curse function
#' results <- curse(total_score = 75, m = 80, sd = 5)
#' print(results)
#'
#' @export


curse <- function(total_score, m = NULL, sd = NULL, rel = 0.85, rtm = TRUE, ci = 95,
                  normative_dataset = NULL, age = NULL, dob = NULL, doa = NULL,
                  sex = NULL, edu_years = NULL, edu_level = NULL, race = NULL,
                  laterality = NULL, lang = "en") {

  # Check if necessary parameters are provided
  if (is.null(m) || is.null(sd) || is.null(total_score)) {
    stop("m, sd, and total_score must be provided.")
  }

  # Calculate Z-Score using the convert_standard_score function
  z_score <- convert_standard_score(total_score, from = "custom", to = "z_score", m = m, sd = sd)

  # Calculate additional scores
  t_score <- convert_standard_score(z_score, from = "z_score", to = "t_score")
  scaled_score <- convert_standard_score(z_score, from = "z_score", to = "scaled")
  percentile <- convert_standard_score(z_score, from = "z_score", to = "percentile")

  # Calculate confidence intervals for z-score
  z_score_ci <- ci_calc(z_score, m = 0, sd = 1)

  # Convert z-score confidence intervals to other score types
  total_score_ci <- convert_standard_score(z_score_ci, from = "z_score", to = "custom", m = m, sd = sd)
  t_score_ci <- convert_standard_score(z_score_ci, from = "z_score", to = "t_score")
  scaled_score_ci <- convert_standard_score(z_score_ci, from = "z_score", to = "scaled")
  percentile_ci <- convert_standard_score(z_score_ci, from = "z_score", to = "percentile")

  # Create a tibble with the results
  results_tibble <- tibble(
    score_type = c("total_score", "z_score", "t_score", "scaled", "percentile"),
    score = c(total_score, z_score, t_score, scaled_score, percentile),
    ci_lower = c(total_score_ci[[1]], z_score_ci[[1]], t_score_ci[[1]], scaled_score_ci[[1]], percentile_ci[[1]]),
    ci_upper = c(total_score_ci[[2]], z_score_ci[[2]], t_score_ci[[2]], scaled_score_ci[[2]], percentile_ci[[2]]),
    rel = rel,
    rtm = rtm,
    ci = ci
  )

  return(results_tibble)
}

