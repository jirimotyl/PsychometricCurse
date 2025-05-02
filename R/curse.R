# curse
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

curse <- function(total_score, m = NULL, sd = NULL, rel = 0.85, rtm = TRUE, ci = 95,
                  normative_dataset = NULL, age = NULL, dob = NULL, doa = NULL,
                  sex = NULL, edu_years = NULL, edu_level = NULL, race = NULL,
                  laterality = NULL, lang = "en", data) {

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
    score_type = c("Total Score", "Z-Score", "T-Score", "Scaled Score", "Percentile"),
    score = c(total_score, z_score, t_score, scaled_score, percentile),
    ci_lower = c(total_score_ci[[1]], z_score_ci[[1]], t_score_ci[[1]], scaled_score_ci[[1]], percentile_ci[[1]]),
    ci_upper = c(total_score_ci[[2]], z_score_ci[[2]], t_score_ci[[2]], scaled_score_ci[[2]], percentile_ci[[2]]),
    rel = rel,
    rtm = rtm,
    ci = ci
  )

  return(results_tibble)
}

