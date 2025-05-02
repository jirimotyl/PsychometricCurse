# curse
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

curse <- function(total_score, m = NULL, sd = NULL, rel = 0.85, rtm = TRUE, ci = 95,
                  normative_dataset = NULL, age = NULL, dob = NULL, doa = NULL,
                  sex = NULL, edu_years = NULL, edu_level = NULL, race = NULL,
                  laterality = NULL, lang = "en", data) {

  {

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

  # Calculate confidence intervals
  total_score_ci <- ci_calc(total_score, m = m, sd = sd)
  z_score_ci <- ci_calc(z_score, m = 0, sd = 1)
  t_score_ci <- ci_calc(t_score, m = 50, sd = 10)
  scaled_score_ci <- ci_calc(scaled_score, m = 10, sd = 3)

  # Create a tibble with the results
  results_tibble <- tibble(
    score = c(total_score, z_score, t_score, scaled_score, percentile),
    ci_lower = c(total_score_ci[[1]], z_score_ci[[1]], t_score_ci[[1]], scaled_score_ci[[1]], NA),
    ci_upper = c(total_score_ci[[2]], z_score_ci[[2]], t_score_ci[[2]], scaled_score_ci[[2]], NA),
    rel = rel,
    rtm = rtm,
    ci = ci
  )

  return(results_tibble)
}

  # Calculate age if not provided
  if (is.null(age)) {
    if (!is.null(dob) && !is.null(doa)) {
      age <- as.numeric(time_length(difftime(ymd(doa), ymd(dob), units = "days"), "years"))
    } else {
      stop("Either age or both date of birth and date of assessment must be provided.")
    }
  }


  # Handle specific normative dataset "bvmt_tl_01"
  if (normative_dataset == "bvmt_tl_01") {
    # Load the dataset from the package's data directory
    data_path <- system.file("data", "bvmt_havlik.rda", package = "PsychometricCurse")
    load(data_path)

    bvmt_krivka <- c(data$bvmt_01, data$bvmt_02, data$bvmt_03)

    bvmt_havlik$agecheck <- as.logical(NA)
    for (i in 1:nrow(bvmt_havlik)) {
      bvmt_havlik$agecheck[[i]] <- between(age, bvmt_havlik$age_min[[i]], bvmt_havlik$age_max[[i]])
    }

    bvmt_havlik_irs <- filter(bvmt_havlik, agecheck == TRUE & test == "bvmt_total")

    z_score <- fn_2decimals((data$bvmt_total - bvmt_havlik_irs$mean) / bvmt_havlik_irs$sd)
    ci_upper <- fn_2decimals(fn_ci_upper(0, 1, rel$bvmt_tl, data$bvmt_tl_z))
    ci_lower <- fn_2decimals(fn_ci_lower(0, 1, rel$bvmt_tl, data$bvmt_tl_z))
    z_score_eval <- z_eval(z_score)

    if (any(!is.na(bvmt_krivka))) {
      data$bvmt_learning_maxscore <- max(c(bvmt_krivka[[2]], bvmt_krivka[[3]]))
      data$bvmt_learning_minscore <- min(c(bvmt_krivka[[2]], bvmt_krivka[[3]]))
      data$bvmt_l1 <- data$bvmt_learning_maxscore - bvmt_krivka[[1]]
      data$bvmt_l4 <- data$bvmt_total * (data$bvmt_learning_maxscore - bvmt_krivka[[1]])

      bvmt_havlik_l1_final <- filter(bvmt_havlik, agecheck == TRUE & test == "bvmt_l1_total")

      data$bvmt_l1_z <- fn_2decimals((data$bvmt_l1 - bvmt_havlik_l1_final$mean) / bvmt_havlik_l1_final$sd)
      data$bvmt_l1_ci_upper <- fn_2decimals(fn_ci_upper(0, 1, rel$bvmt_tl, data$bvmt_l1_z))
      data$bvmt_l1_ci_lower <- fn_2decimals(fn_ci_lower(0, 1, rel$bvmt_tl, data$bvmt_l1_z))
      data$bvmt_l1_z_eval <- fn_z_eval(data$bvmt_l1_z)

      bvmt_havlik_l4_final <- filter(bvmt_havlik, agecheck == TRUE & test == "bvmt_l4_total")

      data$bvmt_l4_z <- fn_2decimals((data$bvmt_l4 - bvmt_havlik_l4_final$mean) / bvmt_havlik_l4_final$sd)
      data$bvmt_l4_ci_upper <- fn_2decimals(fn_ci_upper(0, 1, rel$bvmt_tl, data$bvmt_l4_z))
      data$bvmt_l4_ci_lower <- fn_2decimals(fn_ci_lower(0, 1, rel$bvmt_tl, data$bvmt_l4_z))
      data$bvmt_l4_z_eval <- fn_z_eval(data$bvmt_l4_z)
    }

    return(data)
  }

  # Add more conditional blocks for other normative datasets here

  # Default return if no specific dataset logic is executed
  return(list(age = age))

}
