#' Package Data Objects
#'
#' This file contains shared data objects used across the package.

#' Standard Score Parameters
#'
#' A named list of parameters (mean and sd) for common standard score types.
#' @noRd
score_params <- list(
  z_score   = list(m = 0,   sd = 1),
  t_score   = list(m = 50,  sd = 10),
  scaled    = list(m = 10,  sd = 3),
  iq        = list(m = 100, sd = 15),
  sten      = list(m = 5.5, sd = 2),
  stanine   = list(m = 5,   sd = 2),
  percentile= NULL,  # Special case
  custom    = NULL   # Placeholder for custom m/sd
)

#' Valid Score Types
#'
#' A character vector of valid score types used in the package.
#' @noRd
score_valid_types <- c("z_score", "t_score", "scaled", "iq", "sten", "stanine", "percentile", "custom")
