#' Package Data Objects
#'
#' This file contains shared data objects used across the package.
#' @noRd

#' Standard Score Parameters
#'
#' A named list of parameters (mean and sd) for common standard score types.
score_params <- list(
  t_score   = c(mean = 50,  sd = 10),
  scaled    = c(mean = 10,  sd = 3),
  iq        = c(mean = 100, sd = 15),
  sten      = c(mean = 5.5, sd = 2),
  stanine   = c(mean = 5,   sd = 2),
  z_score   = c(mean = 0,   sd = 1),
  percentile= NULL,  # Special case
  custom    = NULL   # Placeholder for custom m/sd
)

# Add other shared data objects below as needed, e.g.:
# 
# #' Lookup table for X
# #' @noRd
# x_lookup <- list(...)

NULL