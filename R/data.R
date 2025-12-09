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

#' Calculate Standard Error of Measurement (SEM)
#'
#' @param sd Standard deviation of the normative mean score.
#' @param rel Reliability of the measurement.
#' @return Standard Error of Measurement (SEM).
calculate_sem <- function(sd, rel) {
  sd * sqrt(1 - rel)
}

#' Calculate Confidence Interval for SEM
#'
#' @param sem Standard Error of Measurement.
#' @param ci Confidence interval percentage.
#' @return Confidence interval for SEM.
calculate_sem_ci <- function(sem, ci) {
  abs(qnorm((100 - ci) / 200)) * sem
}

#' Calculate Estimated True Score
#'
#' @param score Observed score.
#' @param m Normative mean score.
#' @param rel Reliability of the measurement.
#' @return True score.
calculate_true_score <- function(score, m, rel) {
  rel * score + (1 - rel) * m
}

#' Calculate Standard Error of Estimate (SEE)
#'
#' @param sd Standard deviation of the normative mean score.
#' @param rel Reliability of the measurement.
#' @return Standard Error of Estimate (SEE).
calculate_see <- function(sd, rel) {
  sd * sqrt(rel * (1 - rel))
}

#' Calculate Confidence Interval for SEE
#'
#' @param see Standard Error of Estimate.
#' @param ci Confidence interval percentage.
#' @return Confidence interval for SEE.
calculate_see_ci <- function(see, ci) {
  abs(qnorm((100 - ci) / 200)) * see
}
