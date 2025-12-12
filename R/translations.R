#' Language translations used in PsychometricCurse
#'
#'

#' List of supported languages.
#' @noRd
lang_supported <- c(
  "en","cs","de"
)

#' A named list containing translations of various labels used across the package.
#' @noRd
zscore_labels <- list(
  en = c(
    "Extremely Below Average",
    "Below Average",
    "Lower Average",
    "Average",
    "Higher Average",
    "Above Average",
    "High Above Average"
  ),
  cs = c(
    "extrémní podprůměr",
    "podprůměr",
    "nižší průměr",
    "průměr",
    "vyšší průměr",
    "nadprůměr",
    "vysoký nadprůměr"
  ),
  de = c(
    "Extrem unterdurchschnittlich",
    "Unterdurchschnittlich",
    "Leicht unterdurchschnittlich",
    "Durchschnittlich",
    "Leicht überdurchschnittlich",
    "Überdurchschnittlich",
    "Hoch überdurchschnittlich"
  )
)

rci_labels <- list(
  en = c(
    "Significant improvement",
    "Change not confirmed",
    "Significant decline"
  ),
  cs = c(
    "Signifikantní zlepšení",
    "Změna nepotvrzena",
    "Signifikantní pokles"
  ),
  de = c(
    "Signifikante Verbesserung",
    "Veränderung nicht bestätigt",
    "Signifikante Verschlechterung"
  )
)
