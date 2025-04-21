# curse
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

curse <- function(total_score, sex, age = NULL, edu_years,
                  edu_level, normative_dataset, laterality, dob = NULL, doa = NULL,
                  lang = "en") {

  # Calculate age if not provided
  if (is.null(age)) {
    if (!is.null(dob) & !is.null(doa)) {
      age <- as.numeric(time_length(difftime(ymd(doa), ymd(dob), units = "days"), "years"))
    } else {
      stop("Either age or both date of birth and date of assessment must be provided.")
    }
  }

  }
