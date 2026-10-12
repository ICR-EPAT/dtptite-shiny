# From UI settings to design inputs, with messages a user can act on.

#' Parse a user-entered skeleton
#'
#' @param text Prior DLT probabilities separated by commas or spaces.
#' @param num_doses Number of dose levels.
#' @return A list with `skeleton` (numeric, or `NULL`) and `problem` (a
#'   message, or `NULL`).
parse_skeleton <- function(text, num_doses) {
  parts <- strsplit(trimws(text %||% ""), "[,;[:space:]]+")[[1]]
  parts <- parts[nzchar(parts)]
  values <- suppressWarnings(as.numeric(parts))
  problem <- if (length(parts) == 0) {
    "Enter one prior DLT probability per dose."
  } else if (anyNA(values)) {
    sprintf("Not a number: %s.", paste(parts[is.na(values)], collapse = ", "))
  } else if (length(values) != num_doses) {
    sprintf("Enter %d probabilities, one per dose (got %d).", num_doses,
            length(values))
  } else if (any(values <= 0 | values >= 1)) {
    "Probabilities must lie strictly between 0 and 1."
  } else if (is.unsorted(values, strictly = TRUE)) {
    "Probabilities must increase with dose."
  }
  list(skeleton = if (is.null(problem)) values, problem = problem)
}

#' Skeleton from the prior settings
#'
#' @param prior_type `"calibrate"` or `"enter"`.
#' @param target,halfwidth,prior_mtd,num_doses See [calibrate_skeleton()].
#' @param text User-entered skeleton, see [parse_skeleton()].
#' @return As [parse_skeleton()].
skeleton_from_settings <- function(prior_type, target, halfwidth, prior_mtd,
                                   num_doses, text) {
  if (identical(prior_type, "enter")) {
    return(parse_skeleton(text, num_doses))
  }
  tryCatch(
    list(skeleton = calibrate_skeleton(target, halfwidth, prior_mtd,
                                       num_doses),
         problem = NULL),
    error = function(e) list(skeleton = NULL, problem = conditionMessage(e))
  )
}

#' Beta prior for the safety stopping rule
#'
#' @param use_default Use the weakly informative Be(0.1, 0.9).
#' @param mean,ess Prior mean DLT rate at the lowest dose and effective
#'   sample size, used otherwise.
#' @return A list with `a`, `b` and `problem` (a message, or `NULL`).
stopping_prior <- function(use_default, mean, ess) {
  if (isTRUE(use_default)) {
    return(list(a = 0.1, b = 0.9, problem = NULL))
  }
  if (!is.numeric(mean) || is.na(mean) || mean <= 0 || mean >= 1) {
    return(list(a = NULL, b = NULL,
                problem = "Prior mean must lie strictly between 0 and 1."))
  }
  if (!is.numeric(ess) || is.na(ess) || ess <= 0) {
    return(list(a = NULL, b = NULL,
                problem = "Effective sample size must be positive."))
  }
  c(beta_prior_from_mean(mean, ess), list(problem = NULL))
}

#' Check the design settings that are typed in
#'
#' @param obswin DLT observation window, in days.
#' @param variance Prior variance of the model parameter.
#' @param day_of_assessment Study day of the analysis.
#' @param stop_n_min Patients needed at the lowest dose before stopping.
#' @return A character vector of problems, empty when all are valid.
validate_settings <- function(obswin, variance, day_of_assessment,
                              stop_n_min) {
  whole <- function(x, min) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && x >= min && x == round(x)
  }
  c(
    if (!whole(obswin, 1)) "DLT observation window must be a whole number of days, 1 or more.",
    if (!is.numeric(variance) || is.na(variance) || variance <= 0) "Prior variance must be positive.",
    if (!whole(day_of_assessment, 1)) "Day of assessment must be a whole number, 1 or more.",
    if (!whole(stop_n_min, 0)) "Minimum patients for stopping must be a whole number, 0 or more."
  )
}
