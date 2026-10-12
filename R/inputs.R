# Trial data input: from what sites record (days) to what dtptite needs
# (TITE weights).
#
# Each patient has a dose level, an entry day and a DLT-follow-up status:
#   Ongoing    still being followed for DLT; days followed are derived from
#              the day of assessment and projected forward by DTP
#   Completed  completed the DLT observation window without a DLT
#   DLT        had a DLT (weight 1, as in dfcrm and escalation)
#   Withdrawn  left DLT follow-up early without a DLT (e.g. progression);
#              their partial weight is frozen

trial_statuses <- c("Ongoing", "Completed", "DLT", "Withdrawn")

#' Normalise trial data and derive TITE weights
#'
#' @param data A data.frame with columns `dose`, `entry_day`, `status` and
#'   `days_followed`. `days_followed` is only used for DLT and Withdrawn
#'   patients; it may be `NA` otherwise.
#' @param day_of_assessment Study day of the analysis (T_A).
#' @param obswin Length of the DLT observation window, in days.
#' @return `data` sorted by entry day with `patient`, `tox`, `monitored`,
#'   `days_followed` (derived where needed) and `weight` columns added.
normalise_trial_data <- function(data, day_of_assessment, obswin) {
  stopifnot(is.data.frame(data),
            all(c("dose", "entry_day", "status") %in% names(data)))
  if (!"days_followed" %in% names(data)) data$days_followed <- NA_real_

  data <- data[order(data$entry_day), , drop = FALSE]
  rownames(data) <- NULL
  status <- as.character(data$status)
  days <- as.numeric(data$days_followed)

  days[status == "Ongoing"] <- pmin(
    day_of_assessment - data$entry_day[status == "Ongoing"] + 1, obswin
  )
  days[status == "Completed"] <- obswin

  data.frame(
    patient = seq_len(nrow(data)),
    dose = as.integer(data$dose),
    entry_day = as.numeric(data$entry_day),
    status = factor(status, levels = trial_statuses),
    days_followed = days,
    tox = as.integer(status == "DLT"),
    monitored = status == "Ongoing",
    weight = ifelse(status == "DLT", 1, pmin(days / obswin, 1))
  )
}

#' Convert normalised trial data to escalation/dtptite outcomes
#'
#' @param data Output of [normalise_trial_data()].
#' @return A data.frame with columns `dose`, `tox`, `weight`, `cohort`, in
#'   entry order (escalation takes the last row as the current dose).
to_outcomes <- function(data) {
  data.frame(dose = data$dose, tox = data$tox, weight = data$weight,
             cohort = seq_len(nrow(data)))
}

#' Validate trial data before fitting
#'
#' @inheritParams normalise_trial_data
#' @param num_doses Number of dose levels in the design.
#' @return A character vector of problems, empty when the data are valid.
validate_trial_data <- function(data, num_doses, day_of_assessment, obswin) {
  problems <- character()
  if (nrow(data) == 0) return("Enter at least one patient.")

  row_msg <- function(rows, what) {
    sprintf("Patient(s) %s: %s", paste(rows, collapse = ", "), what)
  }
  bad <- which(is.na(data$dose) | data$dose < 1 | data$dose > num_doses |
                 data$dose != round(data$dose))
  if (length(bad)) {
    problems <- c(problems, row_msg(bad, sprintf(
      "dose level must be a whole number from 1 to %d.", num_doses)))
  }
  bad <- which(is.na(data$entry_day) | data$entry_day < 1)
  if (length(bad)) {
    problems <- c(problems, row_msg(bad, "entry day must be 1 or later."))
  }
  bad <- which(!is.na(data$entry_day) & data$entry_day > day_of_assessment)
  if (length(bad)) {
    problems <- c(problems, row_msg(bad, sprintf(
      "entry day is after the day of assessment (day %d).",
      day_of_assessment)))
  }
  bad <- which(is.na(data$status) | !data$status %in% trial_statuses)
  if (length(bad)) {
    problems <- c(problems, row_msg(bad, sprintf(
      "status must be one of %s.", paste(trial_statuses, collapse = ", "))))
  }
  needs_days <- data$status %in% c("DLT", "Withdrawn")
  days <- suppressWarnings(as.numeric(data$days_followed))
  bad <- which(needs_days & (is.na(days) | days < 1 | days > obswin))
  if (length(bad)) {
    problems <- c(problems, row_msg(bad, sprintf(
      "days followed must be from 1 to %d for DLT and Withdrawn patients.",
      obswin)))
  }
  bad <- which(needs_days & !is.na(days) & !is.na(data$entry_day) &
                 data$entry_day + days - 1 > day_of_assessment)
  if (length(bad)) {
    problems <- c(problems, row_msg(bad, sprintf(
      "follow-up runs past the day of assessment (day %d).",
      day_of_assessment)))
  }
  problems
}
