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
  if (!"days_followed" %in% names(data)) {
    data$days_followed <- rep(NA_real_, nrow(data))
  }

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
  bad <- which(data$status %in% "Completed" & !is.na(data$entry_day) &
                 data$entry_day + obswin - 1 > day_of_assessment)
  if (length(bad)) {
    problems <- c(problems, row_msg(bad, sprintf(
      "cannot have completed the %d-day window by the day of assessment (day %d).",
      obswin, day_of_assessment)))
  }
  problems
}

#' Map the legacy app's five-column CSV to the status-based format
#'
#' The legacy upload was headerless: dose level, DLT (0/1), entry day,
#' follow-up days, still monitored for DLT (0/1).
#'
#' @param legacy A data.frame with the five legacy columns, in order.
#' @inheritParams normalise_trial_data
#' @return A data.frame with `dose`, `entry_day`, `status`, `days_followed`.
from_legacy_format <- function(legacy, obswin) {
  stopifnot(ncol(legacy) == 5)
  names(legacy) <- c("level", "tox", "entry", "followup", "monitored")
  status <- ifelse(
    legacy$tox == 1, "DLT",
    ifelse(legacy$monitored == 1, "Ongoing",
           ifelse(legacy$followup >= obswin, "Completed", "Withdrawn"))
  )
  data.frame(
    dose = legacy$level,
    entry_day = legacy$entry,
    status = status,
    days_followed = ifelse(status %in% c("DLT", "Withdrawn"),
                           legacy$followup, NA_real_)
  )
}

# Accepted spellings for each column of an uploaded file.
column_aliases <- list(
  dose = c("dose", "dose_level", "level"),
  entry_day = c("entry_day", "entry", "entry_date"),
  status = c("status", "follow_up_status", "dlt_status"),
  days_followed = c("days_followed", "followup", "follow_up", "days")
)

#' Read uploaded trial data
#'
#' Accepts the status-based format (a CSV or Excel file with a header row)
#' and the legacy headerless five-column CSV.
#'
#' @param path Path to the uploaded file.
#' @param name Original file name, used to pick the reader.
#' @inheritParams normalise_trial_data
#' @return A data.frame with `dose`, `entry_day`, `status`, `days_followed`.
read_trial_file <- function(path, name = path, obswin) {
  if (grepl("[.]xlsx?$", name, ignore.case = TRUE)) {
    raw <- as.data.frame(readxl::read_excel(path))
  } else {
    if (all(!nzchar(trimws(readLines(path, warn = FALSE))))) {
      stop("The file is empty.", call. = FALSE)
    }
    raw <- utils::read.csv(path, header = FALSE, strip.white = TRUE,
                           stringsAsFactors = FALSE)
    first_row_numeric <- !anyNA(suppressWarnings(as.numeric(unlist(raw[1, ]))))
    if (ncol(raw) == 5 && first_row_numeric) {
      return(from_legacy_format(raw, obswin))
    }
    raw <- utils::read.csv(path, strip.white = TRUE, stringsAsFactors = FALSE)
  }

  key <- gsub("[^a-z]+", "_", tolower(trimws(names(raw))))
  out <- lapply(column_aliases, function(aliases) {
    hit <- match(aliases, key)
    hit <- hit[!is.na(hit)]
    if (length(hit)) raw[[hit[1]]] else NULL
  })
  missing <- setdiff(c("dose", "entry_day", "status"),
                     names(Filter(Negate(is.null), out)))
  if (length(missing)) {
    stop("Missing column(s): ", paste(missing, collapse = ", "),
         ". Expected dose, entry_day, status, days_followed.", call. = FALSE)
  }
  if (is.null(out$days_followed)) {
    out$days_followed <- rep(NA_real_, nrow(raw))
  }

  # Match status case-insensitively to the canonical labels.
  status <- trimws(as.character(out$status))
  canonical <- trial_statuses[match(tolower(status), tolower(trial_statuses))]
  data.frame(
    dose = as.numeric(out$dose),
    entry_day = as.numeric(out$entry_day),
    status = ifelse(is.na(canonical), status, canonical),
    days_followed = suppressWarnings(as.numeric(out$days_followed))
  )
}

#' Example trial data, also offered as the upload template
#'
#' Cohorts of three for the default design (30-day window, assessment on
#' day 50), each patient at the dose the design recommended on their entry
#' day: three patients complete dose 1; at dose 2 one completes, one is
#' withdrawn on day 24 and one has a DLT on day 33, so the third cohort
#' stays at dose 2. On day 50 the design stays at dose 2, escalating to
#' dose 3 from day 63 if no new DLTs are observed.
example_trial_data <- function() {
  data.frame(
    dose = c(1, 1, 1, 2, 2, 2, 2, 2, 2),
    entry_day = c(1, 2, 7, 9, 12, 14, 36, 37, 42),
    status = c("Completed", "Completed", "Completed", "Completed",
               "Withdrawn", "DLT", "Ongoing", "Ongoing", "Ongoing"),
    days_followed = c(NA, NA, NA, NA, 13, 20, NA, NA, NA)
  )
}
