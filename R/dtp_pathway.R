# Dose Transition Pathway over calendar days.
#
# dtptite::apply_dtp() answers "how long until the recommendation rises?".
# The app also needs the whole pathway: for each future day, assuming no
# new DLTs, would the model de-escalate (D), stay (S) or escalate (E)
# relative to the current dose, or stop? Unlike apply_dtp(), only patients
# still being followed for DLT accrue follow-up; withdrawn patients keep
# their partial weight.

#' Project the dose recommendation day by day
#'
#' @param design The design *without* DTP, e.g. `build_design()$parent`.
#' @param data Patients in entry order, with columns `dose`, `tox`,
#'   `weight` and `monitored` (logical: still followed for DLT). The last
#'   row's dose is the current dose.
#' @param day_of_assessment Study day of the analysis (T_A).
#' @param obswin DLT observation window, in days.
#' @param t_max Longest wait to project, in days (`>= 0`, `Inf` allowed).
#'   Defaults to the time until every monitored patient completes
#'   follow-up.
#' @return A list of class `dtp_pathway` with:
#'   \describe{
#'     \item{days}{One row per projected day: `day`, `wait` (days after
#'       T_A), `dose` (recommended, `NA` if stopped), `decision` (`"D"`,
#'       `"S"`, `"E"` or `"Stop"`).}
#'     \item{runs}{Consecutive days with the same dose and decision:
#'       `from_day`, `to_day`, `dose`, `decision`.}
#'     \item{current_dose, recommended_dose}{Last dose given; recommendation
#'       at T_A.}
#'     \item{complete_day}{Day every monitored patient completes follow-up
#'       (T_A when none is pending), even when `t_max` stops `days` earlier.}
#'     \item{fits}{Fitted selectors: `current` (T_A), `complete` (all
#'       monitored patients fully followed up, even when `t_max` stops the
#'       projected days earlier) and `changes` (one per run, at its first
#'       day).}
#'   }
dtp_pathway <- function(design, data, day_of_assessment, obswin,
                        t_max = NULL) {
  stopifnot(nrow(data) > 0,
            all(c("dose", "tox", "weight", "monitored") %in% names(data)))
  # Same checks as dtptite::apply_dtp().
  stopifnot(is.numeric(obswin), length(obswin) == 1, is.finite(obswin),
            obswin > 0)
  if (!is.null(t_max)) {
    stopifnot(is.numeric(t_max), length(t_max) == 1, !is.na(t_max),
              t_max >= 0)
  }
  outcomes <- data.frame(dose = data$dose, tox = data$tox,
                         weight = data$weight, cohort = seq_len(nrow(data)))
  current_dose <- utils::tail(data$dose, 1)

  pending <- data$monitored & data$tox == 0 & data$weight < 1
  # Days until every monitored patient completes follow-up; t_max may cap
  # the projected days, but not the complete-follow-up fit.
  complete_wait <- if (any(pending)) {
    ceiling(max(1 - data$weight[pending]) * obswin)
  } else {
    0
  }
  horizon <- if (is.null(t_max)) complete_wait else min(complete_wait, t_max)

  fit_at <- function(wait) {
    projected <- outcomes
    projected$weight[pending] <- pmin(
      1, data$weight[pending] + wait / obswin
    )
    escalation::fit(design, projected)
  }

  waits <- seq(0, horizon)
  fits <- lapply(waits, fit_at)
  doses <- vapply(fits, function(f) {
    rec <- escalation::recommended_dose(f)
    if (is.na(rec)) NA_integer_ else as.integer(rec)
  }, integer(1))
  decision <- ifelse(is.na(doses), "Stop",
                     ifelse(doses > current_dose, "E",
                            ifelse(doses < current_dose, "D", "S")))

  days <- data.frame(day = day_of_assessment + waits, wait = waits,
                     dose = doses, decision = decision)

  key <- paste(days$decision, days$dose)
  starts <- which(c(TRUE, key[-1] != key[-length(key)]))
  ends <- c(starts[-1] - 1, length(key))
  runs <- data.frame(from_day = days$day[starts], to_day = days$day[ends],
                     dose = days$dose[starts],
                     decision = days$decision[starts])

  structure(
    list(
      days = days,
      runs = runs,
      current_dose = current_dose,
      recommended_dose = doses[1],
      day_of_assessment = day_of_assessment,
      complete_day = day_of_assessment + complete_wait,
      fits = list(current = fits[[1]],
                  complete = if (horizon < complete_wait) {
                    fit_at(complete_wait)
                  } else {
                    fits[[length(fits)]]
                  },
                  changes = fits[starts])
    ),
    class = "dtp_pathway"
  )
}

#' First day the pathway reaches a decision
#'
#' @param pathway A `dtp_pathway`.
#' @param decision One of `"E"`, `"S"`, `"D"`, `"Stop"`.
#' @return The study day, or `NA` if the decision is never reached.
first_day_of <- function(pathway, decision) {
  hit <- pathway$runs$from_day[pathway$runs$decision == decision]
  if (length(hit)) hit[1] else NA_real_
}

print.dtp_pathway <- function(x, ...) {
  cat(sprintf("DTP from day %d, current dose %d\n", x$day_of_assessment,
              x$current_dose))
  print(x$runs, row.names = FALSE)
  invisible(x)
}
