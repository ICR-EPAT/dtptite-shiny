# Recommendation text and model-recommendation table from a dtp_pathway.
#
# Legacy built both from fixed slots (current, T_E or T_S, complete
# follow-up): it labelled the current dose from the complete-follow-up fit
# minus one, and compared recommendations with `!=`, which errors as soon
# as any of them is NA (stopped). Here both read the pathway's runs, so any
# shape and any stopped fit is handled the same way.

describe_dose <- function(dose, current_dose) {
  if (is.na(dose)) {
    "stop the trial"
  } else if (dose > current_dose) {
    sprintf("escalate to dose level %d", dose)
  } else if (dose < current_dose) {
    sprintf("de-escalate to dose level %d", dose)
  } else {
    sprintf("stay at dose level %d", dose)
  }
}

#' Describe the recommendation and how it changes over time
#'
#' @param pathway A `dtp_pathway`.
#' @return A list with `decision` (at the day of assessment: `"D"`, `"S"`,
#'   `"E"` or `"Stop"`), `headline` (one sentence) and `details` (zero or
#'   more sentences, one per later change, then a closing note).
recommendation_text <- function(pathway) {
  runs <- pathway$runs
  t_a <- pathway$day_of_assessment
  current <- pathway$current_dose
  now <- runs$decision[1]

  if (now == "Stop") {
    return(list(
      decision = now,
      headline = sprintf(
        "Day %d: stop the trial, the lowest dose is too toxic.", t_a
      ),
      details = character()
    ))
  }

  headline <- sprintf("Day %d: %s (%s).", t_a,
                      describe_dose(runs$dose[1], current),
                      if (now == "S") "the current dose" else
                        sprintf("current dose level %d", current))
  later <- runs[-1, , drop = FALSE]
  details <- sprintf("From day %d: %s, if no new DLTs are observed by then.",
                     later$from_day,
                     vapply(later$dose, describe_dose, character(1),
                            current_dose = current))

  last_day <- max(pathway$days$day)
  closing <- if (pathway$complete_day == t_a) {
    "No patients are still in DLT follow-up."
  } else if (last_day < pathway$complete_day) {
    sprintf("Projected to day %d; follow-up completes on day %d.",
            last_day, pathway$complete_day)
  } else if (nrow(later) == 0) {
    sprintf(paste("With no new DLTs, this holds until follow-up completes",
                  "on day %d."), pathway$complete_day)
  } else {
    character()
  }

  list(decision = now, headline = headline, details = c(details, closing))
}

#' Fits at the key days of the pathway
#'
#' The day of assessment, each later change of recommendation and complete
#' follow-up. A later change falling on the complete-follow-up day is
#' reported as complete follow-up; complete follow-up on the day of
#' assessment is reported once, as current.
#'
#' @param pathway A `dtp_pathway`.
#' @return A list of `list(curve, day, fit)`, in time order.
key_fits <- function(pathway) {
  runs <- pathway$runs
  t_a <- pathway$day_of_assessment
  complete <- pathway$complete_day

  later <- seq_len(nrow(runs))[-1]
  later <- later[runs$from_day[later] != complete]
  c(
    list(list(curve = sprintf("Current (day %d)", t_a), day = t_a,
              fit = pathway$fits$current)),
    lapply(later, function(i) {
      list(curve = sprintf("From day %d", runs$from_day[i]),
           day = runs$from_day[i], fit = pathway$fits$changes[[i]])
    }),
    if (complete != t_a) {
      list(list(curve = sprintf("Complete follow-up (day %d)", complete),
                day = complete, fit = pathway$fits$complete))
    }
  )
}

#' Model recommendation at the key days of the pathway
#'
#' One row per [key_fits()] entry.
#'
#' @param pathway A `dtp_pathway`.
#' @param level Coverage of the posterior credible interval.
#' @return A data frame with `curve`, `day`, `dose` (`NA` when stopped),
#'   `decision`, and the posterior DLT probability at `dose`: `prob_tox`,
#'   `lower`, `upper` (all `NA` when stopped). `level` is kept as an
#'   attribute.
model_table <- function(pathway, level = 0.9) {
  stopifnot(is.numeric(level), length(level) == 1, level > 0, level < 1)
  tail_p <- (1 - level) / 2
  out <- do.call(rbind, lapply(key_fits(pathway), function(r) {
    dose <- escalation::recommended_dose(r$fit)
    dose <- if (is.na(dose)) NA_integer_ else as.integer(dose)
    at <- function(x) if (is.na(dose)) NA_real_ else x[dose]
    data.frame(
      curve = r$curve, day = r$day, dose = dose,
      decision = if (is.na(dose)) "Stop" else
        c("D", "S", "E")[sign(dose - pathway$current_dose) + 2],
      prob_tox = at(escalation::mean_prob_tox(r$fit)),
      lower = at(escalation::prob_tox_quantile(r$fit, tail_p)),
      upper = at(escalation::prob_tox_quantile(r$fit, 1 - tail_p))
    )
  }))
  attr(out, "level") <- level
  out
}

#' Format a model table for display
#'
#' @param table A `model_table()` result.
#' @param digits Decimal places for probabilities.
#' @return A character data frame with columns `Curve`, `Recommended dose`
#'   and `Posterior DLT rate [<level> CrI]`; stopped rows read "Stop" and
#'   have no posterior.
format_model_table <- function(table, digits = 3) {
  num <- function(x) formatC(x, format = "f", digits = digits)
  post <- ifelse(is.na(table$dose), "—",
                 sprintf("%s [%s, %s]", num(table$prob_tox),
                         num(table$lower), num(table$upper)))
  out <- data.frame(
    table$curve,
    ifelse(is.na(table$dose), "Stop", as.character(table$dose)),
    post
  )
  names(out) <- c("Curve", "Recommended dose",
                  sprintf("Posterior DLT rate [%g%% CrI]",
                          100 * attr(table, "level")))
  out
}
