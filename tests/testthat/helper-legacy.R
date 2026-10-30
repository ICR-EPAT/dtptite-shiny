# The legacy app's dose transition pathway, for parity tests.
#
# A port of the legacy app's dtptitecrm() and dtp.titecrm() (2021): the
# same data preparation and day-by-day loop, with dtpcrm::applied_titecrm()
# as the engine. Two changes, both so the comparison can be made at all:
# the stopping prior Be(a, b) is an argument (legacy fixed it at
# Be(0.1, 0.9) whatever the user set), and the stopping rule can be turned
# off (legacy always applied it).

#' Legacy pathway
#'
#' @param legacy Legacy five-column trial data: `level`, `tox`, `entry`,
#'   `followup`, `continue` (still monitored for DLT, 0/1), in entry order.
#' @param day_of_assessment Day of assessment (T_A).
#' @param stop_confidence Confidence for the stopping rule, or `NULL` for
#'   none.
#' @return A data.frame with one row per day T_A, ..., T_A + obswin - 1:
#'   `day`, `dose` (the recommended dose, `NA` when stopped), `model_dose`
#'   (the TITE-CRM dose before any rule) and `ind` (legacy's D/S/E/Stop
#'   label).
legacy_pathway <- function(legacy, day_of_assessment, skeleton, target,
                           variance, obswin, no_skip_esc, no_skip_deesc,
                           global_coherence, stop_confidence = NULL,
                           stop_n_min = 1, stop_a = 0.1, stop_b = 0.9) {
  names(legacy) <- c("level", "tox", "entry", "followup", "continue")
  tox <- legacy$tox
  level <- legacy$level
  entry <- legacy$entry
  followup <- legacy$followup
  current_dose <- level[length(level)]

  # dtptitecrm(): ongoing patients are followed up to T_A, at most obswin.
  exit <- pmin(entry + obswin - 1, day_of_assessment)
  us <- which(legacy$continue == 1)
  followup[us] <- exit[us] - entry[us] + 1

  # dtptitecrm()'s stop_for_excess_toxicity_betabinomial(), dose 1.
  stop_func <- if (!is.null(stop_confidence)) {
    function(x) {
      at_dose <- x$level == 1
      if (sum(at_dose) >= stop_n_min) {
        n_tox <- sum(x$tox[at_dose])
        p <- stats::pbeta(target, stop_a + n_tox,
                          stop_b + sum(at_dose) - n_tox, lower.tail = FALSE)
        if (p > stop_confidence) x$mtd <- NA
      }
      x
    }
  }

  fit <- function(f, ...) {
    dtpcrm::applied_titecrm(prior = skeleton, target = target, tox = tox,
                            level = level, followup = f, obswin = obswin,
                            scale = sqrt(variance), ...)
  }
  # dtp.titecrm(): on day T_A + i - 1, ongoing patients have i - 1 more
  # days of follow-up.
  rows <- lapply(seq_len(obswin), function(i) {
    f <- followup
    f[us] <- f[us] + i - 1
    out <- fit(f, no_skip_esc = no_skip_esc, no_skip_deesc = no_skip_deesc,
               global_coherent_esc = global_coherence, stop_func = stop_func)
    model <- fit(f, no_skip_esc = FALSE, no_skip_deesc = FALSE,
                 global_coherent_esc = FALSE)
    ind <- if (is.na(out$mtd)) {
      "Stop"
    } else if (out$mtd >= current_dose + 1) {
      "E"
    } else if (out$mtd == current_dose - 1) {
      "D"
    } else {
      "S"
    }
    data.frame(day = day_of_assessment + i - 1, dose = out$mtd,
               model_dose = model$mtd, ind = ind)
  })
  do.call(rbind, rows)
}

#' The app's pathway on legacy trial data
#'
#' Runs the app's run_model() with an entered skeleton and variance, the
#' legacy file mapped by from_legacy_format(), and the stopping prior
#' Be(a, b). Days after every patient completes follow-up keep the last
#' day's dose, as weights stop growing.
#'
#' @inheritParams legacy_pathway
#' @return A data.frame with `day`, `dose` and `decision`, over the same
#'   days as [legacy_pathway()].
app_pathway <- function(legacy, day_of_assessment, skeleton, target,
                        variance, obswin, no_skip_esc, no_skip_deesc,
                        global_coherence, stop_confidence = NULL,
                        stop_n_min = 1, stop_a = 0.1, stop_b = 0.9) {
  settings <- list(
    num_doses = length(skeleton), target = target, obswin = obswin,
    prior_type = "enter", prior_mtd = 1, halfwidth = target / 4,
    skeleton_text = paste(skeleton, collapse = ", "),
    variance_type = "own", variance = variance,
    no_skip_esc = no_skip_esc, no_skip_deesc = no_skip_deesc,
    global_coherence = global_coherence,
    stop_on = !is.null(stop_confidence),
    stop_confidence = stop_confidence %||% 0.9, stop_n_min = stop_n_min,
    stop_default_prior = FALSE, stop_prior_mean = stop_a / (stop_a + stop_b),
    stop_prior_ess = stop_a + stop_b, day_of_assessment = day_of_assessment
  )
  result <- run_model(settings, from_legacy_format(legacy, obswin))
  if (length(result$problems)) {
    stop(paste(result$problems, collapse = "\n"), call. = FALSE)
  }
  days <- result$pathway$days
  row <- pmin(seq_len(obswin), nrow(days))
  data.frame(day = day_of_assessment + seq_len(obswin) - 1,
             dose = days$dose[row], decision = days$decision[row])
}

#' Random legacy trial data
#'
#' Doses move by at most one level between patients; entry days strictly
#' increase. Each patient has a DLT, is still monitored, completed
#' follow-up or was withdrawn early.
#'
#' @return A list with `legacy` (as in [legacy_pathway()]) and
#'   `day_of_assessment`.
random_legacy_trial <- function(num_doses, obswin, p_tox = 0.25) {
  n <- sample(3:12, 1)
  level <- cumsum(c(1, sample(-1:1, n - 1, replace = TRUE,
                              prob = c(0.2, 0.4, 0.4))))
  level <- pmin(pmax(level, 1), num_doses)
  entry <- cumsum(sample(1:10, n, replace = TRUE))
  day_of_assessment <- entry[n] + sample(0:10, 1)
  so_far <- pmin(day_of_assessment - entry + 1, obswin)

  tox <- stats::rbinom(n, 1, p_tox)
  continue <- integer(n)
  followup <- so_far
  for (i in seq_len(n)) {
    if (tox[i] == 1) {
      followup[i] <- sample(seq_len(so_far[i]), 1)
    } else if (so_far[i] < obswin && stats::runif(1) < 0.2) {
      followup[i] <- sample(seq_len(so_far[i]), 1)  # withdrawn
    } else {
      continue[i] <- as.integer(so_far[i] < obswin)
    }
  }
  list(legacy = data.frame(level, tox, entry, followup, continue),
       day_of_assessment = day_of_assessment)
}
