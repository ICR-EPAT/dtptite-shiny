# DTP-TITE-CRM design: the escalation/dtptite chain built from app settings.

#' Calibrate a TITE-CRM skeleton
#'
#' Wraps [dfcrm::getprior()] using the same working model as the fit, so
#' the calibrated skeleton matches the model it is used with (the legacy
#' app calibrated with the logistic model but fitted the empiric one).
#'
#' @param target Target DLT rate.
#' @param halfwidth Half-width of the indifference interval.
#' @param prior_mtd Prior guess of the MTD (dose level).
#' @param num_doses Number of dose levels.
#' @param model Working model, `"empiric"` (default) or `"logistic"`.
#' @return Numeric vector of prior DLT probabilities, one per dose.
calibrate_skeleton <- function(target, halfwidth, prior_mtd, num_doses,
                               model = "empiric") {
  stopifnot(prior_mtd >= 1, prior_mtd <= num_doses,
            halfwidth > 0, halfwidth < target)
  dfcrm::getprior(halfwidth = halfwidth, target = target, nu = prior_mtd,
                  nlevel = num_doses, model = model)
}

#' Beta prior parameters from a prior mean and effective sample size
#'
#' @param mean Prior mean DLT rate.
#' @param ess Prior effective sample size.
#' @return A list with `a` and `b`.
beta_prior_from_mean <- function(mean, ess) {
  stopifnot(mean > 0, mean < 1, ess > 0)
  list(a = mean * ess, b = (1 - mean) * ess)
}

#' Build the DTP-TITE-CRM design
#'
#' The chain is, in order: TITE-CRM, dose-skipping restrictions, global
#' coherence, safety stopping at the lowest dose, then DTP. This is the
#' order the legacy engine (dtpcrm::applied_titecrm) applied them in.
#'
#' @param skeleton Prior DLT probabilities, one per dose.
#' @param target Target DLT rate.
#' @param obswin DLT observation window, in days.
#' @param variance Prior variance of the model parameter.
#' @param no_skip_esc,no_skip_deesc Forbid skipping doses when escalating or
#'   de-escalating ([escalation::dont_skip_doses()]: at most one level from
#'   the last dose given).
#' @param global_coherence Apply [enforce_global_coherence()].
#' @param stop_confidence Confidence level for stopping for toxicity at the
#'   lowest dose, or `NULL` for no stopping rule.
#' @param stop_n_min Patients needed at the lowest dose before the stopping
#'   rule applies.
#' @param stop_a,stop_b Beta prior for the stopping rule.
#' @param t_max Longest wait DTP considers, in days. Defaults to `obswin`.
#' @return A list with `parent` (the design without DTP, refitted for each
#'   projected day) and `dtp` (the full design).
build_design <- function(skeleton, target, obswin, variance = 1.34,
                         no_skip_esc = TRUE, no_skip_deesc = TRUE,
                         global_coherence = TRUE,
                         stop_confidence = NULL, stop_n_min = 1,
                         stop_a = 0.1, stop_b = 0.9,
                         t_max = obswin) {
  stopifnot(all(skeleton > 0), all(skeleton < 1), !is.unsorted(skeleton),
            variance > 0)

  parent <- escalation::get_dfcrm_tite(skeleton = skeleton, target = target,
                                       scale = sqrt(variance))
  if (no_skip_esc || no_skip_deesc) {
    parent <- parent |>
      escalation::dont_skip_doses(when_escalating = no_skip_esc,
                                  when_deescalating = no_skip_deesc)
  }
  if (global_coherence) {
    parent <- parent |> enforce_global_coherence()
  }
  if (!is.null(stop_confidence)) {
    parent <- parent |>
      dtptite::stop_for_beta_binomial_toxicity(
        dose = 1, tox_threshold = target, confidence = stop_confidence,
        a = stop_a, b = stop_b, n_min = stop_n_min
      )
  }

  list(
    parent = parent,
    dtp = parent |> dtptite::apply_dtp(t_max = t_max, obswin = obswin,
                                       verbose = FALSE)
  )
}
