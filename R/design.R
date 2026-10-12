# DTP-TITE-CRM design: the escalation/dtptite chain built from app settings.

#' Calibrate a TITE-CRM skeleton
#'
#' Wraps [dfcrm::getprior()] with the empiric working model, the model the
#' design always fits (the legacy app calibrated with the logistic model
#' but fitted the empiric one).
#'
#' @param target Target DLT rate.
#' @param halfwidth Half-width of the indifference interval.
#' @param prior_mtd Prior guess of the MTD (dose level).
#' @param num_doses Number of dose levels.
#' @return Numeric vector of prior DLT probabilities, one per dose.
calibrate_skeleton <- function(target, halfwidth, prior_mtd, num_doses) {
  stopifnot(prior_mtd >= 1, prior_mtd <= num_doses, halfwidth > 0)
  if (target - halfwidth <= 0 || target + halfwidth >= 1) {
    stop("The indifference interval target +/- halfwidth must lie within ",
         "(0, 1).", call. = FALSE)
  }
  # getprior() warns "NaNs produced" on bad inputs; the check below gives
  # a clearer error instead.
  skeleton <- suppressWarnings(
    dfcrm::getprior(halfwidth = halfwidth, target = target, nu = prior_mtd,
                    nlevel = num_doses, model = "empiric")
  )
  if (anyNA(skeleton) || any(skeleton <= 0 | skeleton >= 1) ||
      is.unsorted(skeleton, strictly = TRUE)) {
    stop("Calibration did not give increasing DLT probabilities within ",
         "(0, 1); try a smaller halfwidth.", call. = FALSE)
  }
  skeleton
}

#' Prior probability that each dose is the MTD
#'
#' Under the empiric model with `beta ~ N(0, variance)`, dose `i` is the
#' model's MTD when `beta` falls in its indifference interval, as given by
#' [dfcrm::crmsens()]. The outer intervals are unbounded.
#'
#' @param skeleton Prior DLT probabilities, one per dose.
#' @param target Target DLT rate.
#' @param variance Prior variance of the model parameter.
#' @return Numeric vector of probabilities, one per dose, summing to 1.
prior_mtd_distribution <- function(skeleton, target, variance) {
  stopifnot(variance > 0)
  hset <- dfcrm::crmsens(skeleton, target = target, model = "empiric",
                         detail = TRUE)$Hset
  cuts <- hset[-nrow(hset), 2]
  diff(c(0, stats::pnorm(cuts, mean = 0, sd = sqrt(variance)), 1))
}

#' Least informative prior variance (Lee and Cheung, 2011)
#'
#' The variance whose prior MTD distribution ([prior_mtd_distribution()])
#' is closest to uniform, by Kullback-Leibler divergence. The search over
#' the prior standard deviation is continuous: a coarse grid, then
#' [stats::optimize()] around its best point.
#'
#' @param skeleton Prior DLT probabilities, one per dose.
#' @param target Target DLT rate.
#' @param sd_min,sd_max Range of prior standard deviations searched; a
#'   warning is given when the optimum is at either limit, as then the
#'   least informative variance lies outside the range.
#' @return The prior variance.
#' @references Lee SM, Cheung YK (2011). Calibration of prior variance in
#'   the Bayesian continual reassessment method. Statistics in Medicine
#'   30(17):2081-2089.
least_informative_variance <- function(skeleton, target, sd_min = 0.001,
                                       sd_max = 10) {
  stopifnot(is.numeric(sd_min), is.numeric(sd_max), length(sd_min) == 1,
            length(sd_max) == 1, sd_min > 0, sd_max > sd_min)
  k <- length(skeleton)
  divergence <- function(sd) {
    mu <- prior_mtd_distribution(skeleton, target, sd^2)
    sum(ifelse(mu > 0, mu * log(mu * k), 0))
  }
  # A coarse grid first, so optimize() starts in the right basin.
  grid <- exp(seq(log(sd_min), log(sd_max), length.out = 250))
  best <- which.min(vapply(grid, divergence, numeric(1)))
  lower <- grid[max(best - 1, 1)]
  upper <- grid[min(best + 1, length(grid))]
  sd <- stats::optimize(divergence, c(lower, upper), tol = 1e-10)$minimum
  # Judge the refined optimum, not the grid point: it is at a limit only
  # if optimize() ran into it.
  at_limit <- function(limit) abs(log(sd / limit)) < 1e-4
  if (at_limit(sd_min) || at_limit(sd_max)) {
    warning(sprintf(
      "The least informative prior SD is at the search limit (%g); it lies outside [%g, %g].",
      if (at_limit(sd_min)) sd_min else sd_max, sd_min, sd_max
    ), call. = FALSE)
  }
  sd^2
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
#' The chain is, in order: TITE-CRM (empiric model), safety stopping at the
#' lowest dose, dose-skipping restrictions, global coherence, then DTP. This
#' follows the order used in dtptite's own tests
#' (tests/testthat/test-depth-valve.R).
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
                                       model = "empiric",
                                       scale = sqrt(variance))
  if (!is.null(stop_confidence)) {
    parent <- parent |>
      dtptite::stop_for_beta_binomial_toxicity(
        dose = 1, tox_threshold = target, confidence = stop_confidence,
        a = stop_a, b = stop_b, n_min = stop_n_min
      )
  }
  if (no_skip_esc || no_skip_deesc) {
    parent <- parent |>
      escalation::dont_skip_doses(when_escalating = no_skip_esc,
                                  when_deescalating = no_skip_deesc)
  }
  if (global_coherence) {
    parent <- parent |> enforce_global_coherence()
  }

  list(
    parent = parent,
    dtp = parent |> dtptite::apply_dtp(t_max = t_max, obswin = obswin,
                                       verbose = FALSE)
  )
}
