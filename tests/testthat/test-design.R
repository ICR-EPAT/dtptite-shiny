# Scenarios follow dtptite's own TITE-CRM tests (tests/testthat/
# test-dtp-titecrm.R): five doses, target 25%, 56-day window.
skeleton <- c(0.124, 0.25, 0.398, 0.542, 0.666)
target <- 0.25

plain_design <- function(...) {
  build_design(skeleton, target, obswin = 56, no_skip_esc = FALSE,
               no_skip_deesc = FALSE, global_coherence = FALSE, ...)
}

test_that("unrestricted chain reproduces dtptite's wait-then-escalate case", {
  # dtptite test 4: de-escalate now, but dose 2 again after 12 days.
  outcomes <- data.frame(dose = c(2, 2, 2, 2), tox = c(0, 1, 0, 0),
                         weight = c(1, 1, 1, 0.3), cohort = 1:4)
  x <- escalation::fit(plain_design(variance = 1.34)$dtp, outcomes)

  expect_equal(escalation::recommended_dose(x), 1)
  expect_true(dtptite::dtp_should_wait(x))
  expect_equal(dtptite::dtp_wait_time(x), 12)
  expect_equal(dtptite::dtp_projected_dose(x), 2L)
})

test_that("full chain matches dtptite's canonical DTP-TITE-CRM design", {
  # dtptite tests/testthat/test-depth-valve.R, without stop_at_n().
  canonical <- escalation::get_dfcrm_tite(skeleton = skeleton, target = target,
                                          model = "empiric", scale = 0.759) |>
    dtptite::stop_for_beta_binomial_toxicity(a = 1, b = 1, dose = 1,
                                             tox_threshold = target,
                                             confidence = 0.88) |>
    escalation::dont_skip_doses(when_escalating = TRUE,
                                when_deescalating = TRUE) |>
    dtptite::apply_dtp(t_max = 35, obswin = 56, verbose = FALSE)
  ours <- build_design(skeleton, target, obswin = 56, variance = 0.759^2,
                       global_coherence = FALSE, stop_confidence = 0.88,
                       stop_a = 1, stop_b = 1, t_max = 35)$dtp

  scenarios <- list(
    data.frame(dose = c(2, 2, 2, 2), tox = c(0, 1, 0, 0),
               weight = c(1, 1, 1, 0.3), cohort = 1:4),
    data.frame(dose = c(1, 1, 1), tox = c(0, 0, 0), weight = c(1, 1, 0.5),
               cohort = 1:3),
    data.frame(dose = c(1, 1, 1), tox = c(1, 1, 1), weight = c(1, 1, 1),
               cohort = 1:3)
  )
  for (outcomes in scenarios) {
    a <- escalation::fit(canonical, outcomes)
    b <- escalation::fit(ours, outcomes)
    expect_equal(escalation::recommended_dose(b),
                 escalation::recommended_dose(a))
    expect_equal(dtptite::dtp_wait_time(b), dtptite::dtp_wait_time(a))
    expect_equal(escalation::mean_prob_tox(b), escalation::mean_prob_tox(a))
  }
})

test_that("parent design is the DTP design without the projection", {
  outcomes <- data.frame(dose = c(2, 2, 2, 2), tox = c(0, 1, 0, 0),
                         weight = c(1, 1, 1, 0.3), cohort = 1:4)
  design <- plain_design()
  expect_equal(
    escalation::recommended_dose(escalation::fit(design$parent, outcomes)),
    escalation::recommended_dose(escalation::fit(design$dtp, outcomes))
  )
  expect_false(inherits(design$parent, "dtp_selector_factory"))
})

test_that("stopping-rule Beta prior is used (legacy app ignored it)", {
  # 2 DLTs in 3 patients at dose 1, stop if P(p1 > 25%) > 0.8.
  # Be(0.1, 0.9): P = 0.869, stop. Be(2.5, 7.5): P = 0.757, continue.
  outcomes <- data.frame(dose = c(1, 1, 1), tox = c(1, 1, 0),
                         weight = c(1, 1, 1), cohort = 1:3)
  default_prior <- plain_design(stop_confidence = 0.8)
  prior <- beta_prior_from_mean(mean = 0.25, ess = 10)
  informative <- plain_design(stop_confidence = 0.8, stop_a = prior$a,
                              stop_b = prior$b)

  expect_true(is.na(escalation::recommended_dose(
    escalation::fit(default_prior$parent, outcomes))))
  expect_false(is.na(escalation::recommended_dose(
    escalation::fit(informative$parent, outcomes))))
})

test_that("stopping rule waits for the minimum number at the lowest dose", {
  outcomes <- data.frame(dose = c(1, 1, 1), tox = c(1, 1, 0),
                         weight = c(1, 1, 1), cohort = 1:3)
  design <- plain_design(stop_confidence = 0.8, stop_n_min = 4)
  expect_false(is.na(escalation::recommended_dose(
    escalation::fit(design$parent, outcomes))))
})

test_that("no skipping on de-escalation keeps within one level of last dose", {
  outcomes <- data.frame(dose = c(1, 2, 3, 4, 4, 4), tox = c(0, 0, 0, 1, 1, 1),
                         weight = 1, cohort = 1:6)
  free <- plain_design()
  no_skip <- build_design(skeleton, target, obswin = 56, no_skip_esc = FALSE,
                          no_skip_deesc = TRUE, global_coherence = FALSE)
  expect_equal(escalation::recommended_dose(
    escalation::fit(free$parent, outcomes)), 1)
  expect_equal(escalation::recommended_dose(
    escalation::fit(no_skip$parent, outcomes)), 3L)
})

test_that("prior variance is passed to the model", {
  outcomes <- data.frame(dose = c(2, 2, 2, 2), tox = c(0, 1, 0, 0),
                         weight = c(1, 1, 1, 0.3), cohort = 1:4)
  narrow <- escalation::fit(plain_design(variance = 0.1)$parent, outcomes)
  wide <- escalation::fit(plain_design(variance = 1.34)$parent, outcomes)
  expect_false(isTRUE(all.equal(escalation::mean_prob_tox(narrow),
                                escalation::mean_prob_tox(wide))))
})

test_that("calibrated skeleton uses the fitted model and the dose count", {
  empiric <- calibrate_skeleton(target = 0.25, halfwidth = 0.05,
                                prior_mtd = 3, num_doses = 6)
  # Legacy calibrated with the logistic model but fitted the empiric one.
  logistic <- dfcrm::getprior(halfwidth = 0.05, target = 0.25, nu = 3,
                              nlevel = 6, model = "logistic")
  expect_equal(empiric, dfcrm::getprior(halfwidth = 0.05, target = 0.25,
                                        nu = 3, nlevel = 6,
                                        model = "empiric"))
  expect_false("model" %in% names(formals(calibrate_skeleton)))
  expect_length(empiric, 6)
  expect_equal(empiric[3], 0.25)
  expect_false(is.unsorted(empiric))
  expect_false(isTRUE(all.equal(empiric, logistic)))
  expect_error(calibrate_skeleton(0.25, 0.05, prior_mtd = 7, num_doses = 6))
})

test_that("calibration rejects intervals outside (0, 1)", {
  # dfcrm::getprior() returns NaN or values >= 1 for these.
  expect_error(calibrate_skeleton(target = 0.8, halfwidth = 0.3,
                                  prior_mtd = 3, num_doses = 5),
               "within \\(0, 1\\)")
  expect_error(calibrate_skeleton(target = 0.8, halfwidth = 0.2,
                                  prior_mtd = 3, num_doses = 5),
               "within \\(0, 1\\)")
  expect_error(calibrate_skeleton(target = 0.1, halfwidth = 0.1,
                                  prior_mtd = 3, num_doses = 5),
               "within \\(0, 1\\)")
})

test_that("calibration rejects skeletons getprior cannot make valid", {
  # The interval is valid, but the lowest dose underflows to exactly 0.
  expect_error(calibrate_skeleton(target = 0.05, halfwidth = 0.04,
                                  prior_mtd = 10, num_doses = 10),
               "increasing DLT probabilities")
})

test_that("calibration accepts the app's slider extremes", {
  for (target in c(0.05, 0.5)) {
    for (halfwidth in c(0.03, min(0.13, target - 0.01))) {
      skeleton <- calibrate_skeleton(target, halfwidth, prior_mtd = 1,
                                     num_doses = 10)
      expect_false(is.unsorted(skeleton, strictly = TRUE))
    }
  }
})

test_that("beta prior from mean and effective sample size", {
  expect_equal(beta_prior_from_mean(0.1, 1), list(a = 0.1, b = 0.9))
  expect_equal(beta_prior_from_mean(0.25, 10), list(a = 2.5, b = 7.5))
})

test_that("prior MTD distribution covers every dose and sums to 1", {
  skeleton <- calibrate_skeleton(0.25, 0.05, prior_mtd = 3, num_doses = 5)
  mu <- prior_mtd_distribution(skeleton, target = 0.25, variance = 1)
  expect_length(mu, 5)
  expect_equal(sum(mu), 1)
  expect_true(all(mu > 0))
  # A tight prior puts the mass on the prior guess of the MTD.
  tight <- prior_mtd_distribution(skeleton, target = 0.25, variance = 1e-4)
  expect_equal(which.max(tight), 3)
  expect_gt(tight[3], 0.99)
})

test_that("least informative variance matches a fine grid search", {
  grid_search <- function(skeleton, target) {
    kl <- function(sd) {
      mu <- prior_mtd_distribution(skeleton, target, sd^2)
      sum(mu * log(mu * length(mu)))
    }
    sd <- seq(0.1, 2, by = 0.001)
    sd[which.min(vapply(sd, kl, numeric(1)))]^2
  }
  four_doses <- calibrate_skeleton(0.25, 0.25 / 4, prior_mtd = 3,
                                   num_doses = 4)
  five_doses <- calibrate_skeleton(0.25, 0.05, prior_mtd = 1, num_doses = 5)
  for (skeleton in list(four_doses, five_doses)) {
    # Within the grid step (0.001 in sd).
    expect_equal(least_informative_variance(skeleton, 0.25),
                 grid_search(skeleton, 0.25), tolerance = 2e-3)
  }
  expect_equal(least_informative_variance(four_doses, 0.25), 0.3436,
               tolerance = 1e-3)
})

test_that("least informative variance is closest to a uniform MTD prior", {
  skeleton <- c(0.124, 0.25, 0.398, 0.542, 0.666)
  v <- least_informative_variance(skeleton, 0.25)
  kl <- function(variance) {
    mu <- prior_mtd_distribution(skeleton, 0.25, variance)
    sum(mu * log(mu * length(mu)))
  }
  expect_lt(kl(v), kl(v * 0.9))
  expect_lt(kl(v), kl(v * 1.1))
})

test_that("least informative variance warns at the edge of its search", {
  skeleton <- calibrate_skeleton(0.25, 0.05, prior_mtd = 1, num_doses = 5)
  expect_warning(least_informative_variance(skeleton, 0.25, sd_max = 0.5),
                 "search limit \\(0.5\\)")
  # Review of #8: the lower limit warns too, instead of returning it.
  tight <- c(0.249, 0.2495, 0.25, 0.2505, 0.251)
  expect_warning(least_informative_variance(tight, 0.25, sd_min = 0.01),
                 "search limit \\(0.01\\)")
  expect_error(least_informative_variance(skeleton, 0.25, sd_min = 1,
                                          sd_max = 0.5))
})

test_that("tightly spaced skeletons find their small optimum", {
  # Review of #8: a fixed lower limit of 0.01 returned 0.01 here.
  tight <- c(0.249, 0.2495, 0.25, 0.2505, 0.251)
  sd <- sqrt(least_informative_variance(tight, 0.25))
  expect_equal(sd, 0.00259, tolerance = 1e-3)
})

test_that("an optimum just inside the upper limit gives no warning", {
  # Review of #8: the warning followed the coarse grid's best point, so it
  # fired when the refined optimum lay just inside the limit.
  skeleton <- c(0.05, 0.12, 0.25, 0.4, 0.55)
  v <- least_informative_variance(skeleton, 0.25)
  expect_no_warning(
    near <- least_informative_variance(skeleton, 0.25,
                                       sd_max = sqrt(v) * 1.01)
  )
  expect_equal(near, v, tolerance = 1e-6)
})
