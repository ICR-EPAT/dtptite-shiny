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
  logistic <- calibrate_skeleton(target = 0.25, halfwidth = 0.05,
                                 prior_mtd = 3, num_doses = 6,
                                 model = "logistic")
  expect_length(empiric, 6)
  expect_equal(empiric[3], 0.25)
  expect_false(is.unsorted(empiric))
  expect_false(isTRUE(all.equal(empiric, logistic)))
  expect_error(calibrate_skeleton(0.25, 0.05, prior_mtd = 7, num_doses = 6))
})

test_that("beta prior from mean and effective sample size", {
  expect_equal(beta_prior_from_mean(0.1, 1), list(a = 0.1, b = 0.9))
  expect_equal(beta_prior_from_mean(0.25, 10), list(a = 2.5, b = 7.5))
})
