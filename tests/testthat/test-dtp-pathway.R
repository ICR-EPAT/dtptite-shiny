# Scenarios follow dtptite's TITE-CRM tests: five doses, target 25%,
# 56-day window, designs built as in dtptite's tests/testthat.
skeleton <- c(0.124, 0.25, 0.398, 0.542, 0.666)
target <- 0.25
tite_crm <- escalation::get_dfcrm_tite(skeleton = skeleton, target = target)

# dtptite test-dtp-titecrm.R test 4: de-escalate now, dose 2 after 12 days.
wait_case <- data.frame(dose = c(2, 2, 2, 2), tox = c(0, 1, 0, 0),
                        weight = c(1, 1, 1, 0.3),
                        monitored = c(FALSE, FALSE, FALSE, TRUE))

test_that("D then S pathway matches dtptite's wait time", {
  p <- dtp_pathway(tite_crm, wait_case, day_of_assessment = 100, obswin = 56)

  expect_equal(p$runs$decision, c("D", "S"))
  expect_equal(p$runs$dose, c(1L, 2L))
  expect_equal(first_day_of(p, "S"), 112)
  expect_true(is.na(first_day_of(p, "E")))

  dtp <- escalation::fit(
    tite_crm |> dtptite::apply_dtp(t_max = Inf, obswin = 56, verbose = FALSE),
    wait_case[c("dose", "tox", "weight")] |> transform(cohort = 1:4)
  )
  expect_equal(first_day_of(p, "S") - 100, dtptite::dtp_wait_time(dtp))
})

test_that("projection ends when every monitored patient has completed", {
  p <- dtp_pathway(tite_crm, wait_case, day_of_assessment = 100, obswin = 56)
  # 70% of 56 days remain: 39.2, rounded up to 40.
  expect_equal(max(p$days$wait), 40)
  expect_equal(escalation::weight(p$fits$complete), c(1, 1, 1, 1))
  expect_equal(p$days$day[1], 100)
})

test_that("withdrawn patients keep their partial weight", {
  withdrawn <- wait_case
  withdrawn$monitored[4] <- FALSE
  p <- dtp_pathway(tite_crm, withdrawn, day_of_assessment = 100, obswin = 56)

  # Nothing left to accrue, so de-escalation stands; dtptite alone would
  # have projected this patient forward and suggested waiting.
  expect_equal(p$runs$decision, "D")
  expect_equal(nrow(p$days), 1)
})

test_that("S then E pathway reports the escalation day", {
  x <- data.frame(dose = c(1, 1, 1, 2, 2, 2, 2, 2),
                  tox = c(0, 0, 0, 1, 0, 0, 0, 0),
                  weight = c(1, 1, 1, 1, 1, 0.05, 0.05, 0.05),
                  monitored = c(rep(FALSE, 5), rep(TRUE, 3)))
  p <- dtp_pathway(tite_crm, x, day_of_assessment = 100, obswin = 56)

  expect_equal(p$runs$decision, c("S", "E"))
  expect_equal(first_day_of(p, "E"), 149)
})

test_that("escalation reached only on the last projected day is reported", {
  # Legacy bug: escalation was only reported when at least two projected
  # days recommended it.
  x <- data.frame(dose = c(1, 1, 1, 2, 2, 2, 2, 2),
                  tox = c(0, 0, 0, 1, 0, 0, 0, 0),
                  weight = c(1, 1, 1, 1, 1, 0.05, 0.05, 0.05),
                  monitored = c(rep(FALSE, 5), rep(TRUE, 3)))
  p <- dtp_pathway(tite_crm, x, day_of_assessment = 100, obswin = 56,
                   t_max = 49)
  expect_equal(sum(p$days$decision == "E"), 1)
  expect_equal(first_day_of(p, "E"), 149)
})

test_that("stopping for toxicity holds on every projected day", {
  design <- tite_crm |>
    dtptite::stop_for_beta_binomial_toxicity(dose = 1, tox_threshold = target,
                                             confidence = 0.88)
  x <- data.frame(dose = c(1, 1, 1, 1), tox = c(1, 1, 1, 0),
                  weight = c(1, 1, 1, 0.2),
                  monitored = c(FALSE, FALSE, FALSE, TRUE))
  p <- dtp_pathway(design, x, day_of_assessment = 50, obswin = 56)

  expect_equal(p$runs$decision, "Stop")
  expect_true(is.na(p$recommended_dose))
})

test_that("escalate-now and complete-data cases give a single run", {
  x <- data.frame(dose = c(1, 1, 1), tox = c(0, 0, 0), weight = c(1, 1, 1),
                  monitored = FALSE)
  p <- dtp_pathway(tite_crm, x, day_of_assessment = 30, obswin = 56)
  expect_equal(p$runs$decision, "E")
  expect_equal(nrow(p$days), 1)
})

test_that("invalid observation window or t_max is rejected", {
  expect_error(dtp_pathway(tite_crm, wait_case, 100, obswin = 0))
  expect_error(dtp_pathway(tite_crm, wait_case, 100, obswin = -56))
  expect_error(dtp_pathway(tite_crm, wait_case, 100, obswin = Inf))
  expect_error(dtp_pathway(tite_crm, wait_case, 100, obswin = c(28, 56)))
  # A negative t_max would otherwise project backwards from T_A.
  expect_error(dtp_pathway(tite_crm, wait_case, 100, obswin = 56,
                           t_max = -3))
  expect_error(dtp_pathway(tite_crm, wait_case, 100, obswin = 56,
                           t_max = NA_real_))
})

test_that("t_max of 0 or Inf is accepted", {
  p0 <- dtp_pathway(tite_crm, wait_case, 100, obswin = 56, t_max = 0)
  expect_equal(nrow(p0$days), 1)
  p_inf <- dtp_pathway(tite_crm, wait_case, 100, obswin = 56, t_max = Inf)
  expect_equal(p_inf$runs,
               dtp_pathway(tite_crm, wait_case, 100, obswin = 56)$runs)
})

# Run segmentation for a D -> S -> E pathway, using a stub design whose
# recommendation is driven by the total weight.
stub_factory <- structure(list(), class = c("stub_factory", "selector_factory"))
fit.stub_factory <- function(selector_factory, outcomes, ...) {
  structure(list(total = sum(outcomes$weight)), class = "stub_selector")
}
recommended_dose.stub_selector <- function(x, ...) {
  as.integer(findInterval(x$total, c(-Inf, 2.5, 3)))
}
registerS3method("fit", "stub_factory", fit.stub_factory,
                 envir = asNamespace("escalation"))
registerS3method("recommended_dose", "stub_selector",
                 recommended_dose.stub_selector,
                 envir = asNamespace("escalation"))

test_that("D, S and E runs are split in order", {
  x <- data.frame(dose = c(2, 2, 2), tox = 0, weight = c(1, 1, 0),
                  monitored = c(FALSE, FALSE, TRUE))
  p <- dtp_pathway(stub_factory, x, day_of_assessment = 10, obswin = 10)

  expect_equal(p$runs$decision, c("D", "S", "E"))
  expect_equal(p$runs$from_day, c(10, 15, 20))
  expect_equal(p$runs$to_day, c(14, 19, 20))
  expect_length(p$fits$changes, 3)
})
