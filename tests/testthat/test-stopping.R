skeleton <- c(0.03, 0.07, 0.20, 0.30, 0.40)
target <- 0.25
tite_crm <- escalation::get_dfcrm_tite(skeleton = skeleton, target = target,
                                       model = "empiric", scale = sqrt(1.34))
outcomes <- function(dose, tox) {
  data.frame(dose = dose, tox = tox, weight = 1, cohort = seq_along(dose))
}
rec <- function(design, x) {
  escalation::recommended_dose(escalation::fit(design, x))
}

test_that("stops when the lowest dose is too toxic", {
  # 2 DLTs in 3 at dose 1: P(p1 > 25% | Be(0.25, 0.75) prior) = 0.902.
  x <- outcomes(c(1, 1, 1), c(1, 1, 0))
  fit <- escalation::fit(
    tite_crm |> stop_for_lowest_dose_toxicity(target, confidence = 0.8), x
  )
  expect_true(is.na(escalation::recommended_dose(fit)))
  expect_false(escalation::continue(fit))
  expect_false(is.na(rec(
    tite_crm |> stop_for_lowest_dose_toxicity(target, confidence = 0.95), x
  )))
})

test_that("stops only when the probability is greater than the confidence", {
  x <- outcomes(c(1, 1, 1), c(1, 1, 0))
  p <- stats::pbeta(target, target + 2, 1 - target + 1, lower.tail = FALSE)
  expect_false(is.na(rec(
    tite_crm |> stop_for_lowest_dose_toxicity(target, confidence = p), x
  )))
})

test_that("waits for the minimum number at the lowest dose", {
  x <- outcomes(c(1, 1, 1), c(1, 1, 0))
  expect_false(is.na(rec(
    tite_crm |> stop_for_lowest_dose_toxicity(target, confidence = 0.8,
                                              n_min = 4),
    x
  )))
})

test_that("does not rule out higher doses (dtptite 0.1.0's rule does)", {
  # 2 DLTs in 3 at dose 3: P(p3 > 25%) = 0.902 > 0.7, but TITE-CRM still
  # recommends dose 3.
  x <- outcomes(c(1, 1, 1, 2, 2, 2, 3, 3, 3), c(0, 0, 0, 0, 0, 0, 1, 1, 0))
  expect_equal(rec(tite_crm, x), 3)
  expect_equal(rec(tite_crm |> stop_for_lowest_dose_toxicity(
    target, confidence = 0.7, n_min = 2
  ), x), 3)
  expect_equal(rec(tite_crm |> dtptite::stop_for_beta_binomial_toxicity(
    dose = 1, tox_threshold = target, confidence = 0.7, n_min = 2
  ), x), 2)
})

test_that("passes the parent's recommendation through otherwise", {
  x <- outcomes(c(1, 1, 2, 2), c(0, 0, 0, 1))
  expect_equal(
    rec(tite_crm |> stop_for_lowest_dose_toxicity(target, confidence = 0.7),
        x),
    rec(tite_crm, x)
  )
  expect_true(escalation::continue(escalation::fit(
    tite_crm |> stop_for_lowest_dose_toxicity(target, confidence = 0.7), x
  )))
})
