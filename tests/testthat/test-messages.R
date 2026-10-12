# Scenarios as in test-dtp-pathway.R, following dtptite's TITE-CRM tests.
skeleton <- c(0.124, 0.25, 0.398, 0.542, 0.666)
target <- 0.25
tite_crm <- escalation::get_dfcrm_tite(skeleton = skeleton, target = target)

wait_case <- data.frame(dose = c(2, 2, 2, 2), tox = c(0, 1, 0, 0),
                        weight = c(1, 1, 1, 0.3),
                        monitored = c(FALSE, FALSE, FALSE, TRUE))
escalate_case <- data.frame(dose = c(1, 1, 1, 2, 2, 2, 2, 2),
                            tox = c(0, 0, 0, 1, 0, 0, 0, 0),
                            weight = c(1, 1, 1, 1, 1, 0.05, 0.05, 0.05),
                            monitored = c(rep(FALSE, 5), rep(TRUE, 3)))
stop_case <- data.frame(dose = c(1, 1, 1, 1), tox = c(1, 1, 1, 0),
                        weight = c(1, 1, 1, 0.2),
                        monitored = c(FALSE, FALSE, FALSE, TRUE))
stopping_design <- tite_crm |>
  dtptite::stop_for_beta_binomial_toxicity(dose = 1, tox_threshold = target,
                                           confidence = 0.88)

test_that("D then S: current dose from the current fit, then the wait", {
  p <- dtp_pathway(tite_crm, wait_case, day_of_assessment = 100, obswin = 56)
  msg <- recommendation_text(p)

  # Legacy read the current dose off the complete-follow-up fit, minus one.
  expect_equal(msg$decision, "D")
  expect_equal(msg$headline,
               "Day 100: de-escalate to dose level 1 (current dose level 2).")
  expect_equal(msg$details, paste("From day 112: stay at dose level 2,",
                                  "if no new DLTs are observed by then."))

  tbl <- model_table(p)
  expect_equal(tbl$curve, c("Current (day 100)", "From day 112",
                            "Complete follow-up (day 140)"))
  expect_equal(tbl$dose, c(1L, 2L, 2L))
  expect_equal(tbl$decision, c("D", "S", "S"))
})

test_that("S then E reports the escalation day", {
  p <- dtp_pathway(tite_crm, escalate_case, day_of_assessment = 100,
                   obswin = 56)
  msg <- recommendation_text(p)

  expect_equal(msg$headline, "Day 100: stay at dose level 2 (the current dose).")
  expect_equal(msg$details, paste("From day 149: escalate to dose level 3,",
                                  "if no new DLTs are observed by then."))
  expect_equal(model_table(p)$day, c(100, 149, 154))
})

test_that("a projection capped by t_max says where it ends", {
  p <- dtp_pathway(tite_crm, escalate_case, day_of_assessment = 100,
                   obswin = 56, t_max = 30)
  msg <- recommendation_text(p)

  expect_equal(msg$details,
               "Projected to day 130; follow-up completes on day 154.")
  # Complete follow-up is still reported, past the projected days.
  tbl <- model_table(p)
  expect_equal(tbl$day, c(100, 154))
  expect_equal(tbl$dose, c(2L, 3L))
})

test_that("an unchanging recommendation says how long it holds", {
  x <- data.frame(dose = c(1, 1, 1), tox = c(1, 1, 0), weight = c(1, 1, 0.2),
                  monitored = c(FALSE, FALSE, TRUE))
  p <- dtp_pathway(tite_crm, x, day_of_assessment = 30, obswin = 56)
  msg <- recommendation_text(p)

  expect_equal(msg$decision, "S")
  expect_equal(msg$details, paste("With no new DLTs, this holds until",
                                  "follow-up completes on day 75."))
})

test_that("with nobody in follow-up there is a single current row", {
  withdrawn <- wait_case
  withdrawn$monitored[4] <- FALSE
  p <- dtp_pathway(tite_crm, withdrawn, day_of_assessment = 100, obswin = 56)

  expect_equal(recommendation_text(p)$details,
               "No patients are still in DLT follow-up.")
  expect_equal(model_table(p)$curve, "Current (day 100)")
})

test_that("a change on the complete-follow-up day is listed once", {
  # Found by random search: dose 1 until day 103, dose 2 from day 104,
  # when every monitored patient completes.
  x <- data.frame(dose = c(1, 1, 2, 2, 2, 2, 3),
                  tox = c(0, 0, 0, 1, 0, 0, 1),
                  weight = c(0.2, 0.8, 0.2, 1, 0.2, 1, 1),
                  monitored = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))
  p <- dtp_pathway(tite_crm, x, day_of_assessment = 100, obswin = 5)
  expect_equal(p$runs$from_day, c(100, 104))
  expect_equal(p$complete_day, 104)

  expect_equal(model_table(p)$curve,
               c("Current (day 100)", "Complete follow-up (day 104)"))
  expect_equal(model_table(p)$dose, c(1L, 2L))
  # Same decision, new dose: still a change worth reporting.
  expect_equal(recommendation_text(p)$details,
               paste("From day 104: de-escalate to dose level 2,",
                     "if no new DLTs are observed by then."))
})

test_that("stopped fits give Stop rows instead of an error", {
  # Legacy NA-mtd bug: comparing an NA recommendation with `!=` errored.
  p <- dtp_pathway(stopping_design, stop_case, day_of_assessment = 50,
                   obswin = 56)
  msg <- recommendation_text(p)
  expect_equal(msg$decision, "Stop")
  expect_equal(msg$headline,
               "Day 50: stop the trial, the lowest dose is too toxic.")
  expect_length(msg$details, 0)

  tbl <- model_table(p)
  expect_equal(tbl$decision, c("Stop", "Stop"))
  expect_true(all(is.na(tbl[c("dose", "prob_tox", "lower", "upper")])))

  shown <- format_model_table(tbl)
  expect_equal(shown[["Recommended dose"]], c("Stop", "Stop"))
  expect_equal(shown[["Posterior DLT rate [90% CI]"]], c("—", "—"))
})

test_that("posterior interval matches dfcrm, as legacy displayed it", {
  p <- dtp_pathway(tite_crm, wait_case, day_of_assessment = 100, obswin = 56)
  tbl <- model_table(p, level = 0.9)

  ref <- dfcrm::titecrm(prior = skeleton, target = target,
                        tox = wait_case$tox, level = wait_case$dose,
                        weights = wait_case$weight, scale = sqrt(1.34),
                        conf.level = 0.9)
  expect_equal(tbl$dose[1], ref$mtd)
  expect_equal(tbl$prob_tox[1], ref$ptox[ref$mtd])
  expect_equal(tbl$lower[1], ref$ptoxL[ref$mtd])
  expect_equal(tbl$upper[1], ref$ptoxU[ref$mtd])
})

test_that("interval level sets the width and the column name", {
  p <- dtp_pathway(tite_crm, wait_case, day_of_assessment = 100, obswin = 56)
  wide <- model_table(p, level = 0.9)
  narrow <- model_table(p, level = 0.5)

  expect_true(all(wide$lower <= narrow$lower & narrow$upper <= wide$upper))
  expect_true(all(narrow$lower <= narrow$prob_tox &
                    narrow$prob_tox <= narrow$upper))
  expect_equal(names(format_model_table(narrow))[3],
               "Posterior DLT rate [50% CI]")
  expect_equal(format_model_table(wide, digits = 2)[[3]][1],
               sprintf("%.2f [%.2f, %.2f]", wide$prob_tox[1], wide$lower[1],
                       wide$upper[1]))
  expect_error(model_table(p, level = 1))
})
