skeleton <- c(0.05, 0.12, 0.25, 0.40, 0.55)

coherent_design <- function(target = 0.25) {
  escalation::get_dfcrm_tite(skeleton = skeleton, target = target) |>
    enforce_global_coherence()
}

test_that("no escalation above a dose whose observed DLT rate exceeds target", {
  # 1/2 DLTs at the current dose 2 (50% > 25%), but the low skeleton and the
  # clean dose-1 data make the model want to go higher.
  outcomes <- data.frame(dose = c(1, 1, 1, 1, 1, 1, 2, 2),
                         tox = c(0, 0, 0, 0, 0, 0, 1, 0),
                         weight = c(1, 1, 1, 1, 1, 1, 1, 1), cohort = 1:8)
  low_skeleton <- c(0.01, 0.02, 0.04, 0.08, 0.15)
  unconstrained <- escalation::get_dfcrm_tite(skeleton = low_skeleton,
                                              target = 0.25) |>
    escalation::fit(outcomes)
  coherent <- escalation::get_dfcrm_tite(skeleton = low_skeleton,
                                         target = 0.25) |>
    enforce_global_coherence() |>
    escalation::fit(outcomes)

  expect_gt(escalation::recommended_dose(unconstrained), 2)
  expect_equal(escalation::recommended_dose(coherent), 2L)
})

test_that("no constraint when the observed rate at the current dose is low", {
  outcomes <- data.frame(dose = c(1, 1, 1), tox = c(0, 0, 0),
                         weight = c(1, 1, 0.5), cohort = 1:3)
  plain <- escalation::get_dfcrm_tite(skeleton = skeleton, target = 0.25) |>
    escalation::fit(outcomes)
  coherent <- coherent_design() |> escalation::fit(outcomes)
  expect_equal(escalation::recommended_dose(coherent),
               escalation::recommended_dose(plain))
})

test_that("other selector methods forward to the parent", {
  outcomes <- data.frame(dose = c(1, 2), tox = c(0, 1), weight = c(1, 1),
                         cohort = 1:2)
  x <- coherent_design() |> escalation::fit(outcomes)
  expect_equal(escalation::num_patients(x), 2)
  expect_length(escalation::mean_prob_tox(x), 5)
})

test_that("DTP projection refits go through the decorator", {
  # 1/3 DLTs at the current dose 2 (above the 25% target), one patient still
  # pending. Both designs recommend dose 2 now, so the cap can only act in
  # the refits apply_dtp() makes while projecting the wait.
  outcomes <- data.frame(dose = c(1, 2, 2, 2), tox = c(0, 1, 0, 0),
                         weight = c(1, 1, 1, 0.3), cohort = 1:4)
  low_skeleton <- c(0.01, 0.02, 0.04, 0.08, 0.15)
  dtp <- function(design) {
    design |>
      dtptite::apply_dtp(t_max = Inf, obswin = 56, verbose = FALSE) |>
      escalation::fit(outcomes)
  }
  plain <- escalation::get_dfcrm_tite(skeleton = low_skeleton,
                                      target = 0.25) |> dtp()
  coherent <- escalation::get_dfcrm_tite(skeleton = low_skeleton,
                                         target = 0.25) |>
    enforce_global_coherence() |>
    dtp()

  # Without coherence, waiting 29 days would allow escalation to dose 3.
  expect_equal(escalation::recommended_dose(plain), 2)
  expect_equal(dtptite::dtp_wait_time(plain), 29)
  expect_equal(dtptite::dtp_projected_dose(plain), 3L)

  # With coherence, no amount of waiting escalates past dose 2.
  expect_s3_class(coherent, "dtp_selector")
  expect_equal(escalation::recommended_dose(coherent), 2)
  expect_false(dtptite::dtp_should_wait(coherent))
  expect_true(is.na(dtptite::dtp_projected_dose(coherent)))
})

test_that("matches dtpcrm::applied_titecrm global coherence", {
  skip_if_not_installed("dtpcrm")
  # A low skeleton plus a DLT in the last patient makes the rule bind in
  # roughly a third of replicates; both binding and non-binding cases must
  # agree with the legacy engine.
  low_skeleton <- c(0.01, 0.02, 0.04, 0.08, 0.15)
  target <- 0.25
  set.seed(2026)
  n_binding <- 0
  for (i in 1:100) {
    n <- sample(4:12, 1)
    level <- c(sample(1:3, n - 1, replace = TRUE), sample(1:3, 1))
    tox <- c(rbinom(n - 1, 1, 0.1), 1)
    followup <- ifelse(tox == 1, sample(1:30, n, replace = TRUE),
                       sample(c(1:30, rep(30, 30)), n, replace = TRUE))
    legacy <- function(coherent) {
      dtpcrm::applied_titecrm(
        prior = low_skeleton, target = target, tox = tox, level = level,
        followup = followup, obswin = 30, no_skip_esc = FALSE,
        no_skip_deesc = FALSE, global_coherent_esc = coherent
      )$mtd
    }
    outcomes <- data.frame(dose = level, tox = tox,
                           weight = ifelse(tox == 1, 1, followup / 30),
                           cohort = seq_len(n))
    new <- escalation::get_dfcrm_tite(skeleton = low_skeleton,
                                      target = target) |>
      enforce_global_coherence() |>
      escalation::fit(outcomes)
    expect_equal(escalation::recommended_dose(new), legacy(TRUE),
                 info = paste("replicate", i))
    n_binding <- n_binding + (legacy(TRUE) != legacy(FALSE))
  }
  expect_gt(n_binding, 10)
})
