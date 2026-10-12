# The example trial must follow the app's default design: each patient
# gets the dose the design recommended on their entry day, from what was
# known then.

default_design <- function(target = 0.25, obswin = 30) {
  skeleton <- skeleton_from_settings("calibrate", target, target / 4,
                                     prior_mtd = 1, num_doses = 5,
                                     text = "")$skeleton
  variance <- least_informative_variance(skeleton, target)
  build_design(skeleton, target, obswin, variance = variance,
               stop_confidence = 0.7, stop_n_min = 2)
}

# Trial data as known at the start of `day`: patients who entered before
# it, still Ongoing unless their follow-up had ended.
known_on <- function(trial, day, obswin) {
  before <- trial[trial$entry_day < day, , drop = FALSE]
  followed <- ifelse(is.na(before$days_followed), obswin,
                     before$days_followed)
  ended <- before$entry_day + followed - 1 < day
  data.frame(
    dose = before$dose, entry_day = before$entry_day,
    status = ifelse(ended, before$status, "Ongoing"),
    days_followed = ifelse(ended, before$days_followed, NA)
  )
}

test_that("every example patient got the dose the design recommended", {
  design <- default_design()
  trial <- canonical_trial(example_trial_data())
  for (i in seq_len(nrow(trial))[-1]) {
    day <- trial$entry_day[i]
    known <- normalise_trial_data(known_on(trial[seq_len(i - 1), ], day, 30),
                                  day, 30)
    fit <- escalation::fit(design$parent, to_outcomes(known))
    expect_equal(escalation::recommended_dose(fit), trial$dose[i],
                 info = sprintf("patient %d, day %d", i, day))
  }
  expect_equal(trial$dose[1], 1)
})

test_that("the example shows a pathway that changes after day 50", {
  design <- default_design()
  data <- normalise_trial_data(example_trial_data(), 50, 30)
  expect_length(validate_trial_data(example_trial_data(), 5, 50, 30), 0)
  p <- dtp_pathway(design$parent, data, 50, 30)
  expect_equal(p$runs$decision, c("S", "E"))
  expect_equal(p$runs$dose, c(2L, 3L))
  expect_equal(first_day_of(p, "E"), 63)
})
