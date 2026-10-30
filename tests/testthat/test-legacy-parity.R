# Parity with the legacy app: the recommended dose on every projected day,
# compared with legacy's loop over dtpcrm::applied_titecrm() (see
# helper-legacy.R), on the legacy test files and on random trials.
#
# Both apps fit the same TITE-CRM model, so with the dose-skipping and
# stopping rules off they must agree exactly. With those rules on, they
# differ where the app's rules differ from legacy's; each test below
# checks that every difference is the one expected.
skip_if_not_installed("dtpcrm")

design <- list(skeleton = c(0.03, 0.07, 0.20, 0.30, 0.40), target = 0.25,
               variance = 1.34, obswin = 30)

legacy_files <- lapply(1:3, function(i) {
  path <- test_path("fixtures", sprintf("legacy_test%d.csv", i))
  list(legacy = utils::read.csv(path, header = FALSE),
       day_of_assessment = 50)
})
set.seed(2026)
trials <- c(legacy_files,
            replicate(100, random_legacy_trial(5, 30), simplify = FALSE))

compare <- function(trial, ...) {
  args <- c(list(legacy = trial$legacy,
                 day_of_assessment = trial$day_of_assessment),
            design, list(...))
  list(legacy = do.call(legacy_pathway, args),
       app = do.call(app_pathway, args))
}

test_that("the dose matches legacy on every day without skipping or stopping rules", {
  for (coherence in c(FALSE, TRUE)) {
    for (trial in trials) {
      r <- compare(trial, no_skip_esc = FALSE, no_skip_deesc = FALSE,
                   global_coherence = coherence)
      expect_identical(r$app$dose, as.integer(r$legacy$dose))
    }
  }
})

test_that("decisions match legacy, except legacy's S for a drop of two or more levels", {
  # Legacy labelled a recommendation D only one level below the current
  # dose; two or more levels below it said S.
  n_drops <- 0
  for (trial in trials) {
    r <- compare(trial, no_skip_esc = FALSE, no_skip_deesc = FALSE,
                 global_coherence = TRUE)
    current <- utils::tail(trial$legacy[[1]], 1)
    differ <- r$app$decision != r$legacy$ind
    expect_true(all(r$app$decision[differ] == "D" &
                      r$legacy$ind[differ] == "S" &
                      r$app$dose[differ] <= current - 2))
    n_drops <- n_drops + sum(differ)
  }
  expect_gt(n_drops, 0)
})

# The dose from the TITE-CRM model, after the skipping rules and global
# coherence. Legacy bounded the dose by the doses tried so far (at most one
# above the highest, one below the lowest); the app, as
# escalation::dont_skip_doses(), by the current dose (at most one either
# side).
bounded_dose <- function(model_dose, legacy, no_skip_esc, no_skip_deesc,
                         bounds = c("legacy", "app")) {
  level <- legacy[[1]]
  tox <- legacy[[2]]
  current <- utils::tail(level, 1)
  dose <- model_dose
  if (match.arg(bounds) == "legacy") {
    lower <- min(level) - 1
    upper <- max(level) + 1
  } else {
    lower <- current - 1
    upper <- current + 1
  }
  if (no_skip_esc) dose <- pmin(dose, upper)
  if (no_skip_deesc) dose <- pmax(dose, lower)
  if (mean(tox[level == current]) > design$target) {
    dose <- pmin(dose, current)
  }
  as.integer(dose)
}

test_that("skipping rules differ from legacy only by the dose they are counted from", {
  for (rules in list(c(TRUE, FALSE), c(FALSE, TRUE), c(TRUE, TRUE))) {
    n_differ <- 0
    for (trial in trials) {
      r <- compare(trial, no_skip_esc = rules[1], no_skip_deesc = rules[2],
                   global_coherence = TRUE)
      expect_identical(
        as.integer(r$legacy$dose),
        bounded_dose(r$legacy$model_dose, trial$legacy, rules[1], rules[2],
                     "legacy")
      )
      expect_identical(
        r$app$dose,
        bounded_dose(r$legacy$model_dose, trial$legacy, rules[1], rules[2],
                     "app")
      )
      n_differ <- n_differ + sum(r$app$dose != r$legacy$dose)
    }
    # The random trials include cases where the two rules disagree.
    expect_gt(n_differ, 0)
  }
})

test_that("stopping differs from legacy only where dtptite rules out a higher dose", {
  # Legacy only checked the lowest dose. dtptite's beta-binomial rule also
  # rules out any dose, and every dose above it, whose DLT rate exceeds the
  # target with at least the confidence level.
  confidence <- 0.7
  n_min <- 2
  n_differ <- 0
  for (trial in trials) {
    r <- compare(trial, no_skip_esc = FALSE, no_skip_deesc = FALSE,
                 global_coherence = TRUE, stop_confidence = confidence,
                 stop_n_min = n_min)
    level <- trial$legacy[[1]]
    tox <- trial$legacy[[2]]
    n <- tabulate(level, length(design$skeleton))
    s <- tabulate(level[tox == 1], length(design$skeleton))
    too_toxic <- n >= n_min &
      stats::pbeta(design$target, 0.1 + s, 0.9 + n - s,
                   lower.tail = FALSE) >= confidence
    expected <- as.integer(r$legacy$dose)
    if (any(too_toxic)) {
      expected <- pmin(expected, which(too_toxic)[1] - 1L)
      expected[expected == 0] <- NA
    }
    expect_identical(r$app$dose, expected)
    n_differ <- n_differ + sum(r$app$dose != r$legacy$dose, na.rm = TRUE)
  }
  expect_gt(n_differ, 0)
})
