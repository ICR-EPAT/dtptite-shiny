test_that("weights follow status: ongoing from T_A, completed and DLT full", {
  d <- data.frame(dose = c(1, 1, 2, 2), entry_day = c(1, 10, 20, 45),
                  status = c("Completed", "DLT", "Withdrawn", "Ongoing"),
                  days_followed = c(NA, 5, 9, NA))
  x <- normalise_trial_data(d, day_of_assessment = 50, obswin = 30)

  expect_equal(x$days_followed, c(30, 5, 9, 6))
  expect_equal(x$weight, c(1, 1, 9 / 30, 6 / 30))
  expect_equal(x$tox, c(0L, 1L, 0L, 0L))
  expect_equal(x$monitored, c(FALSE, FALSE, FALSE, TRUE))
})

test_that("ongoing follow-up is capped at the observation window", {
  d <- data.frame(dose = 1, entry_day = 1, status = "Ongoing")
  x <- normalise_trial_data(d, day_of_assessment = 100, obswin = 30)
  expect_equal(x$days_followed, 30)
  expect_equal(x$weight, 1)
})

test_that("patients are ordered by entry so the last row is the current dose", {
  d <- data.frame(dose = c(3, 1), entry_day = c(20, 1),
                  status = c("Ongoing", "Completed"))
  x <- normalise_trial_data(d, day_of_assessment = 30, obswin = 30)
  expect_equal(x$dose, c(1L, 3L))
  expect_equal(to_outcomes(x)$cohort, 1:2)
})

test_that("legacy five-column rows map to statuses", {
  legacy <- data.frame(c(1, 1, 1, 1, 2), c(0, 0, 1, 0, 0), c(1, 2, 3, 4, 5),
                       c(30, 4, 12, 30, 6), c(0, 0, 0, 1, 1))
  x <- from_legacy_format(legacy, obswin = 30)
  expect_equal(x$status,
               c("Completed", "Withdrawn", "DLT", "Ongoing", "Ongoing"))
  expect_equal(x$days_followed, c(NA, 4, 12, NA, NA))
  expect_true(all(x$evaluable))
})

test_that("legacy fixture files are detected and read", {
  x <- read_trial_file(test_path("fixtures", "legacy_test3.csv"), obswin = 30)
  expect_equal(x$status, c("Withdrawn", "Withdrawn", "Ongoing", "Ongoing"))
  expect_equal(x$dose, c(3, 3, 3, 3))

  x <- read_trial_file(test_path("fixtures", "legacy_test2.csv"), obswin = 30)
  expect_equal(x$status, c("Completed", "Completed", "DLT", "Ongoing"))
})

test_that("headered uploads accept aliases and any status case", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Dose level,Entry,Status,Follow-up",
               "1,1,completed,",
               "2,10,dlt,5"), path)
  x <- read_trial_file(path, obswin = 30)
  expect_equal(x$dose, c(1, 2))
  expect_equal(x$status, c("Completed", "DLT"))
  expect_equal(x$days_followed, c(NA, 5))
  # No evaluable column: everyone is evaluable.
  expect_equal(x$evaluable, c(TRUE, TRUE))
})

test_that("evaluability is read from Yes/No, TRUE/FALSE or 1/0", {
  expect_equal(parse_evaluable(c("Yes", "no", " Y ", "n", "TRUE", "false",
                                 "1", "0", "", NA, "maybe")),
               c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, NA,
                 NA, NA))
  expect_equal(parse_evaluable(c(1, 0)), c(TRUE, FALSE))
  expect_equal(parse_evaluable(c(TRUE, NA)), c(TRUE, NA))

  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("dose,entry_day,status,days_followed,Evaluable",
               "1,1,Completed,,Yes",
               "1,2,Withdrawn,4,No"), path)
  expect_equal(read_trial_file(path, obswin = 30)$evaluable, c(TRUE, FALSE))
})

test_that("uploads missing a required column are rejected", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("dose,status", "1,Ongoing"), path)
  expect_error(read_trial_file(path, obswin = 30), "entry_day")
})

test_that("Excel uploads are read", {
  skip_if_not_installed("writexl")
  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(example_trial_data(), path)
  x <- read_trial_file(path, obswin = 30)
  expect_equal(x, example_trial_data(), ignore_attr = TRUE)
})

test_that("validation reports each problem with patient numbers", {
  d <- data.frame(dose = c(1, 6, 2), entry_day = c(1, 60, 5),
                  status = c("Ongoing", "Ongoing", "Withdrawn"),
                  days_followed = c(NA, NA, NA))
  problems <- validate_trial_data(d, num_doses = 5, day_of_assessment = 50,
                                  obswin = 30)
  expect_length(problems, 3)
  expect_match(problems[1], "Patient\\(s\\) 2: dose level")
  expect_match(problems[2], "Patient\\(s\\) 2: entry day is after")
  expect_match(problems[3], "Patient\\(s\\) 3: days followed")
})

test_that("evaluability must be recorded, with at least one evaluable", {
  d <- example_trial_data()
  d$evaluable[c(2, 5)] <- NA
  problems <- validate_trial_data(d, num_doses = 5, day_of_assessment = 50,
                                  obswin = 30)
  expect_equal(problems, "Patient(s) 2, 5: evaluable must be Yes or No.")

  d$evaluable <- FALSE
  expect_equal(validate_trial_data(d, 5, 50, 30),
               "At least one patient must be evaluable.")
})

test_that("only evaluable patients are kept for the model", {
  d <- example_trial_data()
  d$evaluable[c(6, 9)] <- FALSE
  x <- normalise_trial_data(d, day_of_assessment = 50, obswin = 30)
  expect_equal(nrow(x), 9)
  kept <- evaluable_patients(x)
  expect_equal(kept$patient, c(1:5, 7:8))
  expect_true(all(kept$evaluable))
})

test_that("example data is valid for the default design", {
  expect_length(validate_trial_data(example_trial_data(), num_doses = 5,
                                    day_of_assessment = 50, obswin = 30), 0)
})

test_that("completed patients must have finished the window by T_A", {
  d <- data.frame(dose = c(1, 1), entry_day = c(21, 45),
                  status = "Completed", days_followed = NA)
  problems <- validate_trial_data(d, num_doses = 5, day_of_assessment = 50,
                                  obswin = 30)
  expect_length(problems, 1)
  expect_match(problems, "Patient\\(s\\) 2: cannot have completed")
})

test_that("header-only uploads give an empty table, not an error", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("dose,entry_day,status", path)
  x <- read_trial_file(path, obswin = 30)
  expect_equal(nrow(x), 0)
  expect_equal(validate_trial_data(x, 5, 50, 30), "Enter at least one patient.")
  expect_equal(nrow(normalise_trial_data(x[c("dose", "entry_day", "status")],
                                         50, 30)), 0)
})

test_that("empty uploads are rejected with a clear message", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("", path)
  expect_error(read_trial_file(path, obswin = 30), "The file is empty")
})
