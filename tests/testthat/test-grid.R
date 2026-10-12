example <- canonical_trial(example_trial_data())

test_that("canonical trial data is typed and sorted by entry day", {
  shuffled <- example_trial_data()[c(8, 2, 9, 5, 1, 3, 4, 7, 6), ]
  expect_equal(canonical_trial(shuffled), example)
  expect_type(example$dose, "double")
  expect_type(example$status, "character")
  # Days followed are only kept where they are entered.
  x <- example_trial_data()
  x$days_followed[1] <- 12
  expect_true(is.na(canonical_trial(x)$days_followed[1]))
})

test_that("grid shows derived days and weights, read-only where derived", {
  g <- grid_frame(example, day_of_assessment = 50, obswin = 30)
  expect_equal(names(g), grid_columns)
  expect_equal(g$`Days followed`, c(30, 30, 30, 30, 13, 20, 15, 14, 9))
  expect_equal(g$Weight,
               c(1, 1, 1, 1, 13 / 30, 1, 15 / 30, 14 / 30, 9 / 30))

  hot <- render_grid(g, num_doses = 5)
  read_only <- vapply(hot$x$cell, function(c) c$row, numeric(1))
  # rhandsontable rows are 0-based: Completed P1-P4, Ongoing P7-P9.
  expect_equal(sort(read_only), c(0, 1, 2, 3, 6, 7, 8))
})

test_that("grid round-trips to the same trial data", {
  g <- grid_frame(example, day_of_assessment = 50, obswin = 30)
  expect_equal(trial_from_grid(g), example)
})

test_that("switching to DLT keeps the days the grid showed", {
  g <- grid_frame(example, day_of_assessment = 50, obswin = 30)
  g$Status[7] <- "DLT"
  back <- trial_from_grid(g)
  expect_equal(back$status[7], "DLT")
  expect_equal(back$days_followed[7], 15)
})

test_that("incomplete rows keep the grid usable", {
  x <- rbind(example, data.frame(dose = NA, entry_day = NA, status = NA,
                                 days_followed = NA))
  g <- grid_frame(canonical_trial(x), 50, 30)
  expect_equal(nrow(g), 10)
  expect_true(is.na(g$Weight[10]))
  # Entry after T_A gives a negative weight: shown as missing.
  y <- example
  y$entry_day[9] <- 60
  expect_true(is.na(grid_frame(canonical_trial(y), 50, 30)$Weight[9]))
})

test_that("a new patient enters on T_A at the last dose", {
  added <- add_patient(example, day_of_assessment = 50)
  expect_equal(nrow(added), 10)
  expect_equal(added$dose[10], 2)
  expect_equal(added$entry_day[10], 50)
  expect_equal(added$status[10], "Ongoing")
  first <- add_patient(canonical_trial(example[0, ]), 1)
  expect_equal(first$dose, 1)
})
