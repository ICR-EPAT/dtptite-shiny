test_that("entered skeletons are parsed with commas or spaces", {
  expect_equal(parse_skeleton("0.03, 0.07, 0.20, 0.3, 0.40", 5)$skeleton,
               c(0.03, 0.07, 0.2, 0.3, 0.4))
  expect_equal(parse_skeleton(" 0.1 0.2;0.3 ", 3)$skeleton, c(0.1, 0.2, 0.3))
  expect_null(parse_skeleton("0.1, 0.2", 2)$problem)
})

test_that("entered skeletons explain what is wrong", {
  expect_match(parse_skeleton("", 3)$problem, "one prior DLT probability")
  expect_match(parse_skeleton("0.1, abc, 0.3", 3)$problem, "Not a number: abc")
  expect_match(parse_skeleton("0.1, 0.2", 3)$problem,
               "Enter 3 probabilities.*got 2")
  expect_match(parse_skeleton("0.1, 0.2, 1", 3)$problem, "between 0 and 1")
  expect_match(parse_skeleton("0.1, 0.3, 0.2", 3)$problem, "increase")
  expect_match(parse_skeleton("0.1, 0.1, 0.2", 3)$problem, "increase")
  expect_null(parse_skeleton("0.1, 0.3, 0.2", 3)$skeleton)
})

test_that("skeleton comes from calibration or entry", {
  calibrated <- skeleton_from_settings("calibrate", target = 0.25,
                                       halfwidth = 0.05, prior_mtd = 3,
                                       num_doses = 5, text = "ignored")
  expect_equal(calibrated$skeleton, calibrate_skeleton(0.25, 0.05, 3, 5))
  entered <- skeleton_from_settings("enter", 0.25, 0.05, 3, 3,
                                    text = "0.1, 0.2, 0.3")
  expect_equal(entered$skeleton, c(0.1, 0.2, 0.3))

  bad <- skeleton_from_settings("calibrate", target = 0.1, halfwidth = 0.1,
                                prior_mtd = 3, num_doses = 5, text = "")
  expect_null(bad$skeleton)
  expect_match(bad$problem, "within \\(0, 1\\)")
})

test_that("stopping prior is the default or from mean and sample size", {
  expect_equal(stopping_prior(TRUE, NA, NA)[c("a", "b")],
               list(a = 0.1, b = 0.9))
  expect_equal(stopping_prior(FALSE, 0.2, 10)[c("a", "b")],
               list(a = 2, b = 8))
  expect_match(stopping_prior(FALSE, 1.2, 10)$problem, "between 0 and 1")
  expect_match(stopping_prior(FALSE, 0.2, 0)$problem, "positive")
  expect_match(stopping_prior(FALSE, NA, 10)$problem, "between 0 and 1")
})

test_that("typed design settings are checked", {
  expect_length(validate_settings(30, 50, 2), 0)
  problems <- validate_settings(obswin = 0, day_of_assessment = 2.5,
                                stop_n_min = NA)
  expect_length(problems, 3)
  expect_match(problems[1], "observation window")
  expect_match(problems[3], "Minimum patients")
})

test_that("prior variance is least informative unless set", {
  skeleton <- calibrate_skeleton(0.25, 0.05, prior_mtd = 1, num_doses = 5)
  li <- variance_from_settings("least_informative", 99, skeleton, 0.25)
  expect_equal(li$variance, least_informative_variance(skeleton, 0.25))
  expect_null(li$problem)
  expect_equal(variance_from_settings("own", 0.65, skeleton, 0.25)$variance,
               0.65)
  expect_match(variance_from_settings("own", -1, skeleton, 0.25)$problem,
               "positive")
  expect_match(variance_from_settings("own", NA, skeleton, 0.25)$problem,
               "positive")
  # Without a skeleton the skeleton's own problem is reported instead.
  none <- variance_from_settings("least_informative", NA, NULL, 0.25)
  expect_null(none$variance)
  expect_null(none$problem)
})
