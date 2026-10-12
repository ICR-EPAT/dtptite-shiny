test_that("legacy fixtures are present", {
  fixtures <- list.files(test_path("fixtures"), pattern = "^legacy_test")
  expect_length(fixtures, 3)
})
