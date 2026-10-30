# End-to-end test of the app in headless Chrome (shinytest2).
skip_on_cran()
skip_if_not_installed("shinytest2")
skip_if(is.null(suppressMessages(chromote::find_chrome())),
        "Chrome is not available")

# Shiny only renders outputs on the visible tab, so open each results tab
# before reading its output.
open_tab <- function(app, title) {
  app$run_js(sprintf(
    "[...document.querySelectorAll('[data-bs-toggle=\"tab\"]')]
       .find(a => a.textContent.trim() === '%s').click();", title
  ))
  app$wait_for_idle()
}

errors <- function(app) {
  app$get_js(
    "[...document.querySelectorAll('.shiny-output-error')]
       .filter(e => !e.classList.contains('shiny-output-error-validation'))
       .map(e => e.id)"
  )
}

test_that("the app runs on the example and on an uploaded legacy file", {
  app <- shinytest2::AppDriver$new(test_path("..", ".."), name = "app",
                                   load_timeout = 60 * 1000)
  withr::defer(app$stop())

  # Start-up: the example is fitted and every result renders.
  summary <- app$get_value(output = "summary_boxes")$html
  expect_match(summary, "Day 50<")
  expect_false(is.null(app$get_value(output = "timeline_plot")))
  open_tab(app, "Dose–toxicity curves")
  expect_false(is.null(app$get_value(output = "dose_toxicity_plot")))
  open_tab(app, "Model recommendations")
  expect_match(app$get_value(output = "model_table"), "<table")
  expect_length(errors(app), 0)

  # Upload a legacy headerless CSV: the results go stale until updated.
  app$upload_file(upload = test_path("fixtures", "legacy_test1.csv"))
  expect_match(app$get_value(output = "stale_note")$html,
               "changed since the last update")
  app$click("run")
  app$wait_for_idle()
  expect_null(app$get_value(output = "stale_note")$html)
  expect_false(identical(app$get_value(output = "summary_boxes")$html,
                         summary))
  expect_match(app$get_value(output = "model_table"), "<table")
  expect_length(errors(app), 0)
})
