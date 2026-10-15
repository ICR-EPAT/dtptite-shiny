# The UI's default inputs.
defaults <- list(
  num_doses = 5, target = 0.25, obswin = 30, prior_type = "calibrate",
  prior_mtd = 1, halfwidth = 0.0625,
  skeleton_text = "0.03, 0.07, 0.20, 0.30, 0.40",
  variance_type = "least_informative", variance = 1, no_skip_esc = TRUE,
  no_skip_deesc = TRUE, global_coherence = TRUE, stop_on = TRUE,
  stop_confidence = 0.7, stop_n_min = 2, stop_default_prior = TRUE,
  stop_prior_mean = 0.2, stop_prior_ess = 10, day_of_assessment = 50
)
start <- function(session, ...) {
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

test_that("the example is fitted on start-up with defaults", {
  shiny::testServer(app_server, {
    start(session)
    r <- result()
    expect_length(r$problems, 0)
    skeleton <- round(calibrate_skeleton(0.25, 0.0625, prior_mtd = 1,
                                         num_doses = 5), 2)
    expect_equal(r$skeleton, skeleton)
    # Least informative variance by default.
    expect_equal(r$variance, least_informative_variance(skeleton, 0.25))
    expect_s3_class(r$pathway, "dtp_pathway")
    expect_match(as.character(output$summary_boxes$html),
                 "Day 50<")
  })
})

test_that("a variance of my own is used when chosen", {
  shiny::testServer(app_server, {
    start(session)
    li <- escalation::mean_prob_tox(result()$pathway$fits$current)
    session$setInputs(variance_type = "own", variance = 0.65, run = 1)
    expect_equal(result()$variance, 0.65)
    own <- escalation::mean_prob_tox(result()$pathway$fits$current)
    expect_false(isTRUE(all.equal(li, own)))
  })
})

test_that("settings problems stop the fit and are listed", {
  shiny::testServer(app_server, {
    start(session, prior_type = "enter", skeleton_text = "0.1, 0.2")
    r <- result()
    expect_match(r$problems, "Enter 5 probabilities", all = FALSE)
    expect_null(r$pathway)
    expect_match(as.character(output$summary_boxes$html),
                 "No recommendation")
  })
})

test_that("results update only on request, with a note until then", {
  shiny::testServer(app_server, {
    start(session)
    first <- result()
    session$setInputs(target = 0.3)
    expect_identical(result(), first)
    expect_match(as.character(output$stale_note$html), "changed since")

    session$setInputs(run = 1)
    expect_equal(result()$settings$target, 0.3)
    expect_null(output$stale_note)
  })
})

test_that("adding a patient and invalid data are shown before fitting", {
  shiny::testServer(app_server, {
    start(session)
    session$setInputs(add_patient = 1)
    expect_equal(nrow(trial()), 10)
    expect_equal(trial()$entry_day[10], 50)
    expect_null(output$data_problems)

    session$setInputs(day_of_assessment = 40)
    expect_match(as.character(output$data_problems$html),
                 "after the day of assessment")
    session$setInputs(run = 1)
    expect_match(result()$problems, "after the day of assessment",
                 all = FALSE)
  })
})

test_that("the recommendation tile's text contrasts with its colour", {
  shiny::testServer(app_server, {
    start(session)
    html <- as.character(output$summary_boxes$html)
    colour <- dtp_colours$decision[[recommendation_text(result()$pathway)$decision]]
    expect_match(html, paste0("color:", text_on(colour)), fixed = TRUE)
  })
})

test_that("the default stopping prior follows the target", {
  shiny::testServer(app_server, {
    start(session)
    expect_match(as.character(output$stop_prior_preview$html),
                 "Be(0.25, 0.75)", fixed = TRUE)
    session$setInputs(target = 0.3)
    expect_match(as.character(output$stop_prior_preview$html),
                 "Be(0.3, 0.7)", fixed = TRUE)
    session$setInputs(stop_default_prior = FALSE)
    expect_match(as.character(output$stop_prior_preview$html),
                 "Be(2, 8)", fixed = TRUE)
  })
})

test_that("the next-change tile lists every change after day 50", {
  shiny::testServer(app_server, {
    start(session)
    html <- as.character(output$summary_boxes$html)
    # The example stays at dose 2, then escalates from day 63.
    expect_match(html, "Day 63")
    expect_match(html, "Escalate to dose 3")
    # The separate recommendation card is gone.
    expect_error(output$recommendation, "hasn't been defined")
  })
})

test_that("the next-change icon shows the next decision in its colour", {
  shiny::testServer(app_server, {
    start(session)
    html <- as.character(output$summary_boxes$html)
    # Next change in the example: escalate.
    # A real CSS colour declaration, not nested in another property.
    expect_match(html, paste0(";color:", dtp_colours$decision[["E"]], ";"),
                 fixed = TRUE)
    expect_no_match(html, "style:color")
  })
})

test_that("only evaluable patients go into the model and the pathway", {
  # The ongoing dose-2 patients P7-P9 are not evaluable, and P9 is the last
  # patient entered.
  trial_data <- example_trial_data()
  trial_data$evaluable[7:9] <- FALSE
  excluded <- run_model(defaults, trial_data)
  dropped <- run_model(defaults, example_trial_data()[1:6, ])

  expect_length(excluded$problems, 0)
  expect_equal(nrow(excluded$data), 9)
  expect_equal(excluded$pathway$days, dropped$pathway$days)
  expect_equal(excluded$pathway$current_dose, 2)
  # No evaluable patient is still in follow-up.
  expect_equal(excluded$pathway$complete_day, 50)
  html <- as.character(summary_boxes(excluded))
  expect_match(html, "0 patients in follow-up")
  expect_match(html, "3 not evaluable, not in the model")
})

test_that("a non-evaluable last patient does not set the current dose", {
  trial_data <- rbind(
    example_trial_data(),
    data.frame(dose = 3, entry_day = 45, status = "Ongoing",
               days_followed = NA, evaluable = FALSE)
  )
  r <- run_model(defaults, trial_data)
  expect_length(r$problems, 0)
  expect_equal(r$pathway$current_dose, 2)
  expect_equal(r$pathway$days,
               run_model(defaults, example_trial_data())$pathway$days)
})
