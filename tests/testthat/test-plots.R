# Example trial (30-day window, assessed on day 50) with the dtptite
# skeleton and target.
skeleton <- c(0.124, 0.25, 0.398, 0.542, 0.666)
target <- 0.25
design <- build_design(skeleton = skeleton, target = target, obswin = 30)
trial <- normalise_trial_data(example_trial_data(), day_of_assessment = 50,
                              obswin = 30)
pathway <- dtp_pathway(design$parent, trial, day_of_assessment = 50,
                       obswin = 30)

layer_data_of <- function(plot, geom_class) {
  built <- ggplot2::ggplot_build(plot)
  i <- which(vapply(plot$layers, function(l) inherits(l$geom, geom_class),
                    logical(1)))
  built$data[[i[1]]]
}

test_that("timeline ends follow-up on the last day observed", {
  d <- timeline_data(trial, day_of_assessment = 50, obswin = 30)
  # Entry day counts as day 1 of follow-up.
  expect_equal(d$last_day[d$status == "Completed"],
               d$entry_day[d$status == "Completed"] + 29)
  expect_equal(d$last_day[d$status == "DLT"], 18 + 12 - 1)
  expect_equal(d$last_day[d$status == "Withdrawn"], 22 + 9 - 1)
  expect_equal(d$last_day[d$status == "Ongoing"], c(50, 50))
  expect_equal(d$window_end[d$status == "Ongoing"], c(34, 41) + 29)
  expect_match(d$tooltip[5], "DLT: 12 of 30 days \\(weight 1.00\\)")
})

test_that("study timeline has one row per patient", {
  p <- plot_timeline(trial, 50, 30, num_doses = 5)
  y <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y
  expect_equal(y$get_labels(), paste0("P", 1:8))
})

test_that("pathway adds a Next row coloured by decision", {
  p <- plot_timeline(trial, 50, 30, num_doses = 5, pathway = pathway)
  y <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y
  expect_equal(y$get_labels(), c(paste0("P", 1:8), "Next"))

  band <- layer_data_of(p, "GeomRect")
  expect_equal(nrow(band), nrow(pathway$runs))
  expect_equal(band$fill,
               unname(dtp_colours$decision[pathway$runs$decision]))
  expect_equal(band$xmin, pathway$runs$from_day)
})

test_that("dose colours are fixed by level, whichever doses were given", {
  p <- plot_timeline(trial, 50, 30, num_doses = 5)
  seg <- layer_data_of(p, "GeomSegment")
  expect_equal(seg$colour, dose_colours(5)[trial$dose])

  only_dose_3 <- trial[trial$dose == 3, ]
  only_dose_3$patient <- seq_len(nrow(only_dose_3))
  seg3 <- layer_data_of(plot_timeline(only_dose_3, 50, 30, num_doses = 5),
                        "GeomSegment")
  expect_equal(unique(seg3$colour), dose_colours(5)[3])
})

test_that("projected follow-up is drawn only for Ongoing patients", {
  p <- plot_timeline(trial, 50, 30, num_doses = 5)
  dashed <- ggplot2::ggplot_build(p)$data[[2]]
  # The y scale is reversed so patient 1 is on top.
  expect_equal(-dashed$y, trial$patient[trial$status == "Ongoing"])
  expect_equal(dashed$xend, c(34, 41) + 29)
})

test_that("dose-toxicity data has one panel per key day, with the prior", {
  d <- dose_toxicity_data(pathway, skeleton, 1.34, target, level = 0.9)
  fits <- key_fits(pathway)
  expect_equal(nlevels(d$panel), length(fits))
  expect_equal(unique(d$curve[d$role != "prior"]),
               vapply(fits, `[[`, character(1), "curve"))
  expect_match(levels(d$panel)[1], "Current \\(day 50\\)\nRecommended dose 2")
  for (panel in levels(d$panel)) {
    expect_equal(d$prob_tox[d$panel == panel & d$role == "prior"], skeleton)
  }

  current <- d[d$role == "current", ]
  expect_equal(current$prob_tox, escalation::mean_prob_tox(fits[[1]]$fit))
  expect_equal(current$dose[current$recommended],
               escalation::recommended_dose(fits[[1]]$fit))
  # Same numbers as the model table.
  tbl <- model_table(pathway, level = 0.9)
  rec <- d[d$recommended, ]
  expect_equal(rec$lower, tbl$lower)
  expect_equal(rec$upper, tbl$upper)
})

test_that("prior has its credible interval under the empiric model", {
  d <- dose_toxicity_data(pathway, skeleton, variance = 1.34, target)
  prior <- d[d$role == "prior" & d$panel == levels(d$panel)[1], ]
  z <- stats::qnorm(0.95)
  expect_equal(prior$lower, skeleton^exp(z * sqrt(1.34)))
  expect_equal(prior$upper, skeleton^exp(-z * sqrt(1.34)))
  expect_true(all(prior$lower < skeleton & skeleton < prior$upper))
  expect_match(prior$tooltip[1], "90% CrI")

  # A smaller prior variance narrows the band.
  narrow <- dose_toxicity_data(pathway, skeleton, variance = 0.25, target)
  expect_true(all(narrow$upper[narrow$role == "prior"] <
                    d$upper[d$role == "prior"]))
})

test_that("bands follow the credible intervals; only recommended is filled", {
  # Review of #7: intervals sat beside their points, and the prior was
  # drawn with filled points too.
  plot <- plot_dose_toxicity(pathway, skeleton, 1.34, target)
  built <- ggplot2::ggplot_build(plot)
  d <- dose_toxicity_data(pathway, skeleton, 1.34, target)
  post <- d[d$role != "prior", ]

  ribbons <- built$data[[2]]
  expect_equal(ribbons$ymin, post$lower)
  expect_equal(ribbons$ymax, post$upper)
  expect_equal(built$data[[1]]$ymin, d$lower[d$role == "prior"])

  prior_points <- built$data[[5]]
  expect_true(all(prior_points$fill == "white"))
  n <- length(plot$layers)
  expect_true(all(built$data[[n - 1]]$fill == "white"))
  filled <- built$data[[n]]
  expect_equal(nrow(filled), sum(post$recommended))
  role_colour <- c(current = dtp_colours$accent, change = "#CC79A7",
                   complete = "#56B4E9")
  expect_equal(as.character(filled$fill),
               unname(role_colour[post$role[post$recommended]]))
})

test_that("dose-toxicity legend uses symbols, not a caption", {
  plot <- plot_dose_toxicity(pathway, skeleton, 1.34, target)
  expect_null(plot$labels$caption)
  lines <- ggplot2::get_guide_data(plot, "linetype")
  expect_equal(lines$.label, c("Prior, 90% CrI", "Posterior mean, 90% CrI",
                               "Target 0.25"))
  expect_equal(lines$linetype, c("22", "solid", "dotted"))
  expect_equal(ggplot2::get_guide_data(plot, "shape")$.label,
               "Recommended dose")
})

test_that("an MTD estimate the rules do not allow is named in its tooltip", {
  # Three patients at dose 1 without DLT: the model's closest-to-target dose
  # is above dose 2, but no-skip escalation recommends dose 2.
  x <- normalise_trial_data(
    data.frame(dose = 1, entry_day = c(1, 5, 9), status = "Completed"),
    day_of_assessment = 50, obswin = 30
  )
  p <- dtp_pathway(design$parent, x, 50, 30)
  d <- dose_toxicity_data(p, skeleton, 1.34, target)
  current <- d[d$role == "current", ]
  expect_equal(current$dose[current$recommended], 2)
  mtd <- current$dose[current$mtd_estimate]
  expect_gt(mtd, 2)
  expect_match(current$tooltip[mtd], "MTD estimate")
  expect_match(current$tooltip[2], "Recommended dose")
})

test_that("stopped fits are plotted without a recommended dose", {
  stopping <- build_design(skeleton = skeleton, target = target, obswin = 56,
                           stop_confidence = 0.88)
  x <- normalise_trial_data(
    data.frame(dose = 1, entry_day = c(1, 10, 20, 45),
               status = c("DLT", "DLT", "DLT", "Ongoing"),
               days_followed = c(20, 15, 30, NA)),
    day_of_assessment = 50, obswin = 56
  )
  p <- dtp_pathway(stopping$parent, x, 50, 56)
  d <- dose_toxicity_data(p, skeleton, 1.34, target)
  expect_false(any(d$recommended))
  expect_match(levels(d$panel)[1], "Stop: lowest dose too toxic")
  expect_false(anyNA(d$prob_tox))
  stopped <- plot_dose_toxicity(p, skeleton, 1.34, target)
  expect_no_warning(ggplot2::ggplot_build(stopped))
  expect_null(ggplot2::get_guide_data(stopped, "shape"))
  band <- layer_data_of(plot_timeline(x, 50, 56, 5, p), "GeomRect")
  expect_equal(band$fill, unname(dtp_colours$decision["Stop"]))
})

test_that("plots render as ggiraph widgets with tooltips", {
  w <- as_girafe(plot_timeline(trial, 50, 30, 5, pathway),
                 height = timeline_height(9))
  expect_s3_class(w, "girafe")
  expect_match(w$x$html, "Patient 1")
  expect_match(w$x$html, "if no new DLTs are observed")

  w2 <- as_girafe(plot_dose_toxicity(pathway, skeleton, 1.34, target),
                  height = dose_toxicity_height(pathway))
  expect_match(w2$x$html, "Recommended dose")
})
