# Server: trial data in a reactive value, results recomputed on "Update
# recommendation" (and once on start-up), with a note when settings or
# data have changed since.

decision_icons <- c(D = "arrow-down-circle", S = "arrow-right-circle",
                    E = "arrow-up-circle", Stop = "x-octagon")

#' Fit the design to the trial data and project the pathway
#'
#' @param settings A list of the sidebar settings (see `app_server()`).
#' @param trial Canonical trial data.
#' @return A list with `problems` (character, empty when fitted) and, when
#'   fitted, `design`, `data`, `pathway` and the settings used.
run_model <- function(settings, trial) {
  s <- settings
  skeleton <- skeleton_from_settings(s$prior_type, s$target, s$halfwidth,
                                     s$prior_mtd, s$num_doses,
                                     s$skeleton_text)
  variance <- variance_from_settings(s$variance_type, s$variance,
                                     skeleton$skeleton, s$target)
  stop_prior <- stopping_prior(s$stop_default_prior, s$stop_prior_mean,
                               s$stop_prior_ess, s$target)
  problems <- c(
    validate_settings(s$obswin, s$day_of_assessment,
                      if (isTRUE(s$stop_on)) s$stop_n_min else 0),
    skeleton$problem,
    variance$problem,
    if (isTRUE(s$stop_on)) stop_prior$problem
  )
  if (length(problems) == 0) {
    problems <- validate_trial_data(trial, s$num_doses, s$day_of_assessment,
                                    s$obswin)
  }
  if (length(problems)) {
    return(list(problems = problems))
  }

  tryCatch({
    design <- build_design(
      skeleton = skeleton$skeleton, target = s$target, obswin = s$obswin,
      variance = variance$variance, no_skip_esc = s$no_skip_esc,
      no_skip_deesc = s$no_skip_deesc,
      global_coherence = s$global_coherence,
      stop_confidence = if (isTRUE(s$stop_on)) s$stop_confidence,
      stop_n_min = s$stop_n_min, stop_a = stop_prior$a,
      stop_b = stop_prior$b
    )
    data <- normalise_trial_data(trial, s$day_of_assessment, s$obswin)
    pathway <- dtp_pathway(design$parent, data, s$day_of_assessment,
                           s$obswin)
    list(problems = character(), design = design, data = data,
         pathway = pathway, skeleton = skeleton$skeleton,
         variance = variance$variance, settings = s)
  }, error = function(e) {
    list(problems = paste("The model could not be fitted:",
                          conditionMessage(e)))
  })
}

summary_boxes <- function(result) {
  p <- result$pathway
  msg <- recommendation_text(p)
  now <- msg$decision
  colour <- dtp_colours$decision[[now]]
  first <- p$runs[1, ]
  later <- p$runs[-1, , drop = FALSE]
  pending <- sum(result$data$monitored &
                   result$data$weight < 1)

  bslib::layout_columns(
    fill = FALSE,
    bslib::value_box(
      title = sprintf("Day %d recommendation", p$day_of_assessment),
      value = if (now == "Stop") "Stop the trial" else
        sprintf("Dose %d", first$dose),
      showcase = bsicons::bs_icon(decision_icons[[now]]),
      theme = bslib::value_box_theme(bg = colour, fg = text_on(colour)),
      htmltools::p(if (now == "Stop") "The lowest dose is too toxic." else
        paste0(toupper(substring(decision_labels[[now]], 1, 1)),
               substring(tolower(decision_labels[[now]]), 2),
               sprintf(" (current dose %d)", p$current_dose)))
    ),
    bslib::value_box(
      title = "Next change if no new DLTs",
      value = if (nrow(later)) sprintf("Day %d", later$from_day[1]) else
        "None",
      # Neutral tile; the icon shows the next decision in its colour, as on
      # the pathway bar.
      showcase = if (nrow(later)) {
        bsicons::bs_icon(
          decision_icons[[later$decision[1]]],
          color = dtp_colours$decision[[later$decision[1]]]
        )
      } else {
        bsicons::bs_icon("calendar-check")
      },
      theme = "light",
      # Every later change, so nothing is lost when there are several.
      if (nrow(later)) {
        lapply(seq_len(nrow(later)), function(i) {
          what <- describe_dose(later$dose[i], p$current_dose)
          htmltools::p(class = "mb-0", if (i == 1) {
            paste0(toupper(substring(what, 1, 1)), substring(what, 2))
          } else {
            sprintf("Then from day %d: %s", later$from_day[i], what)
          })
        })
      } else {
        htmltools::p("The recommendation holds.")
      }
    ),
    bslib::value_box(
      title = "DLT follow-up completes",
      value = if (p$complete_day == p$day_of_assessment) "Complete" else
        sprintf("Day %d", p$complete_day),
      showcase = bsicons::bs_icon("hourglass-split"),
      theme = "light",
      htmltools::p(sprintf("%d patient%s still in follow-up", pending,
                           if (pending == 1) "" else "s"))
    )
  )
}

problems_alert <- function(problems, title) {
  htmltools::div(
    class = "alert alert-warning mb-0", role = "alert",
    htmltools::strong(title),
    htmltools::tags$ul(class = "mb-0",
                       lapply(problems, htmltools::tags$li))
  )
}

#' The app's server
app_server <- function(input, output, session) {
  trial <- shiny::reactiveVal(canonical_trial(example_trial_data()))
  set_trial <- function(x) {
    x <- canonical_trial(x)
    if (!identical(x, trial())) trial(x)
  }

  settings <- shiny::reactive({
    ids <- c("num_doses", "target", "obswin", "prior_type", "prior_mtd",
             "halfwidth", "skeleton_text", "variance_type", "variance",
             "no_skip_esc",
             "no_skip_deesc", "global_coherence", "stop_on",
             "stop_confidence", "stop_n_min", "stop_default_prior",
             "stop_prior_mean", "stop_prior_ess", "day_of_assessment")
    stats::setNames(lapply(ids, function(id) input[[id]]), ids)
  })

  # The half-width defaults to a quarter of the target.
  shiny::observeEvent(input$target, ignoreInit = TRUE, {
    shiny::updateNumericInput(session, "halfwidth",
                              value = input$target / 4)
  })

  # The MTD guess cannot exceed the number of doses (legacy kept the old
  # maximum until the prior type was switched).
  shiny::observeEvent(input$num_doses, {
    shiny::updateSliderInput(session, "prior_mtd", max = input$num_doses,
                             value = min(input$prior_mtd, input$num_doses))
  })

  # --- Trial data --------------------------------------------------------
  grid_view <- shiny::reactive({
    shiny::req(input$obswin, input$day_of_assessment)
    grid_frame(trial(), input$day_of_assessment, input$obswin)
  })
  output$grid <- rhandsontable::renderRHandsontable({
    render_grid(grid_view(), input$num_doses)
  })
  shiny::observeEvent(input$grid, {
    set_trial(trial_from_grid(rhandsontable::hot_to_r(input$grid)))
  })
  shiny::observeEvent(input$add_patient, {
    set_trial(add_patient(trial(), input$day_of_assessment))
  })
  shiny::observeEvent(input$load_example, {
    set_trial(example_trial_data())
  })
  shiny::observeEvent(input$upload, {
    loaded <- tryCatch(
      read_trial_file(input$upload$datapath, input$upload$name,
                      obswin = input$obswin),
      error = function(e) e
    )
    if (inherits(loaded, "error")) {
      bslib::show_toast(bslib::toast(
        conditionMessage(loaded), header = "Could not read the file",
        type = "danger"
      ))
    } else {
      set_trial(loaded)
      bslib::show_toast(bslib::toast(
        sprintf("%d patients loaded from %s.", nrow(loaded),
                input$upload$name),
        type = "success"
      ))
    }
  })
  output$data_problems <- shiny::renderUI({
    shiny::req(input$obswin, input$day_of_assessment, input$num_doses)
    problems <- validate_trial_data(trial(), input$num_doses,
                                    input$day_of_assessment, input$obswin)
    if (length(problems)) {
      problems_alert(problems, "Please check the trial data:")
    }
  })
  write_csv <- function(x, file) utils::write.csv(x, file, row.names = FALSE,
                                                  na = "")
  output$download_data <- shiny::downloadHandler(
    filename = function() sprintf("dtp-titecrm-data-%s.csv", Sys.Date()),
    content = function(file) write_csv(trial(), file)
  )
  output$download_template <- shiny::downloadHandler(
    filename = "dtp-titecrm-template.csv",
    content = function(file) write_csv(example_trial_data(), file)
  )

  # --- Prior previews ----------------------------------------------------
  current_skeleton <- shiny::reactive({
    s <- settings()
    skeleton_from_settings(s$prior_type, s$target, s$halfwidth,
                           s$prior_mtd, s$num_doses, s$skeleton_text)
  })
  output$skeleton_preview <- shiny::renderUI({
    sk <- current_skeleton()
    if (!is.null(sk$problem)) {
      htmltools::div(class = "form-text text-danger mb-3", sk$problem)
    } else {
      htmltools::div(class = "form-text form-text-skeleton mb-3",
                     "Skeleton: ", paste(format(sk$skeleton, nsmall = 2),
                                         collapse = ", "))
    }
  })
  least_informative <- shiny::reactive({
    variance_from_settings("least_informative", NA,
                           current_skeleton()$skeleton, input$target)
  })
  output$variance_preview <- shiny::renderUI({
    shiny::req(input$variance_type == "least_informative")
    v <- least_informative()
    if (!is.null(v$problem)) {
      htmltools::div(class = "form-text text-danger mb-3", v$problem)
    } else if (!is.null(v$variance)) {
      htmltools::div(class = "form-text form-text-skeleton mb-3",
                     sprintf("Variance: %.3f, calculated from the skeleton.",
                             v$variance))
    }
  })
  # Start "Set my own" from the calculated value.
  shiny::observeEvent(input$variance_type, ignoreInit = TRUE, {
    v <- shiny::isolate(least_informative()$variance)
    if (input$variance_type == "own" && !is.null(v)) {
      shiny::updateNumericInput(session, "variance", value = round(v, 3))
    }
  })
  output$stop_prior_preview <- shiny::renderUI({
    p <- stopping_prior(input$stop_default_prior, input$stop_prior_mean,
                        input$stop_prior_ess, input$target)
    htmltools::div(
      class = if (is.null(p$problem)) "form-text" else
        "form-text text-danger",
      p$problem %||% sprintf("Be(%g, %g)", p$a, p$b)
    )
  })

  # --- Results -----------------------------------------------------------
  result <- shiny::reactiveVal()
  ran_with <- shiny::reactiveVal()
  shiny::observeEvent(input$run, ignoreNULL = FALSE, {
    snapshot <- list(settings = settings(), trial = trial())
    result(run_model(snapshot$settings, snapshot$trial))
    ran_with(snapshot)
  })

  output$stale_note <- shiny::renderUI({
    shiny::req(ran_with())
    if (!identical(ran_with(), list(settings = settings(), trial = trial()))) {
      htmltools::div(
        class = "small text-warning-emphasis mt-2",
        bsicons::bs_icon("exclamation-triangle"),
        "Settings or data changed since the last update."
      )
    }
  })

  fitted <- shiny::reactive({
    r <- result()
    shiny::req(r, length(r$problems) == 0)
    r
  })

  output$summary_boxes <- shiny::renderUI({
    r <- result()
    shiny::req(r)
    if (length(r$problems)) {
      return(problems_alert(r$problems,
                            "No recommendation: please fix the following."))
    }
    summary_boxes(r)
  })
  output$timeline_plot <- ggiraph::renderGirafe({
    r <- fitted()
    as_girafe(
      plot_timeline(r$data, r$settings$day_of_assessment,
                    r$settings$obswin, r$settings$num_doses, r$pathway),
      height = timeline_height(nrow(r$data) + 1)
    )
  })
  output$dose_toxicity_plot <- ggiraph::renderGirafe({
    r <- fitted()
    as_girafe(
      plot_dose_toxicity(r$pathway, r$skeleton, r$variance,
                         r$settings$target),
      height = dose_toxicity_height(r$pathway)
    )
  })
  output$model_table <- shiny::renderTable(
    format_model_table(model_table(fitted()$pathway)),
    striped = TRUE, hover = TRUE, width = "100%", align = "lcl"
  )
}
