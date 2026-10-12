# Page layout (bslib, Bootstrap 5).
#
# Sidebar: an accordion with one panel per group of design settings, and
# the update button. Main: trial data, then the recommendation (value
# boxes) and a tabbed card with the plots and the model table.

#' Neutral light theme: system fonts, navy accent, ICR logo grey
app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF", fg = dtp_colours$ink,
    primary = dtp_colours$accent, secondary = dtp_colours$muted,
    base_font = bslib::font_collection(
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue",
      "Arial", "sans-serif"
    ),
    "font-size-base" = "0.95rem"
  ) |>
    bslib::bs_add_rules("
      .table, .handsontable { font-variant-numeric: tabular-nums; }
      .app-title img { height: 32px; }
      .accordion-button { font-weight: 600; }
      .form-text-skeleton { font-variant-numeric: tabular-nums; }
      /* fillable = FALSE leaves the main area a block, where bslib's gap
         spacing (which replaces card margins) has no effect. */
      .bslib-page-main.bslib-gap-spacing {
        display: flex; flex-direction: column;
      }
    ")
}

# Label with an info icon showing `help` on hover or focus.
with_help <- function(label, help) {
  htmltools::tagList(
    label, " ",
    bslib::tooltip(
      bsicons::bs_icon("info-circle", title = "Help",
                       class = "text-secondary"),
      help
    )
  )
}

design_panel <- function() {
  bslib::accordion_panel(
    "Design", value = "design", icon = bsicons::bs_icon("sliders"),
    shiny::sliderInput("num_doses", "Number of dose levels", min = 2,
                       max = 10, value = 5, step = 1, ticks = FALSE),
    shiny::sliderInput(
      "target",
      with_help("Target DLT rate",
                "The DLT probability the design aims for at the MTD."),
      min = 0.05, max = 0.5, value = 0.25, step = 0.01, ticks = FALSE
    ),
    shiny::numericInput(
      "obswin",
      with_help("DLT observation window (days)",
                "Each patient is followed for DLT for this many days, counting the entry day as day 1."),
      value = 30, min = 1, step = 1
    )
  )
}

prior_panel <- function() {
  bslib::accordion_panel(
    "Prior", value = "prior", icon = bsicons::bs_icon("graph-up"),
    shiny::radioButtons(
      "prior_type", "Prior DLT probabilities (skeleton)",
      choices = c("Calibrate" = "calibrate", "Enter my own" = "enter"),
      inline = TRUE
    ),
    shiny::conditionalPanel(
      "input.prior_type == 'calibrate'",
      shiny::sliderInput(
        "prior_mtd",
        with_help("Prior guess of the MTD (dose level)",
                  "The dose whose prior DLT probability equals the target."),
        min = 1, max = 5, value = 1, step = 1, ticks = FALSE
      ),
      shiny::numericInput(
        "halfwidth",
        with_help("Half-width of the indifference interval",
                  "Doses with DLT probability within target \u00b1 half-width are considered acceptable (Lee and Cheung, 2009). Defaults to a quarter of the target, and resets to it when the target changes."),
        value = 0.0625, min = 0.005, max = 0.25, step = 0.005
      )
    ),
    shiny::conditionalPanel(
      "input.prior_type == 'enter'",
      shiny::textInput(
        "skeleton_text", "One probability per dose, increasing",
        value = "0.03, 0.07, 0.20, 0.30, 0.40"
      )
    ),
    shiny::uiOutput("skeleton_preview"),
    shiny::radioButtons(
      "variance_type",
      with_help("Prior variance of the model parameter",
                "Empiric model: DLT probability = skeleton ^ exp(\u03b2), \u03b2 ~ N(0, variance). The least informative variance makes every dose about equally likely to be the MTD a priori (Lee and Cheung, 2011); a smaller variance gives a more informative prior."),
      choices = c("Least informative" = "least_informative",
                  "Set my own" = "own"),
      inline = TRUE
    ),
    shiny::conditionalPanel(
      "input.variance_type == 'own'",
      shiny::numericInput("variance", "Variance", value = 1, min = 0.01,
                          step = 0.05)
    ),
    shiny::uiOutput("variance_preview")
  )
}

rules_panel <- function() {
  bslib::accordion_panel(
    "Escalation rules", value = "rules",
    icon = bsicons::bs_icon("signpost-split"),
    bslib::input_switch("no_skip_esc",
                        "No skipping of untried doses when escalating",
                        value = TRUE),
    bslib::input_switch(
      "no_skip_deesc",
      with_help("No skipping of doses when de-escalating",
                "De-escalate at most one level below the last dose given."),
      value = TRUE
    ),
    bslib::input_switch(
      "global_coherence",
      with_help("No escalation above the target DLT rate",
                "Do not escalate if the observed DLT rate at the current dose is above the target."),
      value = TRUE
    )
  )
}

stopping_panel <- function() {
  bslib::accordion_panel(
    "Safety stopping", value = "stopping",
    icon = bsicons::bs_icon("sign-stop"),
    bslib::input_switch(
      "stop_on",
      with_help("Stop if the lowest dose is too toxic",
                "Beta-binomial rule: stop when P(DLT rate at dose 1 > target | data) exceeds the confidence level."),
      value = TRUE
    ),
    shiny::conditionalPanel(
      "input.stop_on",
      shiny::sliderInput("stop_confidence", "Confidence level", min = 0.5,
                         max = 0.99, value = 0.7, step = 0.01,
                         ticks = FALSE),
      shiny::numericInput(
        "stop_n_min",
        "Minimum patients at the lowest dose before stopping applies",
        value = 2, min = 0, step = 1
      ),
      bslib::input_switch(
        "stop_default_prior",
        "Weakly informative prior Be(target, 1 \u2212 target), effective sample size 1",
        value = TRUE
      ),
      shiny::conditionalPanel(
        "!input.stop_default_prior",
        bslib::layout_columns(
          shiny::numericInput("stop_prior_mean", "Prior mean DLT rate",
                              value = 0.2, min = 0.01, max = 0.99,
                              step = 0.05),
          shiny::numericInput("stop_prior_ess", "Effective sample size",
                              value = 10, min = 0.1, step = 1)
        )
      ),
      shiny::uiOutput("stop_prior_preview")
    )
  )
}

data_card <- function() {
  bslib::card(
    bslib::card_header(
      class = "d-flex align-items-center gap-2 flex-wrap",
      bsicons::bs_icon("table"), "Trial data",
      htmltools::div(
        class = "ms-auto d-flex gap-2",
        shiny::actionButton("add_patient", "Add patient",
                            icon = shiny::icon("plus"),
                            class = "btn-sm btn-outline-primary"),
        bslib::popover(
          shiny::actionButton("file_menu", "Files",
                              icon = shiny::icon("file-arrow-up"),
                              class = "btn-sm btn-outline-secondary"),
          title = "Load or save trial data",
          shiny::fileInput("upload", "Upload CSV or Excel",
                           accept = c(".csv", ".xlsx", ".xls")),
          htmltools::p(
            class = "small text-secondary",
            "Columns: dose, entry_day, status (Ongoing, Completed, DLT,",
            "Withdrawn), days_followed (for DLT and Withdrawn). The legacy",
            "app's five-column CSV is also accepted."
          ),
          htmltools::div(
            class = "d-flex flex-column gap-2",
            shiny::downloadButton("download_data", "Save current data",
                                  class = "btn-sm"),
            shiny::downloadButton("download_template", "Download template",
                                  class = "btn-sm btn-outline-secondary"),
            shiny::actionButton("load_example", "Load example data",
                                class = "btn-sm btn-outline-secondary")
          )
        )
      )
    ),
    bslib::card_body(
      bslib::layout_columns(
        col_widths = c(4, 8),
        shiny::numericInput(
          "day_of_assessment",
          with_help("Day of assessment",
                    "Study day of this analysis (T_A). Ongoing patients have been followed up to this day."),
          value = 50, min = 1, step = 1
        ),
        htmltools::p(
          class = "small text-secondary mb-0 align-self-center",
          "Edit cells directly; right-click a row to insert or remove it.",
          "Days followed are filled in for Ongoing and Completed patients."
        )
      ),
      rhandsontable::rHandsontableOutput("grid", height = "auto"),
      shiny::uiOutput("data_problems")
    )
  )
}

results_tabs <- function() {
  bslib::navset_card_underline(
    full_screen = TRUE,
    bslib::nav_panel(
      "Dose transition pathway",
      ggiraph::girafeOutput("timeline_plot", height = "auto")
    ),
    bslib::nav_panel(
      "Dose–toxicity curves",
      ggiraph::girafeOutput("dose_toxicity_plot", height = "auto")
    ),
    bslib::nav_panel(
      "Model recommendations",
      shiny::tableOutput("model_table")
    )
  )
}

#' The app's page
app_ui <- function() {
  bslib::page_sidebar(
    title = htmltools::span(
      class = "app-title d-flex align-items-center gap-3",
      htmltools::img(src = "icr_logo.png",
                     alt = "The Institute of Cancer Research"),
      htmltools::span("TITE-CRM with Dose Transition Pathways")
    ),
    window_title = "DTP-TITE-CRM",
    theme = app_theme(),
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 360,
      bslib::accordion(
        id = "settings", open = "design", multiple = FALSE,
        design_panel(), prior_panel(), rules_panel(), stopping_panel()
      ),
      bslib::input_task_button("run", "Update recommendation",
                               icon = shiny::icon("rotate"),
                               label_busy = "Updating..."),
      shiny::uiOutput("stale_note")
    ),
    data_card(),
    shiny::uiOutput("summary_boxes"),
    results_tabs(),
    htmltools::p(
      class = "small text-secondary text-center mt-2",
      "DTP-TITE-CRM, The Institute of Cancer Research. Built on the",
      htmltools::a("dtptite", href = "https://github.com/ICR-EPAT/dtptite"),
      "and",
      htmltools::a("escalation",
                   href = "https://github.com/brockk/escalation"),
      "R packages."
    )
  )
}
