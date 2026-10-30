# dtptite-shiny 1.0.0-rc1

* Rebuild of the legacy DTP-TITE-CRM Shiny app (2021, deployed as
  `dosepath.shinyapps.io/dtp-titecrm_test`) on top of the `dtptite` package
  (0.1.0) and the `escalation` design grammar.
* Patients can be marked as not evaluable (the Evaluable column in the
  grid, or an optional `evaluable` column in uploads). Only evaluable
  patients go into the model and the dose transition pathway; the others
  stay in the grid and are greyed out on the timeline.
* The app runs in the browser as a Shinylive build on GitHub Pages
  (<https://icr-epat.github.io/dtptite-shiny/>), so trial data stays on the
  user's computer.
* The recommended dose on each projected day is checked against the legacy
  engine (`dtpcrm::applied_titecrm()`, looped as the legacy app did) on the
  legacy test files and 100 random trials (`test-legacy-parity.R`). Without
  the skipping rules the two agree on every day. The differences are:
  * The no-skipping rules count from the current dose, as
    `escalation::dont_skip_doses()`: at most one level above or below it.
    Legacy counted from the doses tried so far (one above the highest, one
    below the lowest), so its no-skipping rule on de-escalation had no
    effect once dose 1 had been given.
  * The safety stopping rule uses the Beta prior that is set; legacy always
    used Be(0.1, 0.9). As in legacy, it monitors only the lowest dose and
    rules out no other dose.
  * A recommendation two or more levels below the current dose is labelled
    de-escalate (D); legacy labelled it stay (S).
