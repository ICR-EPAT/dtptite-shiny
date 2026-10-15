# dtptite-shiny (development)

* Rebuild of the legacy DTP-TITE-CRM Shiny app (2021, deployed as
  `dosepath.shinyapps.io/dtp-titecrm_test`) on top of the `dtptite` package
  (v0.1.0-rc1) and the `escalation` design grammar.
* Patients can be marked as not evaluable (the Evaluable column in the
  grid, or an optional `evaluable` column in uploads). Only evaluable
  patients go into the model and the dose transition pathway; the others
  stay in the grid and are greyed out on the timeline.
