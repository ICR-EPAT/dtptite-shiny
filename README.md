# dtptite-shiny

Shiny app for conducting a dose-finding trial with **TITE-CRM and Dose
Transition Pathways (DTP)**. Given the trial data observed so far, it reports
the model's recommended dose and how long to wait, with no new DLTs, before
the recommendation changes.

The statistical engine is the [`dtptite`](https://github.com/ICR-EPAT/dtptite)
package, built on the [`escalation`](https://github.com/brockk/escalation)
design grammar. This app replaces the legacy DTP-TITE-CRM app (ICR, 2021).

## Running locally

```r
# install.packages("remotes")
remotes::install_github("ICR-EPAT/dtptite")
install.packages(c("shiny", "bslib", "rhandsontable", "readxl", "ggplot2"))
shiny::runApp()
```

## Tests

```r
testthat::test_dir("tests/testthat")
```

## Layout

- `app.R`: UI and server.
- `R/`: engine helpers (auto-sourced by Shiny). Each file is written so it
  can move into `dtptite` later.
- `tests/testthat/`: unit, regression and legacy-parity tests.
