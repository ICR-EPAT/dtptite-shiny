# dtptite-shiny

Shiny app for conducting a dose-finding trial with **TITE-CRM and Dose
Transition Pathways (DTP)**. Given the trial data observed so far, it reports
the model's recommended dose and how long to wait, with no new DLTs, before
the recommendation changes.

The statistical engine is the [`dtptite`](https://github.com/ICR-EPAT/dtptite)
package, built on the [`escalation`](https://github.com/brockk/escalation)
design grammar. This app replaces the legacy DTP-TITE-CRM app (ICR, 2021).

## Using the app

The app runs in the browser at <https://icr-epat.github.io/dtptite-shiny/>.
It is a [Shinylive](https://posit-dev.github.io/r-shinylive/) build: R runs
inside the browser, so trial data you enter or upload never leaves your
computer. The first load takes up to a minute while R starts.

## Running locally

```r
install.packages(c("dtptite", "shiny", "bslib", "bsicons", "rhandsontable",
                   "readxl", "ggplot2", "ggiraph"))
shiny::runApp()
```

## Tests

```r
testthat::test_dir("tests/testthat")
```

The end-to-end test (`test-app-browser.R`) drives the app in headless
Chrome with `shinytest2`. It runs when `NOT_CRAN=true` and Chrome is
available, as in the `tests` workflow on every pull request.

## Layout

- `app.R`: UI and server.
- `R/`: engine helpers (auto-sourced by Shiny). Each file is written so it
  can move into `dtptite` later.
- `tests/testthat/`: unit, regression and legacy-parity tests. The
  parity tests compare with `dtpcrm`, the legacy engine, when installed.
- `webr/`: the Shinylive build (`export.R`) and its smoke test
  (`smoke-test.R`), run by `.github/workflows/shinylive.yml`. Pushes to
  `main` publish the build to GitHub Pages.

## License

GPL (>= 3), the same as `dtptite`. Copyright The Institute of Cancer
Research.
