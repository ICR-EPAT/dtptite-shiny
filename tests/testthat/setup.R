# Source the app's helpers the same way shiny::runApp() does.
app_dir <- testthat::test_path("..", "..")
for (f in list.files(file.path(app_dir, "R"), pattern = "[.][Rr]$",
                     full.names = TRUE)) {
  source(f, local = FALSE)
}
