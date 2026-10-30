# The app's version, shown in the footer so that a result can be traced to
# the build that produced it.

app_version <- "1.0.0-rc1"

#' Git commit the app was built from
#'
#' The Shinylive build (webr/export.R) writes the commit to a `BUILD` file
#' in the app folder; a local run from a git checkout has none.
#'
#' @param path Path to the `BUILD` file.
#' @return The commit hash, or `NULL`.
build_commit <- function(path = "BUILD") {
  if (!file.exists(path)) return(NULL)
  commit <- trimws(readLines(path, n = 1, warn = FALSE))
  if (length(commit) == 0 || !nzchar(commit)) NULL else commit
}

#' Version line for the footer
#'
#' @param commit Git commit of the build, or `NULL`.
#' @return For example "Version 1.0.0 · dtptite 0.1.0 · build 1a2b3c4".
version_text <- function(commit = build_commit()) {
  parts <- c(
    paste("Version", app_version),
    paste("dtptite", utils::packageVersion("dtptite")),
    if (!is.null(commit)) paste("build", substr(commit, 1, 7))
  )
  paste(parts, collapse = " · ")
}
