# Build the Shinylive (browser) version of the app into `_site/`.
#
# webR cannot install two packages the app needs: dtptite (on CRAN, but
# not in the webR repository) and trialr (escalation imports it; it needs
# rstan). Both are installed
# here as pure-R packages and added to the export's package bundle, where
# Shinylive installs them like any other package. trialr is a stand-in
# whose functions stop with an error (see webr/trialr); the app never
# calls them.
#
# Run from the repository root: Rscript webr/export.R
dtptite_ref <- Sys.getenv("DTPTITE_REF", "v0.1.0")

stage <- file.path(tempdir(), "app")
lib <- file.path(tempdir(), "webr-lib")
unlink(c(stage, lib), recursive = TRUE)
dir.create(stage)
dir.create(lib)

install <- function(source) {
  status <- system2(file.path(R.home("bin"), "R"),
                    c("CMD", "INSTALL", "--no-test-load", "-l", lib, source))
  if (status != 0) stop("R CMD INSTALL failed for ", source, call. = FALSE)
}
install("webr/trialr")
archive <- file.path(tempdir(), "dtptite.tar.gz")
utils::download.file(
  sprintf("https://github.com/ICR-EPAT/dtptite/archive/refs/tags/%s.tar.gz",
          dtptite_ref),
  archive, mode = "wb"
)
source_dir <- file.path(tempdir(), "dtptite-src")
utils::untar(archive, exdir = source_dir)
install(list.files(source_dir, full.names = TRUE)[1])

# Only the app's own files go into the site, with the git commit they are
# built from (shown in the app's footer, see R/version.R).
file.copy(c("app.R", "R", "www"), stage, recursive = TRUE)
commit <- Sys.getenv("GITHUB_SHA")
if (!nzchar(commit)) commit <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
writeLines(commit, file.path(stage, "BUILD"))
shinylive::export(stage, "_site")

# Add the two packages to the bundle, in the form Shinylive uses for the
# packages it downloads: the installed package folder as a .tgz, and an
# entry in packages/metadata.rds pointing to it.
packages <- normalizePath("_site/shinylive/webr/packages")
metadata_file <- file.path(packages, "metadata.rds")
metadata <- readRDS(metadata_file)
for (pkg in c("trialr", "dtptite")) {
  version <- unname(read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")[1, 1])
  filename <- sprintf("%s_%s.tgz", pkg, version)
  dir.create(file.path(packages, pkg), showWarnings = FALSE)
  old <- setwd(lib)
  utils::tar(file.path(packages, pkg, filename), pkg, compression = "gzip",
             tar = "internal")
  setwd(old)
  path <- file.path("packages", pkg, filename)
  metadata[[pkg]] <- list(
    name = stats::setNames(pkg, pkg), version = version, cached = TRUE,
    assets = list(list(filename = filename, url = path, version = version)),
    type = "package", ref = paste0(pkg, "@", version), path = path
  )
}
saveRDS(metadata, metadata_file)
