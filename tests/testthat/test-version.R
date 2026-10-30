test_that("the version line names the app, dtptite and the build", {
  dtptite <- as.character(utils::packageVersion("dtptite"))
  expect_equal(version_text(NULL),
               paste0("Version ", app_version, " · dtptite ", dtptite))
  expect_equal(
    version_text("1a2b3c4d5e6f"),
    paste0("Version ", app_version, " · dtptite ", dtptite,
           " · build 1a2b3c4")
  )
})

test_that("the build commit is read from the BUILD file, if any", {
  path <- withr::local_tempfile()
  expect_null(build_commit(path))
  writeLines("1a2b3c4d5e6f", path)
  expect_equal(build_commit(path), "1a2b3c4d5e6f")
  writeLines("", path)
  expect_null(build_commit(path))
})
