test_that("report preparation is headless and self-contained", {
  gsca <- make_small_gsca()
  report_base <- file.path(tempdir(), "HeadlessReport")

  report_dir <- HTSanalyzeR2:::prepareReport(gsca = gsca, reportDir = report_base)
  on.exit(unlink(report_dir, recursive = TRUE), add = TRUE)

  expect_true(dir.exists(report_dir))
  expect_true(file.exists(file.path(report_dir, "app.R")))
  expect_true(file.exists(file.path(report_dir, "results.RData")))

  results <- readRDS(file.path(report_dir, "results.RData"))
  expect_s4_class(results$gsca, "GSCA")
  expect_null(results$nwa)

  ## the generated Shiny app must at least be syntactically valid
  expect_silent(parse(file.path(report_dir, "app.R")))
})

test_that("report preparation rejects objects of the wrong class", {
  expect_error(
    HTSanalyzeR2:::prepareReport(gsca = list(1, 2)),
    "should be a GSCA object"
  )
})
