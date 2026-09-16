test_that("dependency declarations include every direct runtime caller", {
  desc <- read.dcf(
    system.file("DESCRIPTION", package = "HTSanalyzeR2"),
    fields = c("Imports", "Suggests")
  )
  imports <- unlist(strsplit(desc[1, "Imports"], ",\\s*"))
  suggests <- unlist(strsplit(desc[1, "Suggests"], ",\\s*"))
  expect_true("BiocParallel" %in% imports)
  expect_true("visNetwork" %in% imports)
  expect_true("bslib" %in% imports)
  expect_true("limma" %in% suggests)
  expect_true("TxDb.Hsapiens.UCSC.hg19.knownGene" %in% suggests)
})
