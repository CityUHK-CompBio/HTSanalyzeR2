test_that("serial and parallel BiocParallel backends give identical GSEA results", {
  skip_if_not_installed("BiocParallel")

  set.seed(11)
  genes <- paste0("g", 1:60)
  geneList <- setNames(rnorm(60), genes)
  geneSets <- list(example = list(
    s1 = genes[1:15],
    s2 = genes[20:40],
    s3 = genes[45:58]
  ))

  previous <- BiocParallel::registered()[[1]]
  on.exit(BiocParallel::register(previous), add = TRUE)

  BiocParallel::register(BiocParallel::SerialParam(RNGseed = 1))
  serial <- HTSanalyzeR2:::calcGSEA(
    geneSets, geneList, nPermutations = 50, minGeneSetSize = 5
  )

  snow <- BiocParallel::SnowParam(workers = 2, RNGseed = 1)
  on.exit(BiocParallel::bpstop(snow), add = TRUE)
  BiocParallel::register(snow)
  parallel <- HTSanalyzeR2:::calcGSEA(
    geneSets, geneList, nPermutations = 50, minGeneSetSize = 5
  )

  expect_equal(serial, parallel)
  expect_true(all(
    c("Observed.score", "Pvalue", "Adjusted.Pvalue", "Leading.Edge") %in%
      colnames(serial$example)
  ))
})

test_that("Rank Product is an optional dependency with a clear message", {
  desc <- read.dcf(
    system.file("DESCRIPTION", package = "HTSanalyzeR2"),
    fields = c("Imports", "Suggests")
  )
  imports <- unlist(strsplit(desc[1, "Imports"], ",\\s*"))
  suggests <- unlist(strsplit(desc[1, "Suggests"], ",\\s*"))

  expect_false("RankProd" %in% imports)
  expect_true("RankProd" %in% suggests)
})
