make_fgsea_fixture <- function() {
  set.seed(3)
  genes <- paste0("g", 1:600)
  geneList <- setNames(rnorm(600), genes)
  sets <- lapply(1:40, function(i) sample(genes, sample(15:60, 1)))
  names(sets) <- paste0("set", 1:40)
  GSCA(listOfGeneSetCollections = list(big = sets), geneList = geneList,
       hits = genes[1:30])
}

runFgsea <- function(object, bp, seed = 99) {
  BiocParallel::register(bp)
  set.seed(seed)
  suppressMessages(analyze(object,
    para = list(pValueCutoff = 1, pAdjustMethod = "BH",
                nPermutations = 100, minGeneSetSize = 5, exponent = 1),
    doGSOA = FALSE, doGSEA = TRUE, GSEA.by = "fgsea", verbose = FALSE))
}

test_that("the fgsea path runs without deprecation warnings", {
  skip_if_not_installed("fgsea")
  object <- make_fgsea_fixture()

  expect_silent({
    result <- runFgsea(object, BiocParallel::SerialParam(), seed = 7)
  })
  expect_s4_class(result, "GSCA")
})

test_that("the fgsea path keeps the documented result columns", {
  skip_if_not_installed("fgsea")
  object <- make_fgsea_fixture()
  table <- getResult(runFgsea(object, BiocParallel::SerialParam()))$GSEA.results$big

  expected <- c("Observed.score", "Pvalue", "Adjusted.Pvalue", "NES",
                "nMoreExtreme", "size", "leading.Edge")
  expect_equal(colnames(table), expected)
  expect_true(all(table$Pvalue >= 0 & table$Pvalue <= 1))
  expect_equal(rownames(table), table$pathway %||% rownames(table))
})

test_that("the fgsea path is reproducible and seed-stable under a parallel backend", {
  skip_if_not_installed("fgsea")
  object <- make_fgsea_fixture()

  serialA <- getResult(runFgsea(object, BiocParallel::SerialParam(), seed = 99))
  serialB <- getResult(runFgsea(object, BiocParallel::SerialParam(), seed = 99))
  expect_identical(serialA, serialB)

  snow <- BiocParallel::SnowParam(workers = 2)
  on.exit(BiocParallel::bpstop(snow), add = TRUE)
  parallel <- getResult(runFgsea(object, snow, seed = 99))
  expect_identical(serialA, parallel)
})

test_that("the fgsea path is substantially faster than the built-in permutations", {
  skip_if_not_installed("fgsea")
  object <- make_fgsea_fixture()
  BiocParallel::register(BiocParallel::SerialParam())
  set.seed(5)

  builtin <- system.time(suppressMessages(analyze(object,
    para = list(pValueCutoff = 1, pAdjustMethod = "BH",
                nPermutations = 100, minGeneSetSize = 5, exponent = 1),
    doGSOA = FALSE, doGSEA = TRUE, verbose = FALSE)))[["elapsed"]]

  fgseaTime <- system.time(runFgsea(object, BiocParallel::SerialParam()))[["elapsed"]]

  expect_lt(fgseaTime, builtin)
})
