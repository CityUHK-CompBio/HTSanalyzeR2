test_that("GSCA preserves required slots and dimensions", {
  object <- make_small_gsca()
  expect_s4_class(object, "GSCA")
  expect_length(object@geneList, 20)
  expect_identical(names(object@listOfGeneSetCollections), "example")
  expect_length(object@listOfGeneSetCollections$example, 2)
  expect_false(object@preprocessed)
})

test_that("GSCA preprocessing keeps real genes and records state", {
  object <- suppressMessages(
    preprocess(make_small_gsca(), initialIDs = "ENTREZID", verbose = FALSE)
  )
  expect_true(object@preprocessed)
  expect_setequal(names(object@geneList), paste0("g", 1:20))
  expect_equal(length(object@hits), 5)
  expect_equal(object@summary$gl[, "input"], 20)
  expect_equal(object@summary$gl[, "valid"], 20)
  expect_equal(object@summary$gl[, "converted to entrez"], 20)
})

test_that("GSCA analysis exposes stable grouped result tables", {
  object <- suppressMessages(
    analyze(
      suppressMessages(
        preprocess(make_small_gsca(), initialIDs = "ENTREZID", verbose = FALSE)
      ),
      para = list(
        pValueCutoff = 0.05,
        pAdjustMethod = "BH",
        nPermutations = 100,
        minGeneSetSize = 5,
        exponent = 1
      ),
      doGSOA = TRUE,
      doGSEA = TRUE,
      verbose = FALSE
    )
  )
  results <- getResult(object)
  expected_result_names <- c(
    "HyperGeo.results", "GSEA.results", "Sig.pvals.in.both", "Sig.adj.pvals.in.both"
  )
  expect_setequal(names(results), expected_result_names)
  for (name in expected_result_names) {
    expect_setequal(names(results[[name]]), "example")
  }
  expect_s3_class(results$HyperGeo.results$example, "data.frame")
  expect_s3_class(results$GSEA.results$example, "data.frame")
  expect_named(
    results$HyperGeo.results$example,
    c(
      "Universe Size", "Gene Set Size", "Total Hits", "Expected Hits",
      "Observed Hits", "Pvalue", "Adjusted.Pvalue", "Overlap.Gene"
    )
  )
  expect_named(
    results$GSEA.results$example,
    c("Observed.score", "Pvalue", "Adjusted.Pvalue", "Leading.Edge")
  )
  expect_true(all(results$HyperGeo.results$example$Pvalue >= 0))
  expect_true(all(results$GSEA.results$example$Pvalue >= 0))
})

test_that("GSCA rejects unnamed gene set collections", {
  gl <- make_small_gene_list()
  expect_error(
    GSCA(list(unlist(list(gs1 = names(gl)[1:10]))), gl),
    "named gene set collections"
  )
})
