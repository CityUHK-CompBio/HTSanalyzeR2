test_that("built-in analysis objects retain their public shapes", {
  gsca <- get("d7_gsca", envir = asNamespace("HTSanalyzeR2"))
  nwa <- get("d7_nwa", envir = asNamespace("HTSanalyzeR2"))
  expect_s4_class(gsca, "GSCA")
  expect_s4_class(nwa, "NWA")
  expect_length(gsca@geneList, 7734)
  expect_s3_class(nwa@interactome, "igraph")
  expect_equal(igraph::vcount(nwa@interactome), 20223)
  expect_equal(igraph::ecount(nwa@interactome), 258975)
  expect_named(getResult(gsca), c(
    "HyperGeo.results", "GSEA.results", "Sig.pvals.in.both", "Sig.adj.pvals.in.both"
  ))
  expect_named(getResult(nwa), c("subnw", "labels"))
})
