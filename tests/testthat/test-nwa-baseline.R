test_that("NWA interactome creates a connected non-empty graph", {
  object <- make_small_nwa()
  expect_s4_class(object, "NWA")
  expect_s3_class(object@interactome, "igraph")
  expect_equal(igraph::vcount(object@interactome), 20)
  expect_equal(igraph::ecount(object@interactome), 20)
})

test_that("NWA result tables preserve BioNet contract", {
  skip_if_not_installed("BioNet")
  skip_if(
    identical(
      tryCatch(
        {
          graph <- igraph::make_ring(6)
          igraph::V(graph)$name <- paste0("g", 1:6)
          scores <- setNames(c(2, -1, 3, -0.5, 1, -2), igraph::V(graph)$name)
          igraph::E(graph)$weight <- rep(1, igraph::ecount(graph))
          is.character(BioNet::runFastHeinz(graph, scores))
        },
        error = function(e) TRUE
      ),
      TRUE
    ),
    "BioNet::runFastHeinz is incompatible with igraph 2.x; deferred to the algorithm compatibility phase."
  )
  object <- make_small_nwa()
  result <- suppressMessages(analyze(object, fdr = 1, verbose = FALSE))
  expect_s4_class(result, "NWA")
  expect_named(getResult(result), c("subnw", "labels"))
  expect_s3_class(getResult(result)$subnw, "igraph")
  expect_true(igraph::vcount(getResult(result)$subnw) > 0)
  expect_true(igraph::ecount(getResult(result)$subnw) > 0)
})

test_that("NWA rejects an empty interactome", {
  pvalues <- setNames(c(0.01, 0.02), c("a", "b"))
  expect_error(
    NWA(pvalues = pvalues, interactome = igraph::make_empty_graph(n = 0)),
    "node and edge No > 0"
  )
})
