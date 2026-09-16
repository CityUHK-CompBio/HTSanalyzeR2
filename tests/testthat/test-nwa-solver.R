test_that("the greedy solver returns a connected, high-scoring subgraph", {
  graph <- igraph::make_ring(8)
  igraph::V(graph)$name <- paste0("g", 1:8)
  scores <- setNames(c(2, -1, 3, -0.5, 1, -2, 1.5, -1.5), igraph::V(graph)$name)

  module <- HTSanalyzeR2:::greedySubnetwork(graph, scores)

  expect_s3_class(module, "igraph")
  expect_gt(igraph::vcount(module), 0)
  expect_true(igraph::is_connected(module))
  ## the module must beat the best single node
  expect_gt(sum(scores[igraph::V(module)$name]), max(scores))
})

test_that("the greedy solver can cross a negative node to join two components", {
  ## b1 is only reachable from the a-cluster through the negative connector
  graph <- igraph::make_empty_graph(n = 5, directed = FALSE)
  igraph::V(graph)$name <- c("a1", "a2", "a3", "cost", "b1")
  graph <- igraph::add_edges(graph, c("a1", "a2", "a2", "a3", "a3", "cost",
                                      "cost", "b1"))
  scores <- setNames(c(5, 5, 5, -1, 6), igraph::V(graph)$name)

  module <- HTSanalyzeR2:::greedySubnetwork(graph, scores)
  nodes <- igraph::V(module)$name

  expect_true("cost" %in% nodes)
  expect_true(all(c("a1", "a2", "a3", "b1") %in% nodes))
})

test_that("the greedy solver never returns a disconnected module", {
  graph <- igraph::make_empty_graph(n = 4, directed = FALSE)
  igraph::V(graph)$name <- c("p1", "p2", "n1", "n2")
  scores <- setNames(c(4, 3, -1, -1), igraph::V(graph)$name)

  module <- HTSanalyzeR2:::greedySubnetwork(graph, scores)

  expect_equal(igraph::vcount(module), 1)
  expect_true(igraph::V(module)$name %in% c("p1", "p2"))
})

test_that("the BUM fit is reproducible and preserves the caller RNG stream", {
  skip_if_not_installed("BioNet")

  pvalues <- setNames(
    c(0.001, 0.002, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, 0.95),
    paste0("g", 1:12)
  )

  first <- HTSanalyzeR2:::fitBumModelStable(pvalues, attempts = 3)
  second <- HTSanalyzeR2:::fitBumModelStable(pvalues, attempts = 3)

  expect_identical(first$lambda, second$lambda)
  expect_identical(first$a, second$a)

  set.seed(123)
  expected <- runif(3)
  set.seed(123)
  invisible(HTSanalyzeR2:::fitBumModelStable(pvalues, attempts = 3))
  expect_identical(runif(3), expected)
})

test_that("network analysis is deterministic for a fixed input", {
  skip_if_not_installed("BioNet")

  object <- make_small_nwa()
  analyzeOnce <- function() {
    result <- suppressWarnings(suppressMessages(analyze(object, fdr = 1, verbose = FALSE)))
    subnetwork <- getResult(result)$subnw
    paste(igraph::vcount(subnetwork), igraph::ecount(subnetwork),
          paste(sort(igraph::V(subnetwork)$name), collapse = ","))
  }

  expect_equal(analyzeOnce(), analyzeOnce())
})
