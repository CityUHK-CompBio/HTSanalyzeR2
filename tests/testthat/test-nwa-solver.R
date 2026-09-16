## Exhaustive optimum of the maximum-weight connected subgraph problem, used as
## the reference for the small instances below.
bruteForceMwcs <- function(graph, scores) {
  n <- igraph::vcount(graph)
  names <- igraph::V(graph)$name
  weight <- as.numeric(scores[names])
  name <- NA_character_
  best <- -Inf
  for (mask in seq_len(2^n - 1)) {
    ids <- which(bitwAnd(mask, bitwShiftL(1L, seq_len(n) - 1L)) > 0L)
    if (!igraph::is_connected(igraph::induced_subgraph(graph, ids))) next
    value <- sum(weight[ids])
    if (value > best + 1e-9) {
      best <- value
      name <- paste(sort(names[ids]), collapse = ",")
    }
  }
  list(nodes = name, value = best)
}

test_that("the exact solver matches exhaustive search on random graphs", {
  skip_if_not_installed("lpSolve")
  set.seed(2024)

  compared <- 0
  for (trial in seq_len(40)) {
    n <- sample(5:10, 1)
    graph <- igraph::sample_gnm(n, sample(n:(2 * n), 1), directed = FALSE)
    if (igraph::ecount(graph) == 0 || !igraph::is_connected(graph)) next
    igraph::V(graph)$name <- paste0("g", seq_len(n))
    scores <- stats::setNames(round(stats::rnorm(n), 2), igraph::V(graph)$name)
    reference <- bruteForceMwcs(graph, scores)
    if (is.infinite(reference$value)) next

    found <- HTSanalyzeR2:::mwcsIlp(graph, scores, timeLimit = 30)
    value <- sum(scores[igraph::V(graph)$name][found])
    expect_equal(value, reference$value, tolerance = 1e-7)
    compared <- compared + 1
  }
  expect_gt(compared, 20)
})

test_that("the exact solver agrees with BioNet's FastHeinz where that solver runs", {
  skip_if_not_installed("BioNet")
  skip_if_not_installed("lpSolve")
  set.seed(77)

  compared <- 0
  for (trial in seq_len(20)) {
    n <- sample(20:45, 1)
    graph <- igraph::sample_gnm(n, sample(2 * n:(3 * n), 1), directed = FALSE)
    if (!igraph::is_connected(graph)) next
    igraph::V(graph)$name <- paste0("g", seq_len(n))
    scores <- stats::setNames(round(stats::rnorm(n), 2), igraph::V(graph)$name)

    fastHeinz <- tryCatch(BioNet::runFastHeinz(graph, scores),
                          error = function(e) NULL)
    if (is.null(fastHeinz)) next
    found <- HTSanalyzeR2:::mwcsIlp(graph, scores, timeLimit = 30)
    ## FastHeinz pre-filters negative nodes with a greedy test, so the exact
    ## solver may find a strictly better module; it must never find a worse one
    expect_gte(sum(scores[igraph::V(graph)$name][found]),
               sum(scores[igraph::V(fastHeinz)$name]) - 1e-7)
    compared <- compared + 1
  }
  expect_gt(compared, 8)
})

test_that("reductions preserve the optimum", {
  skip_if_not_installed("lpSolve")

  ## a profitable pair connected through a negative node, plus a useless tail
  graph <- igraph::make_empty_graph(n = 6, directed = FALSE)
  igraph::V(graph)$name <- c("a1", "cost", "a2", "tail1", "tail2", "junk")
  graph <- igraph::add_edges(graph, c("a1", "cost", "cost", "a2",
                                      "a2", "tail1", "tail1", "tail2"))
  scores <- stats::setNames(c(5, -1, 6, -2, -2, -3), igraph::V(graph)$name)

  reduced <- HTSanalyzeR2:::reduceForMwcs(graph, scores)
  expect_true(all(c("a1", "cost", "a2") %in% igraph::V(reduced$graph)$name))
  expect_false(any(c("tail1", "tail2", "junk") %in% igraph::V(reduced$graph)$name))

  module <- HTSanalyzeR2:::maximumScoringSubgraph(graph, scores)
  expect_equal(sum(scores[igraph::V(module)$name]), 10)   # 5 - 1 + 6
  expect_true(igraph::is_connected(module))
})

test_that("the empty module is returned when nothing scores positively", {
  skip_if_not_installed("lpSolve")
  graph <- igraph::make_ring(4)
  igraph::V(graph)$name <- paste0("g", 1:4)
  scores <- stats::setNames(c(-1, -2, -3, -4), igraph::V(graph)$name)

  expect_null(HTSanalyzeR2:::reduceForMwcs(graph, scores))
  module <- HTSanalyzeR2:::maximumScoringSubgraph(graph, scores)
  expect_equal(igraph::vcount(module), 0)
})

test_that("the solver reports the size limit instead of running unbounded", {
  skip_if_not_installed("lpSolve")
  graph <- igraph::make_ring(12)
  igraph::V(graph)$name <- paste0("g", 1:12)
  scores <- stats::setNames(rep(c(1, -1), 6), igraph::V(graph)$name)

  expect_error(
    HTSanalyzeR2:::maximumScoringSubgraph(graph, scores, maxNodes = 5),
    "limited to 5 nodes"
  )
})

test_that("the solver result is connected and reproducible", {
  skip_if_not_installed("lpSolve")
  set.seed(5)
  graph <- igraph::sample_gnm(40, 100, directed = FALSE)
  igraph::V(graph)$name <- paste0("g", seq_len(40))
  scores <- stats::setNames(round(stats::rnorm(40), 2), igraph::V(graph)$name)

  first <- HTSanalyzeR2:::maximumScoringSubgraph(graph, scores)
  second <- HTSanalyzeR2:::maximumScoringSubgraph(graph, scores)

  expect_true(igraph::is_connected(first))
  expect_equal(sort(igraph::V(first)$name), sort(igraph::V(second)$name))
})

test_that("the BUM fit is reproducible and preserves the caller RNG stream", {
  skip_if_not_installed("BioNet")

  pvalues <- stats::setNames(
    c(0.001, 0.002, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, 0.95),
    paste0("g", 1:12)
  )

  first <- HTSanalyzeR2:::fitBumModelStable(pvalues, attempts = 3)
  second <- HTSanalyzeR2:::fitBumModelStable(pvalues, attempts = 3)
  expect_identical(first$lambda, second$lambda)
  expect_identical(first$a, second$a)

  set.seed(123)
  expected <- stats::runif(3)
  set.seed(123)
  invisible(HTSanalyzeR2:::fitBumModelStable(pvalues, attempts = 3))
  expect_identical(stats::runif(3), expected)
})

test_that("network analysis is deterministic for a fixed input", {
  skip_if_not_installed("BioNet")
  skip_if_not_installed("lpSolve")

  object <- make_small_nwa()
  analyzeOnce <- function() {
    result <- suppressWarnings(suppressMessages(analyze(object, fdr = 1, verbose = FALSE)))
    subnetwork <- getResult(result)$subnw
    paste(igraph::vcount(subnetwork), igraph::ecount(subnetwork),
          paste(sort(igraph::V(subnetwork)$name), collapse = ","))
  }
  expect_equal(analyzeOnce(), analyzeOnce())
})
