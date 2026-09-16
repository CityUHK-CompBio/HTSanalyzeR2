## Identification of the maximum-scoring connected subgraph.
##
## The classical solver is BioNet's exact FastHeinz implementation, which is
## what `identifySubnetwork()` calls first. FastHeinz works on igraph objects,
## and a change in igraph 2.x makes it abort on networks where the profitable
## negative nodes are pairwise non-adjacent: internally it takes the minimum
## spanning tree of an edgeless subgraph, and igraph's Prim implementation now
## rejects the zero-length weight vector. That case used to yield a single-node
## path, so the failure is a regression rather than an inherent limitation of
## the method.
##
## When the exact solver cannot run, `greedySubnetwork()` produces a subnetwork
## with the same contract (a connected subgraph with a high total node score)
## so that the analysis still returns a result instead of an error. It is
## deliberately a heuristic: the maximum-weight connected subgraph problem is
## NP-hard, and the point of the fallback is to stay usable, not to pretend to
## be exact.

#' Fit the beta-uniform mixture model reproducibly
#'
#' [BioNet::fitBumModel()] draws its starting values with `runif()`, and its
#' multi-start loop keeps the last successful fit rather than the best one. The
#' node scores derived from that fit therefore moved slightly from run to run,
#' which was enough to change the identified subnetwork (observed node counts of
#' 118, 119 and 121 on the same input). This wrapper searches over a fixed set of
#' seeds, keeps the fit with the lowest negative log-likelihood, and restores the
#' caller's random number stream.
#'
#' @param pvalues a named numeric vector of p-values.
#' @param starts number of random starting points per attempt, passed to
#' [BioNet::fitBumModel()].
#' @param attempts number of deterministically seeded attempts.
#' @param plot a single logical value; when `TRUE` the diagnostic plot of the
#' selected fit is drawn.
#' @return An object of class `bum`.
#' @noRd
#' @importFrom graphics par hist
fitBumModelStable <- function(pvalues, starts = 10, attempts = 5, plot = FALSE) {
  if (is.null(names(pvalues))) {
    names(pvalues) <- as.character(seq_along(pvalues))
  }

  ## Leave the caller's random number stream exactly as we found it.
  hadSeed <- exists(".Random.seed", envir = .GlobalEnv)
  oldSeed <- if (hadSeed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (hadSeed) {
      assign(".Random.seed", oldSeed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  fits <- vector("list", attempts)
  for (attempt in seq_len(attempts)) {
    set.seed(attempt)
    fits[[attempt]] <- BioNet::fitBumModel(pvalues, plot = FALSE, starts = starts)
  }
  negLL <- vapply(fits, function(fit) fit$negLL, numeric(1))
  best <- fits[[which.min(negLL)]]

  if (isTRUE(plot)) {
    par(mfrow = c(1, 2))
    hist(pvalues)
    plot(best)
  }
  best
}

#' Find an enriched subnetwork
#'
#' Uses BioNet's exact FastHeinz solver, falling back to a greedy search when
#' that solver cannot run under the installed igraph version.
#'
#' @param graph an igraph object.
#' @param scores a named numeric vector of node scores; names must match the
#' `name` vertex attribute of `graph`.
#' @param verbose a single logical value.
#' @return An igraph object holding the identified subnetwork. It can be empty
#' when no node has a positive score.
#' @noRd
identifySubnetwork <- function(graph, scores, verbose = TRUE) {
  exact <- tryCatch(
    BioNet::runFastHeinz(network = graph, scores = scores),
    error = function(e) e
  )
  if (!inherits(exact, "error")) {
    return(exact)
  }

  ## Only the known igraph 2.x incompatibility is handled here; anything else
  ## is a real problem and must surface to the caller.
  if (!grepl("REAL\\(\\) can only be applied", conditionMessage(exact))) {
    stop(exact)
  }

  warning(
    "BioNet::runFastHeinz() could not run on this network because of an ",
    "igraph 2.x incompatibility (edgeless internal subgraph).\n",
    "A greedy maximum-scoring subnetwork is returned instead; results may ",
    "differ slightly from the exact solver.\n",
    call. = FALSE
  )
  if (verbose) {
    cat("--Exact solver unavailable, using the greedy solver", "\n")
  }

  greedySubnetwork(graph, scores)
}

#' Greedy maximum-scoring connected subgraph
#'
#' Best-first search seeded from every connected component of the positive
#' nodes. A module grows by repeatedly adding the neighbouring node with the
#' best estimated marginal gain, where the gain accounts for positive nodes the
#' new node would connect. The best module seen is returned.
#'
#' @inheritParams identifySubnetwork
#' @return An igraph object.
#' @noRd
#' @importFrom igraph induced_subgraph components neighbors vertex_attr
greedySubnetwork <- function(graph, scores) {
  nodeNames <- vertex_attr(graph, "name")
  if (is.null(nodeNames)) {
    nodeNames <- as.character(seq_len(vcount(graph)))
  }
  names(scores) <- names(scores)

  scoreOf <- function(nodes) sum(scores[nodes], na.rm = TRUE)

  best <- character(0)
  bestScore <- -Inf
  consider <- function(nodes) {
    if (length(nodes) == 0) return(invisible(NULL))
    value <- scoreOf(nodes)
    if (value > bestScore) {
      best <<- nodes
      bestScore <<- value
    }
    invisible(NULL)
  }

  ## Greedy expansion of one seed set. `gain` is the node's own score plus the
  ## positive scores that become reachable, which lets a module cross a
  ## negative connector to reach a profitable component.
  grow <- function(seed) {
    current <- seed
    repeat {
      boundary <- setdiff(
        unique(unlist(lapply(current, function(node) {
          vertex_attr(graph, "name", neighbors(graph, node))
        }))),
        current
      )
      boundary <- boundary[!is.na(boundary)]
      if (length(boundary) == 0) break

      gains <- vapply(boundary, function(node) {
        reachable <- setdiff(
          vertex_attr(graph, "name", neighbors(graph, node)),
          c(current, node)
        )
        scores[node] + sum(pmax(scores[reachable], 0), na.rm = TRUE)
      }, numeric(1))

      if (max(gains) <= 0) break
      current <- c(current, boundary[which.max(gains)])
      consider(current)
    }
    current
  }

  positive <- nodeNames[!is.na(scores[nodeNames]) & scores[nodeNames] > 0]
  if (length(positive) > 0) {
    positiveGraph <- induced_subgraph(graph, vids = positive)
    membership <- components(positiveGraph)$membership
    for (component in unique(membership)) {
      seed <- vertex_attr(positiveGraph, "name")[membership == component]
      consider(seed)
      grow(seed)
    }
  }

  ## A network whose nodes are all non-positive still has a best single node.
  if (length(nodeNames) > 0) {
    consider(nodeNames[which.max(scores[nodeNames])])
  }

  if (length(best) == 0) {
    return(induced_subgraph(graph, vids = character(0)))
  }
  induced_subgraph(graph, vids = best)
}
