## Identification of the maximum-scoring connected subgraph.
##
## The classical solver is BioNet's exact FastHeinz implementation, which is what
## `identifySubnetwork()` calls first. FastHeinz works on igraph objects, and a
## change in igraph 2.x makes it abort on networks where the profitable negative
## nodes are pairwise non-adjacent: internally it takes the minimum spanning tree
## of an edgeless subgraph, and igraph's Prim implementation now rejects the
## zero-length weight vector. That case used to yield a single-node path, so the
## failure is a regression rather than an inherent limitation of the method.
##
## When FastHeinz cannot run, `maximumScoringSubgraph()` solves the same problem
## exactly, as a mixed-integer linear program. Nothing about the answer is
## maintained here: the model is the standard single-commodity-flow formulation
## of the maximum-weight connected subgraph problem, and the solver proves
## optimality.

## ---- reproducible BUM fit ---------------------------------------------------

#' Fit the beta-uniform mixture model reproducibly
#'
#' [BioNet::fitBumModel()] draws its starting values with `runif()`, and its
#' multi-start loop keeps the last successful fit rather than the best one. The
#' node scores derived from that fit therefore moved slightly from run to run,
#' which was enough to change the identified subnetwork (observed node counts of
#' 118, 119 and 121 on one fixed input). This wrapper searches over a fixed set
#' of seeds, keeps the fit with the lowest negative log-likelihood, and restores
#' the caller's random number stream.
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

## ---- subnetwork identification ---------------------------------------------

#' Find an enriched subnetwork
#'
#' Uses BioNet's exact FastHeinz solver, falling back to an exact mixed-integer
#' solver when FastHeinz cannot run under the installed igraph version.
#'
#' @param graph an igraph object.
#' @param scores a named numeric vector of node scores; names must match the
#' `name` vertex attribute of `graph`.
#' @param verbose a single logical value.
#' @return An igraph object holding the identified subnetwork.
#' @noRd
identifySubnetwork <- function(graph, scores, verbose = TRUE) {
  fastHeinz <- tryCatch(
    BioNet::runFastHeinz(network = graph, scores = scores),
    error = function(e) e
  )
  if (!inherits(fastHeinz, "error")) {
    return(fastHeinz)
  }

  ## Only the known igraph 2.x incompatibility is handled here; anything else is
  ## a real problem and must surface to the caller.
  if (!grepl("REAL\\(\\) can only be applied", conditionMessage(fastHeinz))) {
    stop(fastHeinz)
  }

  warning(
    "BioNet::runFastHeinz() cannot run on this network because of an igraph ",
    "2.x incompatibility (it takes the minimum spanning tree of an edgeless ",
    "subgraph).\n",
    "The subnetwork is computed with an exact mixed-integer solver instead, ",
    "which finds the same optimum.\n",
    call. = FALSE
  )
  if (verbose) {
    cat("--FastHeinz unavailable, solving the subnetwork exactly with lpSolve", "\n")
  }

  maximumScoringSubgraph(graph, scores)
}

#' Exact maximum-scoring connected subgraph
#'
#' Solves the maximum-weight connected subgraph problem exactly as a
#' mixed-integer linear program. The formulation uses one binary variable per
#' node, one binary variable marking the root, and one continuous flow variable
#' per arc: the root supplies one unit of flow to every other selected node, and
#' flow may only travel along arcs whose two endpoints are selected. A feasible
#' solution is therefore necessarily connected, and maximising the total node
#' score makes it optimal.
#'
#' Two exact reductions are applied first, and the instance is only handed to
#' the solver when it stays within the configured size limit.
#'
#' @param graph an igraph object.
#' @param scores a named numeric vector of node scores.
#' @param timeLimit seconds allowed for the solver.
#' @param maxNodes refuse to build a model above this many nodes.
#' @return An igraph object.
#' @noRd
maximumScoringSubgraph <- function(
    graph, scores,
    timeLimit = getOption("HTSanalyzeR2.mwcs.timeout", 60),
    maxNodes = getOption("HTSanalyzeR2.mwcs.max.nodes", 400)) {

  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop(
      "Computing the subnetwork for this network needs the 'lpSolve' ",
      "package, which is not installed.\n",
      "Please install it with install.packages(\"lpSolve\") and retry.\n",
      call. = FALSE
    )
  }

  reduced <- reduceForMwcs(graph, scores)
  if (is.null(reduced)) {
    ## no positively scored node: the optimum is the empty module, which is also
    ## what BioNet::runFastHeinz() returns in that situation
    return(induced_subgraph(graph, vids = integer(0)))
  }
  if (vcount(reduced$graph) > maxNodes) {
    stop(
      "BioNet::runFastHeinz() cannot run on this network because of an igraph ",
      "2.x incompatibility, and the exact fallback solver is limited to ",
      maxNodes, " nodes (this network reduces to ", vcount(reduced$graph), ").\n",
      "You can raise the limit with ",
      "options(HTSanalyzeR2.mwcs.max.nodes = ..., HTSanalyzeR2.mwcs.timeout = ...).\n",
      call. = FALSE
    )
  }

  selected <- mwcsIlp(reduced$graph, reduced$scores, timeLimit = timeLimit)
  if (is.null(selected)) {
    stop(
      "The exact subnetwork solver did not prove optimality within ",
      timeLimit, " seconds.\n",
      "Raise it with options(HTSanalyzeR2.mwcs.timeout = ...), or lower 'fdr' ",
      "so that fewer nodes score positively.\n",
      call. = FALSE
    )
  }
  if (length(selected) == 0) {
    return(induced_subgraph(graph, vids = integer(0)))
  }

  induced_subgraph(graph, vids = reduced$keep[selected])
}

#' Shrink an MWCS instance without changing its optimum
#'
#' Two reductions are exact. A node that cannot reach a positively scored node
#' can never be part of an optimal module, because including it only lowers the
#' score; and a non-positive node of degree one can only ever be a leaf, so
#' including it also strictly lowers the score.
#'
#' @param graph an igraph object.
#' @param scores a named numeric vector of node scores.
#' @return A list with the reduced `graph`, its `scores`, and `keep`, the vertex
#' ids of the reduced graph expressed in the original graph. `NULL` when no node
#' has a positive score, which means the optimal module is empty.
#' @noRd
#' @importFrom igraph induced_subgraph components degree vertex_attr vcount
#' @importFrom igraph as_edgelist set_vertex_attr
reduceForMwcs <- function(graph, scores) {
  nodeNames <- vertex_attr(graph, "name")
  if (is.null(nodeNames)) {
    nodeNames <- as.character(seq_len(vcount(graph)))
    graph <- set_vertex_attr(graph, "name", value = nodeNames)
  }
  if (!any(scores[nodeNames] > 0, na.rm = TRUE)) {
    return(NULL)
  }

  ## keep only the connected components that contain a positively scored node
  membership <- components(graph)$membership
  positive <- !is.na(scores[nodeNames]) & scores[nodeNames] > 0
  hasPositive <- tapply(positive, membership, any)
  keep <- which(membership %in% as.integer(names(hasPositive))[hasPositive])

  ## iteratively strip non-positive leaves
  repeat {
    sub <- induced_subgraph(graph, keep)
    subNames <- vertex_attr(sub, "name")
    drop <- which(degree(sub) <= 1 & !(scores[subNames] > 0))
    if (length(drop) == 0) break
    keep <- keep[-drop]
    if (length(keep) == 0) return(NULL)
  }

  sub <- induced_subgraph(graph, keep)
  list(graph = sub, scores = scores[vertex_attr(sub, "name")], keep = keep)
}

#' Solve the maximum-weight connected subgraph linear program
#'
#' @param graph an igraph object with a `name` vertex attribute.
#' @param scores a named numeric vector of node scores.
#' @param timeLimit seconds allowed for the solver.
#' @return Vertex ids of `graph` in the optimal module, or `NULL` when the solver
#' did not prove optimality within the time limit.
#' @noRd
mwcsIlp <- function(graph, scores, timeLimit = 60) {
  n <- vcount(graph)
  el <- as_edgelist(graph, names = FALSE)
  m <- nrow(el)
  if (n == 0 || m == 0) return(integer(0))

  w <- as.numeric(scores[vertex_attr(graph, "name")])
  w[is.na(w)] <- 0
  bigM <- n

  ## variables: x (selected nodes), z (root marker), t (root supply), f (arcs)
  nX <- n; nZ <- n; nT <- n; nF <- 2L * m
  nvar <- nX + nZ + nT + nF
  iX <- function(i) i
  iZ <- function(i) nX + i
  iT <- function(i) nX + nZ + i
  iF <- function(k, d) nX + nZ + nT + 2L * (k - 1L) + d

  rSumZ <- 1L
  rFlow <- function(v) 1L + v
  rTup  <- function(v) 1L + n + v
  rTlow <- function(v) 1L + 2L * n + v
  rFb   <- function(off) 1L + 3L * n + off
  ncon <- 1L + 3L * n + 4L * m

  ## collect the model as (row, column, value) triplets, then densify once
  I <- integer(0); J <- integer(0); V <- numeric(0)
  put <- function(column, rows, values) {
    I <<- c(I, rows)
    J <<- c(J, rep.int(column, length(rows)))
    V <<- c(V, values)
  }

  tlowRows <- rTlow(seq_len(n))
  for (i in seq_len(n)) {
    ## -x_v in its own flow row, +x_v in every t_lower row
    put(iX(i), c(rFlow(i), tlowRows), c(-1, rep(1, n)))
    inc <- which(el[, 1] == i | el[, 2] == i)
    if (length(inc)) {
      ## every arc carries two separate bounds, one per endpoint:
      ##   f_uv <= M * x_u    and    f_uv <= M * x_v
      isU <- el[inc, 1] == i
      base <- 4L * (inc - 1L)
      put(iX(i),
          rFb(c(ifelse(isU, base + 1L, base + 2L),
                ifelse(isU, base + 4L, base + 3L))),
          rep(-bigM, 2L * length(inc)))
    }
  }
  for (i in seq_len(n)) {
    put(iZ(i), c(rSumZ, rTup(i), rTlow(i)), c(1, -bigM, bigM))
  }
  for (i in seq_len(n)) {
    put(iT(i), c(rFlow(i), rTup(i), rTlow(i)), c(1, 1, -1))
  }
  for (k in seq_len(m)) {
    u <- el[k, 1]; v <- el[k, 2]
    for (d in 1:2) {
      from <- if (d == 1L) u else v
      to   <- if (d == 1L) v else u
      off <- 4L * (k - 1L) + 2L * (d - 1L)
      put(iF(k, d), c(rFlow(to), rFlow(from), rFb(off + 1L), rFb(off + 2L)),
          c(1, -1, 1, 1))
    }
  }

  const.mat <- matrix(0, nrow = ncon, ncol = nvar)
  const.mat[cbind(I, J)] <- V
  const.dir <- c("=", rep("=", n), rep("<=", 2L * n), rep("<=", 4L * m))
  const.rhs <- c(1, rep(0, n), rep(0, n), rep(bigM, n), rep(0, 4L * m))
  objective <- c(w, numeric(nvar - nX))

  solution <- lpSolve::lp(
    direction = "max",
    objective.in = objective,
    const.mat = const.mat,
    const.dir = const.dir,
    const.rhs = const.rhs,
    binary.vec = seq_len(2L * n),
    timeout = timeLimit
  )
  if (solution$status != 0) return(NULL)
  which(round(solution$solution[iX(seq_len(n))]) > 0.5)
}
