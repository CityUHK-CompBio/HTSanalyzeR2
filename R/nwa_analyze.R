## summarize & default show
if (!isGeneric("analyze")) {
  setGeneric("analyze", function(object, ...)
    standardGeneric("analyze"), package = "HTSanalyzeR2")
}

##analysis
#' @export
#' @include nwa_class.R
#' @importFrom igraph vertex_attr vcount ecount
setMethod("analyze",
          signature = "NWA",
          function(object,
                   fdr = 0.001,
                   species,
                   verbose = TRUE) {
            ##check input arguments
            # paraCheck(name = "interactome", para = object@interactome)
            paraCheck(name = "fdr", para = fdr)
            object@fdr <- fdr
            object@summary$input[1, "in interactome"] <-
              length(intersect(names(object@pvalues), vertex_attr(object@interactome, "name")))
            object@summary$para[1, 1] <- fdr
            if (!is.null(object@phenotypes))
              object@summary$input[2, "in interactome"] <-
              length(intersect(names(object@phenotypes), vertex_attr(object@interactome, "name")))
            if (length(object@pvalues) == 0 ||
                object@summary$input[1, "in interactome"] == 0)
              stop("pvalues vector has length 0, or has no overlap ",
                   "with interactome!\n")
            ##perform network analysis
            module <- networkAnalysis(
              pvalues = object@pvalues,
              graph = object@interactome,
              fdr = object@fdr,
              verbose = verbose
            )
            ##update module info in summary
            object@summary$result[, "node No"] <- vcount(module)
            object@summary$result[, "edge No"] <- ecount(module)

            ##To represent the network in a more convenient format,
            ##the symbol identifiers will be mapped and given to the
            ##user (more readable than Entrez.gene IDs)
            if (!missing(species)) {
              paraCheck(name = "species", para = species)
              anno.db.species <- paste("org", species, "eg", "db", sep = ".")
              if (!(paste("package:", anno.db.species, sep = "") %in% search()))
                library(anno.db.species, character.only = TRUE)
              map <- as.list(get(paste("org", species, "egSYMBOL", sep = ".")))
              labels <- map[vertex_attr(module, "name")]
              object@result <- list(subnw = module, labels = labels)
            } else {
              object@result <- list(subnw = module, labels = vertex_attr(module, "name"))
            }

            object
            }
          )


##This function finds subnetworks enriched for genes with significant
##phenotypes.
#' @export
#' @importFrom BioNet fitBumModel scoreNodes runFastHeinz
#' @importFrom igraph vertex_attr vcount
networkAnalysis <-
  function(pvalues,
           graph,
           fdr = 0.001,
           verbose = TRUE) {
    ##check arguments
    paraCheck("pvalues", pvalues)
    # paraCheck("interactome", graph)
    paraCheck("fdr", fdr)
    paraCheck("verbose", verbose)
    cat("-Performing network analysis ... \n")
    ##store the name of the nodes of the igraph object for which we
    ##have p-value information
    scoredNodes <- intersect(names(pvalues), vertex_attr(graph, "name"))
    ##check that there are nodes associated with a p-value
    if (length(scoredNodes) == 0)
      stop(
        "The rownames of your pvalueMatrix do not match to any ",
        "name in the interactionMatrix, check that you have the ",
        "right type of identifiers."
      )
    if (verbose)
      cat(
        paste(
          "--Your network consists of ",
          vcount(graph),
          " nodes, of which ",
          length(scoredNodes),
          " have an associated p-value",
          sep = ""
        ),
        "\n"
      )
    ##Get the pvalue information for the nodes of the igraph object
    ##only, and fit a bum model on these N.B. the fitting of the bum
    ##model will produce a diagnostic plot on the screen, to check the
    ##fitting
    dataForNw <- pvalues[scoredNodes]
    fb <- fitBumModel(dataForNw)
    ##Score the nodes of the network
    ##The nodes without pvalues will get a NA value instead of a score
    scores <- scoreNodes(graph, fb = fb, fdr = fdr)
    ##Compute the mean score, and set the score of all non-scored nodes
    ##(NAs) to this mean
    meanscore <- mean(scores, na.rm = TRUE)
    scoreswMean <- scores
    scoreswMean[which(is.na(scores))] <- meanscore
    ##Find the optimal subnetwork
    if (verbose)
      cat("--Computing the optimal subnetwork", "\n")
    module <- runFastHeinz(network = graph, scores = scoreswMean)
    cat("-Network analysis complete \n")
    ##Return a igraph object consisting of the enriched sub-network
    return(module)
  }
