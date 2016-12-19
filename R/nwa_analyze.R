if (!isGeneric("analyze")) {
  setGeneric("analyze", function(object, ...)
    standardGeneric("analyze"), package = "HTSanalyzeR2")
}

#' @describeIn analyze The function will store the subnetwork module identified
#' by BioNet (if species is given, labels of nodes will also be mapped from
#' Entrez IDs to gene symbols), and update information about these results to
#' slot summary of class NWA.
#'
#' @param species
#' a single character value specifying the species for which the data should be read.
#' @param fdr
#' a single numeric value specifying the false discovery for the scoring of nodes
#' (see BioNet::scoreNodes and Dittrich et al., 2008 for details)
#'
#' @export
#' @include nwa_class.R gsca_preprocess.R
#' @importFrom igraph vertex_attr vcount ecount
#' @importFrom AnnotationDbi as.list
setMethod("analyze",
          signature = "NWA",
          function(object,
                   fdr = 0.001,
                   species,
                   verbose = TRUE) {
            ## check input arguments
            paraCheck("Analyze", "fdr", fdr)
            paraCheck("NWAClass", "interactome", object@interactome)

            object@fdr <- fdr
            object@summary$input[1, "in interactome"] <-
              length(intersect(names(object@pvalues),
                               vertex_attr(object@interactome, "name")))
            object@summary$para[1, 1] <- fdr

            if (!is.null(object@phenotypes)) {
              if(is.matrix(object@phenotypes))
                pnames <- rownames(object@phenotypes)
              else
                pnames <- names(object@phenotypes)

              object@summary$input[2, "in interactome"] <-
                length(intersect(pnames, vertex_attr(object@interactome, "name")))
            }
            if (length(object@pvalues) == 0 ||
                object@summary$input[1, "in interactome"] == 0)
              stop("pvalues vector has length 0, or has no overlap ",
                   "with interactome!\n")
            ## perform network analysis
            module <- networkAnalysis(
              pvalues = object@pvalues,
              graph = object@interactome,
              fdr = object@fdr,
              verbose = verbose
            )
            ## update module info in summary
            object@summary$result[, "node No"] <- vcount(module)
            object@summary$result[, "edge No"] <- ecount(module)

            ## To represent the network in a more convenient format,
            # the symbol identifiers will be mapped and given to the
            # user (more readable than Entrez.gene IDs)
            if (!missing(species)) {
              paraCheck("General", "species", species)
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
            })


#' Identify enriched subnetworks
#'
#' This function finds subnetworks enriched for genes with significant
#' phenotypes based on the package 'BioNet'.
#'
#' @param pvalues a numeric vector of p-values
#' @param graph an object of class igraph, used as the interactome in
#' the network analysis
#' @param fdr a single numeric value specifying the false discovery for
#' the scoring of nodes (see BioNet::scoreNodes and Dittrich et al., 2008
#' for details)
#' @param verbose a single logical value indicating to display detailed
#' messages (when verbose=TRUE) or not (when verbose=FALSE)
#'
#' @details This function takes in a vector of p-values and a graph standing
#' for the interactome to identify the maximum scoring subnetwork based on
#' the BioNet package.
#'
#' @return a subnetwork module of class igraph
#' @export
#' @importFrom BioNet fitBumModel scoreNodes runFastHeinz
#' @importFrom igraph vertex_attr vcount
networkAnalysis <-
  function(pvalues,
           graph,
           fdr = 0.001,
           verbose = TRUE) {
    ##check arguments
    paraCheck("NWAClass", "pvalues", pvalues)
    paraCheck("NWAClass", "interactome", graph)
    paraCheck("Analyze", "fdr", fdr)
    paraCheck("General", "verbose", verbose)

    cat("-Performing network analysis ... \n")

    ## store the name of the nodes of the igraph object for which we
    #  have p-value information
    scoredNodes <- intersect(names(pvalues), vertex_attr(graph, "name"))

    ## check that there are nodes associated with a p-value
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
    ## Get the pvalue information for the nodes of the igraph object
    #  only, and fit a bum model on these N.B. the fitting of the bum
    #  model will produce a diagnostic plot on the screen, to check the
    #  fitting
    dataForNw <- pvalues[scoredNodes]
    fb <- fitBumModel(dataForNw)
    ## Score the nodes of the network
    #  The nodes without pvalues will get a NA value instead of a score
    scores <- scoreNodes(graph, fb = fb, fdr = fdr)
    ## Compute the mean score, and set the score of all non-scored nodes
    #  (NAs) to this mean
    meanscore <- mean(scores, na.rm = TRUE)
    scoreswMean <- scores
    scoreswMean[which(is.na(scores))] <- meanscore
    ## Find the optimal subnetwork
    if (verbose)
      cat("--Computing the optimal subnetwork", "\n")
    module <- runFastHeinz(network = graph, scores = scoreswMean)
    cat("-Network analysis complete \n")
    ## Return a igraph object consisting of the enriched sub-network
    module
  }
