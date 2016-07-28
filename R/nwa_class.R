##class NWA (NetWork Analyses)
##definition of class NWA
#' @include class_union.R
setClass(
  "NWA",
  representation(
    pvalues = "numeric",
    phenotypes = "numeric_Or_integer_Or_NULL",
    interactome = "graphNEL_Or_NULL",
    fdr = "numeric",
    result = "list",
    summary = "list",
    preprocessed = "logical"
  ),
  prototype = list(
    pvalues = numeric(),
    phenotypes = NULL,
    interactome = NULL,
    fdr = 0.001,
    result = list(),
    summary = list(),
    preprocessed = FALSE
  )
)

##initialization method
setMethod("initialize",
          "NWA",
          function(.Object,
                   pvalues,
                   phenotypes = NULL,
                   interactome = NULL) {
            ##check input arguments
            paraCheck(name = "pvalues", para = pvalues)
            if (!is.null(phenotypes))
              paraCheck(name = "phenotypes", para = phenotypes)
            if (!is.null(interactome))
              paraCheck(name = "interactome", para = interactome)
            .Object@pvalues <- pvalues
            .Object@phenotypes <- phenotypes
            .Object@interactome <- interactome

            ##set up summary framework
            sum.info.input <- matrix(NA, 2, 5)
            colnames(sum.info.input) <- c("input",
                                          "valid",
                                          "duplicate removed",
                                          "converted to entrez",
                                          "in interactome")
            rownames(sum.info.input) <- c("p-values", "phenotypes")

            sum.info.db <- matrix(NA, 1, 5)
            colnames(sum.info.db) <- c("name", "species", "genetic",
                                       "node No", "edge No")
            rownames(sum.info.db) <- "Interaction dataset"

            sum.info.para <- matrix(NA, 1, 1)
            colnames(sum.info.para) <- "FDR"
            rownames(sum.info.para) <- "Parameter"

            sum.info.results <- matrix(NA, 1, 2)
            colnames(sum.info.results) <- c("node No", "edge No")
            rownames(sum.info.results) <- "Subnetwork"

            .Object@summary <- list(
              input = sum.info.input,
              db = sum.info.db,
              para = sum.info.para,
              results = sum.info.results
            )
            ##initialization of summary
            .Object@summary$input["p-values", "input"] <- length(pvalues)
            .Object@summary$input["phenotypes", "input"] <-
              length(phenotypes)

            .Object@summary$db[1, "name"] <- "Unknown"
            if (!is.null(interactome)) {
              .Object@summary$db[1, "node No"] <- numNodes(interactome)
              .Object@summary$db[1, "edge No"] <- numEdges(interactome)
            }

            .Object@preprocessed <- FALSE

            .Object
          })
