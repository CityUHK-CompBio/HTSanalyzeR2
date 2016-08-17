##class NWA (NetWork Analyses)
##definition of class NWA
#' @include class_union.R utils.R
setClass(
  "NWA",
  representation(
    pvalues = "numeric",
    # phenotypes = "numeric_Or_integer_Or_NULL",
    # interactome = "igraph_Or_NULL",
    phenotypes = "numeric",
    interactome = "igraph",
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
#' @importFrom igraph vcount ecount
setMethod("initialize",
          signature = "NWA",
          function(.Object,
                   pvalues,
                   phenotypes = NULL,
                   interactome = NULL) {
            ##check input arguments
            paraCheck(name = "pvalues", para = pvalues)
            if (!is.null(phenotypes))
              paraCheck(name = "phenotypes", para = phenotypes)
            # if (!is.null(interactome))
              # paraCheck(name = "interactome", para = interactome)
            .Object@pvalues <- pvalues
            .Object@phenotypes <- phenotypes
            .Object@interactome <- interactome

            ##set up summary framework
            sum.info.input <-
              geneMatrix(
                rowNames = c("p-values", "phenotypes"),
                colNames = c(
                  "input",
                  "valid",
                  "duplicate removed",
                  "converted to entrez",
                  "in interactome"
                )
              )
            sum.info.db <- geneMatrix(
              rowNames = c("Interaction dataset"),
              colNames = c("name", "species", "genetic",
                           "node No", "edge No")
            )
            sum.info.para <- geneMatrix(rowNames = c("Parameter"),
                                        colNames = c("FDR"))

            sum.info.results <- geneMatrix(rowNames = c("Subnetwork"),
                                           colNames = c("node No", "edge No"))

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
              .Object@summary$db[1, "node No"] <- igraph::vcount(interactome)
              .Object@summary$db[1, "edge No"] <- igraph::ecount(interactome)
            }

            .Object@preprocessed <- FALSE
            .Object
          })

#' @export
NWA <- function(pvalues, phenotypes = NULL, interactome = NULL) {
  # paraCheck(name = "", para = pvalues)
  # paraCheck(name = "", para = phenotypes)
  # paraCheck(name = "", para = interactome)

  object <- new(
    Class = "NWA",
    pvalues = pvalues,
    phenotypes = phenotypes,
    interactome = interactome
  )
}

