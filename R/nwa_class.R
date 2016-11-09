#' @include class_union.R utils.R
setClass(
  Class = "NWA",
  representation(
    pvalues = "numeric",
    phenotypes = "numeric_or_integer",
    interactome = "igraph_or_logical",
    fdr = "numeric",
    result = "list",
    summary = "list",
    preprocessed = "logical"
  ),
  prototype = list(
    pvalues = numeric(),
    phenotypes = as.numeric(NA),
    interactome = NA,
    fdr = 0.001,
    result = list(),
    summary = list(),
    preprocessed = FALSE
  )
)

#' @importFrom igraph vcount ecount
setMethod("initialize",
          signature = "NWA",
          function(.Object,
                   pvalues,
                   phenotypes = NA,
                   interactome = NA) {
            ##check input arguments
            paraCheck.old(name = "pvalues", para = pvalues)
            # if (!is.na(phenotypes))
            #   paraCheck.old(name = "phenotypes", para = phenotypes)
            # if (!is.na(interactome))
            #   paraCheck.old(name = "interactome", para = interactome)
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
            if (!is.na(interactome)) {
              .Object@summary$db[1, "node No"] <- igraph::vcount(interactome)
              .Object@summary$db[1, "edge No"] <- igraph::ecount(interactome)
            }

            .Object@preprocessed <- FALSE
            .Object
          })

#' An S4 class for NetWork Analysis on high-throughput screens
#'
#' This class includes a series of methods to do network analysis for
#' high-throughput screens.
#'
#' @slot pvalues a numeric vector of p-values.
#' @slot phenotypes a numeric or integer vector of phenotypes.
#' @slot interactome an object of class graphNEL.
#' @slot fdr one parameter for BioNet to score nodes in the interactome.
#' @slot result a list consisting of subnetwork module identified by BioNet
#' and a vector of labels for nodes of the subnetwork module.
#' @slot summary a list of summary information for p-values, phenotypes,
#' interactome and result.
#' @slot preprocessed a logical value specifying whether or not input data
#' has been preprocessed.
#'
#' @seealso preprocess analyze summarize interactome report
#'
#' @export
NWA <- function(pvalues, phenotypes = as.numeric(NA), interactome = NA) {
  # paraCheck.old(name = "", para = pvalues)
  # paraCheck.old(name = "", para = phenotypes)
  # paraCheck.old(name = "", para = interactome)

  object <- new(
    Class = "NWA",
    pvalues = pvalues,
    phenotypes = phenotypes,
    interactome = interactome
  )
}

