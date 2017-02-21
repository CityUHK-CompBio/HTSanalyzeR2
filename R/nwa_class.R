#' @include class_union.R utils.R
setClass(
  Class = "NWA",
  representation(
    pvalues = "numeric",
    phenotypes = "numeric_or_matrix",
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
            ## check input arguments
            paraCheck("NWAClass", "pvalues", pvalues)
            if (!all(is.na(phenotypes)))
              paraCheck("NWAClass", "phenotypes", phenotypes)
            if (!is.na(interactome))
              paraCheck("NWAClass", "interactome", interactome)

            .Object@pvalues <- pvalues
            .Object@phenotypes <- phenotypes
            .Object@interactome <- interactome

            ## set up summary framework
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
              colNames = c("name", "species", "genetic", "node No", "edge No")
            )
            sum.info.para <- geneMatrix(rowNames = c("Parameter"),
                                        colNames = c("FDR"))
            sum.info.result <- geneMatrix(rowNames = c("Subnetwork"),
                                           colNames = c("node No", "edge No"))

            .Object@summary <- list(
              input = sum.info.input,
              db = sum.info.db,
              para = sum.info.para,
              result = sum.info.result
            )
            ## initialization of summary
            .Object@summary$input["p-values", "input"] <- length(pvalues)
            .Object@summary$input["phenotypes", "input"] <-
              ifelse(is.matrix(phenotypes), nrow(phenotypes), length(phenotypes))
            .Object@summary$db[1, "name"] <- "Unknown"

            if (!is.na(interactome)) {
              .Object@summary$db[1, "node No"] <- igraph::vcount(interactome)
              .Object@summary$db[1, "edge No"] <- igraph::ecount(interactome)
            }

            .Object@preprocessed <- FALSE
            .Object
          })

#' An S4 class for Network Analysis on high-throughput screens
#'
#' This class includes a series of methods to do network analysis for
#' high-throughput screens.
#'
#' @slot pvalues a numeric vector of p-values.
#' @slot phenotypes a numeric or integer vector of phenotypes.
#' @slot interactome an object of class igraph.
#' @slot fdr one parameter for BioNet to score nodes in the interactome.
#' @slot result a list consisting of subnetwork module identified by BioNet
#' and a vector of labels for nodes of the subnetwork module.
#' @slot summary a list of summary information for p-values, phenotypes,
#' interactome and result.
#' @slot preprocessed a logical value specifying whether or not input data
#' has been preprocessed.
#'
#' @seealso \code{\link[HTSanalyzeR2]{preprocess}} \code{\link[HTSanalyzeR2]{analyze}} \code{\link[HTSanalyzeR2]{summarize}} \code{\link[HTSanalyzeR2]{interactome}} \code{\link[HTSanalyzeR2]{report}}
#' @examples
#' # loading the pre_selected sample data
#' data(xn)
#' data(data4enrich)
#' # Conducting one sample t-test & compute the p-values
#' test.stats <- cellHTS2OutputStatTests(cellHTSobject=xn,annotationColumn="GeneID", alternative="two.sided",tests=c("T-test"))
#' library(BioNet)
# pvalues <- BioNet::aggrPvals(test.stats, order=2, plot=FALSE)
# # Conducting the constrction of a S4 class data
# nwa <- NWA(pvalues=pvalues, phenotypes=data4enrich)
#' @export
NWA <- function(pvalues, phenotypes = as.numeric(NA), interactome = NA) {
  object <- new(
    Class = "NWA",
    pvalues = pvalues,
    phenotypes = phenotypes,
    interactome = interactome
  )
}

