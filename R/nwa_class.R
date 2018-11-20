#' @include class_union.R utils.R
setClass(
  Class = "NWA",
  slots = c(
    pvalues = "numeric_or_integer",
    phenotypes = "numeric_or_integer",
    interactome = "igraph_or_logical",
    fdr = "numeric",
    result = "list",
    summary = "list",
    preprocessed = "logical"
  ),
  prototype = list(
    pvalues = numeric(),
    phenotypes = numeric(),
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
                   phenotypes = as.numeric(),
                   interactome = NA) {

            ## check input arguments
            paraCheck("NWAClass", "pvalues", pvalues)
            if (length(phenotypes) > 0){
              paraCheck("NWAClass", "phenotypes", phenotypes)
            }
            if (any(!is.na(interactome))){
              paraCheck("NWAClass", "interactome", interactome)}

            ## update NWA object
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

            .Object@summary$input["phenotypes", "input"] <- length(phenotypes)

            .Object@summary$db[1, "name"] <- "Unknown"

            if (any(!is.na(interactome))){
              .Object@summary$db[1, "node No"] <- igraph::vcount(interactome)
              .Object@summary$db[1, "edge No"] <- igraph::ecount(interactome)
            }

            .Object@preprocessed <- FALSE
            .Object
          })

#' An S4 class for NetWork Analysis on high-throughput data
#'
#' This class includes a series of methods to do network analysis for
#' high-throughput data.
#' @aliases NWA NWA-class
#' @param pvalues A numeric or integer vector of pvalues named by gene identifiers.
#' @param phenotypes A numeric or integer vector of phenotypes named by gene identifiers.
#' When it is available, nodes in identified subnetwork would be coloured by it
#' (red:+, blue:- as default). Otherwise, all nodes in the subnetwork would have no difference.
#' @param interactome An object of class igraph.
#' @slot fdr One parameter for BioNet to score nodes in the interactome.
#' @slot result A list consisting of subnetwork module identified by BioNet
#' and a vector of labels for nodes of the subnetwork module.
#' @slot summary A list of summary information for pvalues, phenotypes,
#' interactome and result.
#' @slot preprocessed A logical value specifying whether or not input data
#' has been preprocessed.
#' @usage NWA(pvalues, phenotypes = as.numeric(), interactome = NA)
#' @return This function will create a new object of class 'NWA'.
#' @seealso \code{\link[HTSanalyzeR2]{preprocess}},
#'  \code{\link[HTSanalyzeR2]{analyze}},
#'   \code{\link[HTSanalyzeR2]{summarize}},
#'    \code{\link[HTSanalyzeR2]{interactome}},
#'     \code{\link[HTSanalyzeR2]{report}}
#' @examples
#' library(org.Hs.eg.db)
#' library(GO.db)
#' ## load data for network analyses
#' data(d7)
#' pvalues <- d7$neg.p.value
#' names(pvalues) <- d7$id
#'
#' ## define phenotypes if you want to color nodes by it, otherwise ignore it!
#' phenotypes <- as.vector(d7$neg.lfc)
#' names(phenotypes) <- d7$id
#'
#' ## Example1: create an object of class 'NWA' with phenotypes
#' nwa <- NWA(pvalues=pvalues, phenotypes=phenotypes)
#'
#' ## Example2: create an object of class 'NWA' without phenotypes
#' nwa <- NWA(pvalues=pvalues)
#' @export
NWA <- function(pvalues, phenotypes = as.numeric(), interactome = NA) {
  ## check input arguments
  paraCheck("NWAClass", "pvalues", pvalues)
  if (length(phenotypes) > 0){
    paraCheck("NWAClass", "phenotypes", phenotypes)
  }
  if (!is.na(interactome))
    paraCheck("NWAClass", "interactome", interactome)

  ## initialize a new object
  object <- methods::new(
    Class = "NWA",
    pvalues = pvalues,
    phenotypes = phenotypes,
    interactome = interactome
  )
}
