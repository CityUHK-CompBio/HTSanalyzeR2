#' @include class_union.R utils.R
setClass(
  Class = "GSCA",
  slots = c(
    listOfGeneSetCollections = "list",
    geneList = "numeric_or_integer",
    hits = "character",
    para = "list",
    result = "list",
    summary = "list",
    preprocessed = "logical"
  ),
  prototype = prototype(
    listOfGeneSetCollections = list(),
    geneList = numeric(),
    hits = character(),
    para = list(),
    result = list(),
    summary = list(),
    preprocessed = FALSE
  )
)

setMethod("initialize",
          signature = "GSCA",
          function(.Object,
                   listOfGeneSetCollections,
                   geneList,
                   hits = character()) {
            .Object@listOfGeneSetCollections <- listOfGeneSetCollections
            .Object@geneList <- geneList
            .Object@hits <- hits
            # gene set collections
            .Object@summary$gsc <- geneMatrix(
              rowNames = names(listOfGeneSetCollections),
              colNames = c("input", "above min size")
            )
            # gene list
            .Object@summary$gl <- geneMatrix(
              rowNames = c("Gene List"),
              colNames = c("input", "valid", "duplicate removed",
                           "converted to entrez")
            )
            # hits
            .Object@summary$hits <-
              geneMatrix(rowNames = c("Hits"),
                         colNames = c("input", "preprocessed"))
            # parameters
            .Object@summary$para <- list(
              hypergeo = geneMatrix(
                rowNames = "HyperGeo Test",
                colNames =  c("minGeneSetSize", "pValueCutoff", "pAdjustMethod")
              ),
              gsea = geneMatrix(
                rowNames = "GSEA",
                colNames =  c(
                  "minGeneSetSize",
                  "pValueCutoff",
                  "pAdjustMethod",
                  "nPermutations",
                  "exponent"
                )
              )
            )
            # results
            .Object@summary$results <- geneMatrix(
              rowNames = c("HyperGeo", "GSEA", "Both"),
              colNames =  names(listOfGeneSetCollections)
            )

            # initialize summary info
            .Object@summary$gsc[, "input"] <-
              sapply(listOfGeneSetCollections, length)
            .Object@summary$gl[, "input"] <- length(geneList)
            .Object@summary$hits[, "input"] <- length(hits)
            .Object
          })


#' An S4 class for Gene Set Collection Analyses on high-throughput screens
#'
#' This S4 class includes a series of methods to do gene set enrichment analysis
#' and hypergeometric tests for high-throughput screens.
#'
#' @slot listOfGeneSetCollections A list of gene set collections (a 'gene
#' set collection' is a list of gene sets).
#' @slot geneList A numeric or integer vector of phenotypes named by gene
#' identifiers.
#' @slot hits A character vector of the gene identifiers (used as hits in
#' the hypergeometric tests).
#' @slot para a list of parameters for hypergeometric tests and GSEA. These
#' parameters are pValueCutoff, pAdjustMethod, nPermutations, minGeneSetSize
#' and exponent.
#' @slot result a list of results.
#'
#' @slot summary a list of summary information for listOfGeneSetCollections,
#'  geneList, hits, para, and result.
#' @slot preprocessed a single logical value specifying whether or not the
#' input data has been preprocessed.
#'
#' @seealso \code{\link[HTSanalyzeR2]{preprocess}},
#' \code{\link[HTSanalyzeR2]{analyze}},
#' \code{\link[HTSanalyzeR2]{appendGSTerms}},
#' \code{\link[HTSanalyzeR2]{summarize}},
#' \code{\link[HTSanalyzeR2]{report}}
#'
#' @export


## constructed function
GSCA <- function(listOfGeneSetCollections, geneList, hits = character()) {
  paraCheck("GSCAClass", "gscs", listOfGeneSetCollections)
  paraCheck("GSCAClass", "genelist", geneList)
  if(length(hits) > 0)  paraCheck("GSCAClass", "hits", hits)


  object <- new(
    Class = "GSCA",
    listOfGeneSetCollections = listOfGeneSetCollections,
    geneList = geneList,
    hits = hits
  )
}
