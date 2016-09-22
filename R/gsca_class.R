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
                   hits) {
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
              colNames = c("input", "valid", "duplicate removed", "converted to entrez")
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
#' @slot listOfGeneSetCollections A list of gene set collections (a 'gene
#' set collection' is a list of gene sets).
#' @slot geneList A numeric or integer vector of phenotypes named by gene
#' identifiers.
#' @slot hits A character vector of the gene identifiers (used as hits in
#' the hypergeometric tests).
#'
#' @export
GSCA <- function(listOfGeneSetCollections, geneList, hits) {
  paraCheck(name = "gscs", para = listOfGeneSetCollections)
  paraCheck(name = "genelist", para = geneList)
  paraCheck(name = "hits", para = hits)

  object <- new(
    Class = "GSCA",
    listOfGeneSetCollections = listOfGeneSetCollections,
    geneList = geneList,
    hits = hits
  )
}
