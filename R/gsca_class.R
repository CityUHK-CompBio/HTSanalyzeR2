#' An S4 class for Gene Set Collection Analyses on high-throughput data
#'
#' This S4 class includes a series of methods to do gene set enrichment analysis
#' and hypergeometric test for high-throughput data.
#' @section Objects from the Class:
#' Objects of class \code{GSCA} can be created by:
#'
#' \code{new("GSCA", listOfGeneSetCollections, geneList, hits = character())}
#' (see the examples below)
#' @include class_union.R utils.R
#' @aliases GSCA
#' @slot listOfGeneSetCollections A list of gene set collections (a 'gene
#' set collection' is a list of gene sets).
#' @slot geneList A numeric or integer vector of phenotypes named by gene
#' identifiers.
#' @slot hits A character vector of the gene identifiers (used as hits in
#' the hypergeometric tests).It's needed if you want to do GSOA
#' (gene set overrepresentation analysis).
#' @slot para A list of parameters for hypergeometric test and GSEA. These
#' parameters are pValueCutoff, pAdjustMethod, nPermutations, minGeneSetSize
#' and exponent.
#' @slot result A list of results.
#'
#' @slot summary A list of summary information for listOfGeneSetCollections,
#'  geneList, hits, para, and result.
#' @slot preprocessed A single logical value specifying whether or not the
#' input data has been preprocessed.
#' @seealso \code{\link[HTSanalyzeR2]{preprocess}},
#' \code{\link[HTSanalyzeR2]{analyze}},
#' \code{\link[HTSanalyzeR2]{appendGSTerms}},
#' \code{\link[HTSanalyzeR2]{summarize}},
#' \code{\link[HTSanalyzeR2]{report}}
#'
#' @export
#' @examples
#' library(org.Hs.eg.db)
#' library(GO.db)
#' ## load data for enrichment analyses
#' data(d7)
#' phenotype <- as.vector(d7$neg.lfc)
#' names(phenotype) <- d7$id
#'
#' ## select hits if you also want to do GSOA, otherwise ignore it
#' hits <- names(phenotype[which(abs(phenotype) > 2)])
#'
#' ## set up a list of gene set collections
#' GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
#' ListGSC <- list(GO_MF=GO_MF)
#'
#' ## Example1: create an object of class 'GSCA' with hits
#' gsca <- new("GSCA", listOfGeneSetCollections = ListGSC, geneList = phenotype, hits = hits)
#' gsca@@summary
#'
#' ## Example2: create an object of class 'GSCA' without hits
#' gsca <- new("GSCA", listOfGeneSetCollections = ListGSC, geneList = phenotype)
#' gsca@@summary

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

            ## check parameters
            paraCheck("GSCAClass", "gscs", listOfGeneSetCollections)
            paraCheck("GSCAClass", "genelist", geneList)
            if(length(hits) > 0)  paraCheck("GSCAClass", "hits", hits)

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
