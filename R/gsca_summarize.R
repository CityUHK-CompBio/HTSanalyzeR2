setGeneric("summarize", function(object, what = "ALL", ...)
  standardGeneric("summarize"), package = "HTSanalyzeR2")
setGeneric("getTopGeneSets", function(object,
                                      resultName,
                                      gscs,
                                      ntop = NULL,
                                      allSig = FALSE)
  standardGeneric("getTopGeneSets"), package = "HTSanalyzeR2")


#' Print summary information for an object of class GSCA or NWA
#'
#' This is a generic function.
#' When implemented as the S4 method for objects of class GSCA or NWA, this
#' function prints a summary of information about the slots of these classes.
#'
#' @describeIn  summarize For an object of class GSCA, the key words are 'GSC'
#' (the slot 'listOfGeneSetCollections'), 'GeneList' (the slot 'geneList'),
#' 'Hits' (the slot 'hits'), 'Para' (the slot 'para'), 'Result' (the slot
#' 'result') and 'ALL' (all slots).
#'
#' @param object A GSCA object or NWA object.
#' @param what A single character value or a character vector of key words
#' specifying what to print (see Methods below). Default will print a summary of all
#' information.
#'
#'
#' @examples
#' # =================================================================
#' # GSCA class
#' library(org.Hs.eg.db)
#' library(GO.db)
#' library(KEGGREST)
#' ## load data for enrichment analyses
#' data(d7)
#' phenotype <- as.vector(d7$neg.lfc)
#' names(phenotype) <- d7$id
#'
#' ## select hits if you also want to do GSOA, otherwise ignore it
#' hits <-  names(phenotype[which(abs(phenotype) > 2)])
#'
#' ## set up a list of gene set collections
#' GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
#' PW_KEGG <- KeggGeneSets(species="Hs")
#' ListGSC <- list(GO_MF=GO_MF, PW_KEGG=PW_KEGG)
#'
#' ## create an object of class 'GSCA'
#' gsca <- new("GSCA", listOfGeneSetCollections = ListGSC, geneList = phenotype, hits = hits)
#'
#' ## do preprocessing
#' gsca <- preprocess(gsca, species="Hs", initialIDs="SYMBOL", keepMultipleMappings=TRUE,
#'                    duplicateRemoverMethod="max", orderAbsValue=FALSE)
#'
#' ## support parallel calculation using doParallel package
#' doParallel::registerDoParallel(cores=4)
#'
#' ## do hypergeometric tests and GSEA
#' gsca <- analyze(gsca, para=list(pValueCutoff=0.05, pAdjustMethod ="BH",
#'                                 nPermutations=100, minGeneSetSize=200, exponent=1),
#'                                 doGSOA = TRUE, doGSEA = TRUE)
#'
#' ## summarize gsca
#' summarize(gsca, what = "ALL")
#' summarize(gsca, what = "Result")
#' @include gsca_class.R
#' @export
setMethod("summarize", signature = "GSCA",
          function(object, what = "ALL") {

            paraCheck("Summarize", "GSCAwhat", what)
            ##what can be "GSC" (gene set collection), "GeneList", "Hits", "Para", "Result"
            if (any(c("ALL", "GSC") %in% what)) {
              cat("\n")
              cat("-No of genes in Gene set collections: \n")
              print(object@summary$gsc, quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "GeneList") %in% what)) {
              cat("\n")
              cat("-No of genes in Gene List: \n")
              print(object@summary$gl, quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "Hits") %in% what)) {
              cat("\n")
              cat("-No of hits: \n")
              print(object@summary$hits, quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "Para") %in% what)) {
              cat("\n")
              cat("-Parameters for analysis: \n")
              print(object@summary$para$hypergeo, quote = FALSE)
              cat("\n")
              print(object@summary$para$gsea, quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "Result") %in% what)) {
              cat("\n")
              cat("-Significant gene sets (adjusted p-value<",
                  object@para$pValueCutoff,
                  "): \n")
              print(object@summary$results, quote = FALSE)
              cat("\n")
            }
          })


#' Select top significant gene sets from results of GSCA object
#'
#' This is a generic function.
#' This function selects top significant gene sets from results of an GSCA object for
#' user-specified gene collections. If 'ntop' is given, then top 'ntop'
#' significant gene sets in gene set collections 'gscs' will be selected
#' and their names will be returned. If 'allSig=TRUE', then all significant
#' (adjusted p-value < 'pValueCutoff' see help("analyze")) gene sets will
#' be selected and their names will be returned.
#'
#' @rdname getTopGeneSets
#'
#' @param object A GSCA object.
#' @param resultName A single character value: 'HyperGeo.results' or
#' 'GSEA.results'.
#' @param gscs A character vector specifying the names of gene set collections
#' from which the top significant gene sets will be selected.
#' @param ntop A single integer or numeric value specifying to select how many
#' gene sets of top significance.
#' @param allSig A single logical value. If 'TRUE', all significant gene sets
#' (adjusted p-value < 'pValueCutoff' of slot 'para') will be selected regardless of 'ntop';
#' otherwise, only top 'ntop' gene sets will be selected.
#'
#' @examples
#' library(org.Hs.eg.db)
#' library(GO.db)
#' library(KEGGREST)
#' ## load data for enrichment analyses
#' data(d7)
#' phenotype <- as.vector(d7$neg.lfc)
#' names(phenotype) <- d7$id
#'
#' ## select hits if you also want to do GSOA, otherwise ignore it
#' hits <-  names(phenotype[which(abs(phenotype) > 2)])
#'
#' ## set up a list of gene set collections
#' GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
#' PW_KEGG <- KeggGeneSets(species="Hs")
#' ListGSC <- list(GO_MF=GO_MF, PW_KEGG=PW_KEGG)
#'
#' ## create an object of class 'GSCA'
#' gsca <- new("GSCA", listOfGeneSetCollections = ListGSC, geneList = phenotype, hits = hits)
#'
#' ## do preprocessing
#' gsca <- preprocess(gsca, species="Hs", initialIDs="SYMBOL", keepMultipleMappings=TRUE,
#'                    duplicateRemoverMethod="max", orderAbsValue=FALSE)
#'
#' ## support parallel calculation using doParallel package
#' doParallel::registerDoParallel(cores=4)
#'
#' ## do hypergeometric tests and GSEA
#' gsca <- analyze(gsca, para=list(pValueCutoff=0.05, pAdjustMethod ="BH",
#'                                 nPermutations=100, minGeneSetSize=200, exponent=1),
#'                                 doGSOA = TRUE, doGSEA = TRUE)
#' summarize(gsca)
#' ## print top significant gene sets in GO_MF
#' topGS_GO_MF <- getTopGeneSets(gsca, "GSEA.results", gscs = "GO_MF", allSig=TRUE)
#'
#' ## print top significant gene sets in GO_MF and PW_KEGG
#' topGS <- getTopGeneSets(gsca, "GSEA.results", gscs = c("GO_MF", "PW_KEGG"), allSig=TRUE)
#'
#' @return A named list of character vectors, each element contains the names of top
#' significant gene sets for each gene set collection.
#' @include gsca_class.R
#' @export
setMethod("getTopGeneSets", signature = "GSCA",
          function(object,
                   resultName,
                   gscs,
                   ntop = NULL,
                   allSig = FALSE) {
            ##check arguments
            if (missing(gscs))
              stop("Please specify the name(s) of Gene Set Collections in 'gscs'! \n")
            paraCheck("Summarize", "gscsNames", gscs)
            paraCheck("Summarize", "resultName", resultName)

            if (!(resultName %in% names(object@result)))
              stop("Please input 'HyperGeo.results' or 'GSEA.results'!\n")
            if (is.null(object@result[[resultName]]))
              stop("Please run Hypergeometric or GSEA analysis before using this function!\n")
            gsc.names <- names(object@result[[resultName]])
            if (!all(gscs %in% gsc.names))
              stop("Wrong Gene Set Collection name(s) in 'gscs'! \n")
            if (!is.null(ntop))
              paraCheck("Summarize", "ntop", ntop)
            paraCheck("Summarize", "allSig", allSig)

            if(is.null(ntop) && !allSig)
              stop("Either specify 'ntop' or set 'allSig' to be TRUE!\n")

            filenames <- list()
            for (gsc in gscs) {
              all.gs.names <- rownames(object@result[[resultName]][[gsc]])
              gs.names <- NULL
              if (allSig) {
                gs.names <-
                  all.gs.names[object@result[[resultName]][[gsc]][, "Adjusted.Pvalue"] < object@para$pValueCutoff]
              } else {
                if (ntop > nrow(object@result[[resultName]][[gsc]])) {
                  # stop("'ntop' is larger than the number of gene sets in specified gene set collection!\n")
                  warning("'ntop' is larger than the number of gene sets in specified gene set collection!\n")
                  ntop <- nrow(object@result[[resultName]][[gsc]])
                }
                if (ntop > 0)
                  gs.names <- all.gs.names[1:ntop]
                else
                  gs.names <- character(0)
              }
              filenames[[gsc]] <-
                unlist(lapply(
                  list(gs.names),
                  gsub,
                  pattern = "/",
                  replacement = "_"
                ))
              names(filenames[[gsc]]) <- gs.names
            }
            return(filenames)
          })

setMethod("show", signature = "GSCA", function(object) {
  cat("A GSCA (Gene Set Collection Analysis) object:\n")
  summarize(object, what = c("GSC", "GeneList", "Hits", "Para"))
})
