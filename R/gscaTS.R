
# gscalist ----------------------------------------------------------------
#' Produce a list of GSCA object for Gene Set Collection Analyses on high-throughput screens
#'
#' This function will produce a list of GSCA object for time-series data to do Gene Set Enrichment Analyses
#'and hypergeometric analysis
#'
#' @param  object A TSImport object.
#' @param  listOfGeneSetCollections A list of gene set collections (a 'gene
#' set collection' is a list of gene sets).
#' @return In the end, this function will return a named list of new GSCA object.
#' @seealso \code{\link[HTSanalyzeR2]{GSCA}}
#' @export
#'
gscaTS <- function(object, listOfGeneSetCollections){
  paraCheck("gscaTS", "object", object)
  geneListTS <- object@results$phenotypeTS
  hitsTS <- object@results$hitsTS

  if(length(hitsTS) > 0){
    tmp <- lapply(1:length(geneListTS), function(x) {
    new("GSCA", listOfGeneSetCollections=listOfGeneSetCollections, geneList=geneListTS[[x]], hits=hitsTS[[x]])
   })} else{
    tmp <-  lapply(geneListTS, function(x) {
       new("GSCA", listOfGeneSetCollections=listOfGeneSetCollections, geneList=x)
     })
   }
  names(tmp) <- names(geneListTS)
  tmp
}


# preprocess --------------------------------------------------------------
#' A preprocessing method for a list of GSCA object of Time-series data
#'
#' This function will do basic preprocessing for each GSCA object of 'gscaList'.
#' @param gscaList A named list of GSCA object
#' @param species A single character value specifying the species for which the
#' data should be read.
#' @param initialIDs A single character value specifying the type of initial
#' identifiers for input gscaList
#' @param keepMultipleMappings A single logical value. If TRUE, the function
#'   keeps the entries with multiple mappings (first mapping is kept). If FALSE,
#'   the entries with multiple mappings will be discarded.
#' @param duplicateRemoverMethod A single character value specifying the method
#'   to remove the duplicates. See duplicateRemover for details.
#' @param orderAbsValue A single logical value indicating whether the values
#'   should be converted to absolute values and then ordered (if TRUE), or
#'   ordered as they are (if FALSE).
#' @param verbose A single logical value specifying to display detailed messages
#'   (when verbose=TRUE) or not (when verbose=FALSE), default is TRUE.
#' @return In the end, this function will return an updated list of GSCA object.
#' @seealso \code{\link[HTSanalyzeR2]{preprocess}}
#' @export
#'
preprocessGscaTS <- function(gscaList, species="Hs", initialIDs="SYMBOL",
                         keepMultipleMappings=TRUE, duplicateRemoverMethod="max",
                         orderAbsValue=FALSE, verbose = TRUE){
    paraCheck("gscaTS", "gscaList", gscaList)
    tmpName <- names(gscaList)
    tmp <- lapply(gscaList, function(x){
    preprocess(x, species=species, initialIDs=initialIDs,
               keepMultipleMappings=keepMultipleMappings, duplicateRemoverMethod=duplicateRemoverMethod,
               orderAbsValue=orderAbsValue, verbose=verbose)
  })
    names(tmp) <- tmpName
    tmp
}

# analyze -----------------------------------------------------------------
#' Gene Set Collection Analysis for Time-series data.
#'
#' For each GSCA object in 'gscaList', this function will store the results from function
#' analyzeGeneSetCollections in slot result, and update information about
#' these results to slot summary of class GSCA.
#' @param gscaList A named list of GSCA object
#' @param para A list of parameters for GSEA and hypergeometric tests. Details please see 'analyze'.
#' @param verbose a single logical value specifying to display detailed messages
#'  (when verbose=TRUE) or not (when verbose=FALSE), default is TRUE.
#' @param doGSOA a single logical value specifying to perform gene set
#' overrepresentation analysis (when doGSOA=TRUE) or not (when doGSOA=FALSE),
#' default is FALSE.
#' @param doGSEA a single logical value specifying to perform gene set
#' enrichment analysis (when doGSEA=TRUE) or not (when doGSEA=FALSE),
#' default is TRUE.
#' @return In the end, this function will return an updated list of GSCA object.
#' @seealso \code{\link[HTSanalyzeR2]{analyze}}
#' @export
#'
analyzeGscaTS <- function(gscaList, para=list(pValueCutoff=0.05, pAdjustMethod="BH",
                                      nPermutations=1000, minGeneSetSize=15,
                                      exponent=1), verbose = TRUE, doGSOA = FALSE,
                          doGSEA = TRUE){
              paraCheck("gscaTS", "gscaList", gscaList)
              tmpName <- names(gscaList)
              tmp <- lapply(gscaList, function(x){
                  analyze(x, para=para, verbose=verbose, doGSOA=doGSOA, doGSEA=doGSEA)
                        })
              names(tmp) <- tmpName
              tmp
}

# appendGSTermsTS ---------------------------------------------------------
#' Append gene set terms to GSCA results for each GSCA object of Time-series data
#'
#' For each GSCA object in 'gscaList', this function
#' finds corresponding annotation terms for GO, KEGG and MSigDB gene sets and
#' inserts a column named "Gene.Set.Term" to each data frame in the GSCA results.
#' @param gscaList A named list of GSCA object.
#' @param keggGSCs a character vector of names of all KEGG gene set collections
#' @param goGSCs a character vector of names of all GO gene set collections
#' @param msigdbGSCs a character vector of names of all MSigDB gene set collections
#'
#' @seealso \code{\link[HTSanalyzeR2]{appendGSTerms}}
#' @export
#'
#'
appendGSTermsTS <- function(gscaList, keggGSCs=NULL, goGSCs=NULL, msigdbGSCs=NULL){
             paraCheck("gscaTS", "gscaList", gscaList)
             tmpName <- names(gscaList)
             tmp <- lapply(gscaList, function(x){
             appendGSTerms(x, keggGSCs = keggGSCs, goGSCs = goGSCs, msigdbGSCs = msigdbGSCs)
           })
             names(tmp) <- tmpName
             tmp
}

