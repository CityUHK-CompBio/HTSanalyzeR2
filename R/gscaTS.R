# Time series

# gscalist ----------------------------------------------------------------
#' @export
#'
gscaList <- function(listOfGeneSetCollections, geneListTS, hitsTS = character()){
  paraCheck("gscaTS", "geneListTS", geneListTS)
  if(length(hitsTS) > 0){
   paraCheck("gscaTS", "hitsTS", hitsTS)
    if(length(hitsTS) != length(geneListTS))
    {stop("'hitsTS' must have the same length as 'geneListTS'")}

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
#' @export
#'
preprocessTS <- function(gscaList, species="Hs", initialIDs="SYMBOL",
                         keepMultipleMappings=TRUE, duplicateRemoverMethod="max",
                         orderAbsValue=FALSE){
    paraCheck("preprocessTS", "gscaList", gscaList)
    tmpName <- names(gscaList)
    tmp <- lapply(gscaList, function(x){
    preprocess(x, species=species, initialIDs=initialIDs,
               keepMultipleMappings=keepMultipleMappings, duplicateRemoverMethod=duplicateRemoverMethod,
               orderAbsValue=orderAbsValue)
  })
    names(tmp) <- tmpName
    tmp
}

# analyze -----------------------------------------------------------------
#' @export
#'
analyzeTS <- function(gscaList, para=list(pValueCutoff=0.05, pAdjustMethod="BH",
                                      nPermutations=1000, minGeneSetSize=15,
                                      exponent=1), doGSOA = FALSE){
              paraCheck("analyzeTS", "gscaList", gscaList)
              tmpName <- names(gscaList)
              tmp <- lapply(gscaList, function(x){
                  analyze(x, para=para, doGSOA = doGSOA)
                        })
              names(tmp) <- tmpName
              tmp
}


# summarizeTS -------------------------------------------------------------
## @export
#'
# summarizeTS <- function(gscaList, what = "ALL"){
#         paraCheck("summarizeTS", "gscaList", gscaList)
#         lapply(gscaList, function(x){
#           HTSanalyzeR2::summarize(x, what = what)
#         })
# }


# appendGSTermsTS ---------------------------------------------------------
#' @export
#'
#'
appendGSTermsTS <- function(gscaList, keggGSCs=NULL, goGSCs=NULL, msigdbGSCs=NULL){
             paraCheck("appendGSTermsTS", "gscaList", gscaList)
             tmpName <- names(gscaList)
             tmp <- lapply(gscaList, function(x){
             appendGSTerms(x, keggGSCs = keggGSCs, goGSCs = goGSCs, msigdbGSCs = msigdbGSCs)
           })
             names(tmp) <- tmpName
             tmp
}

