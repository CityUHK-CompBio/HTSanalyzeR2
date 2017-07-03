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
   lapply(1:length(geneListTS), function(x) {
    new("GSCA", listOfGeneSetCollections=listOfGeneSetCollections, geneList=geneListTS[[x]], hits=hitsTS[[x]])
   })} else{
     lapply(geneListTS, function(x) {
       new("GSCA", listOfGeneSetCollections=listOfGeneSetCollections, geneList=x)
     })
  }
}


# preprocess --------------------------------------------------------------
#' @export
#'
preprocessTS <- function(gscaList, species="Hs", initialIDs="SYMBOL",
                         keepMultipleMappings=TRUE, duplicateRemoverMethod="max",
                         orderAbsValue=FALSE){
    paraCheck("preprocessTS", "gscaList", gscaList)
    lapply(gscaList, function(x){
    preprocess(x, species=species, initialIDs=initialIDs,
               keepMultipleMappings=keepMultipleMappings, duplicateRemoverMethod=duplicateRemoverMethod,
               orderAbsValue=orderAbsValue)
  })
}

# analyze -----------------------------------------------------------------
#' @export
#'
analyzeTS <- function(gscaList, para=list(pValueCutoff=0.05, pAdjustMethod="BH",
                                      nPermutations=1000, minGeneSetSize=15,
                                      exponent=1), doGSOA = FALSE){
              paraCheck("analyzeTS", "gscaList", gscaList)
              lapply(gscaList, function(x){
                  analyze(x, para=para, doGSOA = T)
                        })
}



