# TIme series NWA




# nwaTS -------------------------------------------------------------------

#' @export
nwaTS <- function(pvaluesTS, phenotypesTS = as.numeric(), interactome = NA){
  paraCheck("nwaTS", "pvaluesTS", pvaluesTS)
  if(length(phenotypesTS) > 0){
    paraCheck("nwaTS", "phenotypesTS", phenotypesTS)
    if(length(pvaluesTS) != length(phenotypesTS))
    {stop("'pvaluesTS' must have the same length as 'phenotypesTS'")}
    tmp <- lapply(1:length(pvaluesTS), function(x) {
      new("NWA", pvalues=pvaluesTS[[x]], phenotypes=phenotypesTS[[x]], interactome = interactome)
    })} else{
      tmp <-  lapply(pvaluesTS, function(x) {
        new("NWA", pvalues=x, interactome = interactome)
      })
    }
   names(tmp) <- names(pvaluesTS)
  tmp
}


# preproocess -------------------------------------------------------------

#' @export
preprocessNwaTS <- function(nwaList, species="Hs", initialIDs="SYMBOL",
                            keepMultipleMappings=TRUE, duplicateRemoverMethod="max", verbose = TRUE){
  paraCheck("nwaTS", "nwaList", nwaList)
  tmpName <- names(nwaList)
  tmp <- lapply(nwaList, function(x){
    preprocess(x, species=species, initialIDs=initialIDs,
               keepMultipleMappings=keepMultipleMappings,
               duplicateRemoverMethod=duplicateRemoverMethod, verbose=verbose)
  })
  names(tmp) <- tmpName
  tmp
}


# interactome -------------------------------------------------------------

#' @export
interactomeNwaTS <- function(nwaList, interactionMatrix = NULL, species,
                             link = "http://thebiogrid.org/downloads/archives/Release%20Archive/BIOGRID-3.4.138/BIOGRID-ORGANISM-3.4.138.tab2.zip",
                             reportDir = "HTSanalyzerReport", genetic = FALSE,
                             force = FALSE, verbose = TRUE){
  paraCheck("nwaTS", "nwaList", nwaList)
  tmpName <- names(nwaList)
  tmp <- lapply(nwaList, function(x){
    interactome(x, interactionMatrix=interactionMatrix, species=species,
                link=link, reportDir=reportDir, genetic=genetic, force=force, verbose=verbose)
  })
  names(tmp) <- tmpName
  tmp
}



# analyze -----------------------------------------------------------------

#' @export
analyzeNwaTS <- function(nwaList,  fdr = 0.001,
                         species,
                         verbose = TRUE){
  paraCheck("nwaTS", "nwaList", nwaList)
  tmpName <- names(nwaList)
  tmp <- lapply(nwaList, function(x){
    analyze(x, fdr=fdr, species=species, verbose=verbose)
  })
  names(tmp) <- tmpName
  tmp
}

