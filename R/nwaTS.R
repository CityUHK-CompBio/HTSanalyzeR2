# TIme series NWA
# nwaTS -------------------------------------------------------------------
#' Produce a list of NWA object for NetWork Analysis on high-throughput screens
#'
#' This function will produce a list of NWA object for time-series data to do network analysis
#'
#' @param  object A TSImport object.
#' @param interactome an object of class igraph.
#'
#' @return In the end, this function will return a named list of new NWA object.
#' @seealso \code{\link[HTSanalyzeR2]{NWA}}
#'
#' @export
nwaTS <- function(object, interactome = NA){
  paraCheck("nwaTS", "object", object)
  phenotypesTS <- object@results$phenotypeTS
  pvaluesTS <- object@results$pvaluesTS
  if(length(phenotypesTS) > 0){
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
#'  A preprocessing method for a list of NWA object of Time-series data
#'
#'  This function will do basic preprocessing for each NWA object of 'nwaList'.
#'
#'@param nwaList  A named list of NWA object
#'@param species A single character value specifying the species for which the
#' data should be read.
#'@param initialIDs A single character value specifying the type of initial
#' identifiers for input nwaList.
#'@param keepMultipleMappings A single logical value. If TRUE, the function
#'   keeps the entries with multiple mappings (first mapping is kept). If FALSE,
#'   the entries with multiple mappings will be discarded.
#'@param duplicateRemoverMethod A single character value specifying the method
#'   to remove the duplicates. See duplicateRemover for details.
#'@param verbose A single logical value specifying to display detailed messages
#'   (when verbose=TRUE) or not (when verbose=FALSE), default is TRUE.
#'@return In the end, this function will return an updated list of NWA object.
#'
#'@seealso \code{\link[HTSanalyzeR2]{preprocess}}
#'
#'@export
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
#' Create an interactome from BioGRID data sets
#'
#' This function creates an interactome before conducting network analysis for Time-series data
#'
#' @param nwaList A named list of NWA object
#' @param interactionMatrix an interaction matrix including columns
#' 'InteractionType', 'InteractorA' and 'InteractorB'. If this matrix
#' is available, the interactome can be directly built based on it.
#' @param species a single character value specifying the species for
#' which the data should be read.
#' @param link the link (url) where the data should be downloaded (in
#' tab2 format). The default link is version 3.4.138
#' @param reportDir  a single character value specifying the directory
#' to store reports. The BioGRID data set will be downloaded and stored
#' in a subdirectory called 'Data' in 'reportDir'.
#' @param genetic a single logical value. If TRUE, genetic interactions
#' will be kept; otherwise, they will be removed from the data set.
#' @param force force to download the data set.
#' @param verbose a single logical value indicating to display detailed
#' messages (when verbose=TRUE) or not (when verbose=FALSE)
#'
#' @return In the end, this function will return an updated list of NWA object.
#'
#' @seealso \code{\link[HTSanalyzeR2]{interactome}}
#'
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
#' Subnetwork analysis for Time-series data
#'
#' For each NWA object in 'nwaList', this function will store the subnetwork module identified
#' by BioNet (if species is given, labels of nodes will also be mapped from
#' Entrez IDs to gene symbols), and update information about these results to
#' slot summary of class NWA.
#'
#' @param nwaList  A named list of NWA object
#' @param fdr  a single numeric value specifying the false discovery for the scoring of nodes
#' (see BioNet::scoreNodes and Dittrich et al., 2008 for details)
#' @param species a single character value specifying the species for which the data should be read.
#' @param verbose a single logical value specifying to display detailed messages
#'  (when verbose=TRUE) or not (when verbose=FALSE), default is TRUE.
#'
#' @return In the end, this function will return an updated list of NWA object.
#' @seealso \code{\link[HTSanalyzeR2]{analyze}}
#'
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

