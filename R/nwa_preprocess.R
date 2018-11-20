if (!isGeneric("preprocess")) {
  setGeneric("preprocess", function(object, ...)
    standardGeneric("preprocess"), package = "HTSanalyzeR2")
}
if (!isGeneric("interactome")) {
  setGeneric("interactome", function(object, ...)
    standardGeneric("interactome"), package = "HTSanalyzeR2")
}


#' @rdname preprocess
#' @examples
#' # ===========================================================
#' # NWA class
#' library(org.Hs.eg.db)
#' library(GO.db)
#' ## load data for subnetwork analyses
#' data(d7)
#' pvalues <- d7$neg.p.value
#' names(pvalues) <- d7$id
#'
#' ## input phenotypes if you want to color nodes by it
#' phenotypes <- as.vector(d7$neg.lfc)
#' names(phenotypes) <- d7$id
#'
#' ## create an object of class 'NWA' with phenotypes
#' nwa <- NWA(pvalues=pvalues, phenotypes=phenotypes)
#'
#' ## do preprocessing
#' nwa1 <- preprocess(nwa, species="Hs", initialIDs="SYMBOL", keepMultipleMappings=TRUE,
#'                    duplicateRemoverMethod="max")
#' @export
#' @include nwa_class.R
setMethod("preprocess", signature = "NWA",
  function(object,
           species = "Hs",
           duplicateRemoverMethod = "max",
           initialIDs = "SYMBOL",
           keepMultipleMappings = TRUE,
           verbose = TRUE) {

    ## check input arguments
    paraCheck("General", "verbose", verbose)
    paraCheck("LoadGeneSets", "species", species)
    paraCheck("Annotation", "initialIDs", initialIDs)
    paraCheck("Annotation", "keepMultipleMappings", keepMultipleMappings)
    paraCheck("PreProcess", "duplicateRemoverMethod", duplicateRemoverMethod)

    cat("-Preprocessing for input p-values and phenotypes ...\n")
    pvalues <- object@pvalues
    phenotypes <- object@phenotypes


    ## 1.remove NA
    if(verbose) cat("--Removing invalid p-values and phenotypes ...\n")

      pvalues <- pvalues[which(!is.na(pvalues)
                               & names(pvalues) != "" & !is.na(names(pvalues)))]
      ## valid p-values
      object@summary$input[1, "valid"] <- length(pvalues)
      if(length(pvalues) == 0)
        stop("Input 'pvalues' contains no useful data!\n")

    #----------------------------------------------------------------------
    if(length(phenotypes) > 0) {

        phenotypes <- phenotypes[which((!is.na(phenotypes)) &
                                         (names(phenotypes) != "" &
                                            !is.na(names(phenotypes))))]
        ## valid phenotypes
        object@summary$input[2,"valid"] <- length(phenotypes)
        if(length(phenotypes) == 0)
          stop("Input 'phenotypes' contains no useful data!\n")
    }

    ## 2.duplicate remover
    if(verbose) cat("--Removing duplicated genes ...\n")
    pvalues <- duplicateRemover(geneList = pvalues,
                                method = duplicateRemoverMethod)
    ## p-values after removing duplicates
    object@summary$input[1, "duplicate removed"] <- length(pvalues)
   #-----------------------------------------------------------------------
    if(length(phenotypes) > 0) {
        phenotypes <- duplicateRemover(geneList = phenotypes,
                                       method = duplicateRemoverMethod)
        ## phenotypes after removing duplicates
        object@summary$input[2, "duplicate removed"] <- length(phenotypes)
    }

    ## 3.convert annotations in pvalues
    if(initialIDs != "ENTREZID") {
      if(verbose) cat("--Converting annotations ...\n")
      pvalues <- annotationConvertor(
        geneList = pvalues,
        species = species,
        initialIDs = initialIDs,
        finalIDs = "ENTREZID",
        keepMultipleMappings = keepMultipleMappings,
        verbose = verbose
      )
      if(length(phenotypes) > 0) {
        phenotypes <- annotationConvertor(
          geneList = phenotypes,
          species = species,
          initialIDs = initialIDs,
          finalIDs = "ENTREZID",
          keepMultipleMappings = keepMultipleMappings,
          verbose = verbose
        )
      }
    }
    ## p-values after annotation conversion
    object@summary$input[1, "converted to entrez"] <- length(pvalues)
    ## phenotypes after annotation conversion
    if(length(phenotypes) > 0)
      object@summary$input[2,"converted to entrez"] <- length(phenotypes)

    ## 5.update genelist and hits, and return object
    object@pvalues <- pvalues
    object@phenotypes <- phenotypes
    object@preprocessed <- TRUE

    cat("-Preprocessing complete!\n\n")
    object
  }
)


#' Create an interactome from BioGRID database or input custom interactome
#'
#' This is a generic function.
#' When implemented as the S4 method of class NWA, this function creates an
#' interactome before conducting network analysis.
#'
#' @aliases interactome
#' @rdname interactome
#' @param object An NWA object.
#' @param interactionMatrix An interaction matrix including columns
#' 'InteractionType', 'InteractorA' and 'InteractorB'. If this matrix
#' is available, the interactome can be directly built based on it instead
#' of downloading from BioGRID.
#' @param species A single character value specifying the species for which the data should be downloaded.
#' The current version supports one of the following species: "Dm" ("Drosophila_melanogaster"),
#'  "Hs" ("Homo_sapiens"), "Rn" ("Rattus_norvegicus"), "Mm" ("Mus_musculus"),
#'  "Ce" ("Caenorhabditis_elegans").
#' @param link The link (url) where the data should be downloaded (in
#' tab2 format). The default link is version 3.4.138 of BioGRID.
#' @param reportDir A single character value specifying the directory
#' to store reports. The BioGRID data set will be downloaded and stored
#' in a subdirectory called 'Data' in 'reportDir'.
#' @param genetic A single logical value. If TRUE, genetic interactions
#' will be kept; otherwise, they will be removed from the data set.
#' @param force Force to download the data set.
#' @param verbose A single logical value indicating to display detailed
#' messages (when verbose=TRUE) or not (when verbose=FALSE)
#' @details
#' This function provides two options to create an interactome for network analysis.
#' The user can either input an interaction matrix including columns 'InteractionType',
#' 'InteractionA' and 'InteractionB', or set 'species', 'link' and 'genetic' to download
#' data set from BioGRID and extract corresponding interactions to build the interactome.
#'
#' Another way to set up the interactome is to input an igraph object when the NWA object
#' is created (i.e. nwa=NWA(pvalues, phenotypes, interactome)).
#' @examples
#' library(org.Hs.eg.db)
#' library(GO.db)
#' ## load data for subnetwork analyses
#' data(d7)
#' pvalues <- d7$neg.p.value
#' names(pvalues) <- d7$id
#'
#' ## input phenotypes if you want to color nodes by it
#' phenotypes <- as.vector(d7$neg.lfc)
#' names(phenotypes) <- d7$id
#'
#' ## Example1: create an object of class 'NWA' by inputting an igraph object as the interactome
#' data(Biogrid_HS_Interactome)
#' nwa <- NWA(pvalues=pvalues, phenotypes=phenotypes, interactome=Biogrid_HS_Interactome)
#'
#'
#' ## Example2: create an object of class 'NWA' without interactome
#' nwa <- NWA(pvalues=pvalues, phenotypes=phenotypes)
#' ## create an interactome for nwa by inputting an interaction matrix
#' data(Biogrid_HS_Mat)
#' nwa1 <- interactome(nwa, interactionMatrix = Biogrid_HS_Mat, genetic=FALSE)
#'
#' ## Example3: create an object of class 'NWA' without interactome
#' nwa <- NWA(pvalues=pvalues, phenotypes=phenotypes)
#' \dontrun{
#' ## create an interactome for nwa by downloading for BioGRID database
#' nwa1 <- interactome(nwa, species="Hs", reportDir="HTSanalyzerReport", genetic=FALSE)
#' }
#' @export
#' @return In the end, this function will return an updated object with slot 'interactome'
#' as an object of class 'igraph'.
#' @importFrom BioNet makeNetwork
#' @importFrom igraph vcount ecount
setMethod(
  "interactome",
  signature = "NWA",
  function(object, interactionMatrix = NULL, species,
           link = "https://downloads.thebiogrid.org/Download/BioGRID/Release-Archive/BIOGRID-3.4.162/BIOGRID-ORGANISM-3.4.162.tab2.zip",
           reportDir = "HTSanalyzerReport", genetic = FALSE,
           force = FALSE, verbose = TRUE) {
    ## check arguments
    cat("-Creating interactome ...\n")
    if(missing(species) && is.null(interactionMatrix))
      stop("You should either input 'interactionMatrix' or ",
           "specifiy 'species' to download biogrid dataset!\n")
    if(is.null(interactionMatrix) && !missing(species)) {
      paraCheck("LoadGeneSets", "species", species)
      object@summary$db[, "species"] <- species
    }

    paraCheck("PreProcess", "genetic", genetic)
    paraCheck("General", "verbose", verbose)
    paraCheck("Report", "reportDir", reportDir)

    object@summary$db[, "genetic"] <- genetic

    ## 4.make interactome
    ## download the data from the BioGRID, if no data matrix is
    #  specified by the argument 'interactionMatrix'
    if(is.null(interactionMatrix)) {
      paraCheck("PreProcess", "link", link)
      ## create folders for biogrid date downloading
      biogridDataDir = file.path(reportDir, "Data")
      if(!file.exists(reportDir))
        dir.create(reportDir)

      InteractionsData <- biogridDataDownload(link = link,
                                              species = species,
                                              dataDirectory = biogridDataDir,
                                              force = force,
                                              verbose = verbose)

      object@summary$db[1, "name"] <- "Biogrid"
    } else {
      paraCheck("PreProcess", "interactionMatrix", interactionMatrix)
      InteractionsData <- interactionMatrix
      object@summary$db[1, "name"] <- "User-input"
    }
    ## If it is specified that genetic interactions should be
    #  discarded, remove those rows
    if(!genetic)
      InteractionsData <- InteractionsData[
        which(InteractionsData[, "InteractionType"] != "genetic"), ]
    ## Make an igraph object from the data
    object@interactome <- BioNet::makeNetwork(
      source = InteractionsData[, "InteractorA"],
      target = InteractionsData[, "InteractorB"],
      edgemode = "undirected",
      format = "igraph")
    ## update graph info in summary
    object@summary$db[, "node No"] <- igraph::vcount(object@interactome)
    object@summary$db[, "edge No"] <- igraph::ecount(object@interactome)

    cat("-Interactome created! \n\n")
    object
  }
)


# This functions downloads the data from the Biogrid, puts it in the
# user-specified folder and extracts the interactions data for a given
# species.
#' @importFrom data.table fread
#' @importFrom utils download.file unzip
biogridDataDownload <- function(link, species = "Hs", dataDirectory = ".",
                                force = FALSE, verbose = TRUE) {
  ## check arguments
  if(!missing(link) && !is.null(link))
    paraCheck("PreProcess", "link", link)
  else
    link <- "https://downloads.thebiogrid.org/Download/BioGRID/Release-Archive/BIOGRID-3.4.162/BIOGRID-ORGANISM-3.4.162.tab2.zip"
  paraCheck("LoadGeneSets", "species", species)
  paraCheck("PreProcess", "dataDirectory", dataDirectory)
  paraCheck("General", "verbose", verbose)

  ## need to add more
  bionet.species <- c("Drosophila_melanogaster", "Homo_sapiens",
                      "Rattus_norvegicus", "Mus_musculus",
                      "Caenorhabditis_elegans")
  names(bionet.species) <- c("Dm", "Hs", "Rn", "Mm", "Ce")


  ## download the data from the BioGRID and put the compressed file
  # into directory, then unzip the file

  if(!file.exists(dataDirectory) || force) {
    if(verbose)
      cat("--Downloading BioGRID interactome dataset ...\n")

    if(!file.exists(dataDirectory)) dir.create(dataDirectory)

    download.file(url = link, 
                  destfile = file.path(dataDirectory, "Biogrid-all-org"), 
                  quiet=TRUE)
    unzip(zipfile = file.path(dataDirectory, "Biogrid-all-org"),
          exdir = dataDirectory)
  } else {
    if(verbose)
      cat("--Found local BioGRID interactome dataset!\n")
  }
  ## the data directory now contains one tab delimited file for each
  # species in the biogrid data the next few lines find and read the
  # file that contains the data for the species that we want to use
  listfiles <- list.files(dataDirectory)

  biogridSpecies <- fread(
    file.path(dataDirectory, grep(bionet.species[species],
              listfiles, value = TRUE)), header = TRUE, skip=0,
              data.table = FALSE, stringsAsFactors = FALSE)

  ## Extract the relevant columns from the tab-delimited file that was read
  source <- as.character(biogridSpecies[, "Entrez Gene Interactor A"])
  target <- as.character(biogridSpecies[, "Entrez Gene Interactor B"])
  interac.type <- as.character(biogridSpecies[, "Experimental System Type"])
  ## Create a matrix from the data, and names its columns accordingly
  interactions <- cbind(source, target, interac.type)
  colnames(interactions) <- c("InteractorA", "InteractorB", "InteractionType")
  ## pre-process network: duplication removing(same interactions from different experiment)
  ## and single node edge
  interactions <- unique(interactions)
  dupRow <- unlist(sapply(1:nrow(interactions), function(x) {
    if(interactions[x, 1] == interactions[x, 2]){
      x
    }
  }))
  interactions <- interactions[-dupRow, ]
  #--------------------------------------------
  interactions
}


