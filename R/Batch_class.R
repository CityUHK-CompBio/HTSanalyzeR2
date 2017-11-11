#' An S4 class for Time series data package in Gene Set Collection Analyses on high-throughput screens
#'
#' This S4 class packages time-series data for further GSCA. To put it more clearly, it'll finally generate a list of
#' GSCA objects for further analyses.
#'
#' @include class_union.R utils.R
#' @slot expInfor A character matrix contains experiment information with each experiment in row and information in column.
#' Should at least contain two columns named as 'ID' and 'Desription'.
#' @slot listOfGeneSetCollections A list of gene set collections (a 'gene
#' set collection' is a list of gene sets).
#' @slot phenotypeTS A list of phenotypes, each element of this list is a numeric vector phenotypes named by gene
#' identifiers for each time point. Note: the order of each element of this list must match the order of 'expInfor' ID.
#' @slot hitsTS A list of hits, each element is a character vector of the gene identifiers (used as hits in
#' the hypergeometric tests). It's needed if you want do GSOA. Note: the order of each element of this list
#'  must match the order of 'expInfor' ID.
#' @slot listOfGSCA A list of initialized GSCA object for futher GSCA.
#'
#' @export
#' @examples
#' data(d7, d13, d25)
#'
#' ## generate expInfor to describe the information of time series data
#' expInfor <- matrix(c("d7", "d13", "d25"), nrow = 3, ncol = 2,
#'                    byrow = F, dimnames = list(NULL, c("ID", "Description")))
#'
#' ## package phenotypeTS into a list of phenotypes
#' datalist <- list(d7, d13, d25)
#' phenotypeTS <- lapply(datalist, function(x) {
#'                       tmp <- as.vector(x$neg.lfc)
#'                       names(tmp) <- x$id
#'                       tmp})
#'
#' ## set up a list of gene set collections
#' GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
#' ListGSC <- list(GO_MF=GO_MF)
#'
#' ## package hitsTS if you also want to do GSOA, otherwise ignore it
#' hitsTS <- lapply(datalist, function(x){
#' tmp <- x[x$neg.p.value < 0.01, "id"]
#' tmp})
#'
#' ## create an object of class GSCABatch with hitsTS
#' GSCABatch <- new("GSCABatch", expInfor = expInfor, phenotypeTS = phenotypeTS,
#'                  listOfGeneSetCollections = ListGSC, hitsTS = hitsTS)
#' GSCABatch
#' ## create an object of class GSCABatch without hitsTS
#' GSCABatch <- new("GSCABatch", expInfor = expInfor, phenotypeTS = phenotypeTS,
#'                  listOfGeneSetCollections = ListGSC)
#' GSCABatch

# GSCABatch ----------------------------------------------------------------
setClass("GSCABatch",
         slot = c(
         expInfor = "matrix",
         listOfGeneSetCollections = "list",
         phenotypeTS = "list",
         hitsTS = "list",
         listOfGSCA = "list"
         ))


setMethod("initialize",
          signature = "GSCABatch",
          function(.Object,
                   expInfor,
                   listOfGeneSetCollections,
                   phenotypeTS,
                   hitsTS = list() ) {
            ## check parameters
            paraCheck("GSCABatch", "expInfor", expInfor)
            paraCheck("GSCABatch", "phenotypeTS", phenotypeTS)
            paraCheck("GSCAClass", "gscs", listOfGeneSetCollections)
            ## phenotypeTS
            if(nrow(expInfor) != length(phenotypeTS)){
              stop("nrow of expInfor should equal to the length of 'phenotypeTS'!\n")
            }
            names(phenotypeTS) <- expInfor[, "ID"]
            ## generate a list of GSCA object based on hitsTS
            if(length(hitsTS) > 0){
              paraCheck("GSCABatch", "hitsTS", hitsTS)
              if(nrow(expInfor) != length(hitsTS)){
                stop("nrow of expInfor should equal to the length of 'hitsTS'!\n")
              }
              names(hitsTS) <- expInfor[, "ID"]
              listOfGSCA <- lapply(1:length(phenotypeTS), function(x) {
                new("GSCA", listOfGeneSetCollections=listOfGeneSetCollections, geneList=phenotypeTS[[x]], hits=hitsTS[[x]])
              })
            } else{
              listOfGSCA <-  lapply(phenotypeTS, function(x) {
                new("GSCA", listOfGeneSetCollections=listOfGeneSetCollections, geneList=x)
              })
               } ## hitsTS judge
            names(listOfGSCA) <- expInfor[, "ID"]
            ## update .Object
            .Object@expInfor <- expInfor
            .Object@listOfGeneSetCollections <- listOfGeneSetCollections
            .Object@phenotypeTS <- phenotypeTS
            .Object@hitsTS <- hitsTS
            .Object@listOfGSCA <- listOfGSCA
            .Object
          })


# show --------------------------------------------------------------------
setMethod("show", signature = "GSCABatch", function(object) {
  cat("A GSCABatch object:\n\n")
  ## experimentName
  cat("-expInfor:\n")
  print(object@expInfor, quote = F)
  cat("\n")
  ## phenotypeTS
  phenotypeTSLength <- unlist(lapply(object@phenotypeTS, length))
  phenotypeTSLength <- matrix(phenotypeTSLength, nrow = 1, dimnames = list(c("length"), c(object@expInfor[, "ID"])))
  cat("-phenotypeTS:\n")
  print(phenotypeTSLength, quote = F)
  cat("\n")
  ## hitsTS
  if(length(object@hitsTS) == 0){
    cat("-hitsTS:", NA, "\n")
  } else{
    hitsTSLength <- unlist(lapply(object@hitsTS, length))
    cat("-hitsTS:\n")
    print(matrix(hitsTSLength, nrow = 1, dimnames = list(c("length"), c(object@expInfor[, "ID"]))), quote = F)
    cat("\n")
  }
})

######################################################################

# NWABatch  ----------------------------------------------------------
## constructed function
#'  An S4 class for Time series data package in NetWork Analysis on high-throughput screens
#'
#' This S4 class packages time-series data for further NWA. To put it more clearly, it'll finally generate a list of
#' NWA objects for further analyses.
#'
#' @slot expInfor A character matrix contains experiment information with each experiment in row and information in column.
#' At least contains two columns named as 'ID' and 'Desription'.
#' @slot phenotypeTS A list of phenotypes, each element of this list is a numeric vector phenotypes named by gene
#' identifiers for each time point. Note: the order of each element of this list must match the order of 'expInfor' ID.
#' @slot pvalueTS  A list of pvalues, each element of this list is a numeric vector pvalues named by gene
#' identifiers for each time point. Note: the order of each element of this list must match the order of 'expInfor' ID.
#' @slot interactome object of class igraph.
#'
#' @export
setClass(
  Class = "NWABatch",
  slots = c(
    expInfor = "matrix",
    pvalueTS = "list",
    phenotypeTS = "list",
    interactome = "igraph_or_logical",
    listOfNWA = "list"
    )
)

#' @importFrom igraph vcount ecount
setMethod("initialize",
          signature = "NWABatch",
          function(.Object,
                   expInfor,
                   pvalueTS,
                   phenotypeTS = list(),
                   interactome = NA,
                   listOfNWA) {
            paraCheck("NWABatch", "expInfor", expInfor)
            paraCheck("NWABatch", "pvalueTS", pvalueTS)
            ## interactome
            if (!is.na(interactome))
              paraCheck("NWAClass", "interactome", interactome)
            ## pvalueTS
            if(nrow(expInfor) != length(pvalueTS)){
              stop("nrow of expInfor should equal to the length of 'pvalueTS'!\n")
            }
            names(pvalueTS) <- expInfor[, "ID"]
            ## phenotypeTS
            if (length(phenotypeTS) > 0){
              paraCheck("NWABatch", "phenotypeTS", phenotypeTS)
              if(nrow(expInfor) != length(phenotypeTS)){
                stop("nrow of expInfor should equal to the length of 'phenotypeTS'!\n")
              }
              names(phenotypeTS) <- expInfor[, "ID"]
              listOfNWA <- lapply(1:length(pvalueTS), function(x) {
                new("NWA", pvalues=pvalueTS[[x]], phenotypes=phenotypeTS[[x]], interactome = interactome)
              })} else{
                listOfNWA <- lapply(pvalueTS, function(x) {
                  new("NWA", pvalues=x, interactome = interactome)
                })
              }
            names(listOfNWA) <- expInfor[, "ID"]
            ## update .Object
            .Object@expInfor <- expInfor
            .Object@pvalueTS <- pvalueTS
            .Object@phenotypeTS <- phenotypeTS
            .Object@interactome <- interactome
            .Object@listOfNWA <- listOfNWA
           .Object
          })


# show --------------------------------------------------------------------
setMethod("show", signature = "NWABatch", function(object) {
  cat("A NWABatch object:\n\n")
  ## experimentName
  cat("-expInfor:\n")
  print(object@expInfor, quote = F)
  cat("\n")
  ## phenotypeTS
  phenotypeTSLength <- unlist(lapply(object@phenotypeTS, length))
  phenotypeTSLength <- matrix(phenotypeTSLength, nrow = 1, dimnames = list(c("length"), c(object@expInfor[, "ID"])))
  cat("-phenotypeTS:\n")
  print(phenotypeTSLength, quote = F)
  cat("\n")
  ## pvalueTS
  if(length(object@pvalueTS) == 0){
    cat("-pvalueTS:", NA, "\n")
  } else{
    pvalueTSLength <- unlist(lapply(object@pvalueTS, length))
    cat("-pvalueTS:\n")
    print(matrix(pvalueTSLength, nrow = 1, dimnames = list(c("length"), c(object@expInfor[, "ID"]))), quote = F)
    cat("\n")
  }
})

