## summarize & default show
if (!isGeneric("preprocess")) {
  setGeneric("preprocess", function(object, ...)
    standardGeneric("preprocess"), package = "HTSanalyzeR2")
}

#' A preprocessing method for objects of class GSCA
#'
#' When implemented as the S4 method for objects of class GSCA, this
#' function filters out invalid data, removes duplicated genes,
#' converts annotations to Entrez identifiers, etc.
#'
#'
#' @param object A GSCA object.
#' @param species A single character value specifying the species
#' for which the data should be read.
#' @param initialIDs A single character value specifying the type of
#' initial identifiers for input geneList
#' @param keepMultipleMappings A single logical value. If TRUE, the
#' function keeps the entries with multiple mappings (first mapping
#' is kept). If FALSE, the entries with multiple mappings will be discarded.
#'
#' @param duplicateRemoverMethod A single character value specifying
#' the method to remove the duplicates. See duplicateRemover for details.
#' @param orderAbsValue A single logical value indicating whether the
#' values should be converted to absolute values and then ordered (if
#' TRUE), or ordered as they are (if FALSE).
#' @param verbose A single logical value specifying to display detailed
#' messages (when verbose=TRUE) or not (when verbose=FALSE)
#'
#'
#' @seealso duplicateRemover annotationConvertor
#'
#' @export
#' @include gsca_class.R
setMethod("preprocess", signature = "GSCA",
          function(object,
                   species = "Dm",
                   initialIDs = "FLYBASECG",
                   keepMultipleMappings = TRUE,
                   duplicateRemoverMethod = "max",
                   orderAbsValue = FALSE,
                   verbose = TRUE) {
            #######################
            ##check input arguments
            #######################
            # paraCheck(name = "species", para = species)
            # paraCheck(name = "initialIDs", para = initialIDs)
            paraCheck(name = "keepMultipleMappings", para = keepMultipleMappings)
            paraCheck(name = "duplicateRemoverMethod", para = duplicateRemoverMethod)
            paraCheck(name = "orderAbsValue", para = orderAbsValue)
            paraCheck(name = "verbose", para = verbose)
            #######################
            ##   preprocessing
            #######################
            cat("-Preprocessing for input gene list and hit list ...\n")

            genelist <- object@geneList
            ##remove NA in geneList
            if (verbose)
              cat("--Removing genes without values in geneList ...\n")
            genelist <-
              genelist[which((!is.na(genelist)) &
                               (names(genelist) != "")
                             & (!is.na(names(genelist))))]

            #genes with valid values
            object@summary$gl[, "valid"] <- length(genelist)
            if (length(genelist) == 0)
              stop("Input 'geneList' contains no useful data!\n")
            ##duplicate remover
            if (verbose)
              cat("--Removing duplicated genes ...\n")
            genelist <-
              duplicateRemover(geneList = genelist, method = duplicateRemoverMethod)

            #genes after removing duplicates
            object@summary$gl[, "duplicate removed"] <-
              length(genelist)
            hits <- object@hits[object@hits != "" & !is.na(object@hits)]
            if (length(hits) == 0)
              stop("Input 'hits' contains no useful data!\n")
            hits <- unique(hits)
            # match.ind <- match(hits, names(genelist))
            # hits.vec <- genelist[match.ind[!is.na(match.ind)]]
            hits.vec <- genelist[names(genelist) %in% hits]
            if (length(hits.vec) == 0)
              stop("Hits and geneList have no overlaps!\n")

            ##annotation convertor
            if (initialIDs != "ENTREZID") {
              if (verbose)
                cat("--Converting annotations ...\n")
              genelist <- annotationConvertor(
                geneList = genelist,
                species = species,
                initialIDs = initialIDs,
                finalIDs = "ENTREZID",
                keepMultipleMappings = keepMultipleMappings,
                verbose = verbose
              )
              hits.vec <- annotationConvertor(
                geneList = hits.vec,
                species = species,
                initialIDs = initialIDs,
                finalIDs = "ENTREZID",
                keepMultipleMappings = keepMultipleMappings,
                verbose = verbose
              )
            }

            #genes after annotation conversion
            object@summary$gl[, "converted to entrez"] <-
              length(genelist)

            if (verbose)
              cat("--Ordering Gene List decreasingly ...\n")
            if (!orderAbsValue)
              genelist <- genelist[order(genelist, decreasing = TRUE)]
            else
              genelist <- abs(genelist)[order(abs(genelist), decreasing = TRUE)]
            hits <- names(hits.vec)
            #hits after preprocessed
            object@summary$hits[, "preprocessed"] <- length(hits)
            ##update genelist and hits, and return object
            object@geneList <- genelist
            object@hits <- hits
            object@preprocessed <- TRUE

            cat("-Preprocessing complete!\n\n")
            object
          })


#' Remove duplicates in a named vector of phenotypes
#'
#' This function gets rid of the duplicates in a vector of phenotypes
#' with gene identifiers as names. It is used to prepare the named vector
#' of phenotypes for the over-representation and enrichment analysis.
#'
#' @param geneList A single named numeric or integer vector with gene
#' identifiers as names
#' @param method A single character value specifying the method to remove
#' the duplicates (should the minimum, maximum or average observation for
#' a same construct be kept). The current version provides "min" (minimum),
#' "max" (maximum), "average" and "fc.avg" (fold change average).
#' The minimum and maximum should be understood in terms of absolute values
#' (i.e. min/max effect, no matter the sign). The fold change average method
#' converts the fold changes to ratios, averages them and converts the average
#' back to a fold change
#'
#' @return A named vector of phenotypes with duplicates removed
#'
#' @seealso preprocess
#'
#' @examples
#' x<-c(5,1,3,-2,6)
#' names(x)<-c("gene1","gene3","gene7","gene3","gene4")
#' xprocessed<-duplicateRemover(geneList=x,method="max")
#'
#' @export
duplicateRemover <- function(geneList, method = "max") {
  ##check arguments
  paraCheck("genelist", geneList)
  paraCheck("duplicateRemoverMethod", method)

  ##Get the unique names and create a vector that will store the
  ##processed values corresponding to those names
  geneList.names <- names(geneList)
  datanames <- unique(geneList.names)

  ##If the absolute value of the min is bigger than the absolute
  ##value of the max, then it is the min that is kept
  if (method == "max") {
    data.processed <- sapply(datanames,
                             function(name) {
                               this.range <- range(geneList[geneList.names == name])
                               this.range[which.max(abs(this.range))]
                             })
  } else if (method == "min") {
    data.processed <- sapply(datanames,
                             function(name) {
                               this.range <- range(geneList[which(geneList.names == name)])
                               this.range[which.min(abs(this.range))]
                             })
  } else if (method == "average") {
    data.processed <- sapply(datanames,
                             function(name) {
                               mean(geneList[geneList.names == name])
                             })
  } else if (method == "fc.avg") {
    neg.fcs <- which(geneList < 1)
    geneListRatios <- geneList
    #convert from fold change to ratio
    geneListRatios[neg.fcs] <- abs(1 / geneList[neg.fcs])
    #average the values across replicates
    data.processed <- sapply(datanames, function(name) {
      mean(geneListRatios[geneList.names == name])
    })
    #convert back to fold change
    neg.fcs <- which(data.processed < 1)
    data.processed[neg.fcs] <- (-1 / data.processed[neg.fcs])
  }
  data.processed
}

#' Convert between different types of gene identifiers
#'
#' This function converts an initial data vector named by non-entrez
#' ids to the same vector but with entrez ids, and vice versa. Genes
#' for which no mapping were found will be removed. This function can
#' also take a matrix, with gene identifiers as row names.
#'
#' @param geneList A named integer or numeric vector, or a matrix with
#' rows named by gene identifiers
#' @param species A single character value specifying the species for
#' which the data should be read.
#' @param initialIDs A single character value specifying the type of
#' initial identifiers for input geneList.
#' @param finalIDs A single character value specifying the type of
#' final identifiers for input geneList.
#' @param keepMultipleMappings A single logical value. If TRUE, the
#' function keeps the entries with multiple mappings (first mapping
#' is kept). If FALSE, the entries with multiple mappings will be discarded.
#' @param verbose A single logical value specifying to display detailed
#' messages (when verbose=TRUE) or not (when verbose=FALSE)
#'
#' @return The same data vector/matrix but with names/row names converted.
#'
#' @examples
#'
#' @export
#' @importFrom AnnotationDbi mapIds
annotationConvertor <- function(geneList,
                                species = "Dm",
                                initialIDs = "ENTREZID",
                                finalIDs = "ENTREZID",
                                keepMultipleMappings = TRUE,
                                verbose = TRUE) {
  ##check arguments
  paraCheck("genelist.general", geneList)
  paraCheck("keepMultipleMappings", keepMultipleMappings)
  paraCheck("verbose", verbose)
  paraCheck(name = "species", para = species)
  # paraCheck("initialIDs", initialIDs)
  # paraCheck("finalIDs", finalIDs)

  annopc <- paste("org", species, "eg", "db", sep = ".")
  if(!(annopc %in% rownames(installed.packages()))) {
    stop(paste(
      'Please install library ',
      annopc,
      ' before running this function!',
      sep = ""
    ))}

  annodb <- tryCatch(
    getFromNamespace(annopc, annopc),
    error = function(e)
      NULL
  )
  if (!(initialIDs %in% columns(annodb))) {
    stop(
      paste(
        "Please provide a valid type of identifiers for the ",
        "'initialIDs' parameters ",
        "(see keytypes(", annopc ,"))\n",
        sep = ""
      )
    )}

  dealMulti <- ifelse(keepMultipleMappings, "first", "filter")
  geneListEntrez <- geneList

  ##if a named vector
  if (!is.matrix(geneList)) {
    namesMapping <-
      AnnotationDbi::mapIds(annodb, keys = names(geneList), keytype = initialIDs, column = finalIDs, multiVals = dealMulti)
    names(geneListEntrez) <- namesMapping[names(geneList)]
    geneListEntrez <- geneListEntrez[!is.na(names(geneListEntrez))]
    if (verbose) {
      cat(
        "--",
        paste((length(geneList) - length(geneListEntrez)),
              " genes (out of ",
              length(geneList) ,
              ") could not be mapped to any identifier, ",
              "and were removed from the data. \n"
        )
      )}
  } else {
    namesMapping <-
      AnnotationDbi::mapIds(annodb, keys = rownames(geneList), keytype = initialIDs, column = finalIDs, multiVals = dealMulti)
    rownames(geneListEntrez) <- namesMapping[rownames(geneList)]
    geneListEntrez <- geneListEntrez[!is.na(rownames(geneListEntrez))]
    if (verbose) {
      cat(
        "--",
        paste((nrow(geneList) - nrow(geneListEntrez)),
              " genes (out of ",
              nrow(geneList) ,
              ") could not be mapped to any identifier, ",
              "and were removed from the data. \n"
        )
      )}
  }

  return(geneListEntrez)
}
