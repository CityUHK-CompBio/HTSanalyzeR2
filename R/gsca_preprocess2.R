## summarize & default show
if (!isGeneric("preprocess2")) {
  setGeneric("preprocess2", function(object, ...)
    standardGeneric("preprocess2"), package = "HTSanalyzeR2")
}

#' @export
#' @include gsca_class.R
setMethod("preprocess2", signature = "GSCA",
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
              duplicateRemover2(geneList = genelist, method = duplicateRemoverMethod)

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
              genelist <- annotationConvertor2(
                geneList = genelist,
                species = species,
                initialIDs = initialIDs,
                finalIDs = "ENTREZID",
                keepMultipleMappings = keepMultipleMappings,
                verbose = verbose
              )
              hits.vec <- annotationConvertor2(
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


#' This function gets rid of the duplicates in a gene list.
#' @export
duplicateRemover2 <- function(geneList, method = "max") {
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
#' @export
#' @import AnnotationDbi
annotationConvertor2 <- function(geneList,
                                species = "Dm",
                                initialIDs = "ENTREZID",
                                finalIDs = "ENTREZID",
                                keepMultipleMappings = TRUE,
                                verbose = TRUE) {
  ##check arguments
  paraCheck("genelist.general", geneList)
  paraCheck("keepMultipleMappings", keepMultipleMappings)
  paraCheck("verbose", verbose)

  ##convert annotations if initialIDs are not Entrez
  ##	if(initialIDs != "Entrez.gene") {
  ##check species
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
