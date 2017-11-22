if (!isGeneric("preprocess")) {
  setGeneric("preprocess", function(object, ...)
    standardGeneric("preprocess"), package = "HTSanalyzeR2")
}

#' A preprocessing method for objects of class GSCA or NWA
#'
#' This is a generic function. When implemented as the S4 method for objects of
#' class \code{\link[HTSanalyzeR2]{GSCA-class}} or \code{\link[HTSanalyzeR2]{NWA-class}},
#' this function filters out invalid data, removes duplicated
#' genes, converts annotations to Entrez identifiers, etc.
#'
#' @include gsca_class.R
#' @rdname preprocess
#' @aliases preprocess
#' @param object A \code{\link[HTSanalyzeR2]{GSCA-class}} or \code{\link[HTSanalyzeR2]{NWA-class}} object.
#' @param species A single character value specifying the species of the input.
#' It supports all the species of OrgDb objects in AnnotationDbi.
#' The format should be an abbreviation of the organism as setted by AnnotationDbi.
#' For example, the commonly used ones are "Dm" ("Drosophila_melanogaster"),
#' "Hs" ("Homo_sapiens"), "Rn" ("Rattus_norvegicus"), "Mm" ("Mus_musculus"),
#' "Ce" ("Caenorhabditis_elegans"), and etc.
#'
#' @param initialIDs A single character value specifying the type of
#' initial identifiers for input 'geneList'. The valid terms need match with
#' the keytypes of species db such as keytypes(org.Hs.eg.db).
#' @param keepMultipleMappings A single logical value. If TRUE, the function
#'   keeps the entries with multiple mappings (first mapping is kept). If FALSE,
#'   the entries with multiple mappings will be discarded.
#'
#' @param duplicateRemoverMethod A single character value specifying the method
#'   to remove the duplicates. See help(duplicateRemover) for details.
#' @param orderAbsValue A single logical value indicating whether the values
#'   should be converted to absolute values and then ordered (if TRUE), or
#'   ordered as they are (if FALSE). This argument is only for class \code{\link[HTSanalyzeR2]{GSCA-class}}.
#' @param verbose A single logical value specifying to display detailed messages
#'   (when verbose=TRUE) or not (when verbose=FALSE).
#' @return In the end, this function will return an updated object of class
#' \code{\link[HTSanalyzeR2]{GSCA-class}} or \code{\link[HTSanalyzeR2]{NWA-class}}.
#' @seealso \code{\link[HTSanalyzeR2]{duplicateRemover}}, \code{\link[HTSanalyzeR2]{annotationConvertor}}
#' @examples
#' # ===========================================================
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
#' gsca1 <- preprocess(gsca, species="Hs", initialIDs="SYMBOL", keepMultipleMappings=TRUE,
#'                    duplicateRemoverMethod="max", orderAbsValue=FALSE)
#'
#' ## print gsca1
#' gsca1
#'
#' @details
#' This function will do the following preprocessing steps:
#'
#' 1:filter out p-values (the slot \emph{pvalues} of class NWA), phenotypes
#'  (the slot \emph{phenotypes} of class NWA) and data for enrichment (the slot
#'  \emph{geneList} of class GSCA) with NA values or without valid names, and invalid
#'   gene names (the slot \emph{hits} of class GSCA);
#'
#' 2:invoke function duplicateRemover to remove duplicated genes in the slot \emph{pvalues},
#'  \emph{phenotypes} of class NWA, and the slot \emph{geneList} and \emph{hits} of class GSCA;
#'
#' 3:invoke function annotationConvertor to convert annotations from initialIDs
#' to Entrez identifiers. Please note that the slot \emph{hits} and the names of the slot
#' \emph{geneList} of class GSCA, the names of the slot \emph{pvalues} and the names of the slot
#' \emph{phenotypes} of class NWA must have the same type of gene annotation specified by
#' initialIDs;
#'
#' 4:order the data for enrichment decreasingly for objects of class GSCA.
#'
#' See the function duplicateRemover for more details about how to remove
#' duplicated genes.
#'
#' See the function annotationConvertor for more details about how to convert
#' annotations.
#' @export

setMethod("preprocess", signature = "GSCA",
          function(object,
                   species = "Hs",
                   initialIDs = "SYMBOL",
                   keepMultipleMappings = TRUE,
                   duplicateRemoverMethod = "max",
                   orderAbsValue = FALSE,
                   verbose = TRUE) {

            paraCheck("General", "species", species)
            paraCheck("General", "verbose", verbose)
            paraCheck("PreProcess", "orderAbsValue", orderAbsValue)
            paraCheck("PreProcess", "duplicateRemoverMethod", duplicateRemoverMethod)
            paraCheck("Annotataion", "initialIDs", initialIDs)
            paraCheck("Annotataion", "keepMultipleMappings", keepMultipleMappings)


            ## preprocessing==============================================================
            cat("-Preprocessing for input gene list and hit list ...\n")

            ## genelist preprocessing-----------------------------------------------------
            genelist <- object@geneList

            ## remove NA in geneList
            if (verbose)
              cat("--Removing genes without values in geneList ...\n")
            genelist <-
              genelist[which((!is.na(genelist)) &
                               (names(genelist) != "")
                             & (!is.na(names(genelist))))]

            ## genes with valid values
            object@summary$gl[, "valid"] <- length(genelist)
            if (length(genelist) == 0)
              stop("Input 'geneList' contains no useful data!\n")

            ## duplicate remover
            if (verbose)
              cat("--Removing duplicated genes ...\n")
            genelist <-
              duplicateRemover(geneList = genelist, method = duplicateRemoverMethod)

            ## genes after removing duplicates
            object@summary$gl[, "duplicate removed"] <- length(genelist)

            ## hits preprocessing---------------------------------------------------------
            if(length(object@hits) > 0){
                  hits <- object@hits[object@hits != "" & !is.na(object@hits)]
                  if (length(hits) == 0)
                    stop("test Input 'hits' contains no useful data!\n")

                  hits <- unique(hits)
                  hits.vec <- genelist[names(genelist) %in% hits]
                  if (length(hits.vec) == 0)
                    stop("Hits and geneList have no overlaps!\n")
            }  ## finish hits preprocessing


            ## annotation convertor=======================================================
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

              if(length(object@hits) > 0){
                hits.vec <- annotationConvertor(
                  geneList = hits.vec,
                  species = species,
                  initialIDs = initialIDs,
                  finalIDs = "ENTREZID",
                  keepMultipleMappings = keepMultipleMappings,
                  verbose = verbose
                )}
            }

            ## genes after annotation conversion
            object@summary$gl[, "converted to entrez"] <-
              length(genelist)

            ## update genelist and hits, and return objects================================
            if (verbose)
              cat("--Ordering Gene List decreasingly ...\n")
            if (!orderAbsValue)
              genelist <- genelist[order(genelist, decreasing = TRUE)]
            else
              genelist <- abs(genelist)[order(abs(genelist), decreasing = TRUE)]
            object@geneList <- genelist

            if (length(object@hits) > 0) {
              hits <- names(hits.vec)
              object@summary$hits[, "preprocessed"] <- length(hits)
              object@hits <- hits
            }

            object@preprocessed <- TRUE

            cat("-Preprocessing complete!\n\n")
            object
          })


#' Remove duplicates in a named vector of phenotypes.
#'
#' This function gets rid of the duplicates in a vector of phenotypes
#' with gene identifiers as names. It is used to prepare the named vector
#' of phenotypes for the over-representation and gene set enrichment analysis.
#'
#' @param geneList A single named numeric or integer vector with gene
#' identifiers as names.
#' @param method A single character value specifying the method to remove
#' the duplicates (should the minimum, maximum or average observation for
#' a same construct be kept). The current version provides "min" (minimum),
#' "max" (maximum), "average" and "fc.avg" (fold change average).
#' The minimum and maximum should be understood in terms of absolute values
#' (i.e. min/max effect, no matter the sign). The fold change average method
#' converts the fold changes to ratios, averages them and converts the average
#' back to a fold change.
#'
#' @return A named vector of phenotypes with duplicates removed.
#'
#' @seealso \code{\link[HTSanalyzeR2]{preprocess}}
#'
#' @examples
#' x <- c(5,1,3,-2,6)
#' names(x) <- c("gene1", "gene3", "gene7", "gene3", "gene4")
#' xprocessed <- duplicateRemover(geneList=x, method="max")
#' @export
duplicateRemover <- function(geneList, method = "max") {
  paraCheck("GSCAClass", "genelist", geneList)
  paraCheck("PreProcess", "duplicateRemoverMethod", method)

  ## Get the unique names and create a vector that will store the
  ## processed values corresponding to those names
  geneList.names <- names(geneList)
  datanames <- unique(geneList.names)

  ## If the absolute value of the min is bigger than the absolute
  ## value of the max, then it is the min that is kept
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
#' @usage annotationConvertor(geneList, species="Hs", initialIDs="SYMBOL",
#' finalIDs="ENTREZID", keepMultipleMappings=TRUE, verbose=TRUE)
#' @param geneList A named integer or numeric vector, or a matrix with
#' rows named by gene identifiers.
#' @param species A single character value specifying the species of the input.
#' It supports all the species of OrgDb objects in AnnotationDbi.
#' The format should be an abbreviation of the organism as setted by AnnotationDbi.
#' For example, the commonly used ones are "Dm" ("Drosophila_melanogaster"),
#' "Hs" ("Homo_sapiens"), "Rn" ("Rattus_norvegicus"), "Mm" ("Mus_musculus"),
#' "Ce" ("Caenorhabditis_elegans"), and etc.
#'
#' @param initialIDs A single character value specifying the type of
#' initial identifiers for input geneList. The valid terms need match with
#' the keytypes of species db such as keytypes(org.Hs.eg.db).
#' @param finalIDs A single character value specifying the type of
#' final identifiers for input geneList.
#' @param keepMultipleMappings A single logical value. If TRUE, the
#' function keeps the entries with multiple mappings (first mapping
#' is kept). If FALSE, the entries with multiple mappings will be discarded.
#' @param verbose A single logical value specifying whether to display detailed
#' messages (when verbose=TRUE) or not (when verbose=FALSE).
#'
#' @return The same data vector/matrix but with names/row names converted.
#'
#' @examples
#' library(org.Dm.eg.db)
#' ## Example1: convert a named vector
#' x <- runif(10)
#' names(x) <- names(as.list(org.Dm.egSYMBOL2EG))[1:10]
#' xEntrez <- annotationConvertor(geneList=x, species="Dm", initialIDs="SYMBOL",
#'                                finalIDs="ENTREZID")
#'
#' ## Example2: convert a data matrix with row names as gene ids
#' x <- cbind(runif(10),runif(10))
#' rownames(x) <- names(as.list(org.Dm.egSYMBOL2EG))[1:10]
#' xEntrez <- annotationConvertor(geneList=x, species="Dm", initialIDs="SYMBOL",
#'                                finalIDs="ENTREZID")
#' @export
#' @importFrom AnnotationDbi mapIds columns
#' @importFrom utils installed.packages
#' @importFrom utils getFromNamespace
annotationConvertor <- function(geneList,
                                species = "Hs",
                                initialIDs = "SYMBOL",
                                finalIDs = "ENTREZID",
                                keepMultipleMappings = TRUE,
                                verbose = TRUE) {

  paraCheck("General", "species", species)
  paraCheck("Annotataion", "geneList", geneList)
  paraCheck("Annotataion", "initialIDs", initialIDs)
  paraCheck("Annotataion", "finalIDs", finalIDs)
  paraCheck("Annotataion", "keepMultipleMappings", keepMultipleMappings)
  paraCheck("General", "verbose", verbose)

  annopc <- paste("org", species, "eg", "db", sep = ".")
  if (!(annopc %in% rownames(installed.packages()))) {
    stop(paste(
      'Please install library ',
      annopc,
      ' before running this function!',
      sep = ""
    ))
  }

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
        "(see keytypes(",
        annopc ,
        "))\n",
        sep = ""
      )
    )
  }

  dealMulti <- ifelse(keepMultipleMappings, "first", "filter")
  geneListEntrez <- geneList

  ##if a named vector
  if (!is.matrix(geneList)) {
    namesMapping <-
      AnnotationDbi::mapIds(
        annodb,
        keys = names(geneList),
        keytype = initialIDs,
        column = finalIDs,
        multiVals = dealMulti
      )
    names(geneListEntrez) <- namesMapping[names(geneList)]
    geneListEntrez <- geneListEntrez[!is.na(names(geneListEntrez))]
    if (verbose) {
      cat(
        "--",
        paste((length(geneList) - length(geneListEntrez)),
              " genes (out of ",
              length(geneList),
              ") could not be mapped to any identifier, ",
              "and were removed from the data. \n", sep = "")
      )
    }
  } else {
    namesMapping <-
      AnnotationDbi::mapIds(
        annodb,
        keys = rownames(geneList),
        keytype = initialIDs,
        column = finalIDs,
        multiVals = dealMulti
      )
    rownames(geneListEntrez) <- namesMapping[rownames(geneList)]
    geneListEntrez <- geneListEntrez[!is.na(rownames(geneListEntrez)), ]
    if (verbose) {
      cat(
        "--",
        paste((nrow(geneList) - nrow(geneListEntrez)),
              " genes (out of ",
              nrow(geneList),
              ") could not be mapped to any identifier, ",
              "and were removed from the data. \n", sep = "")
      )
    }
  }

  geneListEntrez
}
