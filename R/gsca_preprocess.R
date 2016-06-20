## summarize & default show
if (!isGeneric("preprocess")) {
  setGeneric("preprocess", function(object, ...)
    standardGeneric("preprocess"), package = "HTSanalyzeR2")
}

#' @export
#' @include gsca_class.R
setMethod("preprocess", signature = "GSCA",
          function(object,
                   species = "Dm",
                   initialIDs = "FlybaseCG",
                   keepMultipleMappings = TRUE,
                   duplicateRemoverMethod = "max",
                   orderAbsValue = FALSE,
                   verbose = TRUE) {
            #######################
            ##check input arguments
            #######################
            paraCheck(name = "species", para = species)
            paraCheck(name = "initialIDs", para = initialIDs)
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

            hits <-
              object@hits[object@hits != "" & !is.na(object@hits)]
            if (length(hits) == 0)
              stop("Input 'hits' contains no useful data!\n")
            hits <- unique(hits)
            match.ind <- match(hits, names(genelist))

            hits.vec <- genelist[match.ind[!is.na(match.ind)]]
            if (length(hits.vec) == 0)
              stop("Hits and geneList have no overlaps!\n")

            ##annotation convertor
            if (initialIDs != "Entrez.gene") {
              if (verbose)
                cat("--Converting annotations ...\n")
              genelist <- annotationConvertor(
                geneList = genelist,
                species = species,
                initialIDs = initialIDs,
                finalIDs = "Entrez.gene",
                keepMultipleMappings = keepMultipleMappings,
                verbose = verbose
              )
              hits.vec <- annotationConvertor(
                geneList = hits.vec,
                species = species,
                initialIDs = initialIDs,
                finalIDs = "Entrez.gene",
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
              genelist <-
              genelist[order(genelist, decreasing = TRUE)]
            else
              genelist <-
              abs(genelist)[order(abs(genelist), decreasing = TRUE)]
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


##This function gets rid of the duplicates in a gene list.
duplicateRemover <- function(geneList, method = "max") {
  ##check arguments
  paraCheck("genelist", geneList)
  paraCheck("duplicateRemoverMethod", method)
  ##Get the unique names and create a vector that will store the
  ##processed values corresponding to those names
  geneList.names <- names(geneList)
  datanames <- unique(geneList.names)
  data.processed <- rep(0, length(datanames))
  names(data.processed) <- datanames
  data.processed2 <- data.processed
  l.data.processed <- length(data.processed)
  ##If the absolute value of the min is bigger than the absolute
  ##value of the max, then it is the min that is kept
  if (method == "max") {
    data.processed <- sapply(datanames,
                             function(i) {
                               this.range <- range(geneList[which(geneList.names == i)])
                               this.range[which.max(abs(this.range))]
                             })
  } else if (method == "min") {
    data.processed <- sapply(datanames,
                             function(i) {
                               this.range <- range(geneList[which(geneList.names == i)])
                               this.range[which.min(abs(this.range))]
                             })
  } else if (method == "average") {
    data.processed <- sapply(datanames,
                             function(i) {
                               mean(geneList[which(geneList.names == i)])
                             })
  } else if (method == "fc.avg") {
    neg.fcs <- which(geneList < 1)
    geneListRatios <- geneList
    #convert from fold change to ratio
    geneListRatios[neg.fcs] <- abs(1 / geneList[neg.fcs])
    #average the values across replicates
    data.processed <- sapply(datanames, function(i) {
      mean(geneListRatios[which(geneList.names == i)])
    })
    #convert back to fold change
    neg.fcs <- which(data.processed < 1)
    data.processed[neg.fcs] <- (-1 / data.processed[neg.fcs])
  }
  data.processed
}






annotationConvertor <- function(geneList,
                                species = "Dm",
                                initialIDs = "Entrez.gene",
                                finalIDs = "Entrez.gene",
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
  if (species == "Dm") {
    paraCheck("dro.initialIDs", initialIDs)
    paraCheck("dro.finalIDs", finalIDs)
    if (!("package:org.Dm.eg.db" %in% search()))
      library(org.Dm.eg.db)
    geneListEntrez <- drosoAnnotationConvertor(
      geneList = geneList,
      initialIDs = initialIDs,
      finalIDs = finalIDs,
      verbose = verbose
    )
  } else if (species == "Hs") {
    paraCheck("mam.initialIDs", initialIDs)
    paraCheck("mam.finalIDs", finalIDs)
    if (!("package:org.Hs.eg.db" %in% search()))
      library(org.Hs.eg.db)
    geneListEntrez <- mammalAnnotationConvertor(
      geneList = geneList,
      initialIDs = initialIDs,
      finalIDs = finalIDs,
      species = species,
      verbose = verbose
    )
  } else if (species == "Rn") {
    paraCheck("mam.initialIDs", initialIDs)
    paraCheck("mam.finalIDs", finalIDs)
    if (!("package:org.Rn.eg.db" %in% search()))
      library(org.Rn.eg.db)
    geneListEntrez <- mammalAnnotationConvertor(
      geneList = geneList,
      initialIDs = initialIDs,
      finalIDs = finalIDs,
      species = species,
      verbose = verbose
    )
  } else if (species == "Mm") {
    paraCheck("mam.initialIDs", initialIDs)
    paraCheck("mam.finalIDs", finalIDs)
    if (!("package:org.Mm.eg.db" %in% search()))
      library(org.Mm.eg.db)
    geneListEntrez <- mammalAnnotationConvertor(
      geneList = geneList,
      initialIDs = initialIDs,
      finalIDs = finalIDs,
      species = species,
      verbose = verbose
    )
  } else if (species == "Ce") {
    paraCheck("cel.initialIDs", initialIDs)
    paraCheck("cel.finalIDs", finalIDs)
    if (!("package:org.Ce.eg.db" %in% search()))
      library(org.Ce.eg.db)
    geneListEntrez <- celAnnotationConvertor(
      geneList = geneList,
      initialIDs = initialIDs ,
      finalIDs = finalIDs,
      verbose = verbose
    )
  }
  ##	}
  ##	else {
  ##		geneListEntrez<-geneList
  ##		names(geneListEntrez)<-names(geneList)
  ##	}
  return(geneListEntrez)
}



##This function converts an initial named data vector to the same vector
##but with a different identifier category for species Drosophila
##Melanogaster. This function can also take a matrix, with rows=gene id's.
##This function removes the genes for which no mapping were found.

drosoAnnotationConvertor <-
  function(geneList,
           initialIDs = "Entrez.gene",
           finalIDs = "Entrez.gene",
           keepMultipleMappings = TRUE,
           verbose = TRUE) {
    ##check arguments
    paraCheck("genelist.general", geneList)
    paraCheck("dro.initialIDs", initialIDs)
    paraCheck("dro.finalIDs", finalIDs)
    paraCheck("keepMultipleMappings", keepMultipleMappings)
    paraCheck("verbose", verbose)
    ##Determine the environment to be used for the mapping
    ##If the type of initial identifiers is not "Entrez.gene", then the mapping will
    ##automatically be from one of the following to Entrez Gene identifiers
    #	if(initialIDs == "Ensembl.transcript")
    #		fromto<-org.Dm.egENSEMBLTRANS2EG
    #	else if(initialIDs == "Ensembl.prot")
    #		fromto<-org.Dm.egENSEMBLPROT2EG
    #	else if(initialIDs == "Ensembl.gene")
    #		fromto<-org.Dm.egENSEMBL2EG
    #	else if(initialIDs == "RefSeq")
    #		fromto<-org.Dm.egREFSEQ2EG
    #	else if(initialIDs == "Symbol")
    #		fromto<-org.Dm.egSYMBOL2EG
    #	else if(initialIDs == "GenBank")
    #		fromto<-org.Dm.egACCNUM2EG
    #	else if(initialIDs == "Flybase")
    #		fromto<-org.Dm.egFLYBASE2EG
    #	else if(initialIDs == "FlybaseCG")
    #		fromto<-org.Dm.egFLYBASECG2EG
    #	else if(initialIDs == "FlybaseProt")
    #		fromto<-org.Dm.egFLYBASEPROT2EG

    if (initialIDs == "Ensembl.transcript")
      fromto <-
      tryCatch(
        get("org.Dm.egENSEMBLTRANS2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "Ensembl.prot")
      fromto <-
      tryCatch(
        get("org.Dm.egENSEMBLPROT2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "Ensembl.gene")
      fromto <-
      tryCatch(
        get("org.Dm.egENSEMBL2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "RefSeq")
      fromto <-
      tryCatch(
        get("org.Dm.egREFSEQ2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "Symbol")
      fromto <-
      tryCatch(
        get("org.Dm.egSYMBOL2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "GenBank")
      fromto <-
      tryCatch(
        get("org.Dm.egACCNUM2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "Flybase")
      fromto <-
      tryCatch(
        get("org.Dm.egFLYBASE2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "FlybaseCG")
      fromto <-
      tryCatch(
        get("org.Dm.egFLYBASECG2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "FlybaseProt")
      fromto <-
      tryCatch(
        get("org.Dm.egFLYBASEPROT2EG"),
        error = function(e)
          NULL
      )
    ##If the initial identifiers is 	"Entrez.gene", then the mapping will
    ##automatically be from Entrez Gene identifiers	to one of the following
    if (initialIDs == "Entrez.gene") {
      if (finalIDs == "Ensembl.gene")
        fromto <-
          tryCatch(
            get("org.Dm.egENSEMBL"),
            error = function(e)
              NULL
          )
      else if (finalIDs == "Ensembl.transcript")
        fromto <-
          tryCatch(
            get("org.Dm.egENSEMBLTRANS"),
            error = function(e)
              NULL
          )
      else if (finalIDs == "Ensembl.prot")
        fromto <-
          tryCatch(
            get("org.Dm.egENSEMBLPROT"),
            error = function(e)
              NULL
          )
      else if (finalIDs == "RefSeq")
        fromto <-
          tryCatch(
            get("org.Dm.egREFSEQ"),
            error = function(e)
              NULL
          )
      else if (finalIDs == "Symbol")
        fromto <-
          tryCatch(
            get("org.Dm.egSYMBOL"),
            error = function(e)
              NULL
          )
      else if (finalIDs == "GenBank")
        fromto <-
          tryCatch(
            get("org.Dm.egACCNUM"),
            error = function(e)
              NULL
          )
      else if (finalIDs == "Flybase")
        fromto <-
          tryCatch(
            get("org.Dm.egFLYBASE"),
            error = function(e)
              NULL
          )
      else if (finalIDs == "FlybaseCG")
        fromto <-
          tryCatch(
            get("org.Dm.egFLYBASECG"),
            error = function(e)
              NULL
          )
      else if (finalIDs == "FlybaseProt")
        fromto <-
          tryCatch(
            get("org.Dm.egFLYBASEPROT"),
            error = function(e)
              NULL
          )
    }
    #	if(initialIDs== "Entrez.gene") {
    #		if(finalIDs == "Ensembl.gene")
    #			fromto<-org.Dm.egENSEMBL
    #		else if(finalIDs == "Ensembl.transcript")
    #			fromto<-org.Dm.egENSEMBLTRANS
    #		else if(finalIDs == "Ensembl.prot")
    #			fromto<-org.Dm.egENSEMBLPROT
    #		else if(finalIDs == "RefSeq")
    #			fromto<-org.Dm.egREFSEQ
    #		else if(finalIDs == "Symbol")
    #			fromto<-org.Dm.egSYMBOL
    #		else if(finalIDs == "GenBank")
    #			fromto<-org.Dm.egACCNUM
    #		else if(finalIDs == "Flybase")
    #			fromto<-org.Dm.egFLYBASE
    #		else if(finalIDs == "FlybaseCG")
    #			fromto<-org.Dm.egFLYBASECG
    #		else if(finalIDs == "FlybaseProt")
    #			fromto<-org.Dm.egFLYBASEPROT
    #	}
    ##Check that the environment has been correctly determined
    annopc <- paste("org", "Dm", "eg", "db", sep = ".")
    if (is.null(fromto))
      stop(paste(
        'Please load library ',
        annopc,
        ' before running this function!',
        sep = ""
      ))
    if (!is(fromto, "AnnDbBimap"))
      stop(
        paste(
          "Please provide a valid type of identifiers for the ",
          "'initialIDs' and 'finalIDs' parameters ",
          "(see help(celAnnotationConvertor))",
          sep = ""
        )
      )
    ##if a named vector
    if (!is.matrix(geneList)) {
      ##Create a list with an element for each name in the geneList,
      ##containing a vector of identifiers of the type finalIDs mapped
      ##to that name in the geneList
      list.new.names <-
        AnnotationDbi::mget(names(geneList), fromto, ifnotfound = NA)
      ##Create a vector that will hold the new names, and a vector
      ##that will tag the names that were mapped to multiple identifiers
      n.new.names <- length(list.new.names)
      new.names <- rep(0, n.new.names)
      tag.multiples <- rep(FALSE, n.new.names)
      #Go through the list of names and:
      #1. assign the first result in each element to the corresponding
      ##position in the new names vector
      #2. check if the element of the list contained more than one result
      #3. if the user asked to keep multiple mappings, just inform the
      ##user that this entry was mapped multiple times
      #4. if the user asked to discard multiple mappings, tag this entry
      ##and inform the user that this entry was mapped multiple times
      sapply(1:n.new.names, function(i) {
        new.names[i] <<- list.new.names[[i]][1]
        if (length(list.new.names[[i]]) > 1) {
          if (keepMultipleMappings) {
            if (verbose) {
              cat(
                "--The following identifier was mapped to ",
                "more than one value (only the first value ",
                "is kept): \n"
              )
              print(list.new.names[i])
            }
          } else {
            if (verbose) {
              cat(
                "--The following identifier was mapped to ",
                "more than one value (this entry will be ",
                "discarded): \n"
              )
              print(list.new.names[i])
            }
            tag.multiples[i] <<- TRUE
          }
        }
      })
      ##If the user asked to keep multiple mappings, the vector of new
      ##names is used to set the names of the vector as it is and the
      ##entries that could not be mapped to any new names are removed
      ##from the data the user is informed of how many entries were
      ##removed because they were not mapped
      if (keepMultipleMappings) {
        newdata <- geneList
        names(newdata) <- new.names
        newdata <- newdata[!is.na(names(newdata))]
        if (verbose)
          cat(
            "--",
            paste((length(geneList) - length(newdata)),
                  " genes (out of ",
                  length(geneList) ,
                  ") could not be mapped to any identifier, ",
                  "and were removed from the data. \n"
            )
          )
      }
      ##If the user asked to discard multiple mappings, the data is
      ##trimmed of all entries that were mapped multiple times
      ##and so is the vector of new names.  This is done using the
      ##information in the tag.multiples vector
      else {
        newdata <- geneList[which(!tag.multiples)]
        names(newdata) <- new.names[which(!tag.multiples)]
        newdata <- newdata[!is.na(names(newdata))]
        if (verbose)
          cat(
            "--",
            paste((length(geneList) - length(newdata)),
                  " genes (out of ",
                  length(geneList) ,
                  ") could not be mapped to any identifier ",
                  "(or were mapped to multiple identifiers),
                  and were removed from the data. \n"
            )
            )
      }
      } else {
        ##This is identical to what is done for vectors, except that we
        ##work on row names
        list.new.names <-
          mget(rownames(geneList), fromto, ifnotfound = NA)
        n.new.names <- length(list.new.names)
        new.names <- rep(0, n.new.names)
        tag.multiples <- rep(0, n.new.names)
        sapply(1:n.new.names, function(i) {
          new.names[i] <<- list.new.names[[i]][1]
          if (length(list.new.names[[i]]) > 1) {
            if (keepMultipleMappings) {
              if (verbose) {
                cat(
                  "--The following identifier was mapped to ",
                  "more than one value (only the first value is ",
                  "kept): \n"
                )
                print(list.new.names[i])
              }
            } else {
              if (verbose) {
                cat(
                  "--The following identifier was mapped to ",
                  "more than one value (this entry will be ",
                  "discarded): \n"
                )
                print(list.new.names[i])
              }
              tag.multiples[i] <<- TRUE
            }
          }
        })
        ##This is identical to what is done for vectors, except that we
        ##work on row names and that we discard the whole row of data when
        ##there is no mapping
        if (keepMultipleMappings) {
          newdata <- geneList
          rownames(newdata) <- new.names
          newdata <- newdata[!is.na(rownames(newdata)), ]
          if (verbose) {
            cat(
              "--",
              paste(((dim(geneList)[1]) - (dim(newdata)[1])),
                    " genes (out of ",
                    dim(geneList)[1] ,
                    ") could not be mapped to any identifier, ",
                    "and were removed from the data. \n"
              )
            )
          }
        } else {
          newdata <- geneList[which(!tag.multiples)]
          rownames(newdata) <- new.names[which(!tag.multiples)]
          newdata <- newdata[!is.na(rownames(newdata)), ]
          if (verbose)
            cat(
              "--",
              paste(((dim(geneList)[1]) - (dim(newdata)[1])),
                    " genes (out of ",
                    dim(geneList)[1] ,
                    ") could not be mapped to any identifier ",
                    "(or were mapped to multiple identifiers),
                    and were removed from the data. \n"
              )
              )
        }
        }
    return(newdata)
    }


##This function converts an initial named data vector to the same vector
##but with a different identifier category for Mammalian species (this
##function also works on matrices with rows=genes (named)).

mammalAnnotationConvertor <-
  function(geneList,
           initialIDs = "Entrez.gene",
           finalIDs = "Entrez.gene",
           species = "Hs",
           keepMultipleMappings = TRUE,
           verbose = TRUE) {
    ##check arguments
    paraCheck("genelist.general", geneList)
    paraCheck("mam.initialIDs", initialIDs)
    paraCheck("mam.finalIDs", finalIDs)
    paraCheck("mam.species", species)
    paraCheck("keepMultipleMappings", keepMultipleMappings)
    paraCheck("verbose", verbose)
    if (species == "Hs") {
      ##Determine the environment to be used for the mapping
      ##If the type of initial identifiers is not "Entrez.gene", then
      ##the mapping will automatically be from one of the following to
      ##Entrez Gene identifiers
      if (initialIDs == "Ensembl.transcript")
        fromto <-
          tryCatch(
            get("org.Hs.egENSEMBLTRANS2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Ensembl.prot")
        fromto <-
          tryCatch(
            get("org.Hs.egENSEMBLPROT2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Ensembl.gene")
        fromto <-
          tryCatch(
            get("org.Hs.egENSEMBL2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "RefSeq")
        fromto <-
          tryCatch(
            get("org.Hs.egREFSEQ2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Symbol")
        fromto <-
          tryCatch(
            get("org.Hs.egSYMBOL2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "GenBank")
        fromto <-
          tryCatch(
            get("org.Hs.egACCNUM2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Entrez.gene") {
        ##If the initial identifiers is "Entrez.gene", then the
        ##mapping will automatically be from Entrez Gene identifiers
        ##to one of the following
        if (finalIDs == "Ensembl.gene")
          fromto <-
            tryCatch(
              get("org.Hs.egENSEMBL"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "Ensembl.transcript")
          fromto <-
            tryCatch(
              get("org.Hs.egENSEMBLTRANS"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "Ensembl.prot")
          fromto <-
            tryCatch(
              get("org.Hs.egENSEMBLPROT"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "RefSeq")
          fromto <-
            tryCatch(
              get("org.Hs.egREFSEQ"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "Symbol")
          fromto <-
            tryCatch(
              get("org.Hs.egSYMBOL"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "GenBank")
          fromto <-
            tryCatch(
              get("org.Hs.egACCNUM"),
              error = function(e)
                NULL
            )
      }
      ##Check that the environment has been correctly determined
      annopc <- paste("org", "Hs", "eg", "db", sep = ".")
      if (is.null(fromto))
        stop(paste(
          'Please load library ',
          annopc,
          ' before running this function!',
          sep = ""
        ))
      if (!is(fromto, "AnnDbBimap"))
        stop(
          "Please provide a valid type of identifiers for the ",
          "'initialIDs' and 'finalIDs' parameters ",
          "(see help(mammalAnnotationConvertor))"
        )
    } else if (species == "Mm") {
      ##Determine the environment to be used for the mapping
      ##If the type of initial identifiers is not "Entrez.gene", then
      ##the mapping will automatically be from one of the following
      ##to Entrez Gene identifiers
      if (initialIDs == "Ensembl.transcript")
        fromto <-
          tryCatch(
            get("org.Mm.egENSEMBLTRANS2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Ensembl.prot")
        fromto <-
          tryCatch(
            get("org.Mm.egENSEMBLPROT2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Ensembl.gene")
        fromto <-
          tryCatch(
            get("org.Mm.egENSEMBL2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "RefSeq")
        fromto <-
          tryCatch(
            get("org.Mm.egREFSEQ2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Symbol")
        fromto <-
          tryCatch(
            get("org.Mm.egSYMBOL2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "GenBank")
        fromto <-
          tryCatch(
            get("org.Mm.egACCNUM2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Entrez.gene") {
        ##If the initial identifiers is "Entrez.gene", then the
        ##mapping will automatically be from Entrez Gene identifiers
        ##to one of the following
        if (finalIDs == "Ensembl.gene")
          fromto <-
            tryCatch(
              get("org.Mm.egENSEMBL"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "Ensembl.transcript")
          fromto <-
            tryCatch(
              get("org.Mm.egENSEMBLTRANS"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "Ensembl.prot")
          fromto <-
            tryCatch(
              get("org.Mm.egENSEMBLPROT"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "RefSeq")
          fromto <-
            tryCatch(
              get("org.Mm.egREFSEQ"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "Symbol")
          fromto <-
            tryCatch(
              get("org.Mm.egSYMBOL"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "GenBank")
          fromto <-
            tryCatch(
              get("org.Mm.egACCNUM"),
              error = function(e)
                NULL
            )
      }
      ##Check that the environment has been correctly determined
      annopc <- paste("org", "Mm", "eg", "db", sep = ".")
      if (is.null(fromto))
        stop(paste(
          'Please load library ',
          annopc,
          ' before running this function!',
          sep = ""
        ))
      if (!is(fromto, "AnnDbBimap"))
        stop(
          "Please provide a valid type of identifiers for the ",
          "'initialIDs' and 'finalIDs' parameters ",
          "(see help(mammalAnnotationConvertor))"
        )
    } else if (species == "Rn") {
      ##Determine the environment to be used for the mapping
      ##If the type of initial identifiers is not "Entrez.gene", then
      ##the mapping will automatically be from one of the following to
      ##Entrez Gene identifiers
      if (initialIDs == "Ensembl.transcript")
        fromto <-
          tryCatch(
            get("org.Rn.egENSEMBLTRANS2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Ensembl.prot")
        fromto <-
          tryCatch(
            get("org.Rn.egENSEMBLPROT2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Ensembl.gene")
        fromto <-
          tryCatch(
            get("org.Rn.egENSEMBL2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "RefSeq")
        fromto <-
          tryCatch(
            get("org.Rn.egREFSEQ2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Symbol")
        fromto <-
          tryCatch(
            get("org.Rn.egSYMBOL2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "GenBank")
        fromto <-
          tryCatch(
            get("org.Rn.egACCNUM2EG"),
            error = function(e)
              NULL
          )
      else if (initialIDs == "Entrez.gene") {
        ##If the initial identifiers is "Entrez.gene", then the
        ##mapping will automatically be from Entrez Gene identifiers
        ##to one of the following
        if (finalIDs == "Ensembl.gene")
          fromto <-
            tryCatch(
              get("org.Rn.egENSEMBL"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "Ensembl.transcript")
          fromto <-
            tryCatch(
              get("org.Rn.egENSEMBLTRANS"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "Ensembl.prot")
          fromto <-
            tryCatch(
              get("org.Rn.egENSEMBLPROT"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "RefSeq")
          fromto <-
            tryCatch(
              get("org.Rn.egREFSEQ"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "Symbol")
          fromto <-
            tryCatch(
              get("org.Rn.egSYMBOL"),
              error = function(e)
                NULL
            )
        else if (finalIDs == "GenBank")
          fromto <-
            tryCatch(
              get("org.Rn.egACCNUM"),
              error = function(e)
                NULL
            )
      }
      ##Check that the environment has been correctly determined
      annopc <- paste("org", "Rn", "eg", "db", sep = ".")
      if (is.null(fromto))
        stop(paste(
          'Please load library ',
          annopc,
          ' before running this function!',
          sep = ""
        ))
      if (!is(fromto, "AnnDbBimap"))
        stop(
          "Please provide a valid type of identifiers for the ",
          "'initialIDs' and 'finalIDs' parameters ",
          "(see help(mammalAnnotationConvertor))"
        )
    }
    ##if a named vector
    if (!is.matrix(geneList)) {
      ##Create a list with an element for each name in the geneList,
      ##containing a vector of identifiers of the type finalIDs mapped
      ##to that name in the geneList
      list.new.names <-
        mget(names(geneList), fromto, ifnotfound = NA)
      ##Create a vector that will hold the new names, and a vector
      ##that will tag the names that were mapped to multiple identifiers
      l.new.names <- length(list.new.names)
      new.names <- rep(0, l.new.names)
      tag.multiples <- rep(FALSE, l.new.names)
      ##Go through the list of names and:
      ##1. assign the first result in each element to the corresponding
      ##position in the new names vector
      ##2. check if the element of the list contained more than one result
      ##3. if the user asked to keep multiple mappings, just inform the
      ##user that this entry was mapped multiple times
      ##4. if the user asked to discard multiple mappings, tag this
      ##entry and inform the user that this entry was mapped multiple times
      sapply(1:l.new.names, function(i) {
        new.names[i] <<- list.new.names[[i]][1]
        if (length(list.new.names[[i]]) > 1) {
          if (keepMultipleMappings) {
            if (verbose) {
              cat(
                "--The following identifier was mapped to ",
                "more than one value (only the first value ",
                "is kept): \n"
              )
              print(list.new.names[i])
            }
          } else {
            if (verbose) {
              cat(
                "--The following identifier was mapped to ",
                "more than one value (this entry will be ",
                "discarded): \n"
              )
              print(list.new.names[i])
            }
            tag.multiples[i] <<- TRUE
          }
        }
      })

      ##If the user asked to keep multiple mappings, the vector of new
      ##names is used to set the names of the vector as it is and the
      ##entries that could not be mapped to any new names are removed
      ##from the data the user is informed of how many entries were
      ##removed because they were not mapped
      if (keepMultipleMappings) {
        newdata <- geneList
        names(newdata) <- new.names
        newdata <- newdata[!is.na(names(newdata))]
        if (verbose)
          cat(
            "--",
            paste((length(geneList) - length(newdata)),
                  " genes (out of ",
                  length(geneList) ,
                  ") could not be mapped to any identifier, ",
                  "and were removed from the data. \n"
            )
          )
      }
      ##If the user asked to discard multiple mappings, the data is
      ##trimmed of all entries that were mapped multiple times
      ##and so is the vector of new names.  This is done using the
      ##information in the tag.multiples vector
      else {
        newdata <- geneList[which(!tag.multiples)]
        names(newdata) <- new.names[which(!tag.multiples)]
        newdata <- newdata[!is.na(names(newdata))]
        if (verbose)
          cat(
            "--",
            paste((length(geneList) - length(newdata)),
                  " genes (out of ",
                  length(geneList) ,
                  ") could not be mapped to any identifier ",
                  "(or were mapped to multiple identifiers), ",
                  "and were removed from the data. \n"
            )
          )
      }
    } else {
      list.new.names <- mget(rownames(geneList), fromto, ifnotfound = NA)
      ##This is identical to what is done for vectors, except that we
      ##work on row names
      l.new.names <- length(list.new.names)
      new.names <- rep(0, l.new.names)
      tag.multiples <- rep(FALSE, l.new.names)
      sapply(1:l.new.names, function(i) {
        new.names[i] <<- list.new.names[[i]][1]
        if (length(list.new.names[[i]]) > 1) {
          if (keepMultipleMappings) {
            if (verbose) {
              cat(
                "--The following identifier was mapped to ",
                "more than one value (only the first value ",
                "is kept): \n"
              )
              print(list.new.names[i])
            }
          } else {
            if (verbose) {
              cat(
                "--The following identifier was mapped to ",
                "more than one value (this entry will be ",
                "discarded): \n"
              )
              print(list.new.names[i])
            }
            tag.multiples[i] <<- TRUE
          }
        }
      })
      ##This is identical to what is done for vectors, except that we
      ##work on row names and that we discard the whole row of data when
      ##there is no mapping
      if (keepMultipleMappings) {
        newdata <- geneList
        rownames(newdata) <- new.names
        newdata <- newdata[!is.na(rownames(newdata)), ]
        if (verbose)
          cat(
            "--",
            paste(((dim(geneList)[1]) - (dim(newdata)[1])),
                  " genes (out of ",
                  dim(geneList)[1] ,
                  ") could not be mapped to any identifier, ",
                  "and were removed from the data. \n"
            )
          )
      } else {
        newdata <- geneList[which(!tag.multiples)]
        rownames(newdata) <- new.names[which(!tag.multiples)]
        newdata <- newdata[!is.na(rownames(newdata)), ]
        if (verbose)
          cat(
            "--",
            paste(((dim(geneList)[1]) - (dim(newdata)[1])),
                  " genes (out of ",
                  dim(geneList)[1] ,
                  ") could not be mapped to any identifier ",
                  "(or were mapped to multiple identifiers), ",
                  "and were removed from the data. \n"
            )
          )
      }
    }
    return(newdata)
  }


##This function converts an initial named data vector to the same
##vector but with a different identifier category. This function can
##also take a matrix, with rows=gene id's. This function removes the
##genes for which no mapping were found.

celAnnotationConvertor <-
  function(geneList,
           initialIDs = "Entrez.gene",
           finalIDs = "Entrez.gene",
           keepMultipleMappings = TRUE,
           verbose = TRUE) {
    ##check arguments
    paraCheck("genelist.general", geneList)
    paraCheck("cel.initialIDs", initialIDs)
    paraCheck("cel.finalIDs", finalIDs)
    paraCheck("keepMultipleMappings", keepMultipleMappings)
    paraCheck("verbose", verbose)
    fromto <- "dummystring"
    #check the environment to be used for the mapping
    #If the type of initial identifiers is not "Entrez.gene", then the
    #mapping will automatically be from one of the following to Entrez
    #Gene identifiers
    #	if(initialIDs == "Ensembl.transcript")
    #		fromto <- org.Ce.egENSEMBLTRANS2EG
    #	else if(initialIDs == "Ensembl.prot")
    #		fromto <- org.Ce.egENSEMBLPROT2EG
    #	else if(initialIDs == "Ensembl.gene")
    #		fromto <- org.Ce.egENSEMBL2EG
    #	else if(initialIDs == "RefSeq")
    #		fromto <- org.Ce.egREFSEQ2EG
    #	else if(initialIDs == "Symbol")
    #		fromto <- org.Ce.egSYMBOL2EG
    #	else if(initialIDs == "GenBank")
    #		fromto <- org.Ce.egACCNUM2EG

    if (initialIDs == "Ensembl.transcript")
      fromto <-
      tryCatch(
        get("org.Ce.egENSEMBLTRANS2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "Ensembl.prot")
      fromto <-
      tryCatch(
        get("org.Ce.egENSEMBLPROT2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "Ensembl.gene")
      fromto <-
      tryCatch(
        get("org.Ce.egENSEMBL2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "RefSeq")
      fromto <-
      tryCatch(
        get("org.Ce.egREFSEQ2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "Symbol")
      fromto <-
      tryCatch(
        get("org.Ce.egSYMBOL2EG"),
        error = function(e)
          NULL
      )
    else if (initialIDs == "GenBank")
      fromto <-
      tryCatch(
        get("org.Ce.egACCNUM2EG"),
        error = function(e)
          NULL
      )
    #If the initial identifiers is 	"Entrez.gene", then the mapping will
    #automatically be from Entrez Gene identifiers	to one of the following
    #	if(initialIDs == "Entrez.gene") {
    #		if(finalIDs == "Ensembl.gene")
    #			fromto <- org.Ce.egENSEMBL
    #		if(finalIDs == "Ensembl.transcript")
    #			fromto <- org.Ce.egENSEMBLTRANS
    #		if(finalIDs == "Ensembl.prot")
    #			fromto <- org.Ce.egENSEMBLPROT
    #		if(finalIDs == "RefSeq")
    #			fromto <- org.Ce.egREFSEQ
    #		if(finalIDs == "Symbol")
    #			fromto <- org.Ce.egSYMBOL
    #		if(finalIDs == "GenBank")
    #			fromto <- org.Ce.egACCNUM
    #		if(finalIDs == "wormbase")
    #			fromto <- org.Ce.egWORMBASE
    #	}
    if (initialIDs == "Entrez.gene") {
      if (finalIDs == "Ensembl.gene")
        fromto <-
          tryCatch(
            get("org.Ce.egENSEMBL"),
            error = function(e)
              NULL
          )
      if (finalIDs == "Ensembl.transcript")
        fromto <-
          tryCatch(
            get("org.Ce.egENSEMBLTRANS"),
            error = function(e)
              NULL
          )
      if (finalIDs == "Ensembl.prot")
        fromto <-
          tryCatch(
            get("org.Ce.egENSEMBLPROT"),
            error = function(e)
              NULL
          )
      if (finalIDs == "RefSeq")
        fromto <-
          tryCatch(
            get("org.Ce.egREFSEQ"),
            error = function(e)
              NULL
          )
      if (finalIDs == "Symbol")
        fromto <-
          tryCatch(
            get("org.Ce.egSYMBOL"),
            error = function(e)
              NULL
          )
      if (finalIDs == "GenBank")
        fromto <-
          tryCatch(
            get("org.Ce.egACCNUM"),
            error = function(e)
              NULL
          )
      if (finalIDs == "wormbase")
        fromto <-
          tryCatch(
            get("org.Ce.egWORMBASE"),
            error = function(e)
              NULL
          )
    }
    #Check that the environment has been correctly determined
    annopc <- paste("org", "Ce", "eg", "db", sep = ".")
    if (is.null(fromto))
      stop(paste(
        'Please load library ',
        annopc,
        ' before running this function!',
        sep = ""
      ))
    if (class(fromto) != "AnnDbBimap")
      stop(
        paste(
          "Please provide a valid type of identifiers for the",
          " 'initialIDs' and 'finalIDs' parameters ",
          "(see help(celAnnotationConvertor))",
          sep = ""
        )
      )
    #for a named vector
    if (!is.matrix(geneList)) {
      #Create a list with an element for each name in the geneList,
      #containing a vector of identifiers of the type finalIDs mapped
      #to that name in the geneList
      list.new.names <-
        mget(names(geneList), fromto, ifnotfound = NA)
      #Create a vector that will hold the new names, and a vector that
      #will tag the names that were mapped to multiple identifiers
      n.new.names <- length(list.new.names)
      new.names <- rep(0, n.new.names)
      tag.multiples <- rep(FALSE, n.new.names)

      sapply(1:n.new.names, function(i) {
        new.names[i] <<- list.new.names[[i]][1]
        if (length(list.new.names[[i]]) > 1) {
          if (keepMultipleMappings) {
            if (verbose) {
              cat(
                "--The following identifier was mapped to ",
                "more than one value (only the first value",
                " is kept): \n"
              )

              cat("--", list.new.names[i], "\n")
            }
          } else {
            if (verbose) {
              cat(
                "--The following identifier was mapped to ",
                "more than one value (this entry will be ",
                "discarded): \n"
              )

              cat("--", list.new.names[i], "\n")
            }
            tag.multiples[i] <<- TRUE
          }
        }
        NULL
      })
      #If multiple mappings should be kept
      if (keepMultipleMappings) {
        newdata <- geneList
        names(newdata) <- new.names
        newdata <- newdata[!is.na(names(newdata))]
        if (verbose)
          cat(
            "--",
            paste((length(geneList) - length(newdata)),
                  " genes (out of ",
                  length(geneList),
                  ") could not be mapped to any identifier, ",
                  "and were removed from the data. \n"
            )
          )
      }
      #If multiple mappings should be discarded
      else {
        newdata <- geneList[which(!tag.multiples)]
        names(newdata) <- new.names[which(!tag.multiples)]
        newdata <- newdata[!is.na(names(newdata))]
        if (verbose)
          cat(
            "--",
            paste((length(geneList) - length(newdata)),
                  " genes (out of ",
                  length(geneList),
                  ") could not be mapped to any identifier ",
                  "(or were mapped to multiple identifiers), ",
                  "and were removed from the data. \n"
            )
          )
      }
    }
    #if a matrix
    else {
      list.new.names <- mget(rownames(geneList), fromto, ifnotfound = NA)
      n.new.names <- length(list.new.names)
      new.names <- rep(0, n.new.names)
      tag.multiples <- rep(0, n.new.names)

      sapply(1:n.new.names, function(i) {
        new.names[i] <<- list.new.names[[i]][1]
        if (length(list.new.names[[i]]) > 1) {
          if (keepMultipleMappings) {
            if (verbose) {
              cat(
                "--The following identifier was mapped to ",
                "more than one value (only the first value ",
                "is kept): \n"
              )

              cat("--", list.new.names[i], "\n")
            }
          } else {
            if (verbose) {
              cat(
                "--The following identifier was mapped to ",
                "more than one value (this entry will be ",
                "discarded): \n"
              )
              cat("--", list.new.names[i], "\n")
            }
            tag.multiples[i] <<- TRUE
          }
        }
        NULL
      })
      if (keepMultipleMappings) {
        newdata <- geneList
        rownames(newdata) <- new.names
        newdata <- newdata[!is.na(rownames(newdata)), ]
        if (verbose)
          cat(
            "--",
            paste(((dim(geneList)[1]) - (dim(newdata)[1])),
                  " genes (out of ",
                  dim(geneList)[1] ,
                  ") could not be mapped to any identifier, and were ",
                  "removed from the data. \n"
            )
          )
      } else {
        newdata <- geneList[which(!tag.multiples)]
        rownames(newdata) <- new.names[which(!tag.multiples)]
        newdata <- newdata[!is.na(rownames(newdata)), ]
        if (verbose)
          cat(
            "--",
            paste(((dim(geneList)[1]) - (dim(newdata)[1])),
                  " genes (out of ",
                  dim(geneList)[1] ,
                  ") could not be mapped to any identifier ",
                  "(or were mapped to multiple identifiers),
                  and were removed from the data. \n"
            )
            )
      }
      }
    return(newdata)

    }
