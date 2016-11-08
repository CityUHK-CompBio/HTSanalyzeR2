geneMatrix <- function(rowNames, colNames) {
  matrix(
    NA,
    nrow =  length(rowNames),
    ncol = length(colNames),
    dimnames = list(rowNames, colNames)
  )
}


## This is the central function for argument checking
paraCheck <- function(group, paraName, para) {
  switch(group,
         LoadGeneSets = {
           if (paraName == "collection" &&
               !(para %in% c("h", "c1", "c2", "c3", "c4", "c5", "c6", "c7")))
             stop(
               paste(
                 "'collection' does not match any of the names recognized by this function,",
                 "please provide one of the following character strings:",
                 "'h', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6' or 'c7'. See ?MSigDBGeneSets for details",
                 sep = " "
               )
             )
           if (paraName == "species" &&
               (!is.character(para) || length(para) != 1))
             stop("'species' should be a character!\n")
           if (paraName == "ontologies" &&
               (!is.character(para) || length(para) == 0 || !all(para %in% c("BP", "MF", "CC"))))
             stop("'ontologies' should be a character vector containing any non redundant combination of 'BP','MF','CC'!\n")
         },

         GSCAClass = {
           if (paraName == "gscs") {
             if (!is.list(para))
               stop("'listOfGeneSetCollections' must be a list of gene set collections!\n")
             if (is.null(names(para)))
               stop("'listOfGeneSetCollections' must be a list of named gene set collections!\n")
             if (!all(unlist(lapply(para, is.list))))
               stop("Each gene set collection in 'listOfGeneSetCollections' must be a list of gene sets!\n")
             if (any(unlist(lapply(para, length)) == 0))
               stop("Empty gene set collection(s) in 'listOfGeneSetCollections'!\n")
           }
           if (paraName == "genelist" &&
               (!(is.numeric(para) || is.integer(para)) || length(para)==0 || is.null(names(para)))) {
             stop("'geneList' should be a named numeric or integer vector with length > 0!\n")
           }
           if (paraName == "hits" &&
               (!is.character(para) || length(para) == 0)) {
             stop("'hits' should be a character vector with length > 0!\n")
           }
         },

         PreProcess = {
           if (paraName == "duplicateRemoverMethod" &&
               (!is.character(para) || length(para) != 1 ||
                !(para %in% c("max","min","average","fold.change.average")))) {
             stop(paste("'duplicateRemoverMethod' should be only one of the following character strings:",
                        "'max', 'min', 'average', 'fc.avg(fold change average)'"))
           }
           if(paraName == "orderAbsValue" &&
              !is.logical(para) || length(para) != 1) {
             stop("'orderAbsValue' should be a logical value!\n")
           }


         },

         Annotation = {
           if (paraName == "geneList") {
             if(is.matrix(para)) {
               if(is.null(rownames(para)) || any(is.na(rownames(para))) || any(rownames(para)==""))
                 stop("geneList should be a matrix with rownames!\n")
             } else {
               if(!(is.numeric(para) || is.integer(para)) || length(para)==0)
                 stop("'geneList' should be a numeric or integer vector with length > 0!\n")
               if(is.null(names(para)) || any(is.na(names(para))) || any(names(para)==""))
                 stop("'geneList' should be a named numeric or integer vector!\n")
             }
           }
           if (paraName == "initialIDs" &&
               (!is.character(para) || length(para) != 1)) {
             stop("'initialIDs' should be a character!\n")
           }
           if (paraName == "finalIDs" &&
               (!is.character(para) || length(para) != 1)) {
             stop("'finalIDs' should be a character!\n")
           }
           if (paraName == "keepMultipleMappings" &&
               (!is.logical(para) || length(para) != 1)) {
             stop("keepMultipleMappings should be a logical value!\n")
           }
         },

         Analyze = {
           if(paraName == "doGSOA" || paraName == "doGSEA") {
             if(length(para) != 1 || !is.logical(para))
               stop("'doGSOA' and 'doGSEA' should be a single logical value!\n ")
           }
           if(paraName == "GSCAPara") {
             checkGSCAPara(para)
           }
           if(paraName =="hits" &&
             (!is.character(para) || length(para)==0)) {
               stop("'hits' should be a character vector with length > 0!\n")
           }

         },

         Summarize = {
           if(paraName == "NWAwhat") {
             if(!any(para %in% c("ALL", "Pval", "Phenotype", "Interactome",
                                 "Para", "Result")) || !is.character(para))
               stop("Wrong what input! Please input \"ALL\"(all summary information), \"Pval\"(p-values), \"Phenotype\", \"Interactome\", \"Para\"(parameters for analysis) and \"Result\"\n")
           }
           if(paraName == "GSCAwhat") {
             if(!any(para %in% c("ALL", "GSC", "GeneList", "Hits", "Para",
                                 "Result")) || !is.character(para))
               stop("Wrong what input! Please input \"ALL\"(all summary information), \"GSC\"(gene set collection), \"GeneList\", \"Hits\", \"Para\"(parameters for analysis) and \"Result\"\n")
           }

           if(paraName == "ntop") {
             if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<=0)
               stop("'ntop' should be a integer or numeric value >0 ! \n")
           }
           if(paraName == "allSig") {
             if(!is.logical(para) || length(para)!=1)
               stop("'allSig' should be a logical value!\n")
           }
           if(paraName == "gscsNames") {
             if(!is.character(para) || length(para)==0)
               stop("'gscs' should be a character! \n")
           }
           if(paraName == "resultName") {
             if(!is.character(para) || length(para)!=1)
               stop("'resultName' should be a character!\n")
           }
         },

         General = {
           if (paraName == "verbose" &&
               (!is.logical(para) || length(para) != 1))
               stop("'verbose' should be a logical value!\n")
           if (paraName == "species" &&
               (!is.character(para) || length(para) != 1))
             stop("'species' should be a character!\n")
         })
}


checkGSCAPara <- function(para) {
  if (missing(para))
    stop("'para' should be provided as a list!\n")

  ##check data type in para
  if (!(is.integer(para$pValueCutoff) ||
        is.numeric(para$pValueCutoff)) ||
      length(para$pValueCutoff) != 1 || para$pValueCutoff > 1)
    stop("'pValueCutoff' should be an integer or numeric value <=1!\n")
  if (!is.character(para$pAdjustMethod) ||
      length(para$pAdjustMethod) != 1 ||
      !(para$pAdjustMethod %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")))
    stop("'pAdjustMethod' should be any one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr' and 'none'!\n")
  if (!(is.integer(para$nPermutations) ||
        is.numeric(para$nPermutations)) ||
      length(para$nPermutations) != 1 || para$nPermutations < 1)
    stop("'nPermutations' should be an integer >=1 !\n'")
  if (!(is.integer(para$minGeneSetSize) ||
        is.numeric(para$minGeneSetSize)) ||
      length(para$minGeneSetSize) != 1 || para$minGeneSetSize < 1)
    stop("'minGeneSetSize' should be an integer >=1 !\n'")
  if (!(is.integer(para$exponent) ||
        is.numeric(para$exponent)) ||
      length(para$pValueCutoff) != 1 || para$exponent < 1)
    stop("'exponent' should be an integer or numeric value >=1 !\n")
}

