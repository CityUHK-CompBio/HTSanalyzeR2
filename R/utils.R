geneMatrix <- function(rowNames, colNames) {
  matrix(
    NA,
    nrow =  length(rowNames),
    ncol = length(colNames),
    dimnames = list(rowNames, colNames)
  )
}

namesToList <- function(x) {
  res <- as.list(names(x))
  names(res) <- names(x)
  res
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

         NWAClass = {
           if(paraName == "pvalues") {
               if(!(is.numeric(para) || is.integer(para)) || length(para) == 0 || is.null(names(para)))
                 stop("'pvalues' should be a named numeric vector with length > 0!\n")
             }
           if(paraName == "phenotypes") {
               if(!(is.numeric(para) || is.integer(para)) || length(para) == 0 || is.null(names(para)))
                 stop("'phenotypes' should be a named numeric vector with length > 0!\n")
             }
           if(paraName == "interactome") {
             if(!is.na(para) && (!is(para,"igraph") || igraph::vcount(para) == 0 || igraph::ecount(para) == 0))
               stop("Input 'interactome/graph' should be a igraph object with node and edge No > 0!\n")
           }
         },

         PreProcess = {
           if (paraName == "duplicateRemoverMethod" &&
               (!is.character(para) || length(para) != 1 ||
                !(para %in% c("max","min","average","fold.change.average")))) {
             stop(paste("'duplicateRemoverMethod' should be only one of the following character strings:",
                        "'max', 'min', 'average', 'fc.avg(fold change average)'"))
           }
           if (paraName == "orderAbsValue" &&
              !is.logical(para) || length(para) != 1) {
             stop("'orderAbsValue' should be a logical value!\n")
           }
           if (paraName == "genetic") {
             if(!is.logical(para) || length(para)!=1)
               stop("'genetic' should be a logical value!\n")
           }
           if (paraName == "link") {
             if(!is.character(para) || length(para)!=1)
               stop("'link' should be a character!\n")
           }
           if (paraName == "interactionMatrix") {
             #If a data matrix is specified, check that it contains the right columns
             if(!is.matrix(para))
               stop("'interactionMatrix' should be a matrix")
             if(!all(c("InteractionType","InteractorA","InteractorB") %in% colnames(para)))
               stop("'interactionMatrix' should contain the following named columns: 'InteractionType','InteractorA','InteractorB'")
           }
           if (paraName == "dataDirectory") {
             if(!is.character(para) || length(para)!=1)
               stop("'dataDirectory' should be a character!\n")
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
           if (paraName == "doGSOA" || paraName == "doGSEA") {
             if(length(para) != 1 || !is.logical(para))
               stop("'doGSOA' and 'doGSEA' should be a single logical value!\n ")
           }
           if (paraName == "GSCAPara") {
             checkGSCAPara(para)
           }
           if (paraName =="hits" &&
             (!is.character(para) || length(para)==0)) {
               stop("GSOA should have 'hits' and hits' should be a character vector with length > 0!\n")
           }
           if (paraName == "fdr") {
             if(!is.numeric(para) || para>1)
               stop("'fdr' should be <=1 ! \n")
           }
           if (paraName == "exponent") {
             if(!(is.integer(para) || is.numeric(para)) ||
                length(para) != 1 || para < 1)
               stop("'exponent' should be an integer or numeric value >= 1!\n")
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
         Report = {
           if (paraName == "filepath" &&
               (!is.character(para) || length(para) != 1)) {
             stop("'filepath' should be a character!\n")
           }
           if (paraName == "filename" &&
               (!is.character(para) || length(para) != 1)) {
             stop("'filename' should be a character!\n")
           }
           if (paraName == "output" &&
               ((!is.character(para) || length(para) != 1) ||
               !all(para %in% c("png","pdf")))) {
             stop("'output' should be 'png' or 'pdf'!\n")
           }
           if (paraName == "resultName") {
             if(!is.character(para) || length(para)!=1)
               stop("'resultName' should be a character!\n")
           }
           if (paraName =="gsNameType") {
             if(!is.character(para) || length(para)!=1 ||
                !(para%in% c("id","term","none")))
               stop("'gsNameType' should be a single character value: 'id', 'term' or 'none'!\n")
           }
           if (paraName =="displayEdgeLabel") {
             if(!is.logical(para) || length(para)!=1)
               stop("'displayEdgeLabel' should be a logical value!\n")
           }
           if (paraName =="layout") {
             if(!is.character(para) || length(para)!=1 ||
                !(para %in% c("layout.fruchterman.reingold", "layout.spring",
                              "layout.circle", "layout.kamada.kawai")))
               stop("'layout' must be one of 'layout.fruchterman.reingold', 'layout.spring', 'layout.circle' and 'layout.kamada.kawai'!\n")
           }
           if (paraName == "plot" &&
               (!is.logical(para) || length(para) != 1)) {
                 stop("'plot' should be a logical value!\n")
           }
           if (paraName == "keggGSCs") {
             if(!is.character(para) || length(para) == 0)
               stop("'keggGSCs' should be a character!\n")
           }
           if (paraName == "goGSCs") {
             if(!is.character(para) || length(para) == 0)
               stop("'goGSCs' should be a character!\n")
           }
           if (paraName == "msigdbGSCs") {
             if(!is.character(para) || length(para) == 0)
               stop("'msigdbGSCs' should be a character!\n")
           }
           if (paraName == "msigdbGSCs") {
             if(!is.character(para) || length(para) == 0)
               stop("'msigdbGSCs' should be a character!\n")
           }
           if (paraName == "reportDir") {
             if(!is.character(para) || length(para) != 1)
               stop("'reportDir' should be a character!\n")
           }
           if (paraName == "gs.single") {
             if (!is.character(para) || length(para) !=1 ||
                 is.na(para) || para == "")
               stop("'geneSet/GeneSet' should be single character!\n")
           }
           if (paraName == "gsc.name") {
             if (!is.character(para) || length(para)!=1)
               stop("'gsc' should be a single character! \n")
           }
           if (paraName == "gs") {
             if (!is.character(para) || length(para)==0 ||
                 any(is.na(para)) || any(para == ""))
               stop("'geneSet/GeneSet' should be a character vector with length > 0, without NA or empty names!\n")
           }
           if (paraName == "gs.single") {
             if (!is.character(para) || length(para) != 1 ||
                 is.na(para) || para == "")
               stop("'geneSet/GeneSet' should be single character!\n")
           }
           if (paraName == "gscs.names") {
             if (!is.character(para) || length(para)==0)
               stop("'gscs' should be a character! \n")
           }
           if (paraName == "gseaScore.mode") {
             if (!is.character(para) || length(para)!=1 || !(para %in% c("graph", "score"))) {
               stop("'mode' should be 'graph' or 'score'!\n")
             }
           }
         },
         StatTest = {
           if (paraName == "normCellHTSobject") {
             if (!is(para,"cellHTS"))
               stop("The argument 'cellHTSobject/normCellHTSobject' should be a cellHTS object")
             if (!state(para)["configured"])
               stop("The cellHTS object should be configured to perform the statistical tests")
             if (!state(para)["normalized"])
               warning("Your cellHTS object has not been normalized, this could impact the results of these tests", immediate.=TRUE)
             if (state(para)["scored"])
               stop("This cellHTS object has been scored; the statistical analysis should be performed on the normalized signal intensities", immediate.=TRUE)
             if (!state(para)["annotated"])
               stop("This cellHTS object has not been annotated",immediate.=TRUE)
           }
           if (paraName == "annotationColumn") {
             if (!is.character(para) || length(para) != 1 )
               stop("'annotationColumn' should be a character value!\n")
           }
           if (paraName == "nwStatsControls") {
             if (!is.character(para) || length(para) != 1)
               stop("'controls/nwStatsControls' should be a character value!\n ")
           }
           if (paraName == "nwStatsAlternative") {
             if (!is.character(para) || length(para) != 1
                   || !(para %in% c("two.sided", "less", "greater")))
               stop("'alternative/nwStatsAlternative' should be one in 'two.sided','less' and 'greater'!\n ")
           }
           if (paraName == "nwStatsTests") {
             if (!is.character(para) || length(para) == 0 ||
                 !(para %in% c("T-test","MannWhitney","RankProduct")))
               stop("'tests/nwStatsTests' should be one or more in 'T-test', 'MannWhitney' and 'RankProduct'!\n ")
           }
         },
         General = {
           if (paraName == "verbose" &&
               (!is.logical(para) || length(para) != 1))
               stop("'verbose' should be a logical value!\n")
           if (paraName == "species" &&
               (!is.character(para) || length(para) != 1))
             stop("'species' should be a character!\n")
         },
         extractTS = {
           if (paraName == "fileList" && (!is.list(para) || length(para) < 2 ||
                                          is.null(names(para)) || any(is.na(names(para))) ))
             stop("'fileList' should be a named list with length more than 1!\n")
         },

         gscaTS = {
           if(paraName == "object" && class(para) != "TSImport"){
             stop("'TSImportData' should be an object of class TSImport!\n")
           }
           if(paraName == "gscaList" && (!is.list(para) || length(para) < 2 || is.null(names(para)) || any(is.na(names(para)))))
           {stop("'gscaList' should be a named list of GSCA objects with length more than 1!\n")}

         },
         nwaTS = {
           if(paraName == "object" && class(para) != "TSImport"){
             stop("'TSImportData' should be an object of class TSImport!\n")
           }
           if(paraName == "nwaList" &&
              (!is.list(para) || length(para) < 2 || is.null(names(para)) || any(is.na(names(para)))))
           {stop("'nwaList' should be a named list of NWA objects with length more than 1!\n")}

         },
         TSImport = {
           if(paraName == "experimentName" && length(para) < 2){
           stop("'experimentName' should be a character vector specifying each experiment names with length more than 1!\n")}
           if(paraName == "phenotypeTS" &&
              (is.null(names(unlist(para))) || any(!is.numeric(unlist(para))) )){
             stop("'phenotypeTS' should be a list, each element should be a numeric vector named with gene identifier!\n")
           }
           if(paraName == "pvaluesTS" &&
              (is.null(names(unlist(para))) || any(!is.numeric(unlist(para))) )){
             stop("'pvaluesTS' should be a list, each element should be a numeric vector named with gene identifier!\n")
           }
           if(paraName == "GSOADesign.matrix" &&
              (rownames(para) != "cutoff" ||  any(!colnames(para) %in% c("phenotype", "pvalues")) ||
               !is.numeric(para[, "phenotype"]) || !is.numeric(para[, "pvalues"]) )){
            stop("'GSOADesign.matrix' should be a numeric matrix with rownames named as 'cutoff' and colnames named as 'phenotype' and 'pvalue'!\n")
           }
         })
}


checkGSCAPara <- function(para) {
  if (missing(para))
    stop("'para' should be provided as a list!\n")

  ##check data type in para
  if (!(is.integer(para$pValueCutoff) ||
        is.numeric(para$pValueCutoff)) ||
      length(para$pValueCutoff) != 1 || para$pValueCutoff > 1)
    stop("'pValueCutoff' of para should be an integer or numeric value <=1!\n")
  if (!is.character(para$pAdjustMethod) ||
      length(para$pAdjustMethod) != 1 ||
      !(para$pAdjustMethod %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")))
    stop("'pAdjustMethod' of para should be any one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr' and 'none'!\n")
  if (!(is.integer(para$nPermutations) ||
        is.numeric(para$nPermutations)) ||
      length(para$nPermutations) != 1 || para$nPermutations < 1)
    stop("'nPermutations' of para should be an integer >=1 !\n")
  if (!(is.integer(para$minGeneSetSize) ||
        is.numeric(para$minGeneSetSize)) ||
      length(para$minGeneSetSize) != 1 || para$minGeneSetSize < 1)
    stop("'minGeneSetSize' of para should be an integer >=1 !\n")
  if (!(is.integer(para$exponent) ||
        is.numeric(para$exponent)) ||
      length(para$pValueCutoff) != 1 || para$exponent < 1)
    stop("'exponent' of para should be an integer or numeric value >=1 !\n")

  ##check parameter names
  AVAILABLE_NAMES <- c("pValueCutoff","pAdjustMethod","nPermutations","minGeneSetSize","exponent")
  if (!all(names(para) %in% AVAILABLE_NAMES)) {
    unavaNames <- paste(names(para)[!(names(para) %in% AVAILABLE_NAMES)], collapse = ", ")
    stop(paste("'para' should not contain", unavaNames, "!\n"))
  }

}

