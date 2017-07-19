
# HTSanalyzeR4MAGeCK ------------------------------------------------------
#' A pipeline function for MAGeCK data.
#'
#' A pipeline function for MAGeCK data gene set enrichment analysis, hypergeometric analysis and network analysis.
#'
#' @export
#'
HTSanalyzeR4MAGeCK <- function(file,
                               selection.dir = "negative",
                               doGSOA = FALSE,
                               doGSEA = TRUE,
                               GSOADesign.matrix = matrix(NA, nrow = 1, ncol = 2,
                                                          dimnames = list(c("cutoff"),c("phenotype", "pvalues"))),
                               listOfGeneSetCollections,
                               species = "Hs",
                               initialIDs = "SYMBOL",
                               keepMultipleMappings = TRUE,
                               duplicateRemoverMethod = "max",
                               orderAbsValue = FALSE,
                               pValueCutoff = 0.05,
                               pAdjustMethod = "BH",
                               nPermutations = 1000,
                               minGeneSetSize = 15,
                               exponent = 1,
                               verbose  = TRUE,
                               keggGSCs,
                               goGSCs,
                               msigdbGSCs,
                               interactionMatrix = NULL,
                               reportDir = "HTSanalyzerReport",
                               nwAnalysisGenetic = FALSE,
                               nwAnalysisFdr = 0.001
                               ){
  #------------------------------------------------------------------
  ## get phenotypes and pvalues
  if(selection.dir != "negative" && selection.dir != "positive"){
    stop("Please specify CRISPR selection direction:either 'positive' or 'negative'!\n")
  }
  if(selection.dir == "negative"){
    data4enrich <- file$neg.lfc
    pvalues <- file$neg.p.value
  }else{
    data4enrich <- file$pos.lfc
    pvalues <- file$pos.p.value
  }
  names(data4enrich) <- file$id
  names(pvalues) <- file$id
  #--------------------------------------------------------------------
  ## GSOA
  if(!is.logical(doGSOA)){
    stop("'doGSOA' must be a logical value specifying whether to do Hypergeometric analysis or not!\n")
  }
  if(doGSOA){
   if(any(!is.na(GSOADesign.matrix))){
    if(rownames(GSOADesign.matrix) != "cutoff" ||  any(!colnames(GSOADesign.matrix) %in% c("phenotype", "pvalues")) ||
       !is.numeric(GSOADesign.matrix[, "phenotype"]) || !is.numeric(GSOADesign.matrix[, "pvalues"]) ){
      stop("'GSOADesign.matrix' must be a numeric matrix with rownames named as 'cutoff'
           and colnames named as 'phenotype' and 'pvalues'!\n")
    }  ## check GSOADesign.matrix
    if(!is.na(GSOADesign.matrix[, "phenotype"])){
      if(all(!is.na(GSOADesign.matrix))){
        warning("Both metrics have value, would only use 'phenotype' to choose hits!\n")
      }
      hits <- names(data4enrich[which(abs(data4enrich) > GSOADesign.matrix[, "phenotype"])])
    }else{
      hits <- names(pvalues[which(pvalues < GSOADesign.matrix[, "pvalues"])])
    }
   }else{
    hits = character()
  }
    }  ## END GSOA set
  #---------------------------------------------------------------------
  ## Gene set enrichment analysis and hypergeometric analysis
  ##create a GSCA object
  gsca <- new("GSCA", listOfGeneSetCollections = listOfGeneSetCollections, geneList = data4enrich, hits = hits)
  ##preprocessing of input gene list and hit list * remove NA;
  ##duplicate operations; annotation conversions; order phenotypes
  gsca <- preprocess(gsca, species = species, initialIDs = initialIDs,
                     keepMultipleMappings = keepMultipleMappings, duplicateRemoverMethod = duplicateRemoverMethod,
                     orderAbsValue = orderAbsValue)
  ##do analysis
  gsca <- analyze(gsca, para=list(pValueCutoff = pValueCutoff, pAdjustMethod = pAdjustMethod,
                                  nPermutations = nPermutations, minGeneSetSize = minGeneSetSize,
                                  exponent = exponent), doGSOA = doGSOA, doGSEA = doGSEA)
  gsca <- appendGSTerms(gsca, keggGSCs=keggGSCs, goGSCs=goGSCs, msigdbGSCs = msigdbGSCs)
  #----------------------------------------------------------------------
  ## Network analysis
  ##create a NWA (NetWork Analysis) object
  nwa <- new("NWA", pvalues=pvalues, phenotypes=data4enrich)
  ##preprocessing
  nwa <- preprocess(nwa, species=species, initialIDs=initialIDs, keepMultipleMappings=keepMultipleMappings,
                    duplicateRemoverMethod=duplicateRemoverMethod)
  ##create an interactome
  nwa <- interactome(nwa, interactionMatrix = interactionMatrix,
                     species = species, reportDir = reportDir, genetic = nwAnalysisGenetic,
                     verbose = verbose)
  ##do analysis
  nwa <- analyze(nwa, fdr = nwAnalysisFdr, species = species, verbose = verbose)
  reportAll(gsca, nwa)
}
