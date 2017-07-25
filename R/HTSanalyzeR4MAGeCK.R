
# HTSanalyzeR4MAGeCK ------------------------------------------------------
#' An analysis pipeline for MAGeCK data.
#'
#' This function writes an html report following a complete analyses of
#' a dataset based on the two classes GSCA (Gene Set Collection Analysis) and
#' NWA (NetWork Analysis) of this package.
#'
#' @param file A result file for CRISPR data using MAGeCK to do preprocessing.
#' @param selection.dir A character specifying which direction to choose form MAGeCK result, should be either
#' 'positive' or 'negative'.
#' @param doGSOA A logic value specifying whether to do hypergeometric test or not, default is FALSE.
#' @param doGSEA A logic value specifying whether to do gene set enrichment analysis or not, default is TRUE.
#' @param GSOADesign.matrix A numeric matrix to specify how to choose hits when doGSOA = TRUE. It must be a 1*2 matrix
#' with rownames named as "cutoff" and colnames named as "phenotype" and "pvalues".
#' @param listOfGeneSetCollections A list of gene set collections (a 'gene
#' set collection' is a list of gene sets).
#' @param species A single character value specifying the species for which the
#'   data should be read.
#' @param initialIDs A single character value specifying the type of initial
#'   identifiers for input geneList
#'   
#' @param keepMultipleMappings A single logical value. If TRUE, the function
#'   keeps the entries with multiple mappings (first mapping is kept). If FALSE,
#'   the entries with multiple mappings will be discarded.
#' @param duplicateRemoverMethod A single character value specifying the method
#'   to remove the duplicates. See duplicateRemover for details.
#' @param orderAbsValue A single logical value indicating whether the values
#'   should be converted to absolute values and then ordered (if TRUE), or
#'   ordered as they are (if FALSE).
#' @param verbose A single logical value specifying to display detailed messages
#'   (when verbose=TRUE) or not (when verbose=FALSE)
#' @param pValueCutoff a single numeric value specifying the cutoff for p-values considered
#' significant
#' @param pAdjustMethod a single character value specifying the p-value adjustment method to be used
#' (see 'p.adjust' for details)
#' @param nPermutations a single integer or numeric value specifying the number of permutations for
#' deriving p-values in GSEA
#' @param minGeneSetSize a single integer or numeric value specifying the minimum number of elements
#' in a gene set that must map to elements of the gene universe. Gene sets with
#' fewer than this number are removed from both hypergeometric analysis and GSEA.
#' @param exponent a single integer or numeric value used in weighting phenotypes in GSEA.
#' @param keggGSCs a character vector of names of all KEGG gene set collections
#' @param goGSCs a character vector of names of all GO gene set collections
#' @param msigdbGSCs a character vector of names of all MSigDB gene set collections
#' @param interactionMatrix an interaction matrix including columns
#' 'InteractionType', 'InteractorA' and 'InteractorB'. If this matrix
#' is available, the interactome can be directly built based on it.
#' @param reportDir reportDir a single character value specifying the directory to store reports. For default
#' the enrichment analysis reports will be stored in the directory called "GSCAReport".
#' @param nwAnalysisGenetic a single logical value. If TRUE, genetic interactions
#' will be kept; otherwise, they will be removed from the data set.
#' @param nwAnalysisFdr a single numeric value specifying the false discovery for the scoring of nodes
#' (see BioNet::scoreNodes and Dittrich et al., 2008 for details)
#' 
#' 
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
