




#' An analysis pipeline for MAGeCK data.
#'
#' This function writes an html report following a complete analyses of
#' a dataset based on the two classes GSCA (Gene Set Collection Analysis) and
#' NWA (NetWork Analysis) of this package.
#'
#' @include  utils.R
#' @param file A result file for CRISPR data using MAGeCK to do preprocessing.
#' @param selectDirection A character specifying which direction to choose form MAGeCK result, should either be
#' 'positive' or 'negative'.
#' @param doGSOA A logic value specifying whether to do hypergeometric test or not, default is FALSE.
#' @param doGSEA A logic value specifying whether to do gene set enrichment analysis or not, default is TRUE.
#' @param hitsCutoffLogFC A numeric value as cutoff to choose hits based on log2fold change when doing GSOA. Genes with absolute
#' log2fold change greater than this cutoff would be choosen as hits. Either
#' 'hitsCutoffLogFC' or 'hitsCutoffPval' is needed when doing GSOA.
#' @param hitsCutoffPval A numeric value as cutoff to choose hits based on pvalue when doing GSOA.
#' Genes with pvalues less than this cutoff would be choosen as hits.
#' Either 'hitsCutoffLogFC' or 'hitsCutoffPval' is needed when doing GSOA.
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
#' significant in gene set collection analysis
#' @param pAdjustMethod a single character value specifying the p-value adjustment method to be used
#' (see 'p.adjust' for details) in gene set collection analysis
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
                               selectDirection = "negative",
                               doGSOA = FALSE,
                               doGSEA = TRUE,
                               hitsCutoffLogFC = NULL,
                               hitsCutoffPval = NULL,
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
  ## check common parameter
  paraCheck("Analyze", "doGSOA", doGSOA)
  paraCheck("Analyze", "doGSEA", doGSEA)
  if(!is.null(hitsCutoffLogFC)){
  paraCheck("Pipeline", "hitsCutoffLogFC", hitsCutoffLogFC)
  }
  if(!is.null(hitsCutoffPval)) {
  paraCheck("Pipeline", "hitsCutoffPval", hitsCutoffPval)
  }
  paraCheck("GSCAClass", "gscs", listOfGeneSetCollections)
  paraCheck("General", "species", species)
  paraCheck("Annotataion", "initialIDs", initialIDs)
  paraCheck("Annotataion", "keepMultipleMappings", keepMultipleMappings)
  paraCheck("PreProcess", "duplicateRemoverMethod", duplicateRemoverMethod)
  paraCheck("PreProcess", "orderAbsValue", orderAbsValue)
  if(!is.null(keggGSCs)) {
    paraCheck("Report", "keggGSCs", keggGSCs)
  }
  if(!is.null(goGSCs)) {
    paraCheck("Report", "goGSCs", goGSCs)
  }
  if(!is.null(msigdbGSCs)) {
    paraCheck("Report", "msigdbGSCs", msigdbGSCs)
  }
  paraCheck("Analyze", "fdr", nwAnalysisFdr)
  paraCheck("PreProcess", "genetic", nwAnalysisGenetic)
  paraCheck("General", "verbose", verbose)
  #----------------------------------------------------------------------
  ## check file
  if(any(!(c("neg.lfc", "neg.p.value", "pos.lfc", "pos.p.value") %in% colnames(file) ))){
    stop("'file' should be a result file directly from MAGecCK!\n")
  }
  file <- as.data.frame(file, stringsAsFactors = F)
  file$neg.p.value <- as.numeric(file$neg.p.value)
  file$neg.lfc <- as.numeric(file$neg.lfc)
  file$pos.p.value <- as.numeric(file$pos.p.value)
  file$pos.lfc <- as.numeric(file$pos.lfc)
  ## get phenotypes and pvalues
  if(selectDirection != "negative" && selectDirection != "positive"){
    stop("Please specify CRISPR selection direction:either 'positive' or 'negative'!\n")
  }
  if(selectDirection == "negative"){
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
  hits = character()  ## initialize hits
  if(doGSOA){
    if(is.null(hitsCutoffLogFC) && is.null(hitsCutoffPval)){
      stop("Please define either 'hitsCutoffLogFC' or 'hitsCutoffPval' to do GSOA!\n")
    }else if(!is.null(hitsCutoffLogFC) && !is.null(hitsCutoffPval)){
      warning("both 'hitsCutoffLogFC' and 'hitsCutoffPval' have vaule, would only use 'hitsCutoffLogFC' to choose hits!\n")
    }else if(!is.null(hitsCutoffLogFC)){
      hits <- names(data4enrich[which(abs(data4enrich) > hitsCutoffLogFC)])
    }else {
      hits <- names(pvalues[which(pvalues < hitsCutoffPval)])
    }}  ## END GSOA set
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
