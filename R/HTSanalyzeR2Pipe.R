
#' An analysis pipeline for common phenotype data
#'
#' This function performs a complete analyses of common phenotype data.
#'
#' @include  utils.R
#' @param data4enrich A numeric or integer vector of phenotypes named by gene identifiers.
#' @param hits A character vector of the gene identifiers (used as hits in the hypergeometric tests).
#' It's needed if you want to do GSOA (gene set overrepresentation analysis).
#' @param doGSOA A logic value specifying whether to do hypergeometric test or not, default is FALSE.
#' @param doGSEA A logic value specifying whether to do gene set enrichment analysis or not, default is TRUE.
#' @param listOfGeneSetCollections A list of gene set collections (a 'gene
#' set collection' is a list of gene sets).
#' @param species A single character value specifying the species for which the
#'   data should be read.
#' @param initialIDs A single character value specifying the type of initial
#'   identifiers for input geneList
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
#' @param pValueCutoff A single numeric value specifying the cutoff for p-values considered
#' significant in gene set collection analysis.
#' @param pAdjustMethod A single character value specifying the p-value adjustment method to be used
#' (see 'p.adjust' for details) in gene set collection analysis.
#' @param nPermutations A single integer or numeric value specifying the number of permutations for
#' deriving p-values in GSEA.
#' @param cores A single integer or numeric value specifying the number of cores to be used for GSEA.
#' @param minGeneSetSize A single integer or numeric value specifying the minimum number of elements
#' shared by a gene set and the input total genes. Gene sets with fewer than this number are removed
#' from both hypergeometric analysis and GSEA.
#' @param exponent A single integer or numeric value used in weighting phenotypes in GSEA.
#' @param GSEA.by A single character value to choose which algorithm to do GSEA. Valid value
#' could either be "HTSanalyzeR2"(default) or "fgsea". If performed by "fgsea", the result explanation
#' please refer to \code{\link[fgsea:fgsea]{fgsea}}.
#' @param keggGSCs A character vector of names of all KEGG gene set collections.
#' @param goGSCs A character vector of names of all GO gene set collections.
#' @param msigdbGSCs A character vector of names of all MSigDB gene set collections.
#' @param doNWA A logic value specifying whether to do subnetwork analysis or not, default is FALSE.
#' @param nwaPvalues A single numeric value specifying the false discovery for the scoring of nodes
#' in NWA analysis (see BioNet::scoreNodes and Dittrich et al., 2008 for details)
#' @param interactionMatrix An interaction matrix including columns
#' 'InteractionType', 'InteractorA' and 'InteractorB'. If this matrix
#' is available, the interactome can be directly built based on it.
#' @param reportDir A single character value specifying the directory to store reports. For default
#' the enrichment analysis reports will be stored in the directory called "HTSanalyzerReport".
#' @param nwAnalysisGenetic A single logical value. If TRUE, genetic interactions
#' will be kept; otherwise, they will be removed from the data set.
#' @param nwAnalysisFdr A single numeric value specifying the false discovery for the scoring of nodes
#' (see BioNet::scoreNodes and Dittrich et al., 2008 for details)
#' @return This pipeline function will finally return a list of GSCA object and NWA object.
#' @examples
#' \dontrun{
#' library(GO.db)
#' library(org.Hs.eg.db)
#' library(KEGGREST)
#'
#' data(d7)
#' ## define data4enrich
#' data4enrich <- as.vector(d7$neg.lfc)
#' names(data4enrich) <- d7$id
#'
#' ## select hits if you also want to do GSOA, otherwise ignore it
#' hits <-  names(data4enrich[which(abs(data4enrich) > 2)])
#'
#'
#' ## set up a list of gene set collections
#' GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
#' PW_KEGG <- KeggGeneSets(species="Hs")
#' ListGSC <- list(GO_MF=GO_MF, PW_KEGG=PW_KEGG)
#'
#' ## start analysis
#' rslt <- HTSanalyzeR2Pipe(data4enrich = data4enrich,
#'                          hits = hits,
#'                          doGSOA = TRUE,
#'                          doGSEA = TRUE,
#'                          listOfGeneSetCollections = ListGSC,
#'                          species = "Hs",
#'                          initialIDs = "SYMBOL",
#'                          pValueCutoff = 0.05,
#'                          nPermutations = 1000,
#'                          cores = 2,
#'                          minGeneSetSize = 100,
#'                          keggGSCs=c("PW_KEGG"),
#'                          goGSCs = c("GO_MF"),
#'                          doNWA = FALSE)
#'
#' report(rslt$gsca)}
#' @export
HTSanalyzeR2Pipe <- function(data4enrich,
                             hits = character(),
                             doGSOA = FALSE,
                             doGSEA = TRUE,
                             listOfGeneSetCollections,
                             species = "Hs",
                             initialIDs = "SYMBOL",
                             keepMultipleMappings = TRUE,
                             duplicateRemoverMethod = "max",
                             orderAbsValue = FALSE,
                             pValueCutoff = 0.05,
                             pAdjustMethod = "BH",
                             nPermutations = 1000,
                             cores = 1,
                             minGeneSetSize = 15,
                             exponent = 1,
                             verbose  = TRUE,
                             GSEA.by = "HTSanalyzeR2",
                             keggGSCs = NULL,
                             goGSCs = NULL,
                             msigdbGSCs = NULL,
                             doNWA = FALSE,
                             nwaPvalues = NULL,
                             interactionMatrix = NULL,
                             reportDir = "HTSanalyzerReport",
                             nwAnalysisGenetic = FALSE,
                             nwAnalysisFdr = 0.001){
  #------------------------------------------------------------------
  ## check common parameter
  paraCheck("Analyze", "doGSOA", doGSOA)
  paraCheck("Analyze", "doGSEA", doGSEA)
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
  #---------------------------------------------------------------------
  ## Gene set enrichment analysis and hypergeometric analysis
  ##create a GSCA object
  gsca <- GSCA(listOfGeneSetCollections = listOfGeneSetCollections,
               geneList = data4enrich, hits = hits)
  ##preprocessing of input gene list and hit list * remove NA;
  ##duplicate operations; annotation conversions; order phenotypes
  gsca <- preprocess(gsca, species = species, initialIDs = initialIDs,
                     keepMultipleMappings = keepMultipleMappings,
                     duplicateRemoverMethod = duplicateRemoverMethod,
                     orderAbsValue = orderAbsValue)
  doParallel::registerDoParallel(cores=cores)
  ##do analysis
  gsca <- analyze(gsca, para=list(pValueCutoff = pValueCutoff, pAdjustMethod = pAdjustMethod,
                                  nPermutations = nPermutations, minGeneSetSize = minGeneSetSize,
                                  exponent = exponent),
                  doGSOA = doGSOA, doGSEA = doGSEA, GSEA.by = GSEA.by)
  gsca <- appendGSTerms(gsca, keggGSCs=keggGSCs, goGSCs=goGSCs, msigdbGSCs = msigdbGSCs)
  #----------------------------------------------------------------------
  ## Network analysis
  ##create a NWA (NetWork Analysis) object
  if(doNWA){
    nwa <- NWA(pvalues=nwaPvalues, phenotypes=data4enrich)
    ##preprocessing
    nwa <- preprocess(nwa, species=species, initialIDs=initialIDs,
                      keepMultipleMappings=keepMultipleMappings,
                      duplicateRemoverMethod=duplicateRemoverMethod)
    ##create an interactome
    nwa <- interactome(nwa, interactionMatrix = interactionMatrix,
                       species = species, reportDir = reportDir,
                       genetic = nwAnalysisGenetic,
                       verbose = verbose)
    ##do analysis
    nwa <- analyze(nwa, fdr = nwAnalysisFdr, species = species, verbose = verbose)
    # saveRDS(gsca, nwa, file = file.path(reportDir,
    # paste(deparse(substitute(MAGeCKdata)), ".RData", sep="")))
  } else {
    nwa = NULL
  }
  list("gsca" = gsca, "nwa" = nwa)
}
