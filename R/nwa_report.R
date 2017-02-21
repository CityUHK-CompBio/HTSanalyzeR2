if (!isGeneric("report")) {
  setGeneric("report",
             function(object, nodeOptions = list(), reportDir = "NWAReport")
    standardGeneric("report"), package = "HTSanalyzeR2")
}


#' @rdname report
#' @examples
#' # Conducting the preparation steps
#' data(xn)
#' data(data4enrich)
#' # Conducting one sample t-test & compute the p-values
#' test.stats <- cellHTS2OutputStatTests(cellHTSobject=xn,annotationColumn="GeneID", alternative="two.sided",tests=c("T-test"))
#' library(BioNet)
#' pvalues <- BioNet::aggrPvals(test.stats, order=2, plot=FALSE)
#' nwa <- NWA(pvalues=pvalues, phenotypes=data4enrich)
#' nwa <- preprocess(nwa, species="Dm", initialIDs="FLYBASECG", keepMultipleMappings=TRUE, duplicateRemoverMethod="max")
#' nwa_inter <- interactome(nwa, species="Dm", reportDir="biogrid", genetic=FALSE)
#' # Case1: Conducting the network analysis step
#' nwa_result <- analyze(nwa_inter, fdr=0.0001, species="Dm")
#' # Case2: Conducting the network analysis step(with the interactionMatrix)
#' data(nwam)
#' nwam_inter <- interactome(nwam, species="Dm", reportDir="biogrid", genetic=FALSE)
#' nwam_result <- analyze(nwam_inter, fdr=0.0001, species="Dm")
#' #report the results with shiny
#' report(nwa_result)
#' report(nwam_result)
#' @export
setMethod("report",
          "NWA",
          function(object, nodeOptions = list(), reportDir = "NWAReport") {
            reportAll(gsca = NULL, nwa = object,
                      gscaNodeOptions = NULL, nwaNodeOptions = nodeOptions,
                      reportDir)
          }
        )
