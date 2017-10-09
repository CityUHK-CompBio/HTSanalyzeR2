if (!isGeneric("summarize")) {
  setGeneric("summarize", function(object, what = "ALL", ...)
    standardGeneric("summarize"), package = "HTSanalyzeR2")
}


#' @describeIn summarize For an object of class NWA, the key words include 'Pval'
#' (the slot 'pvalue'), 'Phenotype' (the slot 'phenotype'), 'Interactome' (the
#' slot 'interactome'), 'Para' (the slot 'fdr'), 'Result' (the slot 'result')
#' and 'ALL' (all slots).
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
#' nwa_result <- analyze(nwa_inter, fdr=0.0001, species="Dm")
#' # Conducting the summary of the results
#' summer_result<-summarize(nwa_result)
#' @export
setMethod("summarize", signature = "NWA",
          function(object, what = "ALL") {
            paraCheck("Summarize", "NWAwhat", what)
            if (any(c("ALL", "Pval") %in% what)) {
              cat("\n")
              cat("-p-values: \n")
              print(object@summary$input[1,], quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "Phenotype") %in% what)) {
              cat("\n")
              cat("-Phenotypes: \n")
              print(object@summary$input[1,], quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "Interactome") %in% what)) {
              cat("\n")
              cat("-Interactome: \n")
              print(object@summary$db, quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "Para") %in% what)) {
              cat("\n")
              cat("-Parameters for analysis: \n")
              print(object@summary$para, quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "Result") %in% what)) {
              cat("\n")
              cat("-Subnetwork identified: \n")
              print(object@summary$result, quote = FALSE)
              cat("\n")
            }
          })

setMethod("show", signature = "NWA", function(object) {
  cat("A NWA (Network Analysis) object:\n")
  summarize(object, what = c("Pval", "Phenotype", "Interactome", "Para"))
})
