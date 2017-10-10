if (!isGeneric("report")) {
  setGeneric("report",
             function(object, specificGeneset = NULL, reportDir = "NWAReport")
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
          function(object, reportDir = "NWAReport") {
            reportAll(gsca = NULL, nwa = object, reportDir = reportDir)
          }
        )


## helper functions for shiny app
generateNWASummary <- function(nwa) {
  tagList(
    tags$h3("Summary"),
    tags$p(),
    tags$p("Interaction dataset:"),
    tags$ul(
      tags$li(paste("Database:", nwa@summary$db[, "name"])),
      tags$li(paste("Species:", nwa@summary$db[, "species"])),
      tags$li(paste("Node Number:", nwa@summary$db[, "node No"])),
      tags$li(paste("Edge Number:", nwa@summary$db[, "edge No"]))
    ),
    tags$p(),
    tags$p(paste("FDR for score calculation:", nwa@summary$para[, "FDR"])),
    tags$p("Subnetwork identified:"),
    tags$ul(
      tags$li(paste("Node Number:", length(V(nwa@result$subnw)))),
      tags$li(paste("Edge Number:", length(E(nwa@result$subnw))))
    )
  )
}
