if (!isGeneric("report")) {
  setGeneric("report",
             function(object, specificGeneset = NULL, reportDir = "NWAReport")
    standardGeneric("report"), package = "HTSanalyzeR2")
}

#' @rdname report
#' @examples
#'
#' # =================================================================
#' # NWA class
#' ## load a NWA object(see the examples of analyze NWA for details)
#' data(nwa)
#'
#' ## report nwa
#' report(nwa)
#'
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
