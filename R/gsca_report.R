if (!isGeneric("report")) {
  setGeneric("report",
             function(object, nodeOptions = list(), reportDir = "GSCAReport")
    standardGeneric("report"), package = "HTSanalyzeR2")
}

## report
#' @export
setMethod("report", signature = "GSCA",
          function(object, nodeOptions = list(), reportDir = "GSCAReport") {
            reportAll(gsca = object, nwa = NULL,
                      gscaNodeOptions = nodeOptions, nwaNodeOptions = NULL,
                      reportDir)
          }
)


#' @export
reportAll <- function(gsca, nwa, gscaNodeOptions = NULL, nwaNodeOptions = NULL,
                      reportDir = "AnalysisReport") {
  if(!is.null(gsca) && class(gsca) != "GSCA") {
    stop("the object gsca should be a GSCA object\n")
  }
  if(!is.null(nwa) && class(nwa) != "NWA") {
    stop("the object nwa should be a NWA object\n")
  }

  if(file.exists(reportDir)) {
    reportDir <- paste(reportDir, format(Sys.time(), "%H%M%S"))
  }
  dir.create(reportDir)

  templateDir <- dir(system.file("templates", package="HTSanalyzeR2"), full.names=TRUE)
  file.copy(from = templateDir, to = reportDir, overwrite = TRUE)

  results <- list(gsca = gsca, nwa = nwa,
                  gscaNodeOptions = gscaNodeOptions,
                  nwaNodeOptions = nwaNodeOptions)
  saveRDS(results, file = file.path(reportDir, "results.RData"))

  shiny::runApp(reportDir)
}
