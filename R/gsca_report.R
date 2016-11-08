if (!isGeneric("report")) {
  setGeneric("report", function(object, what = "ALL", ...)
    standardGeneric("report"), package = "HTSanalyzeR2")
}

## report
#' @export
setMethod("report",
          "GSCA",
          function(object, reportDir = "GSCAReport") {
            reportAll(gsca = object, nwa = NULL, reportDir)
          }
)


#' @export
reportAll <- function(gsca, nwa, reportDir = "AnalysisReport") {
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

  results <- list(gsca = gsca, nwa = nwa)
  saveRDS(results, file = file.path(reportDir, "results.RData"))

  shiny::runApp(reportDir)
}
