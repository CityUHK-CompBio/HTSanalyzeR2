if (!isGeneric("report")) {
  setGeneric("report", function(object, what = "ALL", ...)
    standardGeneric("report"), package = "HTSanalyzeR2")
}

## report
#' @export
setMethod("report",
          "NWA",
          function(object, reportDir = "NWAReport") {
            reportAll(gsca = NULL, nwa = object, reportDir)
          }
          )
