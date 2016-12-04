if (!isGeneric("report")) {
  setGeneric("report",
             function(object, nodeOptions = list(), reportDir = "NWAReport")
    standardGeneric("report"), package = "HTSanalyzeR2")
}

## report
#' @export
setMethod("report",
          "NWA",
          function(object, nodeOptions = list(), reportDir = "NWAReport") {
            reportAll(gsca = NULL, nwa = object,
                      gscaNodeOptions = NULL, nwaNodeOptions = nodeOptions,
                      reportDir)
          }
        )
