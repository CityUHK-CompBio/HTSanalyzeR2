if (!isGeneric("summarize")) {
  setGeneric("summarize", function(object, what = "ALL", ...)
    standardGeneric("summarize"), package = "HTSanalyzeR2")
}


##print summary information on screen
#' @export
setMethod("summarize",
          "NWA",
          function(object, what = "ALL") {
            paraCheck(name = "what.nwa", para = what)
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

##show summary information on screen
#' @export
setMethod("show",
          "NWA",
          function(object) {
            cat("A NWA (Network Analysis) object:\n")
            summarize(object, what = c("Pval", "Phenotype", "Interactome", "Para"))
          })
