
## summarize & default show
setGeneric("summarize", function(object, what, ...)
  standardGeneric("summarize"), package = "HTSanalyzeR2")

#' @include gsca_class.R
#' @export
setMethod("summarize", signature = "GSCA",
          function(object, what = "ALL") {
            # paraCheck("what.gsca",what)
            ##what can be "GSC" (gene set collection), "GeneList", "Hits", "Para", "Result"
            if (any(c("ALL", "GSC") %in% what)) {
              cat("\n")
              cat("-No of genes in Gene set collections: \n")
              print(object@summary$gsc, quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "GeneList") %in% what)) {
              cat("\n")
              cat("-No of genes in Gene List: \n")
              print(object@summary$gl, quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "Hits") %in% what)) {
              cat("\n")
              cat("-No of hits: \n")
              print(object@summary$hits, quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "Para") %in% what)) {
              cat("\n")
              cat("-Parameters for analysis: \n")
              print(object@summary$para$hypergeo, quote = FALSE)
              cat("\n")
              print(object@summary$para$gsea, quote = FALSE)
              cat("\n")
            }
            if (any(c("ALL", "Result") %in% what)) {
              cat("\n")
              cat("-Significant gene sets (adjusted p-value<",
                  object@para$pValueCutoff,
                  "): \n")
              print(object@summary$results, quote = FALSE)
              cat("\n")
            }
          })

setMethod("show", signature = "GSCA", function(object) {
  cat("A GSCA (Gene Set Collection Analysis) object:\n")
  summarize(object, what = c("GSC", "GeneList", "Hits", "Para"))
})
