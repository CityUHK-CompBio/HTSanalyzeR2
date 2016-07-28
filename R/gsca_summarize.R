

## summarize & default show
setGeneric("summarize", function(object, what = "ALL", ...)
  standardGeneric("summarize"), package = "HTSanalyzeR2")
setGeneric("getTopGeneSets", function(object,
                                      resultName,
                                      gscs,
                                      ntop = NULL,
                                      allSig = FALSE)
  standardGeneric("getTopGeneSets"), package = "HTSanalyzeR2")

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


##select top significant gene sets
#' @include gsca_class.R
#' @export
setMethod("getTopGeneSets",
          "GSCA",
          function(object,
                   resultName,
                   gscs,
                   ntop = NULL,
                   allSig = FALSE) {
            ##check arguments
            if (missing(gscs))
              stop("Please specify the name(s) of Gene Set Collections in 'gscs'! \n")
            paraCheck(name = "gscs.names", para = gscs)
            paraCheck(name = "resultName", para = resultName)

            if (!(resultName %in% names(object@result)))
              stop("Please input 'HyperGeo.results' or 'GSEA.results'!\n")
            if (is.null(object@result[[resultName]]))
              stop("Please run Hypergeometric or GSEA analysis before using this function!\n")
            gsc.names <- names(object@result[[resultName]])
            if (!all(gscs %in% gsc.names))
              stop("Wrong Gene Set Collection name(s) in 'gscs'! \n")
            if (!is.null(ntop))
              paraCheck(name = "ntop", para = ntop)
            paraCheck(name = "allSig", para = allSig)

            if ((is.null(ntop) && !allSig) || (!is.null(ntop) && allSig))
              stop("Either specify 'ntop' or set 'allSig' to be TRUE!\n")
            filenames <- list()
            for (gsc in gscs) {
              all.gs.names <- rownames(object@result[[resultName]][[gsc]])
              gs.names <- NULL
              if (allSig) {
                gs.names <-
                  all.gs.names[object@result[[resultName]][[gsc]][, "Adjusted.Pvalue"] < object@para$pValueCutoff]
              } else {
                if (ntop > nrow(object@result[[resultName]][[gsc]]))
                  stop("'ntop' is larger than the number of gene sets in specified gene set collection!\n")
                gs.names <- all.gs.names[1:ntop]
              }
              filenames[[gsc]] <-
                unlist(lapply(
                  list(gs.names),
                  gsub,
                  pattern = "/",
                  replacement = "_"
                ))
              names(filenames[[gsc]]) <- gs.names
            }
            return(filenames)
          })

setMethod("show", signature = "GSCA", function(object) {
  cat("A GSCA (Gene Set Collection Analysis) object:\n")
  summarize(object, what = c("GSC", "GeneList", "Hits", "Para"))
})
