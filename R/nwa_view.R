if (!isGeneric("extractSubNet")) {
  setGeneric("extractSubNet", function(object, ...)
    standardGeneric("extractSubNet"), package = "HTSanalyzeR2")
}
if (!isGeneric("viewSubNet")) {
  setGeneric("viewSubNet", function(object, ...)
    standardGeneric("viewSubNet"), package = "HTSanalyzeR2")
}

## generate the igraph object for "plotD3Graph"
#' @importFrom igraph V E
#' @export
setMethod("extractSubNet", signature = "NWA",
          function(object) {
            subnw <- object@result$subnw
            if(is.null(subnw)) {
              stop("No subnet detected.")
            }

            V(subnw)$label <- unlist(object@result$labels[V(subnw)$name])

            phenotypes <- object@phenotypes
            if(is.matrix(phenotypes)) {
              diff.expr <- phenotypes[V(subnw)$name, ]

              ticks <- colnames(diff.expr)
              for (tick in ticks) {
                vertex_attr(subnw, paste0("diff.", tick)) <- diff.expr[, tick]
              }
              V(subnw)$diff <- diff.expr[, ncol(diff.expr)]
            } else {
              diff.expr <- phenotypes[V(subnw)$name]
              diff.expr[is.na(diff.expr)] <- 0
              V(subnw)$diff <- diff.expr
            }

            subnw
          })


#' @export
#' @importFrom igraph as_data_frame
setMethod("viewSubNet", signature = "NWA",
          function(object,
                   nodeOptions = NULL,
                   options = list(charge = -200, distance = 200)) {

            hasSeriesAttr <- is.matrix(object@phenotypes)
            g <- extractSubNet(object)

            em_nodes <- igraph::as_data_frame(g, "vertices")
            em_links <- igraph::as_data_frame(g, "edge")

            nMappings <- list(id = "name", color = "diff", label = "label")
            lMappings <- list(source = "from",target = "to")

            series <- NULL
            if(hasSeriesAttr) {
              ticks <- grep("^diff.", colnames(em_nodes), value = TRUE)
              series <- sub("diff.", "", ticks)
              names(ticks) <- sub("diff", "color", ticks)
              nMappings <- c(nMappings, ticks)
            }

            forceGraph(em_nodes, em_links, nMappings, lMappings,
                       nodeOptions = nodeOptions,
                       charge = options$charge, distance = options$distance,
                       seriesData = series)
          })

