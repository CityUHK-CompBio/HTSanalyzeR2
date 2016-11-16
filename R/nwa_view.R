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
            phenotypeVector <- object@phenotypes
            diff.expr <- phenotypeVector[vertex_attr(subnw, "name")]
            names(diff.expr) <- vertex_attr(subnw, "name")
            diff.expr[which(is.na(diff.expr))] <- 0

            V(subnw)$diff  <- diff.expr[V(subnw)$name]

            subnw
          })


#' @export
#' @importFrom igraph as_data_frame
setMethod("viewSubNet", signature = "NWA",
          function(object,
                   options = list(charge = -200, distance = 200)) {
            g <- extractSubNet(object)

            em_nodes <- igraph::as_data_frame(g, "vertices")
            em_links <- igraph::as_data_frame(g, "edge")

            nMappings <- list(id = "name", color = "diff", label = "label")
            lMappings <- list(source = "from",target = "to")

            forceGraph(em_nodes, em_links, nMappings, lMappings,
                       charge = options$charge, distance = options$distance)
          })
