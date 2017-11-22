if (!isGeneric("extractSubNet")) {
  setGeneric("extractSubNet", function(object, ...)
    standardGeneric("extractSubNet"), package = "HTSanalyzeR2")
}
if (!isGeneric("viewSubNet")) {
  setGeneric("viewSubNet", function(object, ...)
    standardGeneric("viewSubNet"), package = "HTSanalyzeR2")
}

## Generate the igraph object for "plotD3Graph"
#' Extract the subnetwork as an igraph object
#'
#' Extract the subnetwork form an analyzed NWA object as an igraph object
#' for further external using. Users can also use it to modify the subnetwork.
#' @param object An NWA object.
#'
#' @importFrom igraph V E
#' @export
#' @aliases extractSubNet
#' @return This function would return a subnetwork as an 'igraph' object.
#' @examples
#' ## load a NWA object(see the examples of analyze NWA for details)
#' data(d7_nwa)
#'
#' ## extract the subnetwork as an igraph object
#' library(igraph)
#' subnetwork <- extractSubNet(d7_nwa)
#'
setMethod("extractSubNet", signature = "NWA",
          function(object) {
            subnw <- object@result$subnw
            if(is.null(subnw)) {
              stop("No subnet detected.")
            }

            V(subnw)$label <- unlist(object@result$labels[V(subnw)$name])
            phenotypes <- object@phenotypes
            diff.expr <- phenotypes[V(subnw)$name]
            diff.expr[is.na(diff.expr)] <- 0
            V(subnw)$diff <- diff.expr
            V(subnw)$colorScheme[diff.expr < 0] <- "neg"
            V(subnw)$colorScheme[diff.expr > 0] <- "pos"

            subnw
          })

#' Plot the identified subnetwork of an NWA object
#'
#' Plot the identified subnetwork of an NWA object.
#' @aliases viewSubNet
#' @param object An NWA object.
#' @param options A list of options to modify the enrichmentmap. Details are not showed
#' here due to too many options. Users are highly recommended to modify the enrichment
#' map in a shiny report by  \code{\link[HTSanalyzeR2]{report}}.
#' @param seriesObjs A list of NWA object. Internally used in the shiny report for visualizing
#' the subnetwork of time series data. No need to explicitly set it!
#' @export
#' @return In the end, this function would plot the identified subnetwork.
#' @examples
#' \donttest{
#' ## load a NWA object(see the examples of analyze NWA for details)
#' data(d7_nwa)
#'
#' ## plot the subnetwork
#' viewSubNet(d7_nwa)
#' }
#' @importFrom igraph as_data_frame
#' @importFrom utils modifyList
setMethod("viewSubNet", signature = "NWA",
          function(object,
                   options = list(),
                   seriesObjs = NULL) {

            g <- extractSubNet(object)

            em_nodes <- igraph::as_data_frame(g, "vertices")
            em_links <- igraph::as_data_frame(g, "edge")
            nMappings <- list(id = "name", color = "diff", label = "label", label_id = "name", label_term = "label", scheme = "colorScheme")
            lMappings <- list(source = "from", target = "to")

            series <- NULL
            if(!is.null(seriesObjs)) {
              ## TODO: paraCheck of seriesObj
              series <- names(seriesObjs)
              defaultKey <- series[1]
              # seriesDF: (nodes = nodeDF, edges = edgeDF, nodeSeriesCols = nodeCols, edgeSeriesCols = edgeCols)
              seriesDF <- fetchNWASeriesValues(seriesObjs)
              # Create series mappings
              nodeCols <- seriesDF$nodeSeriesCols
              nodeColNames <- sub("diff", "color", nodeCols)
              nodeColNames <- sub("colorScheme", "scheme", nodeColNames)
              names(nodeCols) <- nodeColNames
              edgeCols <- seriesDF$edgeSeriesCols
              names(edgeCols) <- edgeCols
              # Append series data
              nMappings <- c(nMappings, nodeCols)
              lMappings <- c(lMappings, edgeCols)
              nMappings[c("color", "scheme")] <- paste(nMappings[c("color", "scheme")], defaultKey, sep=".")
              # lMappings[c("weight")] <- paste(lMappings[c("weight")], defaultKey, sep=".")
              em_nodes <- seriesDF$nodes
              em_links <- seriesDF$edges
            }

            options$nodeScheme = "dual"
            defaultOptions = list(type = "NWA")
            defaultOptions$label = list(text = "term")
            graphOptions <- modifyList(defaultOptions, options)

            forceGraph(em_nodes, em_links, nMappings, lMappings, graphOptions, seriesData = series)
          })


#' @importFrom igraph as_data_frame
fetchNWASeriesValues <- function(nwaObjs) {
  # TODO: check the objs
  extractedValues <- lapply(seq_along(nwaObjs), function(i) {
    g <- extractSubNet(nwaObjs[[i]])
    dfList <- igraph::as_data_frame(g, "both")
    # Vertices - ("name"  "score" "label" "diff" )
    colsToAppend <- colnames(dfList$vertices) %in% c("diff", "colorScheme")
    colnames(dfList$vertices)[colsToAppend] <- paste(colnames(dfList$vertices), names(nwaObjs)[i], sep=".")[colsToAppend]
    dfList$vertices <- unique(dfList$vertices)
    # Edges - ("from", "to")
    colsToAppend <- colnames(dfList$edges) %in% c()
    colnames(dfList$edges)[colsToAppend] <- paste(colnames(dfList$edges), names(nwaObjs)[i], sep=".")[colsToAppend]
    dfList$edges <- unique(dfList$edges)
    rownames(dfList$edges) <- paste0(dfList$edges$from, dfList$edges$to)
    dfList
  })

  #Combine nodes
  colsInCommon <- c("name", "label")
  nodeCols <- setdiff(unlist(lapply(extractedValues, function(li) {colnames(li$vertices)})), colsInCommon)
  nodeDF <- unique(Reduce(rbind, lapply(extractedValues, function(li){li$vertices[colsInCommon]})))
  nodeDF[, nodeCols] <- NA
  for(li in extractedValues) {
    cols <- setdiff(colnames(li$vertices), colsInCommon)
    nodeDF[rownames(li$vertices), cols] <- li$vertices[, cols]
  }

  #Combine edges
  colsInCommon <- c("from", "to")
  edgeCols <- setdiff(unlist(lapply(extractedValues, function(li) {colnames(li$edges)})), colsInCommon)
  edgeDF <- unique(Reduce(rbind, lapply(extractedValues, function(li){li$edges[colsInCommon]})))
  edgeDF[, edgeCols] <- NA
  for(li in extractedValues) {
    cols <- setdiff(colnames(li$edges), colsInCommon)
    edgeDF[rownames(li$edges), cols] <- li$edges[, cols]
  }
  rownames(edgeDF) <- NULL

  list(nodes = nodeDF, edges = edgeDF, nodeSeriesCols = nodeCols, edgeSeriesCols = edgeCols)
}
