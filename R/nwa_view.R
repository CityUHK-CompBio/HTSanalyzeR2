if (!isGeneric("viewSubNet")) {
  setGeneric("viewSubNet", function(object, ...)
    standardGeneric("viewSubNet"), package = "HTSanalyzeR2")
}
if (!isGeneric("plotSubNet")) {
  setGeneric("plotSubNet", function(object, ...)
    standardGeneric("plotSubNet"), package = "HTSanalyzeR2")
}
if (!isGeneric("extractSubNet")) {
  setGeneric("extractSubNet", function(object, ...)
    standardGeneric("extractSubNet"), package = "HTSanalyzeR2")
}

if (!isGeneric("viewSubNet2")) {
  setGeneric("viewSubNet2", function(object, ...)
    standardGeneric("viewSubNet2"), package = "HTSanalyzeR2")
}

#' @export
#' @importFrom igraph as_data_frame
setMethod("viewSubNet2", signature = "NWA",
          function(object,
                   options = list(charge = -200,
                                  distance = 200)) {
            g <- extractSubNet(object)

            em_nodes <- as_data_frame(g, "vertices")
            em_links <- as_data_frame(g, "edge")

            ## TODO move to d3plot
            em_nodes["size"] <- 8
            em_links["label"] <- "lb"
            em_links["weight"] <- 1

            nMappings = c("name", "size", "score", "label", "label")
            lMappings = c("from", "to", "label", "weight")
            names(nMappings) = c("id", "size", "color", "label", "desc")
            names(lMappings) = c("source", "target", "label", "weight")

            # TODO the min, 0, max values
            # if(min(em_nodes["color"] < 0 && max(em_nodes["color"] > 0) {
            #   c
            # }

            colorDomain <- c(min(em_nodes["score"]), 0, max(em_nodes["score"]))
            forceGraph(em_nodes, em_links, nMappings, lMappings,
                       charge = options$charge, distance = options$distance,
                       colorDomain = colorDomain
            )
          })


##view subnetwork
#' @export
setMethod("viewSubNet",
          "NWA",
          function(object) {
            networkPlot(nwAnalysisOutput = object@result,
                        phenotypeVector = object@phenotypes)
          })

##This function takes in a subnetwork module resulted from function
##networkAnalysis, a vector of labels for nodes in the module and a
##phenotype vector (optional) and generate a figure stored to
## "filepath" with the name "filename".
#' @importFrom igraph vertex_attr vcount
#' @importFrom BioNet plotModule
networkPlot <- function(nwAnalysisOutput, phenotypeVector = NULL) {
  ##check arguments
  if (!is.list(nwAnalysisOutput) ||
      !(c("subnw") %in% names(nwAnalysisOutput)))
    stop("'nwAnalysisOutput' should contain a subnetwork module!\n")
  subnw <- nwAnalysisOutput$subnw
  labels <- nwAnalysisOutput$labels
  if (!is(subnw, "igraph"))
    stop("The module in 'nwAnalysisOutput' should be an object ",
         "of class 'igraph'!\n")
  ##If no phenotype vector is specified, then we can just plot the module
  if (is.null(phenotypeVector)) {
    ##png("EnrichedSubNw.png", width = 900, height = 900)
    BioNet::plotModule(subnw, labels = labels)
    ##dev.off()
  } else {
    paraCheck("NWAClass", "phenotypes", phenotypeVector)
    ## "diff.expr" holds the phenotype for the nodes of the sub-network
    diff.expr <- phenotypeVector[vertex_attr(subnw, "name")]
    names(diff.expr) <- vertex_attr(subnw, "name")
    ## "present" contains the information of wether a node has an
    ##associated phenotype (1) or not (-1), will be used to give a
    ##different shape to the nodes of the network
    present <- rep(1, vcount(subnw))
    present[which(is.na(diff.expr))] <- -1
    ##replaces all phenotypes of non-phenotyped nodes by a zero
    diff.expr[which(is.na(diff.expr))] <- 0
    names(present) <- vertex_attr(subnw, "name")
    ##Plot the module
    if (vcount(subnw) == 1) {
      ##png(file.path(filepath, filename), width = 900, height = 900)
      BioNet::plotModule(subnw,
                 labels = labels,
                 scores = present,
                 diff.expr = diff.expr)
      ##dev.off()
    } else {
      Tcolors <- diff.expr
      Tcolors2 <- diff.expr
      if (max(abs(Tcolors)) < 5)
        Tcolors <- Tcolors * 5
      ## set red colors
      if (any(Tcolors > 0)) {
        maxRed <- max(ceiling(abs(Tcolors[which(Tcolors > 0)])))
        redCols <- colorRampPalette(colors = c("white", "red"))
        redVec <- redCols(maxRed)
        Tcolors2[which(Tcolors > 0)] <-
          redVec[ceiling(abs(Tcolors[which(Tcolors > 0)]))]
      }
      ##set the greens
      if (any(Tcolors < 0)) {
        maxGreen <- max(ceiling(abs(Tcolors[which(Tcolors < 0)])))
        greenCols <- colorRampPalette(colors = c("white", "green"))
        greenVec <- greenCols(maxGreen)
        Tcolors2[which(Tcolors < 0)] <-
          greenVec[ceiling(abs(Tcolors[which(Tcolors < 0)]))]
      }
      colScale <- unique(Tcolors2)
      colboundary <- rep(0, length(colScale))
      colboundary <- sapply(colScale, function(c) {
        values <- diff.expr[which(Tcolors2 == c)]
        values[which(abs(values) == max(abs(values)))[1]]
      })

      colMatrix <- cbind(colboundary[order(colboundary)],
                         colScale[order(colboundary)])
      ##png(file.path(filepath, filename), width = 900, height = 900)
      BioNet::plotModule(subnw,
                 labels = labels,
                 scores = present,
                 diff.expr = diff.expr)
      points(
        x = rep(-1.2, length(unique(Tcolors2))),
        y = seq(1.2, (1.2 - (
          0.05 * length(colMatrix[, 2])
        )),
        length.out = length(colMatrix[, 2])),
        pch = 15,
        col = colMatrix[, 2]
      )
      text(
        x = rep(-1.1, length(unique(Tcolors2))),
        y = seq(1.2, (1.2 - (
          0.05 * length(colMatrix[, 2])
        )),
        length.out = length(colMatrix[, 2])),
        labels = signif(as.numeric(colMatrix[, 1]), digits = 2),
        cex = 0.8
      )
      ##dev.off()
    }
  }
}


##plot subnetwork
setMethod("plotSubNet",
          "NWA",
          function(object,
                   filepath = ".",
                   filename = "test",
                   output = "png",
                   ...) {
            if (missing(filepath) || missing(filename))
              stop("Please specify 'filepath' and 'filename' ",
                   "to save network plot! \n")
            paraCheck("Report", "filepath", filepath)
            paraCheck("Report", "filename", filename)
            paraCheck("Report", "output", output)
            if (output == "pdf")
              pdf(file.path(filepath, filename), ... = ...)
            if (output == "png")
              png(file.path(filepath, filename), ... = ...)
            networkPlot(nwAnalysisOutput = object@result,
                        phenotypeVector = object@phenotypes)
            dev.off()
          })


## generate the igraph object for "plotD3Graph"
#' @importFrom igraph V E
#' @export
setMethod("extractSubNet", signature = "NWA",
          function(object) {
            g <- object@result$subnw
            if(is.null(g)) {
              stop("No subnet detected.")
            }

            V(g)$label <- unlist(object@result$labels[V(g)$name])

            # val.range = range(V(g)$score)
            # V(g)$color = (V(g)$score - val.range[1]) * 100 / (val.range[2] - val.range[1])
            # V(g)$label = unlist(object@result$labels[V(g)$name])
            # V(g)$size = 6
            # E(g)$width = 2
            #
            # em_nodes <- igraph::as_data_frame(g, "vertices")
            # em_links <- igraph::as_data_frame(g, "edge")
            # idx <- 0:(nrow(em_nodes) - 1)
            # names(idx) <- row.names(em_nodes)
            # E(g)$source <- idx[em_links[, "from"]]
            # E(g)$target <- idx[em_links[, "to"]]

            g
          })

