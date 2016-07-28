if (!isGeneric("viewSubNet")) {
  setGeneric("viewSubNet", function(object, ...)
    standardGeneric("viewSubNet"), package = "HTSanalyzeR2")
}
if (!isGeneric("plotSubNet")) {
  setGeneric("plotSubNet", function(object, ...)
    standardGeneric("plotSubNet"), package = "HTSanalyzeR2")
}

##view subnetwork
setMethod("viewSubNet",
          "NWA",
          function(object) {
            networkPlot(nwAnalysisOutput = object@result,
                        phenotypeVector = object@phenotypes)
          })

##This function takes in a subnetwork module resulted from function
##networkAnalysis, a vector of labels for nodes in the module and a
##phenotype vector (optional) and generate a figure stored to
##'filepath' with the name 'filename'.

networkPlot <- function(nwAnalysisOutput, phenotypeVector = NULL) {
  ##check arguments
  if (!is.list(nwAnalysisOutput) ||
      !(c("subnw") %in% names(nwAnalysisOutput)))
    stop("'nwAnalysisOutput' should contain a subnetwork module!\n")
  subnw <- nwAnalysisOutput$subnw
  labels <- nwAnalysisOutput$labels
  if (!is(subnw, "graphNEL"))
    stop("The module in 'nwAnalysisOutput' should be an object ",
         "of class 'graphNEL'!\n")
  ##If no phenotype vector is specified, then we can just plot the module
  if (is.null(phenotypeVector)) {
    ##png("EnrichedSubNw.png", width = 900, height = 900)
    plotModule(subnw, labels = labels)
    ##dev.off()
  } else {
    paraCheck("phenotypes", phenotypeVector)
    ##'diff.expr' holds the phenotype for the nodes of the sub-network
    diff.expr <- phenotypeVector[nodes(subnw)]
    names(diff.expr) <- nodes(subnw)
    ##'present' contains the information of wether a node has an
    ##associated phenotype (1) or not (-1), will be used to give a
    ##different shape to the nodes of the network
    present <- rep(1, length(nodes(subnw)))
    present[which(is.na(diff.expr))] <- -1
    ##replaces all phenotypes of non-phenotyped nodes by a zero
    diff.expr[which(is.na(diff.expr))] <- 0
    names(present) <- nodes(subnw)
    ##Plot the module
    if (length(nodes(subnw)) == 1) {
      ##png(file.path(filepath, filename), width = 900, height = 900)
      plotModule(subnw,
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
      plotModule(subnw,
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
            paraCheck(name = "filepath", para = filepath)
            paraCheck(name = "filename", para = filename)
            paraCheck(name = "output", para = output)
            if (output == "pdf")
              pdf(file.path(filepath, filename), ... = ...)
            if (output == "png")
              png(file.path(filepath, filename), ... = ...)
            networkPlot(nwAnalysisOutput = object@result,
                        phenotypeVector = object@phenotypes)
            dev.off()
          })
