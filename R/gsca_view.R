## summarize & default show
if(!isGeneric("viewGSEA"))
  setGeneric("viewGSEA",function(object,...)
    standardGeneric("viewGSEA"), package="HTSanalyzeR2")
if(!isGeneric("plotGSEA"))
  setGeneric("plotGSEA",function(object,...)
    standardGeneric("plotGSEA"), package="HTSanalyzeR2")


#' Plot a figure of GSEA results for one gene set
#'
#' This is a generic function. When implemented as the S4 method for
#' objects of class GSCA, this function plots a figure of the positions
#' of the gene sets in the ranked gene list and the location of the enrichment score.
#'
#' @details
#' We suggest to print the names of top significant gene sets using the
#' function \code{\link[HTSanalyzeR2]{getTopGeneSets}} before plotting the GSEA results.
#' @param object A GSCA object.
#' @param gscName A single character value specifying the name of the gene set collection where
#' the gene set is.
#' @param gsName A single character value specifying the name of the gene set to be plotted.
#' @param ES.range A numeric vector for user-defined range of enrichment score showing in the
#' top part of GSEA plot. Default is the range of actual enrichment score of result. However, users are
#' also allowed to set by themselves for better comparison with other GSEA plots.
#' @param rankMetric.range A numeric vector for user-defined range of ranked gene lists showing in the
#' bottom part of GSEA plot. Default is the range of actual ranked gene lists of result. However, users are
#' also allowed to set by themselves for better comparison with other GSEA plots.
#' @param main.title A single character value specifying the main title of the GSEA plot. Default
#' is the name of the gene set.
#' @param ESline.col The color to be used for enrichment score profile line. Defaults to "FireBrick".
#' @param hits.col The color to be used for hits line to show the position of hits. Defaults to "CadetBlue".
#' @param rankMetric.col The color to be used for ranked metric line. Defaults to "CadetBlue".
#' @rdname viewGSEA
#' @aliases viewGSEA
#' @return In the end, this function would draw a GSEA figure for specified gene set.
#' @examples
#' ## load a GSCA object(see the examples of analyze GSCA for details)
#' data(d7_gsca)
#'
#' ## summarize gsca
#' summarize(d7_gsca)
#'
#' ## print top significant gene sets in GO_MF
#' topGS_GO_MF <- getTopGeneSets(d7_gsca, "GSEA.results", gscs = "GO_MF", allSig=TRUE)
#'
#' ## view GSEA results for one gene set
#' \dontrun{
#' viewGSEA(d7_gsca, "GO_MF", topGS_GO_MF[["GO_MF"]][1])
#'
#' ## view GSEA results for the same gene set by user defined enrichment score range,
#' ## ranked gene list range and enrichment score profile line color.
#' viewGSEA(d7_gsca, "GO_MF", topGS_GO_MF[["GO_MF"]][1],
#' ES.range = c(-1, 0), rankMetric.range = c(-5, 2),
#' ESline.col = "green")
#' }
#'
#' @include gsca_class.R
#' @export
setMethod(
  "viewGSEA",
  "GSCA",
  function(object, gscName, gsName,
           ES.range = NULL,
           rankMetric.range = NULL,
           main.title = NULL,
           ESline.col = "FireBrick",
           hits.col = "black",
           rankMetric.col = "CadetBlue") {
    ##check argument
    paraCheck("Report", "gs.single", gsName)
    paraCheck("Report", "gsc.name", gscName)
    if(!("GSEA.results" %in% names(object@result)))
      stop("GSEA not performed!\n")
    gs.all<-lapply(object@result[["GSEA.results"]][1:length(object@listOfGeneSetCollections)], rownames)
    if(length(unlist(gs.all))==0)
      stop("No gene sets in GSEA results!\n")
    if(!(gsName %in% unlist(gs.all)))
      stop("'gs' is not a gene set that passes the 'minGeneSetSize'! \n")
    if(!(gscName %in% names(object@listOfGeneSetCollections)))
      stop("'gsc' is not a gene set collection in 'listOfGeneSetCollections'!\n")

    test <- gseaScores(geneList = object@geneList,
                       geneSet = object@listOfGeneSetCollections[[gscName]][[gsName]],
                       exponent = object@para$exponent,
                       mode = "graph",
                       GSEA.results = object@result$GSEA.results[[gscName]],
                       gsName = gsName)
    gseaPlots(runningScore = test[['runningScore']],
              enrichmentScore = test[['enrichmentScore']],
              positions = test[['positions']], geneList = object@geneList,
              Adjust.P.value = test[['Adjust.P.value']],
              gsName = gsName,
              nPermutations = object@summary$para$gsea[, "nPermutations"],
              ES.range = ES.range,
              rankMetric.range = rankMetric.range,
              main.title = main.title,
              ESline.col = ESline.col,
              hits.col = hits.col,
              rankMetric.col = rankMetric.col)
  }
)


#' Plot and save figures of GSEA results for top significant gene sets
#'
#' This is a generic function.When implemented as the S4 method for objects of class GSCA,
#' this function plots figures of the positions of genes of the gene set in the ranked gene
#' list and the location of the enrichment score for top significant gene sets.
#'
#' @param object A GSCA object.
#' @param gscs A character vector specifying the names of gene set collections whose top
#' significant gene sets will be plotted.
#' @param ntop A single integer or numeric value specifying how many gene sets of top
#'  significance will be plotted.
#' @param allSig A single logical value. If 'TRUE', all significant gene sets (GSEA adjusted
#'  p-value < 'pValueCutoff' of slot 'para') will be plotted; otherwise, only top 'ntop' gene
#'   sets will be plotted.
#' @param filepath A single character value specifying where to store GSEA figures.
#' @param output A single character value specifying the format of output image: "pdf" or "png".
#' @param ES.range A numeric vector for user-defined range of enrichment score showing in the
#' top part of GSEA plot. Default is the range of actual enrichment score of result. However, users are
#' also allowed to set by themselves for better comparison with other GSEA plots.
#' @param rankMetric.range A numeric vector for user-defined range of ranked gene lists showing in the
#' bottom part of GSEA plot. Default is the range of actual ranked gene lists of result. However, users are
#' also allowed to set by themselves for better comparison with other GSEA plots.
#' @param ESline.col The color to be used for enrichment score profile line. Defaults to "FireBrick".
#' @param hits.col The color to be used for hits line to show the position of hits. Defaults to "CadetBlue".
#' @param rankMetric.col The color to be used for ranked metric line. Defaults to "CadetBlue".
#' @param ... Other arguments used by the function png or pdf such as 'width' and 'height'
#' @rdname plotGSEA
#' @return In the end, this function would plot GSEA figure and store them into the specified path.
#' @aliases plotGSEA
#' @examples
#' ## load a GSCA object(see the examples of analyze GSCA for details)
#' data(d7_gsca)
#'
#' ## summarize d7_gsca
#' summarize(d7_gsca)
#'
#' ## plot  significant gene sets in GO_MF and PW_KEGG
#' \dontrun{
#' plotGSEA(d7_gsca, gscs=c("GO_MF","PW_KEGG"), ntop=3, filepath=".")
#' }
#' @export
##plot GSEA for GSCA
setMethod(
  "plotGSEA",
  "GSCA",
  function(object, gscs, ntop=NULL, allSig=FALSE, filepath=".", output="pdf",
           ES.range = NULL,
           rankMetric.range = NULL,
           ESline.col = "FireBrick",
           hits.col = "black",
           rankMetric.col = "CadetBlue", ...) {
    ##check arguments
    paraCheck("Report", "filepath", filepath)
    paraCheck("Report", "output", output)
    paraCheck("Summarize", "allSig", allSig)
    if(!is.null(ntop))
      paraCheck("Summarize", "ntop", ntop)
    paraCheck("Report", "gscs.names", gscs)
    filenames <- getTopGeneSets(object, "GSEA.results", gscs, ntop, allSig)
    for(gsc in gscs) {
      ##plot for all gs.names
      gs.names<-filenames[[gsc]]
      if(!is.null(gs.names)) {
        for(gs.name in gs.names){
          makeGSEAplots(geneList=object@geneList, geneSet=object@listOfGeneSetCollections[[gsc]][[gs.name]],
                        exponent=object@para$exponent, filepath=filepath,
                        filename=gsub("/", "_", gs.name), output=output,
                        GSEA.results = object@result$GSEA.results[[gsc]],
                        gsName = gs.name,
                        nPermutations = object@summary$para$gsea[, "nPermutations"],
                        ES.range = ES.range,
                        rankMetric.range = rankMetric.range,
                        ESline.col = ESline.col,
                        hits.col = hits.col,
                        rankMetric.col = rankMetric.col,
                        ...=...)
        }
      }
    }
  }
)



#' @import graphics
#' @importFrom grDevices colorRampPalette
gseaPlots <- function(runningScore,
                      enrichmentScore,
                      positions,
                      geneList,
                      Adjust.P.value,
                      gsName,
                      nPermutations,
                      ES.range = NULL,
                      rankMetric.range = NULL,
                      main.title = NULL,
                      ESline.col = "FireBrick",
                      hits.col = "black",
                      rankMetric.col = "CadetBlue") {

  ## check arguments
  paraCheck("GSCAClass", "genelist", geneList)

  ## check that the 'runningScore' is a vector of length=length of geneList
  if(!is.numeric(runningScore) || length(runningScore)==0)
    stop("'runningScore' should be a numerical vector!\n")

  ##check that the 'positions' vector contains only zeros and ones,
  ##and is of the right length and class
  if(!(is.numeric(positions) || is.integer(positions)) || length(positions)==0)
    stop("'positions' should be a numerical vector!\n")
  if(!all(unique(positions) %in% c(0,1)))
    stop("'positions' should contains only 0s and 1s, see help(gseaScores)!\n")
  if(length(runningScore) != length(geneList))
    stop("The length of 'runningScore' should be the same as the length of 'geneList'!\n")
  if(length(positions) != length(geneList))
    stop("The length of 'positions' should be the same as the length of 'geneList'!\n")

  ## check the user defined range and mian title, etc.
  if(is.null(ES.range)){
    ES.range <- c(min(runningScore), max(runningScore))
  }

  if(is.null(rankMetric.range)){
    rankMetric.range <- c(min(geneList), max(geneList))
  }

  if(is.null(main.title)){
    main.title <- gsName
  }

  ##set the graphical parameters
  def.par <- par(no.readonly = TRUE)
  # Create a division of the device
  gsea.layout <- layout(matrix(c(1, 2, 3, 4)), heights = c(2, 0.5, 0.2, 1.7))

  ##Plot the running score and add a vertical line at the position of
  ##the enrichment score (maximal absolute value of the running score)
  par(mar = c(0, 5, 2, 2), cex.axis=1.4, cex.lab = 1.6, mgp = c(3.3,1,0))
  plot(x=c(1:length(runningScore)), y=runningScore, type="l",
      lwd=3, col=ESline.col,
      xaxt = "n", xaxs = "i",
      yaxt = "n", ylab="Enrichment score (ES)",
      ylim = ES.range)
  label <- seq(round(min(ES.range), digits = 1),
               round(max(ES.range), digits = 1),
               by = 0.1)
  axis(2, at=label,
       labels=label, las=1)
  title(main = main.title, cex.main = 1.8)
  grid(NULL, NULL, lwd = 2.5)
  abline(h=0, lty = 2)

  nPermutations <- as.numeric(nPermutations)
  minP <- formatC(1/nPermutations, format = "e", digits = 0)
  if(enrichmentScore > 0){
  text(x = length(geneList)/7*5, y = (enrichmentScore)/4*3,
       labels = paste("ES = ", signif(enrichmentScore, 3), "\nFDR ",
                      ifelse(Adjust.P.value == 0,
                             paste0("< ", minP),
                             paste0("= ", signif(Adjust.P.value, 2)))),
       cex = 1.5)} else {
         text(x = length(geneList)/6, y = (enrichmentScore)/4*3,
              labels = paste("ES = ", signif(enrichmentScore, 3), "\nFDR ",
                             ifelse(Adjust.P.value == 0,
                                    paste0("< ", minP),
                                    paste0("= ", signif(Adjust.P.value, 2)))),
              cex = 1.5)
       }
  #-------------------## plot a color barplot indicating the phenotypes
  par(mar = c(0, 5, 0, 2))
  plot(0, type = "n", xaxt = "n", xaxs = "i", xlab = "", yaxt = "n",
       ylab = "", xlim = c(1, length(geneList)), lwd = 3)
  abline(v=which(positions == 1), lwd = 0.75, col = hits.col)
  #------------------------------------------------------------------
  ##Plot the phenotypes along the geneList, and add a vertical line
  ##for each match between geneList and gene set
  ##this is done using the 'positions' output of gseaScores,
  ##which stores a one for each match position and a zero otherwise

  par(mar = c(0, 5, 0, 2))
  ticks = 1000
  ran <- range(geneList, na.rm = TRUE)
  offset <- ceiling(ticks * ran[1] / (ran[1] - ran[2]))
  offset <- ifelse(offset <= 0, 1, offset)
  offset <- ifelse(offset >= ticks, ticks-1, offset)
  palette <- c(colorRampPalette(c("blue", "white"))(offset),
               colorRampPalette(c("white", "red"))(ticks - offset))
  rank.colors <- palette[ceiling((geneList - ran[1]) / (ran[2] - ran[1]) * ticks)]

  rank.colors <- rle(rank.colors)
  barplot(matrix(rank.colors$lengths), col = rank.colors$values,
          border = NA, horiz = TRUE,xaxt = "n", xlim = c(1, length(geneList)))
  box()
  text(length(geneList) * 0.01, 0.7, "Positive", adj = c(0, NA), cex = 1.4)
  text(length(geneList) * 0.99, 0.7, "Negative", adj = c(1, NA), cex = 1.4)

  ## plot rank metric bar
  par(mar = c(5, 5, 0, 2), cex.axis=1.4, cex.lab = 1.6, mgp = c(3,1,0))
  rank.metric <- rle(round(geneList, digits = 2))
  plot(geneList, type = "n", xaxs = "i",
       xlab = "Rank in ordered gene list",
       xlim = c(0, length(geneList)),
       ylim = rankMetric.range, yaxs = "i",
       ylab = "Ranked list metric", yaxt = "n")
  label <- seq(round(min(geneList), digits = 0),
               round(max(geneList), digits = 0),
               by = 1)
  axis(2, at=label,
       labels=label,las=1)
  grid(NULL, NULL, lwd = 2.5)
  barplot(rank.metric$values, col = rankMetric.col, lwd = 1,
          xaxs = "i", xaxt = "n", yaxt = "n",
          xlim = c(0, length(geneList)),
          ylim = c(-1, 1), yaxs = "i",
          width = rank.metric$lengths, border = NA,
          space = 0, add = TRUE)
  box()

  par(def.par)  #- reset to default
}


## Write html reports
#' @importFrom grDevices dev.off pdf png
makeGSEAplots <- function(geneList, geneSet, exponent, filepath,
                          filename, output,
                          GSEA.results,
                          gsName,
                          nPermutations,
                          ES.range = NULL,
                          rankMetric.range = NULL,
                          ESline.col,
                          hits.col,
                          rankMetric.col,
                          ...) {
  test <- gseaScores(geneList = geneList, geneSet = geneSet,
                     exponent = exponent, mode = "graph", GSEA.results = GSEA.results,
                     gsName = gsName)
  filename<-sub("\\W","_", filename, perl=TRUE)
  if(!file.exists(file.path(filepath, "gsea_plots")))
    dir.create(file.path(filepath, "gsea_plots"))
  if(output == "pdf" )
    pdf(file=file.path(filepath, "gsea_plots", paste(filename, ".pdf", sep="")),
        onefile = FALSE, ...=...)
  if(output == "png" )
    png(filename=file.path(filepath, "gsea_plots", paste(filename, ".png", sep="")), ...=...)
  gseaPlots(runningScore = test[['runningScore']],
            enrichmentScore = test[['enrichmentScore']],
            positions = test[['positions']], geneList = geneList,
            Adjust.P.value = test[['Adjust.P.value']],
            gsName = gsName,
            nPermutations = nPermutations,
            ES.range = ES.range,
            rankMetric.range = rankMetric.range,
            ESline.col = ESline.col,
            hits.col = hits.col,
            rankMetric.col = rankMetric.col)
  dev.off()
}


##This function computes enrichment scores for GSEA, running score and
##position of hits for a gene set.
gseaScores <- function(geneList, geneSet, exponent=1, mode="score", gsName = gsName,
                       GSEA.results = GSEA.results) {
  paraCheck("GSCAClass", "genelist", geneList)
  paraCheck("Analyze", "exponent", exponent)
  paraCheck("Report", "gs", geneSet)
  paraCheck("Report", "gseaScore.mode", mode)
  geneSet<-intersect(names(geneList), geneSet)

  nh <- length(geneSet)
  N <- length(geneList)
  ES <- 0
  Phit <- rep(0, N)
  Pmiss <- rep(0, N)
  runningES <- rep(0, N)


  ## calculate the enrichment score
  if(nh > N) {
    stop("Gene Set is larger than Gene List")
  } else {
    hits <- rep(FALSE, N)
    hits[which(!is.na(match(names(geneList), geneSet)))] <- TRUE
    if(sum(hits)!=0) {
      Phit[which(hits)]<-abs(geneList[which(hits)])^exponent
      NR=sum(Phit)
      Pmiss[which(!hits)]<-1/(N-nh)
      Phit=cumsum(Phit/NR)
      Pmiss=cumsum(Pmiss)
      runningES<-Phit-Pmiss
      ESmax<-max(runningES)
      ESmin<-min(runningES)
      ES<-ifelse(abs(ESmin)>abs(ESmax), ESmin, ESmax)
    }
  }
  ## get GSEA adjust pvalues
  adjust.p.value <- GSEA.results[which(rownames(GSEA.results) == gsName),
                                 "Adjusted.Pvalue"]
  ## Return the relevant information according to mode
  if(mode=="score")
    return(ES)
  if(mode=="graph")
    return(list("enrichmentScore"=ES, "runningScore"=runningES,
                "positions"=as.integer(hits), "Adjust.P.value" = adjust.p.value))
}
