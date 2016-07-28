## summarize & default show
if(!isGeneric("viewGSEA"))
  setGeneric("viewGSEA",function(object,...) standardGeneric("viewGSEA"), package="HTSanalyzeR2")
if(!isGeneric("plotGSEA"))
  setGeneric("plotGSEA",function(object,...) standardGeneric("plotGSEA"), package="HTSanalyzeR2")

#' @include gsca_class.R
#' @export
setMethod(
  "viewGSEA",
  "GSCA",
  function(object, gscName, gsName) {
    ##check argument
    paraCheck("gs.single", gsName)
    paraCheck("gsc.name", gscName)
    if(!("GSEA.results" %in% names(object@result)))
      stop("GSEA not performed!\n")
    gs.all<-lapply(object@result[["GSEA.results"]][1:length(object@listOfGeneSetCollections)], rownames)
    if(length(unlist(gs.all))==0)
      stop("No gene sets in GSEA results!\n")
    if(!(gsName %in% unlist(gs.all)))
      stop("'gs' is not a gene set that passes the 'minGeneSetSize'! \n")
    if(!(gscName %in% names(object@listOfGeneSetCollections)))
      stop("'gsc' is not a gene set collection in 'listOfGeneSetCollections'!\n")

    test <- gseaScores(geneList = object@geneList, geneSet = object@listOfGeneSetCollections[[gscName]][[gsName]],
                       exponent = object@para$exponent, mode = "graph")
    gseaPlots(runningScore = test[['runningScore']],
              enrichmentScore = test[['enrichmentScore']],
              positions = test[['positions']], geneList = object@geneList)
  }
)


##plot GSEA for GSCA
setMethod(
  "plotGSEA",
  "GSCA",
  function(object, gscs, ntop=NULL, allSig=FALSE, filepath=".", output="png", ...) {
    ##check arguments
    paraCheck(name="filepath", para=filepath)
    paraCheck(name="output", para=output)
    paraCheck("allSig", allSig)
    if(!is.null(ntop))
      paraCheck("ntop", ntop)
    paraCheck("gscs.names", gscs)
    filenames<-getTopGeneSets(object, "GSEA.results", gscs, ntop, allSig)
    for(gsc in gscs) {
      ##plot for all gs.names
      gs.names<-filenames[[gsc]]
      if(!is.null(gs.names)) {
        for(gs.name in gs.names){
          makeGSEAplots(geneList=object@geneList, geneSet=object@listOfGeneSetCollections[[gsc]][[gs.name]],
                        exponent=object@para$exponent, filepath=filepath,
                        filename=gsub("/", "_", gs.name), output=output, ...=...)
        }
      }
    }
  }
)


gseaPlots <- function(runningScore, enrichmentScore, positions, geneList) {
  ##check arguments
  paraCheck("genelist",geneList)
  ##paraCheck("output",output)
  ##paraCheck("filepath",filepath)
  ##paraCheck("filename",filename)
  ##check that the 'runningScore' is a vector of length=length of geneList
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
  ##open a file
  ##if(output == "pdf" )
  ##	pdf(file.path(filepath, paste("gsea_plots", filename, ".pdf", sep="")))
  ##if(output == "png" )
  ##	png(file.path(filepath, paste("gsea_plots", filename, ".png", sep="")))
  ##set the graphical parameters
  par(pin=c(5, 1.5), mfrow=c(2, 1), lwd=1, mai=c(0.2, 1, 1, 1))
  ##Plot the phenotypes along the geneList, and add a vertical line
  ##for each match between geneList and gene set
  ##this is done using the 'positions' output of gseaScores,
  ##which stores a one for each match position and a zero otherwise
  plot(x=seq(1, length(geneList)), type="l", y=geneList,
       ylab="Phenotypes", xlab=NA, col="magenta", lwd=2, xaxt="n")
  abline(v=which(positions == 1))
  abline(h=0)
  lines(x=seq(1, length(geneList)), type="l", y=geneList,
        ylab="Phenotypes", xlab=NA, col="magenta", lwd=2, xaxt="n")
  ##Plot the running score and add a vertical line at the position of
  ##the enrichment score (maximal absolute value of the running score)
  par(mai=c(1, 1, 0.1, 1))
  plot(x=c(1:length(runningScore)), y=runningScore,type="l",
       xlab="Position in the ranked list of genes", ylab="Running enrichment score")
  abline(h=0)
  abline(v=which(runningScore == enrichmentScore), lty=3, col="magenta", lwd=3)
}


##Write html reports
makeGSEAplots <- function(geneList, geneSet, exponent, filepath,
                          filename, output='png', ...) {
  test <- gseaScores(geneList = geneList, geneSet = geneSet,
                     exponent = exponent, mode = "graph")
  filename<-sub("\\W","_", filename, perl=TRUE)
  if(output == "pdf" )
    pdf(file=file.path(filepath, paste("gsea_plots", filename, ".pdf", sep="")), ...=...)
  if(output == "png" )
    png(filename=file.path(filepath, paste("gsea_plots", filename, ".png", sep="")), ...=...)
  gseaPlots(runningScore = test[['runningScore']],
            enrichmentScore = test[['enrichmentScore']],
            positions = test[['positions']], geneList = geneList)
  dev.off()
}


##This function computes enrichment scores for GSEA, running score and
##position of hits for a gene set.
gseaScores <- function(geneList, geneSet, exponent=1, mode="score") {
  paraCheck("genelist", geneList)
  paraCheck("exponent", exponent)
  paraCheck("gs", geneSet)
  paraCheck("gseaScore.mode", mode)
  geneSet<-intersect(names(geneList), geneSet)

  nh <- length(geneSet)
  N <- length(geneList)

  ES <- 0
  Phit <- rep(0, N)
  Pmiss <- rep(0, N)
  runningES <- rep(0, N)

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
  ##Return the relevant information according to mode
  if(mode=="score")
    return(ES)
  if(mode=="graph")
    return(list("enrichmentScore"=ES, "runningScore"=runningES,
                "positions"=as.integer(hits)))
}
