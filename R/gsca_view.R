## summarize & default show
if(!isGeneric("viewGSEA"))
  setGeneric("viewGSEA",function(object,...) standardGeneric("viewGSEA"), package="HTSanalyzeR2")
if(!isGeneric("plotGSEA"))
  setGeneric("plotGSEA",function(object,...) standardGeneric("plotGSEA"), package="HTSanalyzeR2")

#' @include gsca_class.R
#' @export
setMethod(
  "viewGSEA",
  signature = "GSCA",
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

    # test <- gseaScores(geneList = object@geneList, geneSet = object@listOfGeneSetCollections[[gscName]][[gsName]],
    #                    exponent = object@para$exponent, mode = "graph")
    # gseaPlots(runningScore = test[['runningScore']],
    #           enrichmentScore = test[['enrichmentScore']],
    #           positions = test[['positions']], geneList = object@geneList)
  }
)

##plot GSEA for GSCA
#' @include gsca_class.R
#' @export
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
    # filenames<-getTopGeneSets(object, "GSEA.results", gscs, ntop, allSig)
    # for(gsc in gscs) {
      # ##plot for all gs.names
      # gs.names<-filenames[[gsc]]
      # if(!is.null(gs.names)) {
      #   for(gs.name in gs.names){
      #     ## 属于writeReport的部分
      #     makeGSEAplots(geneList=object@geneList, geneSet=object@listOfGeneSetCollections[[gsc]][[gs.name]],
      #                   exponent=object@para$exponent, filepath=filepath,
      #                   filename=gsub("/", "_", gs.name), output=output, ...=...)
      #   }
      # }
    # }
  }
)
