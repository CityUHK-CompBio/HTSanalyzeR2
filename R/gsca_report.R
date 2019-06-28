if (!isGeneric("report")) {
  setGeneric("report",
             function(object, specificGeneset = NULL,
                      cutoff = NULL,
                      reportDir = "GSCAReport",
                      gseaPlot = TRUE,
                      para = list(output="pdf",
                                  ES.range = NULL,
                                  rankMetric.range = NULL,
                                  ESline.col = "FireBrick",
                                  hits.col = "black",
                                  rankMetric.col = "CadetBlue"))
    standardGeneric("report"), package = "HTSanalyzeR2")
}

#' Write HTML reports for enrichment or network analyses
#'
#' This is a generic function.
#'
#' When implemented as the method of class GSCA or NWA, this function produces reports for
#' either the Gene Set Collection Analysis or the NetWork Analysis.
#' @aliases report
#' @param object  A 'GSCA' or 'NWA'object.
#' @param reportDir A single character value specifying the path to store reports. By default,
#' the enrichment analysis reports will be stored in the directory called "GSCAReport". Once launching,
#' the directory will be generated automatically containing an R code file named app.R, an RDdata
#' object named results.RData storing all basic results which can be loaded into R with readRDS() function and
#' a folder named gsea_plots containing gsea plots for all significant gene sets.
#' @param gseaPlot A logical value to choose whether make gsea plot for significant gene
#' sets or not, default is TRUE.
#' @param para A list of parameters for gsea plot. See \code{\link[HTSanalyzeR2]{plotGSEA}} for details.
#' @param specificGeneset A named list of specific gene sets. See \code{\link[HTSanalyzeR2]{viewEnrichMap,GSCA-method}}
#' for details.
#' @param cutoff A numeric value between 0 and 1. This parameter is setted as a cutoff of edge weight in the enrichment
#' map for better visualization. When the edge weight, namely the Jaccard coefficient between two gene sets, is less than
#' this cutoff, this edge would not be showed in the enrichment map.
#' @details
#' This will generate a shiny report including all the GSCA or NWA results.
#'
#' For GSCA object, users can download the table of GSOA and/or GSEA result in different format
#' such as 'csv' and 'pdf'. In GSEA result of this report, 'Pvalue' or 'Adjusted.Pvalue'
#' equalling to 0 would be replaced by less than 1 divided by permutation times.
#' The enrichment map could be modified according to the user's preferences
#' such as layout, node, label, and etc. Details please see the vignette of our package.
#'
#' For NWA object, the identified subnetwork could be modified according to the user's preferences
#' in many ways such as layout, color, and etc. Details please see the vignette of our package.
#' @return In the end, this function would generate a Shiny report.
#' @examples
#' # =======================================================
#' # GSCA class
#' ## load a GSCA object(see the examples of analyze GSCA for details)
#' data(d7_gsca)
#'
#' ## Example1: report d7_gsca
#' \dontrun{
#' report(d7_gsca)
#' }
#'
#' ## Example2: report d7_gsca containing enrichment map with specificGeneset
#' tmp <- getTopGeneSets(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF"),
#'                       ntop = 20000, allSig = FALSE)
#' ## In that case, we can define specificGeneset as below:
#' GO_MF_geneset <- tmp$GO_MF[20:35]
#' ## the name of specificGenesets also needs to match with the names of tmp
#' specificGeneset <- list("GO_MF"=GO_MF_geneset)
#' \dontrun{
#' report(d7_gsca, specificGeneset=specificGeneset)
#' }
#'
#' ## Example3: report d7_gsca using a cutoff to filter away edges with small weight
#' \dontrun{
#' report(d7_gsca, cutoff = 0.01)
#' }
#'
#' @rdname report
#'
#' @export
setMethod("report", signature = "GSCA",
          function(object, specificGeneset = NULL,
                   cutoff = NULL, reportDir = "GSCAReport",
                   gseaPlot = TRUE,
                   para = list(output="pdf",
                               ES.range = NULL,
                               rankMetric.range = NULL,
                               ESline.col = "FireBrick",
                               hits.col = "black",
                               rankMetric.col = "CadetBlue")) {
            reportAll(gsca = object, nwa = NULL, specificGeneset = specificGeneset,
                      reportDir = reportDir, cutoff = cutoff, para = para,
                      gseaPlot = gseaPlot)
          }
)


#' Write HTML reports for both enrichment and network analyses
#'
#'This function can create shiny reports for both gene sets enrichment analysis and network analysis.
#'
#' @param gsca An analyzed GSCA object or a list of analyzed GSCA objects.
#' @param nwa An NWA object or a list of NWA objects.
#' @param TSOrder A character specifying the visulization order of 'Time Series' data in shiny report. Only works when
#' reporting for 'Time Series' data, default is the ID order in 'expInfor'.
#' @param specificGeneset A named list of specific gene sets. See \code{\link[HTSanalyzeR2]{viewEnrichMap,GSCA-method}}
#' for details.
#' @param cutoff A numeric value between 0 and 1. This parameter is setted as a cutoff of edge weight in the enrichment
#' map for better visualization. When the edge weight, namely the Jaccard coefficient between two gene sets, is less than
#' this cutoff, this edge would not be showed in the enrichment map.
#' @param gseaPlot A logical value to choose whether make gsea plot for significant gene
#' sets, default is TRUE.
#' @param para A list of parameters for gsea plot. See \code{\link[HTSanalyzeR2]{plotGSEA}} for details.
#' @param reportDir A single character value specifying the path to store reports. By default,
#' the enrichment analysis reports will be stored in the directory called "GSCAReport". Once launching,
#' the directory will be generated automatically containing an R code file named app.R, an RDdata
#' object named results.RData storing all basic results which can be loaded into R with readRDS() function and
#' a folder named gsea_plots containing gsea plots for all significant gene sets.
#' @export
#' @return In the end, this function would generate a html report.
#' @examples
#' \dontrun{
#' ## load data
#' data(d7_gsca, d7_nwa, gscaTS, nwaTS)
#'
#' ## Example1: report both gsca and nwa
#' reportAll(gsca=d7_gsca, nwa=d7_nwa)
#'
#' ## Example2: report gscaTS
#' reportAll(gsca=gscaTS)
#'
#' ## Example3: report nwaTS
#' reportAll(nwa=nwaTS)
#'
#' ## Example4: report both gscaTS and nwaTS
#' reportAll(gsca=gscaTS, nwa=nwaTS)
#'
#' ## Example5: change order for time series data
#' reportAll(gsca=gscaTS, TSOrder=names(gscaTS)[c(3, 1, 2)])
#' reportAll(nwa=nwaTS, TSOrder=names(nwaTS)[c(3, 2, 1)])
#'
#' ## Example6: view specificGeneset enrichment map for gscaTS using reportAll
#' library(igraph)
#' ## As told previously, specificGeneset needs to be a subset of all analyzed gene sets
#' ## which can be roughly gotten by:
#' tmp <- getTopGeneSets(gscaTS[[1]], resultName = "GSEA.results",
#'                       gscs=c("GO_BP"), ntop = 20000, allSig = FALSE)
#' ## In that case, we can define specificGeneset as below:
#' GO_BP_geneset <- tmp$GO_BP[c(4:10, 20:30)]
#' ## the name of specificGenesets also needs to match with the names of tmp
#' specificGeneset <- list("GO_BP"=GO_BP_geneset)
#' reportAll(gsca=gscaTS, specificGeneset=specificGeneset)
#'
#' ## Example7: report gscaTS using a cutoff to filter away edges with small weight
#' reportAll(gsca=gscaTS, cutoff = 0.03)
#' }
#'
reportAll <- function(gsca = NULL, nwa = NULL, TSOrder = NULL,
                      specificGeneset = NULL, cutoff = NULL,
                      gseaPlot = TRUE,
                      para = list(output="pdf",
                                  ES.range = NULL,
                                  rankMetric.range = NULL,
                                  ESline.col = "FireBrick",
                                  hits.col = "black",
                                  rankMetric.col = "CadetBlue"),
                      reportDir = "AnalysisReport") {
  if(!is.null(gsca) && class(gsca) != "GSCA") {
    if(class(gsca) != "list" || any(sapply(gsca, class) != "GSCA")) {
      stop("the parameter gsca should be a GSCA object or a list of GSCA objects\n")
    }
    if(!is.null(TSOrder)){
      ## check TSOrder
      if(length(intersect(TSOrder, names(gsca))) < length(names(gsca)) ||
         length(TSOrder) != length(names(gsca))){
        stop("'TSOrder' is not valid, should be experiment ID in 'expInfor'!\n")
      }else{
        gsca <- gsca[TSOrder]
      }
    }
  }

  if(!is.null(nwa) && class(nwa) != "NWA") {
    if(class(nwa) != "list" || any(sapply(nwa, class) != "NWA")) {
      stop("the parameter nwa should be a NWA object or a list of NWA objects\n")
    }
    if(!is.null(TSOrder)){
      ## check TSOrder
      if(length(intersect(TSOrder, names(nwa))) < length(names(nwa)) ||
         length(TSOrder) != length(names(nwa))){
        stop("'TSOrder' is not valid, should be experiment ID in 'expInfor'!\n")
      }else{
        nwa <- nwa[TSOrder]
      }
    }
  }

  reportDir <- paste(reportDir, format(Sys.time(), "%y%m%d-%H%M%S"), sep="-")
  if(file.exists(reportDir)) {
    reportDir <- paste(reportDir, sample(0:65535, 1), sep="-")
  }
  dir.create(reportDir)

  shinyApp <- system.file("templates/app.R", package="HTSanalyzeR2")
  file.copy(from = shinyApp, to = reportDir, overwrite = TRUE)

  results <- list(gsca = gsca, nwa = nwa, specificGeneset = specificGeneset, cutoff = cutoff)
  saveRDS(results, file = file.path(reportDir, "results.RData"))

  ## generate GSEA plots in folder gsea_plots for all significant gene sets
  ## undo: take gscaTS into consideration...
  if(gseaPlot){
  if(!is.null(gsca) && class(gsca) == "GSCA" && !is.null(gsca@result$GSEA.results)){
    plotGSEA(gsca, gscs = names(gsca@result$GSEA.results),
             allSig = TRUE, filepath = reportDir,
             output = para$output, ES.range = para$ES.range,
             rankMetric.range = para$rankMetric.range,
             ESline.col = para$ESline.col, hits.col = para$hits.col,
             rankMetric.col = para$rankMetric.col)
  } else if(!is.null(gsca) && class(gsca) == "list" && all(sapply(gsca, class) == "GSCA")){ ## for gscaTS
    gscaName <- names(gsca)
    lapply(1:length(gsca), function(i){
      dir.create(file.path(".", reportDir, gscaName[i]))
      filepath <- file.path(".", reportDir, gscaName[i])
      plotGSEA(gsca[[i]], gscs = names(gsca[[i]]@result$GSEA.results),
               allSig = TRUE, filepath = filepath,
               output = para$output, ES.range = para$ES.range,
               rankMetric.range = para$rankMetric.range,
               ESline.col = para$ESline.col, hits.col = para$hits.col,
               rankMetric.col = para$rankMetric.col)
    })

  }  ## end else if
  }  ## end gseaPlot

  shiny::runApp(reportDir)
}


## helper functions for shiny app
#' @importFrom shiny tagList
generateGSCASummary <- function(gsca) {
  title <- tags$h3("Summary")
  desc  <- tags$p(paste("The enrichment analysis was performed using the phenotype vector including",
                        gsca@summary$gl[, "input"], "genes and", gsca@summary$gl[, "converted to entrez"],
                        "genes used after filtering."))
  p1 <- tags$p("This analysis was performed using the gene set collection(s):")
  analysis <- tags$ul(lapply(1:length(gsca@listOfGeneSetCollections), function(i){
    tags$li(paste(names(gsca@listOfGeneSetCollections)[i], "(", length(gsca@listOfGeneSetCollections[[i]]),
                  "gene sets, of which", gsca@summary$gsc[i, 2], "were above the minimum size )"))}))
  p2 <- tags$p("The following methods were used:")
  hypgeo <- NULL
  if(!is.null(gsca@result$HyperGeo.results)) {
    hypgeo <- tags$li(list(
      tags$p("Hypergeometric test"),
      tags$ul(
        tags$li(paste("Significant gene set cutoff p-value (adjusted):", gsca@summary$para$hypergeo[, "pValueCutoff"])),
        tags$li(paste("MHT correction method:", gsca@summary$para$hypergeo[, "pAdjustMethod"])),
        tags$li(paste("Minimum gene set size:", gsca@summary$para$hypergeo[, "minGeneSetSize"]))
      )
    ))
  }
  gsea <- NULL
  if(!is.null(gsca@result$GSEA.results)) {
    gsea <- tags$li(list(
      tags$p("Gene Set Enrichment Analysis"),
      tags$ul(
        tags$li(paste("Significant gene set cutoff p-value (adjusted):", gsca@summary$para$gsea[, "pValueCutoff"])),
        tags$li(paste("Minimum gene set size:", gsca@summary$para$gsea[, "minGeneSetSize"])),
        tags$li(paste("MHT correction method:", gsca@summary$para$gsea[, "pAdjustMethod"])),
        tags$li(paste("Number of permutations:", gsca@summary$para$gsea[, "nPermutations"])),
        tags$li(paste("Exponent:", gsca@summary$para$gsea[, "exponent"]))
      )
    ))
  }
  method <- tags$ul(hypgeo, gsea)
  tagList(title, desc, p1, analysis, p2, method)
}


createLink <- function(val) {
  sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
}

createLink_KEGG <- function(val) {
  sprintf('<a href="http://www.genome.jp/dbget-bin/www_bget?pathway:%s" target="_blank" class="btn btn-primary">%s</a>', val, val)
}

createLink_GO <- function(val) {
  sprintf('<a href="http://www.ebi.ac.uk/QuickGO/GTerm?id=%s" target="_blank" class="btn btn-primary">%s</a>', val, val)
}


## append search links for gene set
appendLinks <- function(gsca) {
  for(analysis in c("HyperGeo.results", "GSEA.results", "Sig.pvals.in.both", "Sig.adj.pvals.in.both")) {
    for(geneSets in names(gsca@result[[analysis]])) {
      result <- gsca@result[[analysis]][[geneSets]]
      if(grepl("GO", geneSets)) {  ## can not identify right gene set types when not using GO/KEGG to name gene set...
        setLinks <- createLink_GO(rownames(result));
      } else if(grepl("KEGG", geneSets)) {
        setLinks <- createLink_KEGG(rownames(result));
      } else {
        setLinks <- createLink(rownames(result))
      }
      gsca@result[[analysis]][[geneSets]] <- data.frame(Gene.Set = setLinks, result)
    }
  }
  gsca
}

## combine all results in different gene set collections
combineResults <- function(gsca) {
  for(name in c("HyperGeo.results", "GSEA.results")) {
    if(!is.null(gsca@result[[name]])){
      gsca@result[[name]]$ALL <- do.call(rbind, gsca@result[[name]])
      gsca@result[[name]]$ALL <- gsca@result[[name]]$ALL[order(gsca@result[[name]]$ALL$Adjusted.Pvalue), ]
    }
  }
  if(!is.null(gsca@result$Sig.adj.pvals.in.both)){
    gsca@result$Sig.adj.pvals.in.both$ALL <- do.call(rbind, gsca@result$Sig.adj.pvals.in.both)
    gsca@result$Sig.adj.pvals.in.both$ALL <- gsca@result$Sig.adj.pvals.in.both$ALL[
      order(gsca@result$Sig.adj.pvals.in.both$ALL$HyperGeo.Adj.Pvalue,
            gsca@result$Sig.adj.pvals.in.both$ALL$GSEA.Adj.Pvalue), ]
  }
  gsca
}

availableResults <- function(results, byRow = TRUE) {
  res <- c(0, 0, 0)
  names(res) <- c("HyperGeo", "GSEA", "Significant in both")
  if(byRow) {
    if(!is.null(results$HyperGeo.results)) res["HyperGeo"] <- 1
    if(!is.null(results$GSEA.results)) res["GSEA"] <- 1
    if(length(unlist(results$Sig.adj.pvals.in.both))>0) res["Significant in both"] <- 1
    res <- names(res[res==1])

  } else {
    res <- colnames(results)[colSums(results, na.rm = TRUE) >= 0]
  }
  res
}

