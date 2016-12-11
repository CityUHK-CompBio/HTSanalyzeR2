if (!isGeneric("report")) {
  setGeneric("report",
             function(object, nodeOptions = list(), reportDir = "GSCAReport")
    standardGeneric("report"), package = "HTSanalyzeR2")
}

## report
#' @export
setMethod("report", signature = "GSCA",
          function(object, nodeOptions = list(), reportDir = "GSCAReport") {
            reportAll(gsca = object, nwa = NULL,
                      gscaNodeOptions = nodeOptions, nwaNodeOptions = NULL,
                      reportDir)
          }
)


#' @export
reportAll <- function(gsca, nwa, gscaNodeOptions = NULL, nwaNodeOptions = NULL,
                      reportDir = "AnalysisReport") {
  if(!is.null(gsca) && class(gsca) != "GSCA") {
    stop("the object gsca should be a GSCA object\n")
  }
  if(!is.null(nwa) && class(nwa) != "NWA") {
    stop("the object nwa should be a NWA object\n")
  }

  if(file.exists(reportDir)) {
    reportDir <- paste(reportDir, format(Sys.time(), "%H%M%S"))
  }
  dir.create(reportDir)

  templateDir <- dir(system.file("templates", package="HTSanalyzeR2"), full.names=TRUE)
  file.copy(from = templateDir, to = reportDir, overwrite = TRUE)

  results <- list(gsca = gsca, nwa = nwa,
                  gscaNodeOptions = gscaNodeOptions,
                  nwaNodeOptions = nwaNodeOptions)
  saveRDS(results, file = file.path(reportDir, "results.RData"))

  shiny::runApp(reportDir)
}


## helper functions for shiny app
generateGSCSummary <- function(gsca) {
  paste(sapply(1:length(gsca@listOfGeneSetCollections), function(i){
    (paste("- ", names(gsca@listOfGeneSetCollections)[i],
           " ( ", length(gsca@listOfGeneSetCollections[[i]]),
           " gene sets, of which ", gsca@summary$gsc[i, 2],
           " were above the minimum size )", sep=""))
  }), collapse = "\n")
}

generateMethodsSummary <- function(gsca) {
  summ <- ""
  if(!is.null(gsca@result$HyperGeo.results)) {
    summ <- paste(summ, "- Hypergeometric test",
                  "\n + Significant gene set cutoff p-value (adjusted): ", gsca@summary$para$hypergeo[, "pValueCutoff"],
                  "\n + MHT correction method: ", gsca@summary$para$hypergeo[, "pAdjustMethod"],
                  "\n + Minimum gene set size: ", gsca@summary$para$hypergeo[, "minGeneSetSize"], sep = "")
  }
  if(!is.null(gsca@result$GSEA.results)) {
    summ <- paste(summ, "\n\n- Gene Set Enrichment Analysis",
                  "\n + Significant gene set cutoff p-value (adjusted): ", gsca@summary$para$gsea[, "pValueCutoff"],
                  "\n + Minimum gene set size: ", gsca@summary$para$gsea[, "minGeneSetSize"],
                  "\n + MHT correction method: ", gsca@summary$para$gsea[, "pAdjustMethod"],
                  "\n + Number of permutations: ", gsca@summary$para$gsea[, "nPermutations"],
                  "\n + Exponent: ", gsca@summary$para$gsea[, "exponent"], sep = "")
  }
  summ
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

appendLinks <- function(gsca) {
  for(analysis in c("HyperGeo.results", "GSEA.results", "Sig.pvals.in.both", "Sig.adj.pvals.in.both")) {
    for(geneSets in names(gsca@result[[analysis]])) {
      result <- gsca@result[[analysis]][[geneSets]]
      if(grepl("GO", geneSets)) {
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

combineResults <- function(gsca) {
  for(name in c("HyperGeo.results", "GSEA.results")) {
    if(!is.null(gsca@result[[name]])){
      gsca@result[[name]]$ALL <- do.call(rbind, gsca@result[[name]])
      gsca@result[[name]]$ALL <- gsca@result[[name]]$ALL[order(gsca@result[[name]]$ALL$Adjusted.Pvalue), ]
    }
  }
  if(!is.null(gsca@result$Sig.adj.pvals.in.both)){
    gsca@result$Sig.adj.pvals.in.both$ALL <- do.call(rbind, gsca@result$Sig.adj.pvals.in.both)
    gsca@result$Sig.adj.pvals.in.both$ALL <- gsca@result$Sig.adj.pvals.in.both$ALL[order(gsca@result$Sig.adj.pvals.in.both$ALL$HyperGeo.Adj.Pvalue, gsca@result$Sig.adj.pvals.in.both$ALL$GSEA.Adj.Pvalue), ]
  }
  gsca
}

availableResults <- function(results, byRow = TRUE) {
  res <- c("HyperGeo", "GSEA", "Significant in both")
  if(byRow) {
    res <- res[which(!is.na(rowSums(results)))]
  } else {
    res <- colnames(results)[colSums(results, na.rm = TRUE) > 0]
  }
  res
}

