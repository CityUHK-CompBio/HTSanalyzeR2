## write Summary
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


selectDT <- function(gsca, input) {
  if (input$analysis == "Significant in both") {
    analysis <- "Sig.adj.pvals.in.both"
    res <- gsca@result[[analysis]][[input$genesets]]

    res$HyperGeo.Adj.Pvalue <- round(res$HyperGeo.Adj.Pvalue, digits = 3)
    res$GSEA.Adj.Pvalue <- round(res$GSEA.Adj.Pvalue, digits = 3)

    dt <- DT::datatable(res, filter = 'top', rownames = F, escape = F,
                        options = list(pageLength = 10,
                                       columnDefs = list(list(
                                         targets = which(colnames(res) %in% c("HyperGeo.Adj.Pvalue", "GSEA.Adj.Pvalue"))-1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && Number(data) < 0.001 ?",
                                           "'<0.001' : data;",
                                           "}")
                                       ))), callback = JS('table.page(0).draw(false);'))

  } else {
    analysis <- paste0(input$analysis, ".results")
    res <- gsca@result[[analysis]][[input$genesets]]

    res$Pvalue <- round(res$Pvalue, digits = 3)
    res$Adjusted.Pvalue <- round(res$Adjusted.Pvalue, digits = 3)

    dt <- DT::datatable(res, filter = 'top', rownames = F, escape = F,
                        options = list(pageLength = 10,
                                       columnDefs = list(list(
                                         targets = which(colnames(res) %in% c("Pvalue", "Adjusted.Pvalue"))-1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && Number(data) < 0.001 ?",
                                           "'<0.001' : data;",
                                           "}")
                                       ))), callback = JS('table.page(0).draw(false);')) %>%

      formatStyle('Adjusted.Pvalue', target = "row",
                  fontWeight = styleInterval(gsca@para$pValueCutoff, c('bold', 'weight'))
                  # backgroundColor = styleInterval(gsca@para$pValueCutoff, c('green', 'gray')),
      )

    if (analysis == "HyperGeo.results") dt <- dt %>% formatRound("Expected.Hits", digits = 3)
    if (analysis == "GSEA.results") dt <- dt %>% formatSignif("Observed.score", digits = 3)

  }
  dt
}
