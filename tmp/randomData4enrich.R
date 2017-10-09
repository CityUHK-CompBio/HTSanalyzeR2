pvalues
data4enrich

colCount <- 10

data4enrichMat <- matrix(rep(data4enrich, colCount), length(data4enrich), colCount)

factor <- matrix(0, length(data4enrich), colCount)
factor[, 0] <- runif(length(data4enrich), 0, 0.4)
for(i in c(2:colCount)) factor[, i] <- factor[, i - 1] + runif(length(data4enrich), 0, 0.4)
factor[,colCount] <- 1
factor[factor > 1] <- 1

data4enrichMat <- data4enrichMat * factor

rownames(data4enrichMat) <- names(data4enrich)
colnames(data4enrichMat) <- paste0(c(1:colCount), "h")



factor
data4enrichMat



saveRDS(list(pvalues =pvalues, data4enrich = data4enrich, data4enrichMat = data4enrichMat), "./data4enrich.RData")
saveRDS(list(nwa = nwa, nwam = nwam, gsca = gsca, nwaOpts = nwaOpts, gscaOpts = gscaOpts), "objects.RData")




gscaTS <- list('1h'=gsca, '2h'=gsca, '3h'=gsca, '4h'=gsca)
for (name in names(gscaTS)) {
  res <- gscaTS[[name]]@result
  res$GSEA.results$GO_MF[, "Adjusted.Pvalue"] = res$GSEA.results$GO_MF[, "Adjusted.Pvalue"] + runif(nrow(res$GSEA.results$GO_MF), -0.1, 0.1)
  gscaTS[[name]]@result <- res
}

