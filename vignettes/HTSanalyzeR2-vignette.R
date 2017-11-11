## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "#")
library(HTSanalyzeR2)
library(org.Hs.eg.db)
library(KEGGREST)
library(GO.db)
library(igraph)

## ---- eval=TRUE----------------------------------------------------------
data(d7)
phenotype <- as.vector(d7$neg.lfc)
names(phenotype) <- d7$id

## ---- eval=TRUE----------------------------------------------------------
hits <-  names(phenotype[which(abs(phenotype) > 2)])

## ------------------------------------------------------------------------
GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
PW_KEGG <- KeggGeneSets(species="Hs")
MSig_C2 <- MSigDBGeneSets(collection = "c2")
ListGSC <- list(GO_MF=GO_MF, PW_KEGG=PW_KEGG, MSig_C2=MSig_C2)

## ------------------------------------------------------------------------
gsca <- new("GSCA", listOfGeneSetCollections=ListGSC, geneList=phenotype, hits=hits)
gsca

## ---- message = FALSE, warning=FALSE-------------------------------------
gsca <- preprocess(gsca, species="Hs", initialIDs="SYMBOL",
                     keepMultipleMappings=TRUE, duplicateRemoverMethod="max",
                     orderAbsValue=FALSE)
gsca

## ---- message = FALSE, warning=FALSE-------------------------------------
gsca <- analyze(gsca, para=list(pValueCutoff=0.05, pAdjustMethod="BH", 
                                nPermutations=100, minGeneSetSize=180, 
                                exponent=1), doGSOA = T, doGSEA = T)
gsca

## ------------------------------------------------------------------------
doParallel::registerDoParallel(cores=10)

## ------------------------------------------------------------------------
head(gsca@result$HyperGeo.results$GO_MF, 3)
head(gsca@result$GSEA.results$PW_KEGG, 3)
head(gsca@result$Sig.pvals.in.both$MSig_C2, 3)
head(gsca@result$Sig.adj.pvals.in.both$MSig_C2, 3)

## ---- message = FALSE, warning=FALSE-------------------------------------
gsca <- appendGSTerms(gsca, goGSCs=c("GO_MF"), keggGSCs=c("PW_KEGG"), msigdbGSCs = c("MSig_C2"))
head(gsca@result$GSEA.results$PW_KEGG, 3)

## ------------------------------------------------------------------------
HTSanalyzeR2::summarize(gsca)

## ---- fig.width=6, fig.height=4------------------------------------------
topGS_GO_BP <- getTopGeneSets(gsca, resultName="GSEA.results", 
                              gscs=c("GO_MF", "PW_KEGG"), allSig=TRUE)
topGS_GO_BP
viewGSEA(gsca, "PW_KEGG", topGS_GO_BP[["PW_KEGG"]][2])  ## A bug! 

## ---- eval=FALSE---------------------------------------------------------
#  plotGSEA(gsca, gscs=c("GO_MF", "PW_KEGG"), ntop=3, filepath=".")

## ---- eval=FALSE---------------------------------------------------------
#  viewEnrichMap(gsca, gscs=c("PW_KEGG", "GO_MF"), allSig = F, gsNameType = "term", ntop = 5)

## ------------------------------------------------------------------------
pvalues <- as.vector(d7$neg.p.value)
names(pvalues) <- d7$id
nwa <- new("NWA", pvalues=pvalues, phenotypes=phenotype)

## ------------------------------------------------------------------------
nwa <- preprocess(nwa, species="Hs", initialIDs="SYMBOL", keepMultipleMappings=TRUE, duplicateRemoverMethod="max")

## ------------------------------------------------------------------------
nwa <- interactome(nwa, species="Hs", genetic=FALSE)
nwa@interactome

## ------------------------------------------------------------------------
nwa <- analyze(nwa, fdr=0.0001, species="Hs")

## ---- eval=FALSE---------------------------------------------------------
#  viewSubNet(nwa)

## ------------------------------------------------------------------------
HTSanalyzeR2::summarize(nwa)

## ------------------------------------------------------------------------
data(d7, d13, d25)
expInfor <- matrix(c("d7", "d13", "d25"), nrow = 3, ncol = 2, byrow = F, dimnames = list(NULL, c("ID", "Description")))
datalist <- list(d7, d13, d25)
phenotypeTS <- lapply(datalist, function(x) {
  tmp <- as.vector(x$neg.lfc)
  names(tmp) <- x$id
  tmp
})
ListGSC <- list(GO_MF=GO_MF)

## ------------------------------------------------------------------------
hitsTS <- lapply(datalist, function(x){
  tmp <- x[x$neg.p.value < 0.01, "id"]
  tmp
})

## ------------------------------------------------------------------------
GSCABatch <- new("GSCABatch", expInfor = expInfor, phenotypeTS = phenotypeTS, listOfGeneSetCollections = ListGSC, hitsTS = hitsTS)

## ------------------------------------------------------------------------
gscaTS <- preprocessGscaTS(GSCABatch, species="Hs", initialIDs="SYMBOL",
                         keepMultipleMappings=TRUE, duplicateRemoverMethod="max",
                         orderAbsValue=FALSE)

## ------------------------------------------------------------------------
doParallel::registerDoParallel(cores=40)
gscaTS <- analyzeGscaTS(gscaTS, para=list(pValueCutoff=0.05, pAdjustMethod="BH", 
                                nPermutations=100, minGeneSetSize=180, 
                                exponent=1), doGSOA = TRUE, doGSEA = TRUE)
head(gscaTS[[1]]@result$GSEA.results$GO_MF, 3)

## ---- eval=FALSE---------------------------------------------------------
#  pvalueTS <- lapply(datalist, function(x){
#    tmp <- as.vector(x$neg.p.value)
#    names(tmp) <- x$id
#    tmp
#  })
#  NWABatch <- new("NWABatch", expInfor = expInfor, pvalueTS = pvalueTS, phenotypeTS = phenotypeTS)

## ---- eval=FALSE---------------------------------------------------------
#  nwaTS <- preprocessNwaTS(NWABatch, species="Hs", initialIDs="SYMBOL", keepMultipleMappings=TRUE, duplicateRemoverMethod="max")

## ---- eval=FALSE---------------------------------------------------------
#  nwaTS <- interactomeNwaTS(nwaTS, species="Hs", reportDir="HTSanalyzerReport", genetic=FALSE)
#  nwaTS <- analyzeNwaTS(nwaTS, fdr=0.0001, species="Hs")
#  HTSanalyzeR2::summarize(nwaTS[[1]])
#  viewSubNet(nwaTS[[1]])

## ---- eval = FALSE-------------------------------------------------------
#  report(gsca)
#  report(nwa)
#  reportAll(gsca, nwa)
#  reportAll(gscaTS)
#  reportAll(nwa = nwaTS2)

## ---- eval=FALSE---------------------------------------------------------
#  ListGSC = list(GO_MF=GO_MF, PW_KEGG=PW_KEGG)
#  HTSanalyzeR4MAGeCK(file = d7,selectDirection = "negative",
#                               doGSOA = FALSE,
#                               doGSEA = TRUE,
#                               hitsCutoffLogFC = 1,
#                               hitsCutoffPval = NULL,
#                               listOfGeneSetCollections = ListGSC,
#                               species = "Hs",
#                               initialIDs = "SYMBOL",
#                               keepMultipleMappings = TRUE,
#                               duplicateRemoverMethod = "max",
#                               orderAbsValue = FALSE,
#                               pValueCutoff = 0.05,
#                               pAdjustMethod = "BH",
#                               nPermutations = 100,
#                               minGeneSetSize = 180,
#                               exponent = 1,
#                               verbose  = TRUE,
#                               keggGSCs=c("PW_KEGG"),
#                               goGSCs = c("GO_MF"),
#                               msigdbGSCs = NULL,
#                               interactionMatrix = NULL,
#                               reportDir = "HTSanalyzerReport",
#                               nwAnalysisGenetic = FALSE,
#                               nwAnalysisFdr = 0.001)
#  

