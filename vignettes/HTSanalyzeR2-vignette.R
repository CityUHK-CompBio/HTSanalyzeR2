## ----setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = TRUE)

## ---- results='hide', message=FALSE----------------------------------------
library(HTSanalyzeR2)
library(org.Hs.eg.db)
library(KEGGREST)
library(GO.db)
library(igraph)
library(Homo.sapiens)

## --------------------------------------------------------------------------
data(GSE33113.limma)
phenotype <- as.vector(GSE33113.limma$logFC)
names(phenotype) <- rownames(GSE33113.limma)

## --------------------------------------------------------------------------
## define hits if you want to do hypergeometric test
hits <-  names(phenotype[which(abs(phenotype) > 1)])

## --------------------------------------------------------------------------
GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
PW_KEGG <- KeggGeneSets(species="Hs")
MSig_C2 <- MSigDBGeneSets(collection = "c2")
ListGSC <- list(GO_MF=GO_MF, PW_KEGG=PW_KEGG, MSig_C2=MSig_C2)

## --------------------------------------------------------------------------
gsca <- new("GSCA", listOfGeneSetCollections=ListGSC, geneList=phenotype, hits=hits)

## ---- results='hide', message=FALSE----------------------------------------
gsca1 <- preprocess(gsca, species="Hs", initialIDs="SYMBOL",
                    keepMultipleMappings=TRUE, duplicateRemoverMethod="max",
                    orderAbsValue=FALSE)

## ---- results='hide', eval=FALSE-------------------------------------------
#  gsca2 <- analyze(gsca1, para=list(pValueCutoff=0.05, pAdjustMethod="BH",
#                                    nPermutations=100, minGeneSetSize=180,
#                                    exponent=1), doGSOA = TRUE, doGSEA = TRUE)

## ---- results='hide', message=FALSE----------------------------------------
## analyze using multiple cores
doParallel::registerDoParallel(cores=2)
gsca2 <- analyze(gsca1, para=list(pValueCutoff=0.05, pAdjustMethod="BH",
                                  nPermutations=100, minGeneSetSize=180,
                                  exponent=1), doGSOA = TRUE, doGSEA = TRUE)

## --------------------------------------------------------------------------
head(gsca2@result$HyperGeo.results$GO_MF, 3)
head(gsca2@result$GSEA.results$PW_KEGG, 3)
head(gsca2@result$Sig.pvals.in.both$MSig_C2, 3)
head(gsca2@result$Sig.adj.pvals.in.both$MSig_C2, 3)

## ---- message=FALSE--------------------------------------------------------
gsca3 <- appendGSTerms(gsca2, goGSCs=c("GO_MF"),
                       keggGSCs=c("PW_KEGG"), msigdbGSCs = c("MSig_C2"))
head(gsca3@result$GSEA.results$PW_KEGG, 3)

## --------------------------------------------------------------------------
HTSanalyzeR2::summarize(gsca3)

## ---- fig.height=4, fig.width=7--------------------------------------------
topGS <- getTopGeneSets(gsca3, resultName="GSEA.results",
                        gscs=c("GO_MF", "PW_KEGG"), allSig=TRUE)
topGS
viewGSEA(gsca3, gscName="GO_MF", gsName=topGS[["GO_MF"]][1])

## ---- eval=FALSE-----------------------------------------------------------
#  plotGSEA(gsca3, gscs=c("GO_MF", "PW_KEGG"), ntop=3, filepath=".")

## ---- fig.height=4, fig.width=6--------------------------------------------
## the enrichment map for top 5 significant gene sets in 'PW_KEGG' and 'GO_MF'
viewEnrichMap(gsca3, gscs=c("PW_KEGG", "GO_MF"),
              allSig = FALSE, gsNameType = "term", ntop = 5)

## ---- warning=FALSE, fig.height=3.5, fig.width=6---------------------------
## specificGeneset needs to be a subset of all analyzed gene sets
## which can be roughly gotten by:
tmp <- getTopGeneSets(gsca3, resultName = "GSEA.results", gscs=c("PW_KEGG"),
                      ntop = 20000, allSig = FALSE)
## In that case, we can define specificGeneset as below:
PW_KEGG_geneset <- tmp$PW_KEGG[c(2, 3, 9, 11, 12, 13)]
## the name of specificGenesets also needs to match with the names of tmp
specificGeneset <- list("PW_KEGG"=PW_KEGG_geneset)
viewEnrichMap(gsca3, resultName = "GSEA.results", gscs=c("PW_KEGG"), allSig = FALSE, gsNameType = "term",
              ntop = NULL, specificGeneset = specificGeneset)

## --------------------------------------------------------------------------
pvalues <- GSE33113.limma$adj.P.Val
names(pvalues) <- rownames(GSE33113.limma)
nwa <- new("NWA", pvalues=pvalues, phenotypes=phenotype)

## ---- results='hide', message=FALSE----------------------------------------
nwa1 <- preprocess(nwa, species="Hs", initialIDs="SYMBOL",
                   keepMultipleMappings=TRUE, duplicateRemoverMethod="max")

## ---- message=FALSE, results='hide'----------------------------------------
nwa2 <- interactome(nwa1, species="Hs", genetic=FALSE)

## --------------------------------------------------------------------------
nwa2@interactome

## ---- results='hide', message=FALSE, warning=FALSE-------------------------
nwa3 <- analyze(nwa2, fdr=1e-06, species="Hs")

## --------------------------------------------------------------------------
viewSubNet(nwa3)

## --------------------------------------------------------------------------
HTSanalyzeR2::summarize(nwa3)

## --------------------------------------------------------------------------
data(d7, d13, d25)
expInfor <- matrix(c("d7", "d13", "d25"), nrow = 3, ncol = 2,
                   byrow = FALSE, dimnames = list(NULL, c("ID", "Description")))
datalist <- list(d7, d13, d25)
phenotypeTS <- lapply(datalist, function(x) {
  tmp <- as.vector(x$neg.lfc)
  names(tmp) <- x$id
  tmp
})

GO_BP <- GOGeneSets(species="Hs", ontologies=c("BP"))
ListGSC <- list(GO_BP=GO_BP)

## --------------------------------------------------------------------------
hitsTS <- lapply(datalist, function(x){
  tmp <- x[x$neg.p.value < 0.01, "id"]
  tmp
})

## --------------------------------------------------------------------------
gscaTS <- new("GSCABatch", expInfor = expInfor,
              phenotypeTS = phenotypeTS, listOfGeneSetCollections = ListGSC,
              hitsTS = hitsTS)

## ---- results='hide', message=FALSE----------------------------------------
gscaTS1 <- preprocessGscaTS(gscaTS, species="Hs", initialIDs="SYMBOL",
                            keepMultipleMappings=TRUE, duplicateRemoverMethod="max",
                            orderAbsValue=FALSE)

## ---- results='hide', message=FALSE----------------------------------------
doParallel::registerDoParallel(cores=2)
gscaTS2 <- analyzeGscaTS(gscaTS1, para=list(pValueCutoff=0.05, pAdjustMethod="BH",
                                            nPermutations=100, minGeneSetSize=180,
                                            exponent=1), doGSOA = TRUE, doGSEA = TRUE)
head(gscaTS2[[1]]@result$GSEA.results$GO_BP, 3)

## ---- message=FALSE--------------------------------------------------------
gscaTS3 <- appendGSTermsTS(gscaTS2, goGSCs=c("GO_BP"))
head(gscaTS3[[1]]@result$GSEA.results$GO_BP, 3)

## --------------------------------------------------------------------------
pvalueTS <- lapply(datalist, function(x){
  tmp <- as.vector(x$neg.p.value)
  names(tmp) <- x$id
  tmp
})
nwaTS <- new("NWABatch", expInfor = expInfor,
             pvalueTS = pvalueTS, phenotypeTS = phenotypeTS)

## ---- results='hide', message=FALSE, eval=FALSE----------------------------
#  nwaTS1 <- preprocessNwaTS(nwaTS, species="Hs", initialIDs="SYMBOL",
#                            keepMultipleMappings=TRUE, duplicateRemoverMethod="max")

## ---- results='hide', message=FALSE, eval=FALSE----------------------------
#  nwaTS2 <- interactomeNwaTS(nwaTS1, species="Hs", reportDir="HTSanalyzerReport", genetic=FALSE)
#  nwaTS3 <- analyzeNwaTS(nwaTS2, fdr=0.0001, species="Hs")

## ---- eval=FALSE-----------------------------------------------------------
#  HTSanalyzeR2::summarize(nwaTS3[[1]])

## ---- eval = FALSE---------------------------------------------------------
#  report(gsca3)
#  report(nwa3)
#  reportAll(gsca3)
#  reportAll(gsca3, nwa3)

## ---- eval = FALSE---------------------------------------------------------
#  reportAll(gscaTS3)
#  reportAll(nwa = nwaTS3)
#  reportAll(gscaTS3, TSOrder = names(gscaTS3)[c(3, 1, 2)])

## ---- eval = FALSE---------------------------------------------------------
#  ## As told previously, specificGeneset needs to be a subset of all analyzed gene sets
#  ## which can be roughly gotten by:
#  tmp <- getTopGeneSets(gscaTS3[[1]], resultName = "GSEA.results",
#                        gscs=c("GO_BP"), ntop = 20000, allSig = FALSE)
#  ## In that case, we can define specificGeneset as below:
#  GO_BP_geneset <- tmp$GO_BP[c(4,2,6,9,12)]
#  ## the name of specificGenesets also needs to match with the names of tmp
#  specificGeneset <- list("GO_BP"=GO_BP_geneset)
#  reportAll(gscaTS3, specificGeneset=specificGeneset)
#  

## ---- eval=FALSE-----------------------------------------------------------
#  data(d7)
#  hits <- d7$id[1:200]
#  ## set all the genes of Homo sapiens as phenotype
#  allgenes <- keys(Homo.sapiens, keytype = "SYMBOL")
#  ## give phenotype a pseudo value to fit HTSanalyzeR2
#  phenotype <- rep(1, length(allgenes))
#  names(phenotype) <- allgenes

## ---- eval=FALSE-----------------------------------------------------------
#  gsca <- new("GSCA", listOfGeneSetCollections=ListGSC, geneList=phenotype, hits=hits)
#  ## the following analysis is the same as before

## ---- eval=FALSE-----------------------------------------------------------
#  ## Suppose your own gene sets is geneset1 and geneset2
#  allgenes <- keys(Homo.sapiens, "ENTREZID")
#  geneset1 <- allgenes[sample(length(allgenes), 100)]
#  geneset2 <- allgenes[sample(length(allgenes), 60)]
#  ## Set your custom gene set collection and make the format to fit HTSanalyzeR2
#  CustomGS <- list("geneset1" = geneset1, "geneset2" = geneset2)
#  ## then the gene set collections would be as below:
#  ListGSC <- list(CustomGS=CustomGS)
#  ## other part is the same as before

## ---- eval=FALSE-----------------------------------------------------------
#  ListGSC = list(GO_MF=GO_MF, PW_KEGG=PW_KEGG)
#  HTSanalyzeR4MAGeCK(MAGeCKdata = d7,selectDirection = "negative",
#                               doGSOA = FALSE,
#                               doGSEA = TRUE,
#                               listOfGeneSetCollections = ListGSC,
#                               species = "Hs",
#                               initialIDs = "SYMBOL",
#                               pValueCutoff = 0.05,
#                               pAdjustMethod = "BH",
#                               nPermutations = 100,
#                               minGeneSetSize = 180,
#                               exponent = 1,
#                               keggGSCs=c("PW_KEGG"),
#                               goGSCs = c("GO_MF"),
#                               msigdbGSCs = NULL,
#                               reportDir = "HTSanalyzerReport",
#                               nwAnalysisGenetic = FALSE,
#                               nwAnalysisFdr = 0.001)
#  

## ---- echo=FALSE-----------------------------------------------------------
sessionInfo()

