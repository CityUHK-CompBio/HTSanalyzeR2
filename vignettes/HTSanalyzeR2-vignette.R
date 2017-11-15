## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- results='hide', message=FALSE--------------------------------------
library(HTSanalyzeR2)
library(org.Hs.eg.db)
library(KEGGREST)
library(GO.db)
library(igraph)

## ------------------------------------------------------------------------
data(d7)
phenotype <- as.vector(d7$neg.lfc)
names(phenotype) <- d7$id

## ------------------------------------------------------------------------
hits <-  names(phenotype[which(abs(phenotype) > 2)])

## ------------------------------------------------------------------------
GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
PW_KEGG <- KeggGeneSets(species="Hs")
MSig_C2 <- MSigDBGeneSets(collection = "c2")
ListGSC <- list(GO_MF=GO_MF, PW_KEGG=PW_KEGG, MSig_C2=MSig_C2)

## ------------------------------------------------------------------------
gsca <- new("GSCA", listOfGeneSetCollections=ListGSC, geneList=phenotype, hits=hits)

## ---- results='hide', message=FALSE--------------------------------------
gsca1 <- preprocess(gsca, species="Hs", initialIDs="SYMBOL",
                    keepMultipleMappings=TRUE, duplicateRemoverMethod="max",
                    orderAbsValue=FALSE)

## ---- results='hide', eval=FALSE-----------------------------------------
#  gsca2 <- analyze(gsca1, para=list(pValueCutoff=0.05, pAdjustMethod="BH",
#                                    nPermutations=100, minGeneSetSize=180,
#                                    exponent=1), doGSOA = T, doGSEA = T)

## ---- results='hide', message=FALSE--------------------------------------
## analyze using multiple cores
doParallel::registerDoParallel(cores=10)
gsca2 <- analyze(gsca1, para=list(pValueCutoff=0.05, pAdjustMethod="BH", 
                                  nPermutations=100, minGeneSetSize=180, 
                                  exponent=1), doGSOA = T, doGSEA = T)

## ------------------------------------------------------------------------
head(gsca2@result$HyperGeo.results$GO_MF, 3)
head(gsca2@result$GSEA.results$PW_KEGG, 3)
head(gsca2@result$Sig.pvals.in.both$MSig_C2, 3)
head(gsca2@result$Sig.adj.pvals.in.both$MSig_C2, 3)

## ---- message=FALSE------------------------------------------------------
gsca3 <- appendGSTerms(gsca2, goGSCs=c("GO_MF"), 
                       keggGSCs=c("PW_KEGG"), msigdbGSCs = c("MSig_C2"))
head(gsca3@result$GSEA.results$PW_KEGG, 3)

## ------------------------------------------------------------------------
HTSanalyzeR2::summarize(gsca3)

## ---- fig.small = TRUE---------------------------------------------------
topGS <- getTopGeneSets(gsca3, resultName="GSEA.results", 
                        gscs=c("GO_MF", "PW_KEGG"), allSig=TRUE)
topGS
viewGSEA(gsca3, gscName="GO_MF", gsName=topGS[["GO_MF"]][2])

## ---- eval=FALSE---------------------------------------------------------
#  plotGSEA(gsca3, gscs=c("GO_MF", "PW_KEGG"), ntop=3, filepath=".")

