# Preprocessing HTS
# ==================================================================

library(cellHTS2)

experimentName <- "KcViab"
dataPath <- system.file(experimentName, package = "cellHTS2")
x <- readPlateList("Platelist.txt", name = experimentName, path = dataPath,verbose=TRUE)
x <- configure(x, descripFile = "Description.txt", confFile = "Plateconf.txt", logFile = "Screenlog.txt", path = dataPath)

xn <- normalizePlates(x, scale = "multiplicative", log = FALSE, method = "median", varianceAdjust = "none")
xn <- annotate(xn, geneIDFile = "GeneIDs_Dm_HFA_1.1.txt", path = dataPath)

xsc <- scoreReplicates(xn, sign = "-", method = "zscore")
xsc <- summarizeReplicates(xsc, summary = "mean")



# GSEA Analysis
# ==================================================================

data4enrich <- as.vector(Data(xsc))
names(data4enrich) <- fData(xsc)[, "GeneID"]
data4enrich <- data4enrich[!is.na(names(data4enrich))]
hits <- names(data4enrich)[abs(data4enrich) > 2]

# ------------------------------------------------------------------
library(HTSanalyzeR2)
library(org.Dm.eg.db)
library(GO.db)

GO_MF <- GOGeneSets(species="Dm", ontologies=c("MF"))
PW_KEGG <- KeggGeneSets(species="Dm")
H_MSig <- MSigDBGeneSets(collection = "h")
ListGSC <- list(GO_MF=GO_MF, H_MSig = H_MSig, PW_KEGG=PW_KEGG)

# ------------------------------------------------------------------
gsca <- GSCA(listOfGeneSetCollections = ListGSC, geneList = data4enrich, hits = hits)
gsca <- preprocess(gsca, species="Dm", initialIDs="FLYBASECG", keepMultipleMappings=TRUE, duplicateRemoverMethod="max", orderAbsValue=FALSE)

# ------------------------------------------------------------------
gsca <- analyze(gsca, para=list(pValueCutoff=0.05, pAdjustMethod ="BH", nPermutations=100, minGeneSetSize=100, exponent=1))

# ------------------------------------------------------------------
gsca <- appendGSTerms(gsca, goGSCs=c("GO_MF"), keggGSCs=c("PW_KEGG"), msigdbGSCs=c("H_MSig"))
summarize(gsca)
topGS_GO_MF <- getTopGeneSets(gsca, "GSEA.results", c("GO_MF", "PW_KEGG"), allSig=TRUE)




# Network analysis
# ==================================================================
library(BioNet)
test.stats <- cellHTS2OutputStatTests(cellHTSobject=xn, annotationColumn="GeneID", alternative="two.sided", tests=c("T-test"))
pvalues <- BioNet::aggrPvals(test.stats, order=2, plot=FALSE)


# ------------------------------------------------------------------
nwa <- NWA(pvalues=pvalues, phenotypes=data4enrich)
nwa <- preprocess(nwa, species="Dm", initialIDs="FLYBASECG", keepMultipleMappings=TRUE, duplicateRemoverMethod="max")
nwa <- interactome(nwa, species="Dm", reportDir="biogrid", genetic=FALSE)

# or
# nwa<-interactome(nwa, interactionMatrix = InteractionsData, species="Dm", reportDir="Report", genetic=FALSE)


# ------------------------------------------------------------------
nwa <- analyze(nwa, fdr=0.0001, species="Dm")
summarize(nwa)

nwa <- appendSequence(nwa)


# Report
# ==================================================================
# subnetwork <- extractSubNet(nwa)  #TODO maybe need load igraph library first

report(gsca)
report(nwa)
reportAll(gsca, nwa)



