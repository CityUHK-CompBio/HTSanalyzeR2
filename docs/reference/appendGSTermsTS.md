# Append gene set terms to GSCA results for each GSCA object of Time-series data

For each GSCA object in 'gscaList', this function finds corresponding
annotation terms for GO, KEGG and MSigDB gene sets and inserts a column
named "Gene.Set.Term" to each data frame in the GSCA results. In the
same time, to make results more understandable, it will annotate the
gene list with EntrezID to gene symbol under specific species.

## Usage

``` r
appendGSTermsTS(
  gscaList,
  keggGSCs = NULL,
  goGSCs = NULL,
  msigdbGSCs = NULL,
  species = "Hs"
)
```

## Arguments

- gscaList:

  A named list of GSCA object.

- keggGSCs:

  A character vector of names of all KEGG gene set collections.

- goGSCs:

  A character vector of names of all GO gene set collections.

- msigdbGSCs:

  A character vector of names of all MSigDB gene set collections.

- species:

  A single character value specifying the species of the analyzed data.
  It supports all the species of OrgDb objects in AnnotationDbi. The
  format should be an abbreviation of the organism as setted by
  AnnotationDbi. For example, the commonly used ones are "Dm"
  ("Drosophila_melanogaster"), "Hs" ("Homo_sapiens"), "Rn"
  ("Rattus_norvegicus"), "Mm" ("Mus_musculus"), "Ce"
  ("Caenorhabditis_elegans"), and etc.

## Value

In the end, this function will return an updated list of GSCA object.

## See also

[`appendGSTerms`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/appendGSTerms.md)

## Examples

``` r
data(d7, d13, d25)

## generate expInfor to describe the information of time series data
expInfor <- matrix(c("d7", "d13", "d25"), nrow = 3, ncol = 2,
                   byrow = FALSE, dimnames = list(NULL, c("ID", "Description")))

## package phenotypeTS into a list of phenotypes
datalist <- list(d7, d13, d25)
phenotypeTS <- lapply(datalist, function(x) {
                      tmp <- as.vector(x$neg.lfc)
                      names(tmp) <- x$id
                      tmp})

## set up a list of gene set collections
library(org.Hs.eg.db)
library(GO.db)
GO_BP <- GOGeneSets(species="Hs", ontologies=c("BP"))
ListGSC <- list(GO_BP=GO_BP)

## package hitsTS if you also want to do GSOA, otherwise ignore it
hitsTS <- lapply(datalist, function(x){
tmp <- x[x$neg.p.value < 0.01, "id"]
tmp})

## create an object of class GSCABatch with hitsTS
gscaTS <- GSCABatch(expInfor = expInfor, phenotypeTS = phenotypeTS,
                 listOfGeneSetCollections = ListGSC, hitsTS = hitsTS)

## preprocess GSCABatch
gscaTS1 <- preprocessGscaTS(gscaTS, species="Hs", initialIDs="SYMBOL",
                           keepMultipleMappings=TRUE, duplicateRemoverMethod="max",
                           orderAbsValue=FALSE)
#> -Preprocessing for input gene list and hit list ...
#> --Removing genes without values in geneList ...
#> --Removing duplicated genes ...
#> --Converting annotations ...
#> 'select()' returned 1:many mapping between keys and columns
#> -- 602 genes (out of 8000) could not be mapped to any identifier, and were removed from the data. 
#> 'select()' returned 1:1 mapping between keys and columns
#> -- 35 genes (out of 516) could not be mapped to any identifier, and were removed from the data. 
#> --Ordering Gene List decreasingly ...
#> -Preprocessing complete!
#> 
#> -Preprocessing for input gene list and hit list ...
#> --Removing genes without values in geneList ...
#> --Removing duplicated genes ...
#> --Converting annotations ...
#> 'select()' returned 1:many mapping between keys and columns
#> -- 602 genes (out of 8000) could not be mapped to any identifier, and were removed from the data. 
#> 'select()' returned 1:1 mapping between keys and columns
#> -- 47 genes (out of 636) could not be mapped to any identifier, and were removed from the data. 
#> --Ordering Gene List decreasingly ...
#> -Preprocessing complete!
#> 
#> -Preprocessing for input gene list and hit list ...
#> --Removing genes without values in geneList ...
#> --Removing duplicated genes ...
#> --Converting annotations ...
#> 'select()' returned 1:many mapping between keys and columns
#> -- 602 genes (out of 8000) could not be mapped to any identifier, and were removed from the data. 
#> 'select()' returned 1:1 mapping between keys and columns
#> -- 60 genes (out of 862) could not be mapped to any identifier, and were removed from the data. 
#> --Ordering Gene List decreasingly ...
#> -Preprocessing complete!
#> 

## enable parallel calculation with the Bioconductor backend
BiocParallel::register(BiocParallel::SnowParam(workers = 2))

if (FALSE) { # \dontrun{
## do hypergeometric tests and GSEA
gscaTS2 <- analyzeGscaTS(gscaTS1, para=list(pValueCutoff=0.05, pAdjustMethod="BH",
                        nPermutations=100, minGeneSetSize=100,
                        exponent=1), doGSOA = TRUE, doGSEA = TRUE)
head(getResult(gscaTS2[[1]])$GSEA.results$GO_BP, 3)

## append gene set terms to results

gscaTS3 <- appendGSTermsTS(gscaTS2, goGSCs=c("GO_BP"),
                           species = "Hs")
head(getResult(gscaTS3[[1]])$GSEA.results$GO_BP, 3)
} # }
```
