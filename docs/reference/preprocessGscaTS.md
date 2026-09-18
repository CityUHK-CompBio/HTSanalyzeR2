# A preprocessing method for a GSCABatch object of Time-series data Gene Set Collection Analyses

This function will do basic preprocessing for each GSCA object of
'GSCABatch'.

## Usage

``` r
preprocessGscaTS(
  object,
  species = "Hs",
  initialIDs = "SYMBOL",
  keepMultipleMappings = TRUE,
  duplicateRemoverMethod = "max",
  orderAbsValue = FALSE,
  verbose = TRUE
)
```

## Arguments

- object:

  A GSCABatch object.

- species:

  A single character value specifying the species of the inputs. It
  supports all the species of OrgDb objects in AnnotationDbi. The format
  should be an abbreviation of the organism as setted by AnnotationDbi.
  For example, the commonly used ones are "Dm"
  ("Drosophila_melanogaster"), "Hs" ("Homo_sapiens"), "Rn"
  ("Rattus_norvegicus"), "Mm" ("Mus_musculus"), "Ce"
  ("Caenorhabditis_elegans"), and etc.

- initialIDs:

  A single character value specifying the type of initial identifiers
  for input phenotypeTS The valid terms need match with the keytypes of
  species db such as keytypes(org.Hs.eg.db).

- keepMultipleMappings:

  A single logical value. If TRUE, the function keeps the entries with
  multiple mappings (first mapping is kept). If FALSE, the entries with
  multiple mappings will be discarded.

- duplicateRemoverMethod:

  A single character value specifying the method to remove the
  duplicates. See help(duplicateRemover) for details.

- orderAbsValue:

  A single logical value indicating whether the values should be
  converted to absolute values and then ordered (if TRUE), or ordered as
  they are (if FALSE).

- verbose:

  A single logical value specifying to display detailed messages (when
  verbose=TRUE) or not (when verbose=FALSE).

## Value

In the end, this function will return an updated list of GSCA object.

## See also

[`preprocess`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocess.md)

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
gscaTS1
#> $d7
#> A GSCA (Gene Set Collection Analysis) object:
#> 
#> -No of genes in Gene set collections: 
#>       input above min size
#> GO_BP 11293             NA
#> 
#> 
#> -No of genes in Gene List: 
#>           input valid duplicate removed converted to entrez
#> Gene List  8000  8000              8000                7398
#> 
#> 
#> -No of hits: 
#>      input preprocessed
#> Hits   516          481
#> 
#> 
#> -Parameters for analysis: 
#>               minGeneSetSize pValueCutoff pAdjustMethod
#> HyperGeo Test             NA           NA            NA
#> 
#>      minGeneSetSize pValueCutoff pAdjustMethod nPermutations exponent
#> GSEA             NA           NA            NA            NA       NA
#> 
#> 
#> $d13
#> A GSCA (Gene Set Collection Analysis) object:
#> 
#> -No of genes in Gene set collections: 
#>       input above min size
#> GO_BP 11293             NA
#> 
#> 
#> -No of genes in Gene List: 
#>           input valid duplicate removed converted to entrez
#> Gene List  8000  8000              8000                7398
#> 
#> 
#> -No of hits: 
#>      input preprocessed
#> Hits   636          589
#> 
#> 
#> -Parameters for analysis: 
#>               minGeneSetSize pValueCutoff pAdjustMethod
#> HyperGeo Test             NA           NA            NA
#> 
#>      minGeneSetSize pValueCutoff pAdjustMethod nPermutations exponent
#> GSEA             NA           NA            NA            NA       NA
#> 
#> 
#> $d25
#> A GSCA (Gene Set Collection Analysis) object:
#> 
#> -No of genes in Gene set collections: 
#>       input above min size
#> GO_BP 11293             NA
#> 
#> 
#> -No of genes in Gene List: 
#>           input valid duplicate removed converted to entrez
#> Gene List  8000  8000              8000                7398
#> 
#> 
#> -No of hits: 
#>      input preprocessed
#> Hits   862          802
#> 
#> 
#> -Parameters for analysis: 
#>               minGeneSetSize pValueCutoff pAdjustMethod
#> HyperGeo Test             NA           NA            NA
#> 
#>      minGeneSetSize pValueCutoff pAdjustMethod nPermutations exponent
#> GSEA             NA           NA            NA            NA       NA
#> 
#> 
```
