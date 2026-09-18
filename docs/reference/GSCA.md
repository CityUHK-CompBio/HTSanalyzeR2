# An S4 class for Gene Set Collection Analyses on high-throughput data

This S4 class includes a series of methods to do gene set enrichment
analysis and hypergeometric test for high-throughput data.

## Usage

``` r
GSCA(listOfGeneSetCollections, geneList, hits = character())
```

## Arguments

- listOfGeneSetCollections:

  A list of gene set collections (a 'gene set collection' is a list of
  gene sets).

- geneList:

  A numeric or integer vector of phenotypes named by gene identifiers.

- hits:

  A character vector of the gene identifiers (used as hits in the
  hypergeometric tests).It's needed if you want to do GSOA (gene set
  overrepresentation analysis).

## Value

This function will create a new object of 'GSCA' class.

## Slots

- `para`:

  A list of parameters for hypergeometric test and GSEA. These
  parameters are pValueCutoff, pAdjustMethod, nPermutations,
  minGeneSetSize and exponent.

- `result`:

  A list of results.

- `summary`:

  A list of summary information for listOfGeneSetCollections, geneList,
  hits, para, and result.

- `preprocessed`:

  A single logical value specifying whether or not the input data has
  been preprocessed.

## See also

[`preprocess`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocess.md),
[`analyze`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/analyze.md),
[`appendGSTerms`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/appendGSTerms.md),
[`summarize`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/summarize.md),
[`report`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/report.md)

## Examples

``` r
library(org.Hs.eg.db)
library(GO.db)
## load data for enrichment analyses
data(d7)
phenotype <- as.vector(d7$neg.lfc)
names(phenotype) <- d7$id

## select hits if you also want to do GSOA, otherwise ignore it
hits <- names(phenotype[which(abs(phenotype) > 2)])

## set up a list of gene set collections
GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
ListGSC <- list(GO_MF=GO_MF)

## Example1: create an object of class 'GSCA' with hits
gsca <- GSCA(listOfGeneSetCollections = ListGSC, geneList = phenotype, hits = hits)
getSummary(gsca)
#> $gsc
#>       input above min size
#> GO_MF  4777             NA
#> 
#> $gl
#>           input valid duplicate removed converted to entrez
#> Gene List  8000    NA                NA                  NA
#> 
#> $hits
#>      input preprocessed
#> Hits   247           NA
#> 
#> $para
#> $para$hypergeo
#>               minGeneSetSize pValueCutoff pAdjustMethod
#> HyperGeo Test             NA           NA            NA
#> 
#> $para$gsea
#>      minGeneSetSize pValueCutoff pAdjustMethod nPermutations exponent
#> GSEA             NA           NA            NA            NA       NA
#> 
#> 
#> $results
#>          GO_MF
#> HyperGeo    NA
#> GSEA        NA
#> Both        NA
#> 

## Example2: create an object of class 'GSCA' without hits
gsca <- GSCA(listOfGeneSetCollections = ListGSC, geneList = phenotype)
getSummary(gsca)
#> $gsc
#>       input above min size
#> GO_MF  4777             NA
#> 
#> $gl
#>           input valid duplicate removed converted to entrez
#> Gene List  8000    NA                NA                  NA
#> 
#> $hits
#>      input preprocessed
#> Hits     0           NA
#> 
#> $para
#> $para$hypergeo
#>               minGeneSetSize pValueCutoff pAdjustMethod
#> HyperGeo Test             NA           NA            NA
#> 
#> $para$gsea
#>      minGeneSetSize pValueCutoff pAdjustMethod nPermutations exponent
#> GSEA             NA           NA            NA            NA       NA
#> 
#> 
#> $results
#>          GO_MF
#> HyperGeo    NA
#> GSEA        NA
#> Both        NA
#> 
```
