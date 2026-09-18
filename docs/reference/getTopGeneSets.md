# Select top significant gene sets from results of GSCA object

This is a generic function. This function selects top significant gene
sets from results of an GSCA object for user-specified gene collections.
If 'ntop' is given, then top 'ntop' significant gene sets in gene set
collections 'gscs' will be selected and their names will be returned. If
'allSig=TRUE', then all significant (adjusted p-value \< 'pValueCutoff'
see help("analyze")) gene sets will be selected and their names will be
returned.

## Usage

``` r
# S4 method for class 'GSCA'
getTopGeneSets(object, resultName, gscs, ntop = NULL, allSig = FALSE)
```

## Arguments

- object:

  A GSCA object.

- resultName:

  A single character value: 'HyperGeo.results' or 'GSEA.results'.

- gscs:

  A character vector specifying the names of gene set collections from
  which the top significant gene sets will be selected.

- ntop:

  A single integer or numeric value specifying to select how many gene
  sets of top significance.

- allSig:

  A single logical value. If 'TRUE', all significant gene sets (adjusted
  p-value \< 'pValueCutoff' of slot 'para') will be selected regardless
  of 'ntop'; otherwise, only top 'ntop' gene sets will be selected.

## Value

A named list of character vectors, each element contains the names of
top significant gene sets for each gene set collection.

## Examples

``` r
## load a GSCA object(see the examples of analyze GSCA for details)
data(d7_gsca)

summarize(d7_gsca)
#> 
#> -No of genes in Gene set collections: 
#>         input above min size
#> GO_MF    4110            348
#> PW_KEGG   322            283
#> 
#> 
#> -No of genes in Gene List: 
#>           input valid duplicate removed converted to entrez
#> Gene List  8000  8000              8000                7734
#> 
#> 
#> -No of hits: 
#>      input preprocessed
#> Hits   247          245
#> 
#> 
#> -Parameters for analysis: 
#>               minGeneSetSize pValueCutoff pAdjustMethod
#> HyperGeo Test 10             0.01         BH           
#> 
#>      minGeneSetSize pValueCutoff pAdjustMethod nPermutations exponent
#> GSEA 10             0.01         BH            100           1       
#> 
#> 
#> -Significant gene sets (adjusted p-value< 0.01 ): 
#>          GO_MF PW_KEGG
#> HyperGeo    10      12
#> GSEA        46      31
#> Both        10      12
#> 
## print top significant gene sets in GO_MF
topGS_GO_MF <- getTopGeneSets(d7_gsca, "GSEA.results", gscs = "GO_MF", allSig=TRUE)

## print top significant gene sets in GO_MF and PW_KEGG
topGS <- getTopGeneSets(d7_gsca, "GSEA.results", gscs = c("GO_MF", "PW_KEGG"), allSig=TRUE)
```
