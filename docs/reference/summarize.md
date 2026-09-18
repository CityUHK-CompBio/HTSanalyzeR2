# Print summary information for an object of class GSCA or NWA

This is a generic function. When implemented as the S4 method for
objects of class GSCA or NWA, this function prints a summary of
information about the slots of these classes.

## Usage

``` r
# S4 method for class 'GSCA'
summarize(object, what = "ALL")

# S4 method for class 'NWA'
summarize(object, what = "ALL")
```

## Arguments

- object:

  A GSCA object or NWA object.

- what:

  A single character value or a character vector of key words specifying
  what to print (see Methods below). Default will print a summary of all
  information.

## Value

In the end, this function would return a summary of the NWA object.

In the end, this function would return a summary of the NWA object.

## Methods (by class)

- `summarize(GSCA)`: For an object of class GSCA, the key words are
  'GSC' (the slot 'listOfGeneSetCollections'), 'GeneList' (the slot
  'geneList'), 'Hits' (the slot 'hits'), 'Para' (the slot 'para'),
  'Result' (the slot 'result') and 'ALL' (all slots).

- `summarize(NWA)`: For an object of class NWA, the key words include
  'Pval' (the slot 'pvalues'), 'Phenotype' (the slot 'phenotypes'),
  'Interactome' (the slot 'interactome'), 'Para' (the slot 'fdr'),
  'Result' (the slot 'result') and 'ALL' (all slots).

## Examples

``` r
# =================================================================
# GSCA class
## load a GSCA object(see the examples of analyze GSCA for details)
data(d7_gsca)

## summarize d7_gsca
summarize(d7_gsca, what = "ALL")
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
summarize(d7_gsca, what = "Result")
#> 
#> -Significant gene sets (adjusted p-value< 0.01 ): 
#>          GO_MF PW_KEGG
#> HyperGeo    10      12
#> GSEA        46      31
#> Both        10      12
#> 
if (FALSE) { # \dontrun{
# =================================================================
# NWA class
## load a NWA object(see the examples of analyze NWA for details)
data(d7_nwa)

## summarize d7_nwa
summarize(d7_nwa, what = "ALL")
summarize(d7_nwa, what = "Result")
} # }
```
