# Plot a figure of GSEA results for one gene set

This is a generic function. When implemented as the S4 method for
objects of class GSCA, this function plots a figure of the positions of
the gene sets in the ranked gene list and the location of the enrichment
score.

## Usage

``` r
# S4 method for class 'GSCA'
viewGSEA(
  object,
  gscName,
  gsName,
  ES.range = NULL,
  rankMetric.range = NULL,
  main.title = NULL,
  pheno.title = c("Postive", "Negative"),
  ESline.col = "FireBrick",
  hits.col = "black",
  rankMetric.col = "CadetBlue"
)
```

## Arguments

- object:

  A GSCA object.

- gscName:

  A single character value specifying the name of the gene set
  collection where the gene set is.

- gsName:

  A single character value specifying the name of the gene set to be
  plotted.

- ES.range:

  A numeric vector for user-defined range of enrichment score showing in
  the top part of GSEA plot. Default is the range of actual enrichment
  score of result. However, users are also allowed to set by themselves
  for better comparison with other GSEA plots.

- rankMetric.range:

  A numeric vector for user-defined range of ranked gene lists showing
  in the bottom part of GSEA plot. Default is the range of actual ranked
  gene lists of result. However, users are also allowed to set by
  themselves for better comparison with other GSEA plots.

- main.title:

  A single character value specifying the main title of the GSEA plot.
  Default is the name of the gene set.

- pheno.title:

  A charater vector for user-defined phenotype names. Default is
  c("Positive", "Negative").

- ESline.col:

  The color to be used for enrichment score profile line. Defaults to
  "FireBrick".

- hits.col:

  The color to be used for hits line to show the position of hits.
  Defaults to "CadetBlue".

- rankMetric.col:

  The color to be used for ranked metric line. Defaults to "CadetBlue".

## Value

In the end, this function would draw a GSEA figure for specified gene
set.

## Details

We suggest to print the names of top significant gene sets using the
function
[`getTopGeneSets`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/getTopGeneSets.md)
before plotting the GSEA results.

## Examples

``` r
## load a GSCA object(see the examples of analyze GSCA for details)
data(d7_gsca)

## summarize gsca
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

## view GSEA results for one gene set
if (FALSE) { # \dontrun{
viewGSEA(d7_gsca, "GO_MF", topGS_GO_MF[["GO_MF"]][1])

## view GSEA results for the same gene set by user defined enrichment score range,
## ranked gene list range and enrichment score profile line color.
viewGSEA(d7_gsca, "GO_MF", topGS_GO_MF[["GO_MF"]][1],
ES.range = c(-1, 0), rankMetric.range = c(-5, 2),
ESline.col = "green")
} # }
```
