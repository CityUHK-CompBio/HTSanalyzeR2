# Plot and save figures of GSEA results for top significant gene sets

This is a generic function.When implemented as the S4 method for objects
of class GSCA, this function plots figures of the positions of genes of
the gene set in the ranked gene list and the location of the enrichment
score for top significant gene sets.

## Usage

``` r
# S4 method for class 'GSCA'
plotGSEA(
  object,
  gscs,
  ntop = NULL,
  allSig = FALSE,
  filepath = ".",
  output = "pdf",
  ES.range = NULL,
  rankMetric.range = NULL,
  pheno.title = c("Postive", "Negative"),
  ESline.col = "FireBrick",
  hits.col = "black",
  rankMetric.col = "CadetBlue",
  ...
)
```

## Arguments

- object:

  A GSCA object.

- gscs:

  A character vector specifying the names of gene set collections whose
  top significant gene sets will be plotted.

- ntop:

  A single integer or numeric value specifying how many gene sets of top
  significance will be plotted.

- allSig:

  A single logical value. If 'TRUE', all significant gene sets (GSEA
  adjusted p-value \< 'pValueCutoff' of slot 'para') will be plotted;
  otherwise, only top 'ntop' gene sets will be plotted.

- filepath:

  A single character value specifying where to store GSEA figures.

- output:

  A single character value specifying the format of output image: "pdf"
  or "png".

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

- ...:

  Other arguments used by the function png or pdf such as 'width' and
  'height'

## Value

In the end, this function would plot GSEA figure and store them into the
specified path.

## Examples

``` r
## load a GSCA object(see the examples of analyze GSCA for details)
data(d7_gsca)

## summarize d7_gsca
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

## plot  significant gene sets in GO_MF and PW_KEGG
if (FALSE) { # \dontrun{
plotGSEA(d7_gsca, gscs=c("GO_MF","PW_KEGG"), ntop=3, filepath=".")
} # }
```
