# Extract the enrichment map as an igraph object

Extract the enrichment map from an analyzed GSCA object as igraph object
for further external using. Users can also modify the igraph object.

## Usage

``` r
# S4 method for class 'GSCA'
extractEnrichMap(
  object,
  resultName = "GSEA.results",
  gscs,
  ntop = NULL,
  allSig = TRUE,
  gsNameType = "id",
  specificGeneset = NULL,
  cutoff = NULL
)
```

## Arguments

- object:

  A GSCA object.

- resultName:

  A single character value specifying draw an enrichment map based on
  which result. Could only be 'HyperGeo.results' or 'GSEA.results'.

- gscs:

  A character vector specifying the names of gene set collections of
  which the top significant gene sets will be plotted.

- ntop:

  A single integer or numeric value specifying how many gene sets of top
  significance will be plotted.

- allSig:

  A single logical value. If 'TRUE', all significant gene sets (GSEA
  adjusted p-value \< 'pValueCutoff' of slot 'para') will be used;
  otherwise, only top 'ntop' gene sets will be used.

- gsNameType:

  A single character value specifying the type of the gene set names
  that will be displayed as the names of nodes in the enrichment map.
  The type of the gene set names should be one of the following: "id",
  "term" or "none".

- specificGeneset:

  A named list of specific gene sets. Specifically, this term needs to
  be a subset of all analyzed gene sets which can be roughly gotten by
  **getTopGeneSets(object, resultName, gscs, ntop = 20000, allSig =
  FALSE)**.

- cutoff:

  A numeric value between 0 and 1. This parameter is setted as a cutoff
  of edge weight in the enrichment map for better visualization. When
  the edge weight, namely the Jaccard coefficient between two gene sets,
  is less than this cutoff, this edge would not be showed in the
  enrichment map. The order of the list must match the order of results
  gotten by aboved function **getTopGeneSets**.

## Value

An object of igraph with all attributes about the enrichement map.

## Examples

``` r
## load a GSCA object(see the examples of 'analyze' GSCA for details)
library(igraph)
#> Warning: package ‘igraph’ was built under R version 4.6.1
#> 
#> Attaching package: ‘igraph’
#> The following object is masked from ‘package:IRanges’:
#> 
#>     union
#> The following object is masked from ‘package:S4Vectors’:
#> 
#>     union
#> The following objects are masked from ‘package:BiocGenerics’:
#> 
#>     normalize, path, union
#> The following objects are masked from ‘package:generics’:
#> 
#>     components, union
#> The following objects are masked from ‘package:stats’:
#> 
#>     decompose, spectrum
#> The following object is masked from ‘package:base’:
#> 
#>     union
data(d7_gsca)

## extract the enrichment map for top 30 significant 'GO_MF' gene
## sets of GSEA results in an igraph object
extractEnrichMap(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF"),
                allSig = FALSE, ntop = 30, gsNameType = "term")
#> IGRAPH 829c708 UNW- 30 48 -- 
#> + attr: name (v/c), geneSetSize (v/n), adjPvalue (v/n), obsPvalue
#> | (v/n), colorScheme (v/c), label (v/c), label_id (v/c), label_term
#> | (v/c), weight (e/n)
#> + edges from 829c708 (vertex names):
#>  [1] GO_MF.GO:0004521--GO_MF.GO:0070034 GO_MF.GO:0004521--GO_MF.GO:0019843
#>  [3] GO_MF.GO:0004521--GO_MF.GO:0003743 GO_MF.GO:0004521--GO_MF.GO:0043021
#>  [5] GO_MF.GO:0042923--GO_MF.GO:0017046 GO_MF.GO:0042923--GO_MF.GO:0030165
#>  [7] GO_MF.GO:0070034--GO_MF.GO:0043021 GO_MF.GO:0070034--GO_MF.GO:0003730
#>  [9] GO_MF.GO:0070034--GO_MF.GO:0003697 GO_MF.GO:0070034--GO_MF.GO:0047485
#> [11] GO_MF.GO:0008135--GO_MF.GO:0003743 GO_MF.GO:0008135--GO_MF.GO:0003684
#> + ... omitted several edges
```
