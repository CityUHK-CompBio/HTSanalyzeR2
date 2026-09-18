# Write HTML reports for enrichment or network analyses

This is a generic function.

## Usage

``` r
# S4 method for class 'GSCA'
report(
  object,
  specificGeneset = NULL,
  cutoff = NULL,
  reportDir = "GSCAReport",
  gseaPlot = FALSE,
  para = list(output = "pdf", ES.range = NULL, rankMetric.range = NULL, ESline.col =
    "FireBrick", hits.col = "black", rankMetric.col = "CadetBlue")
)

# S4 method for class 'NWA'
report(object, reportDir = "NWAReport")
```

## Arguments

- object:

  A 'GSCA' or 'NWA'object.

- specificGeneset:

  A named list of specific gene sets. See
  [`viewEnrichMap,GSCA-method`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/viewEnrichMap-GSCA-method.md)
  for details.

- cutoff:

  A numeric value between 0 and 1. This parameter is setted as a cutoff
  of edge weight in the enrichment map for better visualization. When
  the edge weight, namely the Jaccard coefficient between two gene sets,
  is less than this cutoff, this edge would not be showed in the
  enrichment map.

- reportDir:

  A single character value specifying the path to store reports. By
  default, the enrichment analysis reports will be stored in the
  directory called "GSCAReport". Once launching, the directory will be
  generated automatically containing an R code file named app.R, an
  RDdata object named results.RData storing all basic results which can
  be loaded into R with readRDS() function and a folder named gsea_plots
  containing gsea plots for all significant gene sets.

- gseaPlot:

  A logical value to choose whether make gsea plot for significant gene
  sets or not, default is FALSE.

- para:

  A list of parameters for gsea plot. See
  [`plotGSEA`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/plotGSEA.md)
  for details.

## Value

In the end, this function would generate a Shiny report.

## Details

When implemented as the method of class GSCA or NWA, this function
produces reports for either the Gene Set Collection Analysis or the
NetWork Analysis.

This will generate a shiny report including all the GSCA or NWA results.

For GSCA object, users can download the table of GSOA and/or GSEA result
in different format such as 'csv' and 'pdf'. In GSEA result of this
report, 'Pvalue' or 'Adjusted.Pvalue' equalling to 0 would be replaced
by less than 1 divided by permutation times. The enrichment map could be
modified according to the user's preferences such as layout, node,
label, and etc. Details please see the vignette of our package.

For NWA object, the identified subnetwork could be modified according to
the user's preferences in many ways such as layout, color, and etc.
Details please see the vignette of our package.

## Examples

``` r
# =======================================================
# GSCA class
## load a GSCA object(see the examples of analyze GSCA for details)
data(d7_gsca)

## Example1: report d7_gsca
if (FALSE) { # \dontrun{
report(d7_gsca)
} # }

## Example2: report d7_gsca containing enrichment map with specificGeneset
tmp <- getTopGeneSets(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF"),
                      ntop = 20000, allSig = FALSE)
## In that case, we can define specificGeneset as below:
GO_MF_geneset <- tmp$GO_MF[20:35]
## the name of specificGenesets also needs to match with the names of tmp
specificGeneset <- list("GO_MF"=GO_MF_geneset)
if (FALSE) { # \dontrun{
report(d7_gsca, specificGeneset=specificGeneset)
} # }

## Example3: report d7_gsca using a cutoff to filter away edges with small weight
if (FALSE) { # \dontrun{
report(d7_gsca, cutoff = 0.01)
} # }

if (FALSE) { # \dontrun{
# =================================================================
# NWA class
## load a NWA object(see the examples of analyze NWA for details)
data(d7_nwa)

## report d7_nwa
report(d7_nwa)
} # }
```
