# An S4 class for Time series data packaging in Gene Set Collection Analyses on high-throughput screens

This S4 class packages time-series data for further GSCA. To put it more
clearly, it'll finally generate a list of GSCA objects for further
analyses.

## Usage

``` r
GSCABatch(expInfor, listOfGeneSetCollections, phenotypeTS, hitsTS = list())
```

## Arguments

- expInfor:

  A character matrix contains experiment information with each
  experiment in row and information in column. Should at least contain
  two columns named as 'ID' and 'Desription'.

- listOfGeneSetCollections:

  A list of gene set collections (a 'gene set collection' is a list of
  gene sets).

- phenotypeTS:

  A list of phenotypes, each element of this list is a numeric vector
  phenotypes named by gene identifiers for each time point. Note: the
  order of each element of this list must match the order of 'expInfor'
  ID.

- hitsTS:

  A list of hits, each element is a character vector of the gene
  identifiers (used as hits in the hypergeometric tests). It's needed if
  you want do GSOA. Note: the order of each element of this list must
  match the order of 'expInfor' ID.

## Value

This function will create a new object class 'GSCABatch'.

## Slots

- `listOfGSCA`:

  A list of initialized GSCA object for futher GSCA.

## See also

[`GSCA-class`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GSCA.md)
[`preprocessGscaTS`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocessGscaTS.md),
[`analyzeGscaTS`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/analyzeGscaTS.md),
[`appendGSTermsTS`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/appendGSTermsTS.md)

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

## Example1: create an object of class GSCABatch with hitsTS
gscaTS <- GSCABatch(expInfor = expInfor, phenotypeTS = phenotypeTS,
               listOfGeneSetCollections = ListGSC, hitsTS = hitsTS)
gscaTS
#> A GSCABatch object:
#> 
#> -expInfor:
#>      ID  Description
#> [1,] d7  d7         
#> [2,] d13 d13        
#> [3,] d25 d25        
#> 
#> -phenotypeTS:
#>          d7  d13  d25
#> length 8000 8000 8000
#> 
#> -hitsTS:
#>         d7 d13 d25
#> length 516 636 862
#> 

## Example2: create an object of class GSCABatch without hitsTS
gscaTS <- GSCABatch(expInfor = expInfor, phenotypeTS = phenotypeTS,
                 listOfGeneSetCollections = ListGSC)
gscaTS
#> A GSCABatch object:
#> 
#> -expInfor:
#>      ID  Description
#> [1,] d7  d7         
#> [2,] d13 d13        
#> [3,] d25 d25        
#> 
#> -phenotypeTS:
#>          d7  d13  d25
#> length 8000 8000 8000
#> 
#> -hitsTS: NA 
```
