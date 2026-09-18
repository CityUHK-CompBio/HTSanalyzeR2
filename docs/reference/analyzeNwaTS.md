# Subnetwork analysis for Time-series data

For each NWA object in 'nwaList', this function will store the
subnetwork module identified by BioNet (if species is given, labels of
nodes will also be mapped from Entrez IDs to gene symbols), and update
information about these results to slot summary of class NWA.

## Usage

``` r
analyzeNwaTS(
  nwaList,
  fdr = 0.001,
  species,
  plotBumModel = FALSE,
  verbose = TRUE
)
```

## Arguments

- nwaList:

  A named list of NWA object.

- fdr:

  A single numeric value specifying the false discovery for the scoring
  of nodes (see BioNet::scoreNodes and Dittrich et al., 2008 for
  details)

- species:

  A single character value specifying the species for which the data
  should be read.

- plotBumModel:

  Boolean value, whether to plot a histogram and qqplot of the p-values
  with the fitted model.

- verbose:

  A single logical value specifying to display detailed messages (when
  verbose=TRUE) or not (when verbose=FALSE), default is TRUE.

## Value

In the end, this function will return an updated list of NWA objects.

## See also

[`analyze`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/analyze.md)

## Examples

``` r
data(d7, d13, d25)

## generate expInfor to describe the information of time series data
expInfor <- matrix(c("d7", "d13", "d25"), nrow = 3, ncol = 2,
                   byrow = FALSE, dimnames = list(NULL, c("ID", "Description")))

## package pvalueTS into a list of pvalues
datalist <- list(d7, d13, d25)
pvalueTS <- lapply(datalist, function(x){
                   tmp <- as.vector(x$neg.p.value)
                   names(tmp) <- x$id
                   tmp})

## package phenotypeTS into a list of phenotypes if you want to color nodes by it,
## otherwise ignore it!
phenotypeTS <- lapply(datalist, function(x) {
                      tmp <- as.vector(x$neg.lfc)
                      names(tmp) <- x$id
                      tmp})

## create an object of class 'NWABatch' with phenotypes
nwaTS <- NWABatch(expInfor = expInfor, pvalueTS = pvalueTS, phenotypeTS = phenotypeTS)

## preprocess NWABatch
nwaTS1 <- preprocessNwaTS(nwaTS, species="Hs", initialIDs="SYMBOL",
                         keepMultipleMappings=TRUE, duplicateRemoverMethod="max")
#> -Preprocessing for input p-values and phenotypes ...
#> --Removing invalid p-values and phenotypes ...
#> --Removing duplicated genes ...
#> --Converting annotations ...
#> 'select()' returned 1:many mapping between keys and columns
#> -- 602 genes (out of 8000) could not be mapped to any identifier, and were removed from the data. 
#> 'select()' returned 1:many mapping between keys and columns
#> -- 602 genes (out of 8000) could not be mapped to any identifier, and were removed from the data. 
#> -Preprocessing complete!
#> 
#> -Preprocessing for input p-values and phenotypes ...
#> --Removing invalid p-values and phenotypes ...
#> --Removing duplicated genes ...
#> --Converting annotations ...
#> 'select()' returned 1:many mapping between keys and columns
#> -- 602 genes (out of 8000) could not be mapped to any identifier, and were removed from the data. 
#> 'select()' returned 1:many mapping between keys and columns
#> -- 602 genes (out of 8000) could not be mapped to any identifier, and were removed from the data. 
#> -Preprocessing complete!
#> 
#> -Preprocessing for input p-values and phenotypes ...
#> --Removing invalid p-values and phenotypes ...
#> --Removing duplicated genes ...
#> --Converting annotations ...
#> 'select()' returned 1:many mapping between keys and columns
#> -- 602 genes (out of 8000) could not be mapped to any identifier, and were removed from the data. 
#> 'select()' returned 1:many mapping between keys and columns
#> -- 602 genes (out of 8000) could not be mapped to any identifier, and were removed from the data. 
#> -Preprocessing complete!
#> 
if (FALSE) { # \dontrun{
## create an interactome for nwa by downloading for BioGRID database
nwaTS2 <- interactomeNwaTS(nwaTS1, species="Hs", reportDir="HTSanalyzerReport", genetic=FALSE)

## analyze
nwaTS3 <- analyzeNwaTS(nwaTS2, fdr=0.0001, species="Hs")
} # }
```
