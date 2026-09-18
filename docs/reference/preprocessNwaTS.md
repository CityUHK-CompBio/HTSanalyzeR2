# A preprocessing method for a GSCABatch object of Time-series data Network Analyses

This function will do basic preprocessing for each NWA object of
'nwaList'.

## Usage

``` r
preprocessNwaTS(
  object,
  species = "Hs",
  initialIDs = "SYMBOL",
  keepMultipleMappings = TRUE,
  duplicateRemoverMethod = "max",
  verbose = TRUE
)
```

## Arguments

- object:

  An NWABatch object.

- species:

  A single character value specifying the species for which the data
  should be read.

- initialIDs:

  A single character value specifying the type of initial identifiers
  for input nwaList.

- keepMultipleMappings:

  A single logical value. If TRUE, the function keeps the entries with
  multiple mappings (first mapping is kept). If FALSE, the entries with
  multiple mappings will be discarded.

- duplicateRemoverMethod:

  A single character value specifying the method to remove the
  duplicates. See duplicateRemover for details.

- verbose:

  A single logical value specifying whether to display detailed messages
  (when verbose=TRUE) or not (when verbose=FALSE), default is TRUE.

## Value

In the end, this function will return an updated list of NWA object.

## See also

[`preprocess`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocess.md)

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

## preprocess nwaTS
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
```
