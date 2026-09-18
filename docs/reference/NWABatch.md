# An S4 class for Time series data package in NetWork Analysis on high-throughput screens

This S4 class packages time-series data for further time series
analysis. To put it more clearly, it'll finally initialize a list of NWA
objects for further analyses.

## Usage

``` r
NWABatch(expInfor, pvalueTS, phenotypeTS = list(), interactome = NA)
```

## Arguments

- expInfor:

  A character matrix contains experiment information with each
  experiment in row and information in column. Should at least contain
  two columns named as 'ID' and 'Desription'.

- pvalueTS:

  A list of pvalues, each element of this list is a numeric vector
  pvalues named by gene identifiers for each time point. Note: the order
  of each element of this list must match the order of 'expInfor' ID.

- phenotypeTS:

  A list of phenotypes, each element of this list is a numeric vector
  phenotypes named by gene identifiers for each time point. Note: the
  order of each element of this list must match the order of 'expInfor'
  ID. When it is available, nodes in identified subnetworks would be
  coloured by it (red:+, blue:- as default). Otherwise, all nodes in the
  subnetworks would have no difference.

- interactome:

  An object of class igraph.

## Value

This function will create a new object class 'NWABatch'.

## Slots

- `listOfNWA`:

  A list of 'NWA' object.

## See also

[`NWA-class`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/NWA.md),
[`preprocessNwaTS`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocessNwaTS.md),
[`interactomeNwaTS`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/interactomeNwaTS.md),
[`analyzeNwaTS`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/analyzeNwaTS.md)

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
## Example1: create an object of class 'NWABatch' with phenotypes
nwaTS <- NWABatch(expInfor = expInfor, pvalueTS = pvalueTS, phenotypeTS = phenotypeTS)

## Example2: create an object of class 'NWABatch' without phenotypes
nwaTS <- NWABatch(expInfor = expInfor, pvalueTS = pvalueTS)
```
