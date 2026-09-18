# An S4 class for NetWork Analysis on high-throughput data

This class includes a series of methods to do network analysis for
high-throughput data.

## Usage

``` r
NWA(pvalues, phenotypes = as.numeric(), interactome = NA)
```

## Arguments

- pvalues:

  A numeric or integer vector of pvalues named by gene identifiers.

- phenotypes:

  A numeric or integer vector of phenotypes named by gene identifiers.
  When it is available, nodes in identified subnetwork would be coloured
  by it (red:+, blue:- as default). Otherwise, all nodes in the
  subnetwork would have no difference.

- interactome:

  An object of class igraph.

## Value

This function will create a new object of class 'NWA'.

## Slots

- `fdr`:

  One parameter for BioNet to score nodes in the interactome.

- `result`:

  A list consisting of subnetwork module identified by BioNet and a
  vector of labels for nodes of the subnetwork module.

- `summary`:

  A list of summary information for pvalues, phenotypes, interactome and
  result.

- `preprocessed`:

  A logical value specifying whether or not input data has been
  preprocessed.

## See also

[`preprocess`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocess.md),
[`analyze`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/analyze.md),
[`summarize`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/summarize.md),
[`interactome`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/interactome.md),
[`report`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/report.md)

## Examples

``` r
library(org.Hs.eg.db)
library(GO.db)
## load data for network analyses
data(d7)
pvalues <- d7$neg.p.value
names(pvalues) <- d7$id

## define phenotypes if you want to color nodes by it, otherwise ignore it!
phenotypes <- as.vector(d7$neg.lfc)
names(phenotypes) <- d7$id

## Example1: create an object of class 'NWA' with phenotypes
nwa <- NWA(pvalues=pvalues, phenotypes=phenotypes)

## Example2: create an object of class 'NWA' without phenotypes
nwa <- NWA(pvalues=pvalues)
```
