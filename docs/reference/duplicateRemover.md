# Remove duplicates in a named vector of phenotypes.

This function gets rid of the duplicates in a vector of phenotypes with
gene identifiers as names. It is used to prepare the named vector of
phenotypes for the over-representation and gene set enrichment analysis.

## Usage

``` r
duplicateRemover(geneList, method = "max")
```

## Arguments

- geneList:

  A single named numeric or integer vector with gene identifiers as
  names.

- method:

  A single character value specifying the method to remove the
  duplicates (should the minimum, maximum or average observation for a
  same construct be kept). The current version provides "min" (minimum),
  "max" (maximum), "average" and "fc.avg" (fold change average). The
  minimum and maximum should be understood in terms of absolute values
  (i.e. min/max effect, no matter the sign). The fold change average
  method converts the fold changes to ratios, averages them and converts
  the average back to a fold change.

## Value

A named vector of phenotypes with duplicates removed.

## See also

[`preprocess`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocess.md)

## Examples

``` r
x <- c(5,1,3,-2,6)
names(x) <- c("gene1", "gene3", "gene7", "gene3", "gene4")
xprocessed <- duplicateRemover(geneList=x, method="max")
```
