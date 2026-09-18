# Create a list of gene sets based on Gene Ontology terms

This function creates a list of gene sets based on Gene Ontology terms.
It is species-specific, and returns a list of gene sets, each of which
is a character vector of Entrez identifiers.

## Usage

``` r
GOGeneSets(species = "Hs", ontologies = c("MF"))
```

## Arguments

- species:

  A single character value specifying a choice of species, such as "Dm"
  ("Drosophila_melanogaster"), "Hs" ("Homo_sapiens"), "Rn"
  ("Rattus_norvegicus") or "Mm" ("Mus_musculus").

- ontologies:

  A single character value or a character vector specifying an ontology
  or multiple ontologies. Valid format could be any combination of "BP",
  "MF" and "CC".

## Value

A list of gene sets, with names as GO IDs. Each gene set is a character
vector of Entrez identifiers.

## Details

This function relies on the following packages: AnnotationDbi, GO.db and
the species db, such as org.Dm.eg.db.

## See also

[`KeggGeneSets`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/KeggGeneSets.md),
[`MSigDBGeneSets`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/MSigDBGeneSets.md)

## Examples

``` r
library(GO.db)
#> Loading required package: AnnotationDbi
#> Loading required package: stats4
#> Loading required package: BiocGenerics
#> Loading required package: generics
#> 
#> Attaching package: ‘generics’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
#>     setequal, union
#> 
#> Attaching package: ‘BiocGenerics’
#> The following objects are masked from ‘package:stats’:
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from ‘package:base’:
#> 
#>     Filter, Find, Map, Position, Reduce, anyDuplicated, aperm, append,
#>     as.data.frame, basename, cbind, colnames, dirname, do.call,
#>     duplicated, eval, evalq, get, grep, grepl, is.unsorted, lapply,
#>     mapply, match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
#>     rank, rbind, rownames, sapply, saveRDS, table, tapply, unique,
#>     unsplit, which.max, which.min
#> Loading required package: Biobase
#> Welcome to Bioconductor
#> 
#>     Vignettes contain introductory material; view with
#>     'browseVignettes()'. To cite Bioconductor, see
#>     'citation("Biobase")', and for packages 'citation("pkgname")'.
#> Loading required package: IRanges
#> Loading required package: S4Vectors
#> 
#> Attaching package: ‘S4Vectors’
#> The following object is masked from ‘package:utils’:
#> 
#>     findMatches
#> The following objects are masked from ‘package:base’:
#> 
#>     I, expand.grid, unname
library(org.Hs.eg.db)
HS_GO_CC <- GOGeneSets(species="Hs",ontologies=c("CC"))

HS_GO <- GOGeneSets(species="Hs",ontologies=c("CC", "MF", "BP"))
```
