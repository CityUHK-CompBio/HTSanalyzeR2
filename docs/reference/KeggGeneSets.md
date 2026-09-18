# Create a list of KEGG gene sets

This function creates a list of gene sets based on KEGG pathways terms.
It is species-specific, and returns a list of gene sets, each of which
is a character vector of Entrez gene identifiers.

## Usage

``` r
KeggGeneSets(species = "Hs")
```

## Arguments

- species:

  A single character value specifying a choice of species, such as "Dm"
  ("Drosophila_melanogaster"), "Hs" ("Homo_sapiens"), "Rn"
  ("Rattus_norvegicus") or "Mm" ("Mus_musculus").

## Value

A list of gene sets, with names as KEGG pathway IDs. Each gene set is a
group of genes represented by Entrez identifiers.

## Details

This function needs Internet connection and relies on the following
packages: KEGGREST.

## See also

[`GOGeneSets`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GOGeneSets.md),
[`MSigDBGeneSets`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/MSigDBGeneSets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(KEGGREST)
library(org.Hs.eg.db)
HS_KEGG <- KeggGeneSets(species = "Hs")
} # }
```
