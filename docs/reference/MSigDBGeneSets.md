# Create a list of gene sets for specific species from MSigDB database

This function creates gene set collections based on MSigDB database
version 6.2. Currently our package supports all 8 collections for 10
species retrieved by
[`msigdbr`](https://igordot.github.io/msigdbr/reference/msigdbr.html).
It returns a list of gene sets collections with the elements of the gene
sets represented by Entrez Gene IDs.

## Usage

``` r
MSigDBGeneSets(species = "Hs", collection = "C2", subcategory = NULL)
```

## Arguments

- species:

  A single character value specifying the species of the gene sets of
  MSigDB. Now we support 10 species: 'Bt'(Bos taurus),
  'Ce'(Caenorhabditis elegans), 'Cfa'(Canis lupus familiaris),
  'Dm'(Drosophila melanogaster), 'Dr'(Danio rerio), 'Gg'(Gallus gallus),
  'Hs'(Homo sapiens), 'Mm'(Mus musculus), 'Rn'(Rattus norvegicus),
  'Sc'(Saccharomyces cerevisiae) and 'Ss'(Sus scrofa).

- collection:

  A single character value specifying a choice of collection. Valid
  values include 'H'(hallmark gene sets), 'C1'(positional gene sets),
  'C2'(curated gene sets), 'C3'(motif gene sets), 'C4'(computational
  gene sets), 'C5'(GO gene sets), 'C6'(oncogenic signatures),
  'C7'(immunologic signatures). More details please refer to
  [MSigDB](https://software.broadinstitute.org/gsea/msigdb).

- subcategory:

  A single character value or NULL specifying a subcategory of the
  selected MSigDB collection. See
  [`msigdbr`](https://igordot.github.io/msigdbr/reference/msigdbr.html)
  for details.

## Value

Return a list of gene sets of specific collection in
[MSigDB](https://software.broadinstitute.org/gsea/msigdb) of version
6.2.

## See also

[`GOGeneSets`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GOGeneSets.md),
[`KeggGeneSets`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/KeggGeneSets.md)

## Examples

``` r
C2_MSig <- MSigDBGeneSets(species = "Hs", collection = "C2", subcategory = NULL)
```
