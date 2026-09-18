# Convert between different types of gene identifiers

This function converts an initial data vector named by non-entrez ids to
the same vector but with entrez ids, and vice versa. Genes for which no
mapping were found will be removed. This function can also take a
matrix, with gene identifiers as row names.

## Usage

``` r
annotationConvertor(geneList, species="Hs", initialIDs="SYMBOL",
finalIDs="ENTREZID", keepMultipleMappings=TRUE, verbose=TRUE)
```

## Arguments

- geneList:

  A named integer or numeric vector, or a matrix with rows named by gene
  identifiers.

- species:

  A single character value specifying the species of the input. It
  supports all the species of OrgDb objects in AnnotationDbi. The format
  should be an abbreviation of the organism as setted by AnnotationDbi.
  For example, the commonly used ones are "Dm"
  ("Drosophila_melanogaster"), "Hs" ("Homo_sapiens"), "Rn"
  ("Rattus_norvegicus"), "Mm" ("Mus_musculus"), "Ce"
  ("Caenorhabditis_elegans"), and etc.

- initialIDs:

  A single character value specifying the type of initial identifiers
  for input geneList. The valid terms need match with the keytypes of
  species db such as keytypes(org.Hs.eg.db).

- finalIDs:

  A single character value specifying the type of final identifiers for
  input geneList.

- keepMultipleMappings:

  A single logical value. If TRUE, the function keeps the entries with
  multiple mappings (first mapping is kept). If FALSE, the entries with
  multiple mappings will be discarded.

- verbose:

  A single logical value specifying whether to display detailed messages
  (when verbose=TRUE) or not (when verbose=FALSE).

## Value

The same data vector/matrix but with names/row names converted.

## Examples

``` r
library(org.Hs.eg.db)
## Example1: convert a named vector
x <- runif(10)
names(x) <- names(as.list(org.Hs.egSYMBOL2EG))[1:10]
xEntrez <- annotationConvertor(geneList=x, species="Hs", initialIDs="SYMBOL",
                               finalIDs="ENTREZID")
#> 'select()' returned 1:1 mapping between keys and columns
#> -- 0 genes (out of 10) could not be mapped to any identifier, and were removed from the data. 

## Example2: convert a data matrix with row names as gene ids
x <- cbind(runif(10),runif(10))
rownames(x) <- names(as.list(org.Hs.egSYMBOL2EG))[1:10]
xEntrez <- annotationConvertor(geneList=x, species="Hs", initialIDs="SYMBOL",
                               finalIDs="ENTREZID")
#> 'select()' returned 1:1 mapping between keys and columns
#> -- 0 genes (out of 10) could not be mapped to any identifier, and were removed from the data. 
```
