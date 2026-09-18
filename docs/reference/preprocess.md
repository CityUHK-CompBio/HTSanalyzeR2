# A preprocessing method for objects of class GSCA or NWA

This is a generic function. When implemented as the S4 method for
objects of class
[`GSCA-class`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GSCA.md)
or
[`NWA-class`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/NWA.md),
this function filters out invalid data, removes duplicated genes,
converts annotations to Entrez identifiers, etc.

## Usage

``` r
# S4 method for class 'GSCA'
preprocess(
  object,
  species = "Hs",
  initialIDs = "SYMBOL",
  keepMultipleMappings = TRUE,
  duplicateRemoverMethod = "max",
  orderAbsValue = FALSE,
  verbose = TRUE
)

# S4 method for class 'NWA'
preprocess(
  object,
  species = "Hs",
  duplicateRemoverMethod = "max",
  initialIDs = "SYMBOL",
  keepMultipleMappings = TRUE,
  verbose = TRUE
)
```

## Arguments

- object:

  A
  [`GSCA-class`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GSCA.md)
  or
  [`NWA-class`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/NWA.md)
  object.

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
  for input 'geneList'. The valid terms need match with the keytypes of
  species databases such as keytypes(org.Hs.eg.db).

- keepMultipleMappings:

  A single logical value. If TRUE, the function keeps the entries with
  multiple mappings (first mapping is kept). If FALSE, the entries with
  multiple mappings will be discarded.

- duplicateRemoverMethod:

  A single character value specifying the method to remove the
  duplicates. See help(duplicateRemover) for details.

- orderAbsValue:

  A single logical value indicating whether the values should be
  converted to absolute values and then ordered (if TRUE), or ordered as
  they are (if FALSE). This argument is only for class
  [`GSCA-class`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GSCA.md).

- verbose:

  A single logical value specifying to display detailed messages (when
  verbose=TRUE) or not (when verbose=FALSE).

## Value

In the end, this function will return an updated object of class
[`GSCA-class`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GSCA.md)
or
[`NWA-class`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/NWA.md).

## Details

This function will do the following preprocessing steps:

1:filter out p-values (the slot *pvalues* of class NWA), phenotypes (the
slot *phenotypes* of class NWA) and data for enrichment (the slot
*geneList* of class GSCA) with NA values or without valid names, and
invalid gene names (the slot *hits* of class GSCA);

2:invoke function duplicateRemover to remove duplicated genes in the
slot *pvalues*, *phenotypes* of class NWA, and the slot *geneList* and
*hits* of class GSCA;

3:invoke function annotationConvertor to convert annotations from
initialIDs to Entrez identifiers. Please note that the slot *hits* and
the names of the slot *geneList* of class GSCA, the names of the slot
*pvalues* and the names of the slot *phenotypes* of class NWA must have
the same type of gene annotation specified by initialIDs;

4:order the data for enrichment decreasingly for objects of class GSCA.

See the function duplicateRemover for more details about how to remove
duplicated genes.

See the function annotationConvertor for more details about how to
convert annotations.

## See also

[`duplicateRemover`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/duplicateRemover.md),
[`annotationConvertor`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/annotationConvertor.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# ===========================================================
# GSCA class
library(org.Hs.eg.db)
library(GO.db)
library(KEGGREST)
## load data for enrichment analyses
data(d7)
phenotype <- as.vector(d7$neg.lfc)
names(phenotype) <- d7$id

## select hits if you also want to do GSOA, otherwise ignore it
hits <-  names(phenotype[which(abs(phenotype) > 2)])

## set up a list of gene set collections
GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
PW_KEGG <- KeggGeneSets(species="Hs")
ListGSC <- list(GO_MF=GO_MF, PW_KEGG=PW_KEGG)

## create an object of class 'GSCA'
gsca <- GSCA(listOfGeneSetCollections = ListGSC, geneList = phenotype, hits = hits)

## do preprocessing
gsca1 <- preprocess(gsca, species="Hs", initialIDs="SYMBOL", keepMultipleMappings=TRUE,
                   duplicateRemoverMethod="max", orderAbsValue=FALSE)

## print gsca1
gsca1
} # }

# ===========================================================
# NWA class
library(org.Hs.eg.db)
library(GO.db)
## load data for subnetwork analyses
data(d7)
pvalues <- d7$neg.p.value
names(pvalues) <- d7$id

## input phenotypes if you want to color nodes by it
phenotypes <- as.vector(d7$neg.lfc)
names(phenotypes) <- d7$id

## create an object of class 'NWA' with phenotypes
nwa <- NWA(pvalues=pvalues, phenotypes=phenotypes)

## do preprocessing
nwa1 <- preprocess(nwa, species="Hs", initialIDs="SYMBOL", keepMultipleMappings=TRUE,
                   duplicateRemoverMethod="max")
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
