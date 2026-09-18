# Create an interactome from BioGRID database or input custom interactome

This is a generic function. When implemented as the S4 method of class
NWA, this function creates an interactome before conducting network
analysis.

## Usage

``` r
# S4 method for class 'NWA'
interactome(
  object,
  interactionMatrix = NULL,
  species,
  link = defaultBioGridLink(),
  reportDir = "HTSanalyzerReport",
  genetic = FALSE,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- object:

  An NWA object.

- interactionMatrix:

  An interaction matrix including columns 'InteractionType',
  'InteractorA' and 'InteractorB'. If this matrix is available, the
  interactome can be directly built based on it instead of downloading
  from BioGRID.

- species:

  A single character value specifying the species for which the data
  should be downloaded. The current version supports one of the
  following species: "Dm" ("Drosophila_melanogaster"), "Hs"
  ("Homo_sapiens"), "Rn" ("Rattus_norvegicus"), "Mm" ("Mus_musculus"),
  "Ce" ("Caenorhabditis_elegans").

- link:

  The link (url) where the data should be downloaded (in tab2 format).
  By default the current BioGRID "Latest-Release" archive is used; pass
  an explicit release url to pin a specific version.

- reportDir:

  A single character value specifying the directory to store reports.
  The BioGRID data set will be downloaded and stored in a subdirectory
  called 'Data' in 'reportDir'.

- genetic:

  A single logical value. If TRUE, genetic interactions will be kept;
  otherwise, they will be removed from the data set.

- force:

  Force to download the data set.

- verbose:

  A single logical value indicating to display detailed messages (when
  verbose=TRUE) or not (when verbose=FALSE)

## Value

In the end, this function will return an updated object with slot
'interactome' as an object of class 'igraph'.

## Details

This function provides two options to create an interactome for network
analysis. The user can either input an interaction matrix including
columns 'InteractionType', 'InteractionA' and 'InteractionB', or set
'species', 'link' and 'genetic' to download data set from BioGRID and
extract corresponding interactions to build the interactome.

Another way to set up the interactome is to input an igraph object when
the NWA object is created (i.e. nwa=NWA(pvalues, phenotypes,
interactome)).

## Examples

``` r
library(org.Hs.eg.db)
library(GO.db)
## load data for subnetwork analyses
data(d7)
pvalues <- d7$neg.p.value
names(pvalues) <- d7$id

## input phenotypes if you want to color nodes by it
phenotypes <- as.vector(d7$neg.lfc)
names(phenotypes) <- d7$id

## Example1: create an object of class 'NWA' by inputting an igraph object as the interactome
data(Biogrid_HS_Interactome)
nwa <- NWA(pvalues=pvalues, phenotypes=phenotypes, interactome=Biogrid_HS_Interactome)
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...


## Example2: create an object of class 'NWA' without interactome
nwa <- NWA(pvalues=pvalues, phenotypes=phenotypes)
## create an interactome for nwa by inputting an interaction matrix
data(Biogrid_HS_Mat)
nwa1 <- interactome(nwa, interactionMatrix = Biogrid_HS_Mat, genetic=FALSE)
#> -Creating interactome ...
#> -Interactome created! 
#> 

## Example3: create an object of class 'NWA' without interactome
nwa <- NWA(pvalues=pvalues, phenotypes=phenotypes)
if (FALSE) { # \dontrun{
## create an interactome for nwa by downloading for BioGRID database
nwa1 <- interactome(nwa, species="Hs", reportDir="HTSanalyzerReport", genetic=FALSE)
} # }
```
