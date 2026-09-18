# An analysis pipeline for CRISPR data preprocessed by MAGeCK

This function writes a shiny report following a complete analyses of
CRISPR data preprocessed by MAGeCK based on the two classes GSCA (Gene
Set Collection Analysis) and NWA (NetWork Analysis) of this package.

## Usage

``` r
HTSanalyzeR4MAGeCK(
  MAGeCKdata,
  selectDirection = "negative",
  doGSOA = FALSE,
  doGSEA = TRUE,
  hitsCutoffLogFC = NULL,
  hitsCutoffPval = NULL,
  listOfGeneSetCollections,
  species = "Hs",
  initialIDs = "SYMBOL",
  keepMultipleMappings = TRUE,
  duplicateRemoverMethod = "max",
  orderAbsValue = FALSE,
  pValueCutoff = 0.05,
  pAdjustMethod = "BH",
  nPermutations = 1000,
  minGeneSetSize = 15,
  exponent = 1,
  verbose = TRUE,
  GSEA.by = "HTSanalyzeR2",
  keggGSCs = NULL,
  goGSCs = NULL,
  msigdbGSCs = NULL,
  doNWA = FALSE,
  interactionMatrix = NULL,
  reportDir = "HTSanalyzerReport",
  nwAnalysisGenetic = FALSE,
  nwAnalysisFdr = 0.001
)
```

## Arguments

- MAGeCKdata:

  A result file for CRISPR data preprocessed by MAGeCK.

- selectDirection:

  A character specifying which direction to choose from MAGeCK result,
  should either be 'positive' or 'negative'.

- doGSOA:

  A logic value specifying whether to do hypergeometric test or not,
  default is FALSE.

- doGSEA:

  A logic value specifying whether to do gene set enrichment analysis or
  not, default is TRUE.

- hitsCutoffLogFC:

  A numeric value as cutoff to choose hits based on log2fold change when
  doing GSOA. Genes with absolute log2fold change greater than this
  cutoff would be choosen as hits. Either 'hitsCutoffLogFC' or
  'hitsCutoffPval' is needed when doing GSOA.

- hitsCutoffPval:

  A numeric value as cutoff to choose hits based on pvalue when doing
  GSOA. Genes with pvalues less than this cutoff would be choosen as
  hits. Either 'hitsCutoffLogFC' or 'hitsCutoffPval' is needed when
  doing GSOA.

- listOfGeneSetCollections:

  A list of gene set collections (a 'gene set collection' is a list of
  gene sets).

- species:

  A single character value specifying the species for which the data
  should be read.

- initialIDs:

  A single character value specifying the type of initial identifiers
  for input geneList

- keepMultipleMappings:

  A single logical value. If TRUE, the function keeps the entries with
  multiple mappings (first mapping is kept). If FALSE, the entries with
  multiple mappings will be discarded.

- duplicateRemoverMethod:

  A single character value specifying the method to remove the
  duplicates. See duplicateRemover for details.

- orderAbsValue:

  A single logical value indicating whether the values should be
  converted to absolute values and then ordered (if TRUE), or ordered as
  they are (if FALSE).

- pValueCutoff:

  A single numeric value specifying the cutoff for p-values considered
  significant in gene set collection analysis.

- pAdjustMethod:

  A single character value specifying the p-value adjustment method to
  be used (see 'p.adjust' for details) in gene set collection analysis.

- nPermutations:

  A single integer or numeric value specifying the number of
  permutations for deriving p-values in GSEA.

- minGeneSetSize:

  A single integer or numeric value specifying the minimum number of
  elements shared by a gene set and the input total genes. Gene sets
  with fewer than this number are removed from both hypergeometric
  analysis and GSEA.

- exponent:

  A single integer or numeric value used in weighting phenotypes in
  GSEA.

- verbose:

  A single logical value specifying to display detailed messages (when
  verbose=TRUE) or not (when verbose=FALSE)

- GSEA.by:

  A single character value to choose which algorithm to do GSEA. Valid
  value could either be "HTSanalyzeR2"(default) or "fgsea". If performed
  by "fgsea", the result explanation please refer to
  [`fgsea`](https://rdrr.io/pkg/fgsea/man/fgsea.html).

- keggGSCs:

  A character vector of names of all KEGG gene set collections.

- goGSCs:

  A character vector of names of all GO gene set collections.

- msigdbGSCs:

  A character vector of names of all MSigDB gene set collections.

- doNWA:

  A logic value specifying whether to do subnetwork analysis or not,
  default is FALSE.

- interactionMatrix:

  An interaction matrix including columns 'InteractionType',
  'InteractorA' and 'InteractorB'. If this matrix is available, the
  interactome can be directly built based on it.

- reportDir:

  A single character value specifying the directory to store reports.
  For default the enrichment analysis reports will be stored in the
  directory called "HTSanalyzerReport".

- nwAnalysisGenetic:

  A single logical value. If TRUE, genetic interactions will be kept;
  otherwise, they will be removed from the data set.

- nwAnalysisFdr:

  A single numeric value specifying the false discovery for the scoring
  of nodes (see BioNet::scoreNodes and Dittrich et al., 2008 for
  details)

## Value

This pipeline function will finally generate a shiny report including
all the results in. All the results would be stored as RData named
'results.RData' under a new automatic generated directory. It can be
loaded into R by 'readRDS(results.RData)'.

## Examples

``` r
if (FALSE) { # \dontrun{
data(d7)

library(GO.db)
library(org.Hs.eg.db)
library(KEGGREST)

## set up a list of gene set collections
GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
PW_KEGG <- KeggGeneSets(species="Hs")
ListGSC <- list(GO_MF=GO_MF, PW_KEGG=PW_KEGG)

## start analysis
HTSanalyzeR4MAGeCK(MAGeCKdata = d7,
                   selectDirection = "negative",
                   doGSOA = TRUE,
                   doGSEA = TRUE,
                   hitsCutoffPval = 0.01,
                   listOfGeneSetCollections = ListGSC,
                   species = "Hs",
                   initialIDs = "SYMBOL",
                   pValueCutoff = 0.05,
                   nPermutations = 1000,
                   minGeneSetSize = 200,
                   keggGSCs=c("PW_KEGG"),
                   goGSCs = c("GO_MF"),
                   doNWA = TRUE,
                   nwAnalysisFdr = 0.0001)
} # }
```
