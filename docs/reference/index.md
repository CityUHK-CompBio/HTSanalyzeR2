# Package index

## Gene set collections

- [`KeggGeneSets()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/KeggGeneSets.md)
  : Create a list of KEGG gene sets
- [`GOGeneSets()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GOGeneSets.md)
  : Create a list of gene sets based on Gene Ontology terms
- [`MSigDBGeneSets()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/MSigDBGeneSets.md)
  : Create a list of gene sets for specific species from MSigDB database

## Analysis classes

- [`GSCA()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GSCA.md)
  : An S4 class for Gene Set Collection Analyses on high-throughput data
- [`NWA()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/NWA.md)
  : An S4 class for NetWork Analysis on high-throughput data
- [`GSCABatch()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GSCABatch.md)
  : An S4 class for Time series data packaging in Gene Set Collection
  Analyses on high-throughput screens
- [`NWABatch()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/NWABatch.md)
  : An S4 class for Time series data package in NetWork Analysis on
  high-throughput screens
- [`gscaTS`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/gscaTS.md)
  : A list of 'GSCA' object
- [`nwaTS`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/nwaTS.md)
  : A list of 'NWA' object

## Core workflow

- [`preprocess(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocess.md)
  [`preprocess(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocess.md)
  : A preprocessing method for objects of class GSCA or NWA
- [`analyze(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/analyze.md)
  [`analyze(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/analyze.md)
  : Gene Set Collection Analysis or NetWork Analysis
- [`summarize(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/summarize.md)
  [`summarize(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/summarize.md)
  : Print summary information for an object of class GSCA or NWA
- [`report(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/report.md)
  [`report(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/report.md)
  : Write HTML reports for enrichment or network analyses
- [`getPara(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/getPara.md)
  [`getPara(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/getPara.md)
  : Accessors for the 'para' slot of a 'GSCA' or 'fdr' slot of a 'NWA'
  object.
- [`getResult(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/getResult.md)
  [`getResult(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/getResult.md)
  : Accessors for the 'result' slot of a 'GSCA' or 'NWA' object.
- [`getSummary(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/getSummary.md)
  [`getSummary(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/getSummary.md)
  : Accessors for the 'summary' slot of a 'GSCA' or 'NWA' object.

## Enrichment analysis

- [`appendGSTerms(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/appendGSTerms.md)
  : Append gene set terms to GSCA results and gene names annotation
- [`extractEnrichMap(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/extractEnrichMap-GSCA-method.md)
  : Extract the enrichment map as an igraph object
- [`getTopGeneSets(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/getTopGeneSets.md)
  : Select top significant gene sets from results of GSCA object
- [`plotGSEA(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/plotGSEA.md)
  : Plot and save figures of GSEA results for top significant gene sets
- [`viewEnrichMap(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/viewEnrichMap-GSCA-method.md)
  : Plot the enrichment map for GSEA or GSOA result
- [`viewGSEA(`*`<GSCA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/viewGSEA.md)
  : Plot a figure of GSEA results for one gene set

## Network analysis

- [`getInteractome(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/getInteractome.md)
  : Accessors for the 'interactome' slot of a 'NWA' object.
- [`interactome(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/interactome.md)
  : Create an interactome from BioGRID database or input custom
  interactome
- [`extractSubNet(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/extractSubNet-NWA-method.md)
  : Extract the subnetwork as an igraph object
- [`viewSubNet(`*`<NWA>`*`)`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/viewSubNet-NWA-method.md)
  : Plot the identified subnetwork of an NWA object
- [`forceGraph()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/forceGraph.md)
  : Create an interactive force-directed network widget
- [`updateForceGraph()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/updateForceGraph.md)
  : Update a rendered network in place
- [`saveNetwork()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/saveNetwork.md)
  : Save an interactive network graph to HTML or PNG
- [`screenStatTests()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/screenStatTests.md)
  : Statistical tests for high-throughput screen data

## Time series analysis

- [`preprocessGscaTS()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocessGscaTS.md)
  : A preprocessing method for a GSCABatch object of Time-series data
  Gene Set Collection Analyses
- [`analyzeGscaTS()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/analyzeGscaTS.md)
  : Gene Set Collection Analysis for Time-series data.
- [`preprocessNwaTS()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/preprocessNwaTS.md)
  : A preprocessing method for a GSCABatch object of Time-series data
  Network Analyses
- [`analyzeNwaTS()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/analyzeNwaTS.md)
  : Subnetwork analysis for Time-series data
- [`appendGSTermsTS()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/appendGSTermsTS.md)
  : Append gene set terms to GSCA results for each GSCA object of
  Time-series data
- [`interactomeNwaTS()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/interactomeNwaTS.md)
  : Create an interactome from BioGRID database or input custom
  interactome
- [`reportAll()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/reportAll.md)
  : Write HTML reports for both enrichment and network analyses

## MAGeCK integration

- [`HTSanalyzeR2Pipe()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/HTSanalyzeR2Pipe.md)
  : An analysis pipeline for common phenotype data
- [`HTSanalyzeR4MAGeCK()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/HTSanalyzeR4MAGeCK.md)
  : An analysis pipeline for CRISPR data preprocessed by MAGeCK

## Utilities

- [`annotationConvertor()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/annotationConvertor.md)
  : Convert between different types of gene identifiers
- [`duplicateRemover()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/duplicateRemover.md)
  : Remove duplicates in a named vector of phenotypes.

## Data

- [`d7`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/d7.md)
  : CRISPR data set

- [`d7_gsca`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/d7_gsca.md)
  : An object of class 'GSCA'

- [`d7_nwa`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/d7_nwa.md)
  : An object of class 'NWA'

- [`GSE33113_exp`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GSE33113_exp.md)
  : Expression of a demo data: GSE33113

- [`GSE33113_label`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GSE33113_label.md)
  : Concensus molecular subtype label of GSE33113

- [`GSE33113_limma`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/GSE33113_limma.md)
  : limma result of a gene expression data

- [`Biogrid_HS_Interactome`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/Biogrid_HS_Interactome.md)
  : An object of class 'igraph'

- [`Biogrid_HS_Mat`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/Biogrid_HS_Mat.md)
  :

  An interactome matrix for *Homo Sapiens*

## Package

- [`HTSanalyzeR2-package`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/HTSanalyzeR2.md)
  [`HTSanalyzeR2`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/HTSanalyzeR2.md)
  : HTSanalyzeR2 Package Overview
