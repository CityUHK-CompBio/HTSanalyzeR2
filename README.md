# HTSanalyzeR2

[![R-CMD-check](https://github.com/CityUHK-CompBio/HTSanalyzeR2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CityUHK-CompBio/HTSanalyzeR2/actions/workflows/R-CMD-check.yaml)

Gene set over-representation, gene set enrichment, network analysis and
time-series analysis for high-throughput screens — CRISPR, RNA-seq, microarray
and RNAi — behind one consistent S4 workflow, with an interactive report for
exploring and exporting the results.

## Requirements

| | |
| --- | --- |
| R | ≥ 3.5 declared; validated on R 4.6 |
| Bioconductor | 3.23 (validated) |
| Platforms | macOS (Apple silicon and Intel), Linux, Windows |

The package follows the Bioconductor release contract for dependency versions.

## Installation

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")

## annotation and pathway resources the analysis functions call directly
BiocManager::install(c("GO.db", "KEGGREST", "AnnotationDbi"))

## HTSanalyzeR2 itself
BiocManager::install("CityUHK-CompBio/HTSanalyzeR2")
```

`remotes::install_github("CityUHK-CompBio/HTSanalyzeR2")` works the same way if
you prefer `remotes`.

## Quick start

```r
library(HTSanalyzeR2)
library(org.Hs.eg.db)
library(KEGGREST)

data(GSE33113_limma)
phenotype <- GSE33113_limma$logFC
names(phenotype) <- rownames(GSE33113_limma)

## 1. gene set collections
PW_KEGG <- KeggGeneSets(species = "Hs")
gsca <- GSCA(listOfGeneSetCollections = list(PW_KEGG = PW_KEGG),
             geneList = phenotype)

## 2. map identifiers and remove duplicates
gsca <- preprocess(gsca, species = "Hs", initialIDs = "SYMBOL",
                   keepMultipleMappings = TRUE, duplicateRemoverMethod = "max",
                   orderAbsValue = FALSE)

## 3. hypergeometric test and GSEA
gsca <- analyze(gsca,
                para = list(pValueCutoff = 0.05, pAdjustMethod = "BH",
                            nPermutations = 1000, minGeneSetSize = 10,
                            exponent = 1),
                doGSOA = TRUE, doGSEA = TRUE)
gsca <- appendGSTerms(gsca, keggGSCs = "PW_KEGG")

## 4. inspect
topGS <- getTopGeneSets(gsca, resultName = "GSEA.results", gscs = "PW_KEGG")
viewGSEA(gsca, gscName = "PW_KEGG", gsName = topGS[["PW_KEGG"]][1])
viewEnrichMap(gsca, gscs = "PW_KEGG", gsNameType = "term")

## 5. interactive report
report(gsca)
```

### Network analysis

```r
pvalues <- GSE33113_limma$adj.P.Val
names(pvalues) <- rownames(GSE33113_limma)

nwa <- NWA(pvalues = pvalues, phenotypes = phenotype)
nwa <- preprocess(nwa, species = "Hs", initialIDs = "SYMBOL")
nwa <- interactome(nwa, species = "Hs", genetic = FALSE)  # downloads BioGRID
nwa <- analyze(nwa, fdr = 0.001, species = "Hs")
viewSubNet(nwa)
```

### Time series

`GSCABatch` and `NWABatch` apply the same workflow across several time points;
`preprocessGscaTS()`, `analyzeGscaTS()`, `preprocessNwaTS()`, `interactomeNwaTS()`
and `analyzeNwaTS()` operate on the batch objects, and `reportAll()` produces a
report with a time slider.

## Parallel execution

Permutation-based GSEA runs through [BiocParallel](https://bioconductor.org/packages/BiocParallel).
Register a backend once and every analysis call uses it:

```r
BiocParallel::register(BiocParallel::MulticoreParam(workers = 4))  # macOS / Linux
BiocParallel::register(BiocParallel::SnowParam(workers = 4))       # Windows
```

Serial and parallel backends return identical results when the backend is
seeded, which is covered by the test suite.

## Output and export

- **Result tables** live in the `result` slot and are reached through
  `getResult()`, `getSummary()` and `getTopGeneSets()`.
- **GSEA figures**: `viewGSEA()` draws to the current device, and `plotGSEA()`
  writes PDF or PNG files for the top gene sets in one call.
- **Enrichment maps and subnetworks**: `viewEnrichMap()` and `viewSubNet()`
  return interactive widgets, which `saveNetwork()` writes to disk from a
  script:

  ```r
  map <- viewEnrichMap(gsca, gscs = "PW_KEGG", gsNameType = "term")

  saveNetwork(map, "enrichment-map.html")                            # interactive
  saveNetwork(map, "enrichment-map.png", width = 1200, height = 900) # publication
  ```

  PNG output needs the optional [`webshot2`](https://rstudio.github.io/webshot2/)
  package; the HTML output has no extra requirement.
- **Result tables as files**: every table in the report can be exported to
  CSV/TSV/PDF from its toolbar, or written directly:

  ```r
  d <- getResult(gsca)$GSEA.results$PW_KEGG
  write.table(data.frame(Gene.Set = rownames(d), d),
              "top_genesets.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
  ```
- **Reports**: `report()` (single objects) and `reportAll()` (single or
  time-series objects) write a self-contained Shiny application to a directory
  and launch it. Every graph offers pan/zoom, hover details and PNG export.

## Dependency layers

**Imports** — required for normal use:

`GO.db`, `Rcpp`, `igraph`, `BioNet`, `DT`, `shiny`, `bslib`, `colourpicker`,
`visNetwork`, `KEGGREST`, `data.table`, `AnnotationDbi`, `BiocParallel`,
`fgsea`, `msigdbr`, plus base `methods`, `graphics`, `grDevices`, `stats` and
`utils`.

**Suggests** — optional paths and tooling:

`cellHTS2` (legacy `cellHTS2OutputStatTests()`), `RankProd` (the optional
`tests = "RankProduct"` branch), `webshot2` (PNG export through
`saveNetwork()`), `BiocStyle`, `rmarkdown`, `knitr`, `testthat`, `org.Hs.eg.db`,
  `Biobase`, `limma`, `TxDb.Hsapiens.UCSC.hg19.knownGene`.

`cellHTS2` has been removed from Bioconductor, so `cellHTS2OutputStatTests()`
is only available if you install that package from an archive; it is never
needed to install or use the rest of HTSanalyzeR2.

## Implementation notes

- Interactive graphs are rendered with **visNetwork** (vis.js). Older releases
  bundled a hand-maintained copy of Sigma/linkurious.js and jQuery; that bundle
  is gone, so the package no longer ships a GPLv3 JavaScript payload.
- The report UI is built with **bslib** (Bootstrap 5) and standard Shiny inputs,
  with one namespaced settings panel per graph.
- Rank Product is the only non-FOSS dependency, and it is optional: it is
  required only when you ask for `tests = "RankProduct"`.
- BioGRID downloads track the current `Latest-Release` archive instead of a
  pinned release from 2016.

## Troubleshooting

On Linux, some dependencies need system libraries:

```bash
sudo apt-get install -y libssl-dev libcurl4-openssl-dev libxml2-dev libgmp-dev libmpfr-dev
```

- `libssl-dev` for `git2r`/`openssl`, `libcurl4-openssl-dev` for `curl`.
- `libxml2-dev` for `igraph`.
- `libgmp-dev` and `libmpfr-dev` only if you install the optional `RankProd`
  (which depends on `Rmpfr`).

Vignettes additionally need a LaTeX installation and `BiocStyle`; install them
with `BiocManager::install("BiocStyle")` plus TinyTeX
(`tinytex::install_tinytex()`).

## Getting help

Open an [issue](https://github.com/CityUHK-CompBio/HTSanalyzeR2/issues) for bugs
and feature requests, or contact the maintainer listed in `DESCRIPTION`.

## Licence

Apache License 2.0 — see [LICENSE.md](LICENSE.md).
