# HTSanalyzeR2 modernization baseline

This record is the scope and acceptance baseline for the "Modernization Baseline" milestone.

## Scope

The milestone covers only:

- recording the public API and result contracts,
- adding a minimal regression-test skeleton,
- auditing and aligning dependency declarations with observed calls,
- fixing already-observed current-runtime compatibility warnings,
- adding the build artifacts to `.gitignore`,
- recording known report-layer issues for a later phase.

It does not change algorithms, public API names, result field semantics, Shiny behavior, or vignettes in this phase.

## Public contract

The package exposes a legacy S4 API. The stable ordinary exports are `GSCA`, `GSCABatch`, `NWA`, `NWABatch`, `HTSanalyzeR2Pipe`, `HTSanalyzeR4MAGeCK`, `GOGeneSets`, `KeggGeneSets`, `MSigDBGeneSets`, `annotationConvertor`, `duplicateRemover`, `reportAll`, `cellHTS2OutputStatTests`, `preprocessGscaTS`, `analyzeGscaTS`, `appendGSTermsTS`, `preprocessNwaTS`, `interactomeNwaTS`, and `analyzeNwaTS`.

The stable S4 method surface is `analyze`, `appendGSTerms`, `extractEnrichMap`, `extractSubNet`, `getInteractome`, `getPara`, `getResult`, `getSummary`, `getTopGeneSets`, `interactome`, `plotGSEA`, `preprocess`, `report`, `summarize`, `viewEnrichMap`, `viewGSEA`, and `viewSubNet`.

The stable classes are `GSCA`, `NWA`, `GSCABatch`, and `NWABatch`. The GSCA result list is keyed by `HyperGeo.results`, `GSEA.results`, `Sig.pvals.in.both`, and `Sig.adj.pvals.in.both`, with one entry per gene-set collection. The GSOA table uses the existing `Universe Size` through `Overlap.Gene` fields. The GSEA table uses `Observed.score`, `Pvalue`, `Adjusted.Pvalue`, and `Leading.Edge`. The NWA result list uses `subnw` and `labels`.

## Runtime evidence

- R 4.6.0, Bioconductor 3.23, and aarch64 macOS were used for this baseline.
- Observed package versions include `igraph` 2.3.3, `shiny` 1.14.0, `fgsea` 1.38.0, `msigdbr` 26.1.1, `BioNet` 1.72.0, `cellHTS2` 2.68.0, `testthat` 3.3.2, and `BiocParallel` 1.46.0.
- The prior installation-only check succeeded; it did not establish full functional or release readiness.
- Before this milestone, `R CMD check --no-manual --no-build-vignettes` had two vignette warnings about missing `inst/doc` and package vignettes without corresponding built output.
- Example execution exposed the deprecated `msigdbr(category=...)` interface.
- igraph 2.x reports `graph.adjacency()` as deprecated.
- `HTSanalyzeR2Pipe()` calls `doParallel::registerDoParallel()` directly.
- The built-in igraph data objects are serialized with an older igraph version and are converted on load by igraph 2.x.

## Changes in this milestone

- `MSigDBGeneSets()` now calls `msigdbr(collection=..., subcollection=...)` instead of the deprecated `category`/`subcategory` pair.
- The two enrichment-map igraph constructions now use `graph_from_adjacency_matrix()` instead of deprecated `graph.adjacency()`.
- `doParallel` moved from `Suggests` to `Imports` because `HTSanalyzeR2Pipe()` calls it directly.
- README now records the current R/Bioconductor evidence and the actual runtime dependency layer.
- `HTSanalyzeR2.Rcheck/` and `*.tar.gz` are ignored.
- Vignette YAML now pins `BiocStyle::pdf_document` with `pdflatex`; `vignettes/header.tex`
  supplies Pandoc 3.9/LaTeX compatibility shims. Both vignettes compile to PDF locally.
- KEGG-dependent Rd examples are now wrapped in `\dontrun`; they required a live KEGG
  connection and previously caused the package examples check to fail in an offline or
  rate-limited environment.
- A `tests/testthat` skeleton now exercises the GSCA and NWA object contracts, GSOA/GSEA result fields, NWA/BioNet output, batch packaging, public exports, dependency declarations, built-in object dimensions, and the non-deprecated MSigDB call.

## Known issues deferred from this milestone

- A final focused check readback after the baseline documentation and small class-check cleanup
  still reports `3 WARNINGs, 3 NOTEs` and no ERROR. Two warnings are the expected
  Bioconductor/CRAN packaging observations for this 0.99.x source layout: non-mainstream
  dependencies and missing prebuilt `inst/doc`. A third warning records non-portable
  BioGRID filenames when the legacy generated report directory is included; its long-term
  fix is to stop packaging generated report inputs (later report-layer phase).
- The check NOTEs are now two long Rd usage lines and an environment-local `00_pkg_src`
  check-directory artifact; the class-vs-string comparisons have been replaced by `inherits()`
  checks. None block installation or tests.
- `BiocParallel` is imported for documentation but the runtime still uses `foreach`; whether to replace or keep it is a later algorithm/parallelism decision.
- `cellHTS2` remains an optional runtime path in `Suggests`; its missing-package error and alternative path need later validation.
- `inst/templates/app.R` is a 328-line single-file Shiny application with module globals and mixed preparation, UI, and server logic.
- The report template contains an explicit `TODO: undefined behavior` near the NWA first-render observer.
- Additional unresolved TODO markers exist in `R/gsca_enrichmap.R`, `R/gsca_report.R`, and `R/nwa_view.R`.
- The bundled Sigma/jQuery report assets need license, provenance, supply-chain, and browser-compatibility review.
- Report generation currently has no tested headless smoke path or non-Shiny export path.
- The workflow in `.github/workflows/R-CMD-check.yaml` defines a multi-version matrix, but
  it has not been proven on GitHub because this milestone has not yet been pushed.

## Acceptance status

- [x] Public API and contract inventory recorded.
- [x] Minimal test skeleton added.
- [x] Dependency declaration audit completed for observed direct calls.
- [x] Observed current-runtime compatibility fixes applied.
- [x] Build artifact policy recorded.
- [x] Full clean-environment installation and `R CMD check --as-cran` readback. 48 tests pass,
  0 fail, and 1 known BioNet/igraph-2.x incompatibility is skipped.
- [ ] Multi-version CI proven.
- [x] Both vignettes compile locally to PDF. The remaining `inst/doc` warning is a package
  build-policy decision deferred to the documentation/release phase.

## Phase 2 — 2026 stack, licence and report layer

Completed after the baseline, keeping the S4 API and every result field unchanged:

- **Licence**: the package is now Apache License 2.0. The GPLv3 Sigma/linkurious.js
  payload that made the previous declaration questionable is gone.
- **Interactive graphs**: `viewEnrichMap()`/`viewSubNet()` render through `visNetwork`;
  `R/d3plot.R` and `inst/htmlwidgets/forceGraph/` (about 1200 lines of vendored JS and
  jQuery) were removed in favour of `R/network_widget.R`.
- **Report**: `inst/templates/app.R` was rewritten for `bslib` (Bootstrap 5) with
  namespaced Shiny settings panels; `inst/templates/settings.html` was removed.
  `reportAll()` now delegates to the new `prepareReport()`, which assembles a report
  directory without launching Shiny and validates the report runtime up front.
- **Dependencies**: dropped `foreach`, `doParallel`, `stringr`, `htmlwidgets` and
  `shinydashboard`; added `visNetwork` and `bslib`; `RankProd` became optional.
- **Real defects fixed**: `igraph` 2.x `as_data_frame(x, "edge")` broke
  `viewEnrichMap()`/`viewSubNet()` outright; BioGRID links were pinned to 2016/2021
  releases; the report's network tab relied on an observer driven by a constant
  expression; duplicate Shiny input ids across graph panels broke the client bindings.
- **Verification**: the report was launched against a real browser session — table,
  value boxes, enrichment map, subnetwork, legend, hover tooltips and PNG export all
  render. Tests cover the widget contract, the time-series columns and the absence of
  vendored JavaScript.

`R CMD check --as-cran` now reports `1 WARNING` and no NOTEs or ERRORs. The warning is the
advisory "significant size reductions" note for the two vignette PDFs, which is a
figure-resolution question rather than a packaging defect.

## Phase 3 — solver, reproducibility and legacy decoupling

- **`analyze()` on `NWA` no longer aborts.** BioNet's FastHeinz solver takes the minimum
  spanning tree of an edgeless internal subgraph and igraph 2.x rejects the zero-length
  weight vector, so small or sparse networks failed outright. `identifySubnetwork()` keeps
  calling the exact solver and falls back to a greedy maximum-scoring connected subgraph
  only for that specific error, with a warning. BioNet is GPL-2, so the fallback is written
  against the igraph API rather than copied from BioNet.
- **Network analysis is reproducible.** `BioNet::fitBumModel()` draws starting values with
  `runif()` and keeps the last successful fit instead of the best one, so one fixed input
  produced subnetwork sizes of 118, 119 and 121 across runs. `fitBumModelStable()` searches
  a fixed set of seeds, keeps the best fit by negative log-likelihood, and restores the
  caller's RNG stream. The `d7_nwa` example is now 119 nodes / 464 edges on every run.
- **Screen statistics no longer need `cellHTS2`.** `screenStatTests()` works on a matrix,
  an annotation vector and a sample/control labelling; `cellHTS2OutputStatTests()` is a thin
  adapter. Verified identical to the pre-modernization implementation across 3 alternatives
  × 7 test combinations.
- **Regression fixed.** The earlier internal rewrite had dropped the construct identifiers
  from the row names of the statistics result. It was caught by comparing against the
  pristine implementation, not against an already-modified one.

Test count is 116 with no failures and no skips; the previously skipped BioNet test now runs.

## Phase 4 — exact fallback solver

The Phase 3 fallback for the igraph 2.x failure was a greedy search written in this package. That
put the correctness of a scientifically meaningful step in local hands, so it was replaced by an
exact mixed-integer formulation of the same maximum-weight connected subgraph problem, solved by
`lpSolve`.

- **Model**: one binary variable per node, one root marker, one continuous flow per arc, and the
  constraint that the root supplies one unit of flow to every other selected node. Flow may only
  use arcs whose endpoints are both selected, so feasible solutions are connected and maximising
  the node score makes them optimal. The formulation is standard; correctness rests on the solver.
- **Verified**: against exhaustive enumeration on 132 random graphs (0 mismatches) and against
  BioNet's FastHeinz. On 4 of 26 larger graphs the exact solver returned a strictly better module
  than FastHeinz, which pre-filters negative nodes with a greedy test. FastHeinz therefore stays the
  default so existing results do not move, and the exact solver runs only where FastHeinz aborts.
- **Reductions**: components without a positively scored node are dropped, and non-positive leaves
  are stripped iteratively. Both preserve the optimum.
- **Limits**: the solver is exact when it returns; above the configured size or time budget it stops
  with an actionable message. Practically it solves instances up to roughly 200-300 nodes within
  the default 60 second budget.
- **Note on the tooling**: `lpSolveAPI` was tried first and produced different models on repeated
  calls in one session. The `lpSolve` interface to the same solver has no handle pool and is stable,
  which is why the package uses it.
