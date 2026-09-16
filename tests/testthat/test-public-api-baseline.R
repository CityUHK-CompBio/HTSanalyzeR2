test_that("exported GSCA and NWA method surface remains intact", {
  exported <- getNamespaceExports("HTSanalyzeR2")
  functions <- c(
    "GSCA", "GSCABatch", "NWA", "NWABatch", "analyzeGscaTS", "analyzeNwaTS",
    "annotationConvertor", "appendGSTermsTS", "screenStatTests",
    "duplicateRemover", "interactomeNwaTS", "preprocessGscaTS", "preprocessNwaTS",
    "reportAll", "GOGeneSets", "KeggGeneSets", "MSigDBGeneSets", "HTSanalyzeR2Pipe",
    "HTSanalyzeR4MAGeCK"
  )
  methods <- c(
    "analyze", "appendGSTerms", "extractEnrichMap", "extractSubNet", "getInteractome",
    "getPara", "getResult", "getSummary", "getTopGeneSets", "interactome", "plotGSEA",
    "preprocess", "report", "summarize", "viewEnrichMap", "viewGSEA", "viewSubNet"
  )
  expect_true(all(functions %in% exported))
  expect_true(all(methods %in% exported))
})
