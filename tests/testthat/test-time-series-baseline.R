test_that("GSCABatch packages time-series phenotypes", {
  gl <- make_small_gene_list()
  phenotypes <- list(gl, gl * 2)
  expInfor <- matrix(
    c("t0", "control", "t1", "treated"),
    nrow = 2,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("ID", "Description"))
  )
  object <- GSCABatch(
    expInfor = expInfor,
    listOfGeneSetCollections = list(example = list(gs1 = names(gl)[1:10])),
    phenotypeTS = phenotypes
  )
  expect_s4_class(object, "GSCABatch")
  expect_identical(names(object@phenotypeTS), c("t0", "t1"))
  expect_length(object@listOfGSCA, 2)
})

test_that("NWABatch packaging preserves experiment order", {
  gl <- make_small_gene_list()
  pvalues <- setNames(abs(gl) / 20, names(gl))
  expInfor <- matrix(
    c("t0", "control", "t1", "treated"),
    nrow = 2,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("ID", "Description"))
  )
  object <- NWABatch(
    expInfor = expInfor,
    pvalueTS = list(pvalues, pvalues),
    phenotypeTS = list(gl, gl)
  )
  expect_s4_class(object, "NWABatch")
  expect_identical(names(object@pvalueTS), c("t0", "t1"))
  expect_length(object@listOfNWA, 2)
})
