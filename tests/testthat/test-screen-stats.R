## Fixture: four constructs with two wells each (each well measured on two
## replicate plates), plus a small control population. Row names are well names
## and the construct identity lives in the annotation vector, exactly like a
## cellHTS plate: rows sharing an annotation are replicates of one construct.
make_screen_fixture <- function() {
  data <- matrix(
    c(0.10, 0.11,   ## geneA well 1
      0.12, 0.13,   ## geneA well 2
      0.40, 0.41,   ## geneB well 1
      0.42, 0.43,   ## geneB well 2
      0.20, 0.21,   ## geneC well 1
      0.22, 0.23,   ## geneC well 2
      0.30, 0.31,   ## geneD well 1
      0.32, 0.33,   ## geneD well 2
      0.31, 0.32,   ## negative control well 1
      0.29, 0.30),  ## negative control well 2
    ncol = 2, byrow = TRUE,
    dimnames = list(paste0("well", 1:10), c("plate1", "plate2"))
  )
  list(
    data = data,
    annotation = rep(c("geneA", "geneB", "geneC", "geneD", "neg"), each = 2),
    controlStatus = c(rep("sample", 8), "neg", "neg")
  )
}

test_that("screenStatTests needs no cellHTS2 object", {
  fixture <- make_screen_fixture()
  result <- screenStatTests(fixture$data, fixture$annotation,
                            fixture$controlStatus, tests = "T-test")

  expect_true(is.matrix(result))
  expect_equal(rownames(result), c("geneA", "geneB", "geneC", "geneD"))
  expect_equal(colnames(result),
               c("t.test.pvalues.one.sample", "t.test.pvalues.two.samples"))
})

test_that("screenStatTests reproduces the underlying tests exactly", {
  fixture <- make_screen_fixture()
  result <- screenStatTests(fixture$data, fixture$annotation,
                            fixture$controlStatus, tests = "T-test",
                            alternative = "two.sided")

  control <- as.vector(fixture$data[fixture$controlStatus == "neg", ])
  mu <- stats::median(as.vector(fixture$data[fixture$controlStatus == "sample", ]),
                      na.rm = TRUE)
  ## both wells annotated "geneA" are replicates of that construct
  geneA <- as.vector(fixture$data[c("well1", "well2"), ])

  expect_equal(unname(result["geneA", "t.test.pvalues.one.sample"]),
               stats::t.test(geneA, mu = mu, alternative = "two.sided")$p.value)
  expect_equal(unname(result["geneA", "t.test.pvalues.two.samples"]),
               stats::t.test(geneA, y = control, alternative = "two.sided")$p.value)
})

test_that("screenStatTests supports several tests and alternatives", {
  fixture <- make_screen_fixture()

  both <- screenStatTests(fixture$data, fixture$annotation, fixture$controlStatus,
                          tests = c("T-test", "MannWhitney"))
  expect_equal(colnames(both),
               c("t.test.pvalues.one.sample", "t.test.pvalues.two.samples",
                 "mannW.test.pvalues.one.sample", "mannW.test.pvalues.two.samples"))

  greater <- screenStatTests(fixture$data, fixture$annotation, fixture$controlStatus,
                             tests = "MannWhitney", alternative = "greater")
  less <- screenStatTests(fixture$data, fixture$annotation, fixture$controlStatus,
                          tests = "MannWhitney", alternative = "less")
  expect_true(all(greater >= 0 & greater <= 1))
  expect_true(all(less >= 0 & less <= 1))
  ## the alternative actually changes the result
  expect_false(isTRUE(all.equal(greater, less)))
})

test_that("rows without an annotation are dropped", {
  fixture <- make_screen_fixture()
  ## both wells of geneA lose their identifier, so that construct disappears
  fixture$annotation[1:2] <- NA
  result <- screenStatTests(fixture$data, fixture$annotation,
                            fixture$controlStatus, tests = "T-test")
  expect_false("geneA" %in% rownames(result))
  expect_true("geneB" %in% rownames(result))
})

test_that("screenStatTests validates its inputs", {
  fixture <- make_screen_fixture()

  expect_error(screenStatTests(as.data.frame(fixture$data), fixture$annotation,
                               fixture$controlStatus),
               "must be a numeric matrix")
  expect_error(screenStatTests(fixture$data, fixture$annotation[1:2],
                               fixture$controlStatus),
               "one identifier per row")
  expect_error(screenStatTests(fixture$data, fixture$annotation,
                               fixture$controlStatus[1:2]),
               "one label per row")
  expect_error(screenStatTests(fixture$data, fixture$annotation,
                               fixture$controlStatus, controls = "missing"),
               "does not match any value")
})

test_that("screenStatTests is deterministic", {
  fixture <- make_screen_fixture()
  first <- screenStatTests(fixture$data, fixture$annotation,
                           fixture$controlStatus, tests = c("T-test", "MannWhitney"))
  second <- screenStatTests(fixture$data, fixture$annotation,
                            fixture$controlStatus, tests = c("T-test", "MannWhitney"))
  expect_identical(first, second)
})

test_that("the cellHTS2 adapter is optional and explains itself", {
  desc <- read.dcf(
    system.file("DESCRIPTION", package = "HTSanalyzeR2"),
    fields = c("Imports", "Suggests")
  )
  imports <- unlist(strsplit(desc[1, "Imports"], ",\\s*"))
  suggests <- unlist(strsplit(desc[1, "Suggests"], ",\\s*"))

  expect_false("cellHTS2" %in% imports)
  expect_false("Biobase" %in% imports)

  if (!requireNamespace("cellHTS2", quietly = TRUE)) {
    expect_error(cellHTS2OutputStatTests(NULL), "no longer part of Bioconductor")
  }
})
