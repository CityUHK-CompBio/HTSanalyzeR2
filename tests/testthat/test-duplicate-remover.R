test_that("duplicateRemover supports every documented method", {
  x <- setNames(c(2, 3, -1.5, 0.5, 4, -6), c("a", "a", "b", "b", "c", "c"))

  expect_equal(duplicateRemover(x, "max"), c(a = 3, b = -1.5, c = -6))
  expect_equal(duplicateRemover(x, "min"), c(a = 2, b = 0.5, c = 4))
  expect_equal(duplicateRemover(x, "average"), c(a = 2.5, b = -0.5, c = -1))
  expect_silent(duplicateRemover(x, "fc.avg"))
})

test_that("the fold change average method is reachable under both spellings", {
  ## fold changes below 1 are averaged as ratios and converted back; fold
  ## changes of -2 and -4 become the ratios 0.5 and 0.25
  x <- setNames(c(2, 4, -2, -4), c("a", "a", "b", "b"))

  short <- duplicateRemover(x, "fc.avg")
  long <- duplicateRemover(x, "fold.change.average")

  expect_equal(short, long)
  expect_equal(unname(short["a"]), 3)
  expect_equal(unname(short["b"]), -1 / mean(c(0.5, 0.25)))
})

test_that("duplicateRemover keeps the documented result order and names", {
  x <- setNames(c(1, 2, 3), c("zeta", "alpha", "zeta"))
  result <- duplicateRemover(x, "max")

  ## order of first appearance, not alphabetical
  expect_equal(names(result), c("zeta", "alpha"))
  expect_equal(unname(result), c(3, 2))
})

test_that("duplicateRemover rejects unknown methods", {
  x <- setNames(c(1, 2), c("a", "b"))
  expect_error(duplicateRemover(x, "median"), "duplicateRemoverMethod")
})

test_that("duplicateRemover scales to a genome-sized gene list", {
  set.seed(11)
  n <- 20000
  x <- setNames(rnorm(n), sample(paste0("gene", seq_len(n)), n, replace = TRUE))

  elapsed <- system.time(duplicateRemover(x, "max"))[["elapsed"]]
  ## generous bound: the previous quadratic implementation took ~100s here
  expect_lt(elapsed, 10)
})
