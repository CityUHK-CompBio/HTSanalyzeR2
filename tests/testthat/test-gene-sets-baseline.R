test_that("MSigDB uses the non-deprecated collection interface", {
  expect_silent(MSigDBGeneSets(species = "Hs", collection = "H", subcategory = NULL))
})
