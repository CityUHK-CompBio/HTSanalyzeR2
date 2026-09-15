test_that("the network widget is a visNetwork htmlwidget", {
  skip_if_not_installed("visNetwork")

  nodes <- data.frame(
    name = c("a", "b", "c"),
    geneSetSize = c(5, 10, 15),
    adjPvalue = c(0.01, 0.02, 0.03),
    colorScheme = c("pos", "pos", "neg"),
    label = c("a", "b", "c"),
    label_id = c("a", "b", "c"),
    label_term = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  links <- data.frame(from = c("a", "b"), to = c("b", "c"), weight = c(1, 2))
  mappings <- list(id = "name", size = "geneSetSize", color = "adjPvalue",
                   scheme = "colorScheme", label = "label", label_id = "label_id",
                   label_term = "label_term")

  widget <- HTSanalyzeR2:::forceGraph(
    nodes, links, mappings, list(source = "from", target = "to", weight = "weight"),
    list(colorScaler = "log10", label = list(text = "id"))
  )

  expect_s3_class(widget, "visNetwork")
  expect_s3_class(widget, "htmlwidget")
  expect_equal(nrow(widget$x$nodes), 3)
  expect_equal(nrow(widget$x$edges), 2)
  ## the palette is applied per scheme
  expect_false(widget$x$nodes$color.background[1] == widget$x$nodes$color.background[3])
})

test_that("widgets carry the data needed for in-place updates", {
  nodes <- data.frame(
    name = c("a", "b"),
    diff = c(-2, 3),
    colorScheme = c("neg", "pos"),
    label = c("a", "b"),
    stringsAsFactors = FALSE
  )
  mappings <- list(id = "name", color = "diff", scheme = "colorScheme",
                   label = "label", label_id = "name", label_term = "label")

  widget <- HTSanalyzeR2:::forceGraph(nodes, nodes[0, ], mappings, list(), list())

  expect_false(is.null(attr(widget, "htsNodes")))
  expect_true(all(c("id", "color", "scheme") %in% colnames(attr(widget, "htsNodes"))))
})

test_that("time-series widgets can be re-pointed at another tick", {
  nodes <- data.frame(
    name = c("a", "b"),
    adjPvalue = c(0.01, 0.02),
    colorScheme = c("pos", "pos"),
    label = c("a", "b"),
    adjPvalue.t2 = c(0.5, 0.9),
    colorScheme.t2 = c("pos", "pos"),
    stringsAsFactors = FALSE
  )
  mappings <- list(id = "name", color = "adjPvalue", scheme = "colorScheme",
                   label = "label", label_id = "name", label_term = "label",
                   color.t2 = "adjPvalue.t2", scheme.t2 = "colorScheme.t2")

  widget <- HTSanalyzeR2:::forceGraph(nodes, nodes[0, ], mappings, list(), list(),
                                      seriesData = "t2")
  stored <- attr(widget, "htsNodes")

  ## the second tick exists as its own column pair and is selectable
  expect_true(all(c("color.t2", "scheme.t2") %in% colnames(stored)))
  expect_false(identical(stored$color, stored$color.t2))
})

test_that("the package ships no hand-maintained JavaScript bundles", {
  widget_dir <- system.file("htmlwidgets", package = "HTSanalyzeR2")
  expect_false(dir.exists(widget_dir))

  files <- list.files(system.file(package = "HTSanalyzeR2"), recursive = TRUE)
  expect_false(any(grepl("sigma|linkurious|jquery", files, ignore.case = TRUE)))
})

test_that("saveNetwork writes interactive HTML", {
  nodes <- data.frame(
    name = c("a", "b"),
    diff = c(-2, 3),
    colorScheme = c("neg", "pos"),
    label = c("a", "b"),
    stringsAsFactors = FALSE
  )
  mappings <- list(id = "name", color = "diff", scheme = "colorScheme",
                   label = "label", label_id = "name", label_term = "label")
  widget <- HTSanalyzeR2:::forceGraph(nodes, nodes[0, ], mappings, list(), list())

  html <- tempfile(fileext = ".html")
  on.exit(unlink(html), add = TRUE)

  expect_silent(saveNetwork(widget, html))
  expect_true(file.exists(html))
  expect_gt(file.size(html), 1000)
})

test_that("saveNetwork validates its inputs", {
  nodes <- data.frame(name = "a", diff = 1, colorScheme = "pos", label = "a",
                      stringsAsFactors = FALSE)
  mappings <- list(id = "name", color = "diff", scheme = "colorScheme",
                   label = "label", label_id = "name", label_term = "label")
  widget <- HTSanalyzeR2:::forceGraph(nodes, nodes[0, ], mappings, list(), list())

  expect_error(saveNetwork(list(1), tempfile(fileext = ".png")), "htmlwidget")
  expect_error(saveNetwork(widget, tempfile(fileext = ".svg")),
               "must end in")
})

test_that("saveNetwork writes a PNG when webshot2 is available", {
  skip_if_not_installed("webshot2")

  nodes <- data.frame(
    name = c("a", "b", "c"),
    geneSetSize = c(5, 10, 15),
    adjPvalue = c(0.01, 0.02, 0.03),
    colorScheme = c("pos", "pos", "neg"),
    label = c("a", "b", "c"),
    label_id = c("a", "b", "c"),
    label_term = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  links <- data.frame(from = c("a", "b"), to = c("b", "c"), weight = c(1, 2))
  mappings <- list(id = "name", size = "geneSetSize", color = "adjPvalue",
                   scheme = "colorScheme", label = "label", label_id = "label_id",
                   label_term = "label_term")

  widget <- HTSanalyzeR2:::forceGraph(
    nodes, links, mappings,
    list(source = "from", target = "to", weight = "weight"),
    list(colorScaler = "log10", label = list(text = "id"))
  )

  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)

  ## a graph with three nodes needs no long stabilization; webshot2 reports
  ## its progress, so only hard output is suppressed here
  expect_silent(
    suppressMessages(saveNetwork(widget, png, width = 600, height = 400, delay = 1))
  )
  expect_true(file.exists(png))
  expect_gt(file.size(png), 1000)
})
