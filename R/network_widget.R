## Interactive network rendering.
##
## Earlier releases shipped a hand-maintained copy of Sigma/linkurious.js plus
## jQuery inside `inst/htmlwidgets/forceGraph`. That bundle was GPLv3, was never
## updated, and blocked a modern licence declaration. The widget is now built on
## the maintained 'visNetwork' htmlwidget (vis.js), which keeps the same visual
## contract: node size, dual positive/negative colour scales with a legend,
## label selection, and time-series updates.

## The Shiny application written by reportAll() renders its layout with
## 'bslib', its tables with 'DT' and its colour pickers with 'colourpicker'.
## Importing them here keeps the report runtime part of the declared
## dependencies instead of an undeclared, silently optional extra.
#' @import bslib
#' @import colourpicker
#' @import DT
NULL

## ---- value helpers ---------------------------------------------------------

## Normalize values into [minValue, maxValue]. NA values are preserved; a
## constant or empty input falls back to 'defaultValue'.
norm <- function(arr, minValue, maxValue, defaultValue) {
  ran <- suppressWarnings(range(arr, na.rm = TRUE))
  if (!all(is.finite(ran)) || ran[1] == ran[2]) {
    arr[!is.na(arr)] <- defaultValue
    return(arr)
  }
  tmp <- (arr - ran[1]) / (ran[2] - ran[1])
  tmp * (maxValue - minValue) + minValue
}

## Colour domain per scheme ("pos"/"neg") across the current tick and, for
## time-series data, across every tick, so the scale stays stable while the
## slider moves.
niceDomain <- function(nodesDF, seriesData) {
  color <- nodesDF$color
  scheme <- nodesDF$scheme
  if (!is.null(seriesData)) {
    for (tick in seriesData) {
      color <- c(color, nodesDF[, paste("color", tick, sep = ".")])
      scheme <- c(scheme, nodesDF[, paste("scheme", tick, sep = ".")])
    }
  }

  domains <- list()
  for (sch in unique(scheme[!is.na(scheme)])) {
    values <- color[!is.na(scheme) & scheme == sch]
    ran <- suppressWarnings(range(values, na.rm = TRUE))
    if (!all(is.finite(ran))) next
    if (ran[1] == ran[2]) {
      ran[2] <- ran[2] + 0.05
    }
    domains[[sch]] <- round(ran, 3)
  }
  domains
}

hexToRgb <- function(hex) {
  hex <- sub("^#", "", hex)
  strtoi(substring(hex, c(1, 3, 5), c(2, 4, 6)), 16L)
}

rgbToHex <- function(rgb) {
  rgb <- pmax(pmin(round(rgb), 255), 0)
  sprintf("#%02X%02X%02X", as.integer(rgb[1]), as.integer(rgb[2]), as.integer(rgb[3]))
}

## Interpolate between two hex colours; 'factor' runs from 0 (from) to 1 (to).
interpolateHex <- function(from, to, factor) {
  factor <- pmin(pmax(factor, 0), 1)
  fromRgb <- hexToRgb(from)
  toRgb <- hexToRgb(to)
  rgbToHex(fromRgb + factor * (toRgb - fromRgb))
}

## Map values onto a two-colour palette. "log10" replicates the enrichment-map
## convention where small adjusted p-values get the saturated end of the scale.
paletteColor <- function(value, domain, range, scaler = "linear", opacity = 1) {
  if (length(value) == 0) return(character(0))

  if (identical(scaler, "log10") && length(domain) == 2 && domain[1] > 0) {
    bound <- -log10(domain[1])
    factor <- if (!is.finite(bound) || bound == 0) 1 else (-log10(value)) / bound
    factor <- pmin(factor, 1)
    factor <- 1 - factor
  } else {
    span <- domain[2] - domain[1]
    factor <- if (!is.finite(span) || span == 0) 0 else (value - domain[1]) / span
    factor <- pmin(pmax(factor, 0), 1)
  }

  hex <- vapply(factor, function(f) {
    if (is.na(f)) NA_character_ else interpolateHex(range[1], range[2], f)
  }, character(1))

  alpha <- substr(sprintf("%02X", as.integer(round(pmin(pmax(opacity, 0), 1) * 255))), 1, 2)
  ifelse(is.na(hex), NA_character_, paste0(hex, alpha))
}

## ---- style ----------------------------------------------------------------

## Defaults mirror the values the previous JavaScript settings panel shipped.
forceGraphStyle <- function(options = list()) {
  style <- list(
    node = list(
      size = list(min = 3, max = 20, default = 4),
      scale = 1,
      opacity = 0.8,
      borderWidth = 1,
      borderColor = "#DB1F51",
      naColor = "#EDEDED",
      labelScale = 1,
      labelColor = "#0FAABB"
    ),
    edge = list(
      scale = 1,
      color = "#0099FF"
    ),
    scheme = list(
      pos = list(range = c("#9E1617", "#FFF3F3")),
      neg = list(range = c("#006A9C", "#F3F3FF"))
    ),
    physics = list(
      gravity = 10,
      linLogMode = FALSE,
      strongGravityMode = FALSE,
      outboundAttraction = FALSE,
      adjustSizes = FALSE,
      barnesHutOptimize = FALSE
    )
  )
  ## Deep-merge so callers can override a single nested value (for example
  ## only 'node$opacity') without losing the remaining defaults.
  for (section in intersect(names(options), names(style))) {
    if (is.list(style[[section]]) && is.list(options[[section]])) {
      style[[section]] <- modifyList(style[[section]], options[[section]])
    }
  }
  style
}

## ---- data frames ----------------------------------------------------------

## Translate the package's generic node/link frames into vis.js columns.
prepareGraphNodes <- function(nodesDF, options, style, suffix = "") {
  pick <- function(base) {
    column <- paste0(base, suffix)
    if (!is.null(nodesDF[[column]])) nodesDF[[column]] else nodesDF[[base]]
  }

  size <- pick("size")
  if (is.null(size)) size <- style$node$size$default
  size <- norm(size, style$node$size$min, style$node$size$max,
               style$node$size$default) * style$node$scale

  color <- pick("color")
  scheme <- pick("scheme")
  domains <- attr(nodesDF, "colorDomains")

  background <- rep(NA_character_, length(color))
  if (!is.null(domains)) {
    for (sch in names(domains)) {
      isSch <- !is.na(scheme) & scheme == sch
      if (!any(isSch)) next
      background[isSch] <- paletteColor(
        color[isSch], domains[[sch]],
        style$scheme[[sch]]$range,
        options$colorScaler %||% "linear",
        style$node$opacity
      )
    }
  }
  background[is.na(background)] <- paste0(style$node$naColor, "E5")

  labelText <- options$label$text %||% "id"
  label <- switch(
    labelText,
    "none" = rep("", nrow(nodesDF)),
    "term" = as.character(nodesDF$label_term %||% nodesDF$label),
    as.character(nodesDF$label %||% nodesDF$id)
  )

  visNodes <- data.frame(
    id = as.character(nodesDF$id),
    label = label,
    value = as.numeric(size),
    stringsAsFactors = FALSE
  )
  visNodes$color.background <- background
  visNodes$color.border <- style$node$borderColor
  visNodes$color.highlight.background <- background
  visNodes$color.highlight.border <- style$node$borderColor
  visNodes$borderWidth <- style$node$borderWidth
  visNodes$font.size <- 14 * style$node$labelScale
  visNodes$font.color <- style$node$labelColor
  visNodes$title <- graphNodeTooltip(nodesDF, options)
  visNodes
}

prepareGraphEdges <- function(linksDF, style) {
  if (nrow(linksDF) == 0) {
    return(data.frame(from = character(0), to = character(0)))
  }
  weight <- linksDF$weight
  if (is.null(weight)) weight <- 1
  visEdges <- data.frame(
    from = as.character(linksDF$source),
    to = as.character(linksDF$target),
    width = as.numeric(weight) * style$edge$scale,
    color = style$edge$color,
    stringsAsFactors = FALSE
  )
  visEdges
}

## Hover text. The previous widget had no tooltips, so this is additive.
graphNodeTooltip <- function(nodesDF, options) {
  parts <- list()
  if (!is.null(nodesDF$label_term)) {
    parts$Term <- as.character(nodesDF$label_term)
  }
  if (!is.null(nodesDF$label_id)) {
    parts$ID <- as.character(nodesDF$label_id)
  }
  if (!is.null(options$type)) {
    parts$Analysis <- options$type
  }
  if (!is.null(nodesDF$size)) {
    parts$Size <- as.character(round(nodesDF$size, 2))
  }
  if (!is.null(nodesDF$color)) {
    parts$Value <- as.character(signif(nodesDF$color, 3))
  }
  if (length(parts) == 0) return(NULL)
  do.call(paste, c(Map(function(nm, v) paste0("<b>", nm, ":</b> ", v),
                       names(parts), parts), sep = "<br>"))
}

## Discrete legend entries for every colour scale in use.
graphLegend <- function(nodesDF, options, style) {
  domains <- attr(nodesDF, "colorDomains")
  if (is.null(domains) || length(domains) == 0) return(NULL)

  labels <- c()
  colors <- c()
  for (sch in names(domains)) {
    domain <- domains[[sch]]
    ticks <- seq(domain[1], domain[2], length.out = 5)
    for (value in rev(ticks)) {
      colors <- c(colors, paletteColor(value, domain, style$scheme[[sch]]$range,
                                       options$colorScaler %||% "linear", 1))
      labels <- c(labels, format(signif(value, 3), scientific = TRUE))
    }
  }
  data.frame(
    label = labels,
    shape = "dot",
    color = substr(colors, 1, 7),
    stringsAsFactors = FALSE
  )
}

## ---- widget ---------------------------------------------------------------

#' Create an interactive force-directed network widget
#'
#' Builds an interactive network graph for enrichment maps and enriched
#' subnetworks. Rendering is done by \pkg{visNetwork} (vis.js); the widget works
#' in R Markdown documents, in the Shiny report produced by
#' \code{\link[HTSanalyzeR2]{report}} and as a standalone HTML page.
#'
#' @param nodes the node information of the force-directed graph
#' @param links the link information of the force-directed graph
#' @param nMappings indicates which attributes are used for node rendering. The
#' available render options are "id", "size", "color", "scheme", "label",
#' "label_id" and "label_term".
#' @param lMappings indicates which attributes are used for link rendering. The
#' available render options are "source", "target" and "weight".
#' @param options rendering options such as the title, legend title, the label
#' type ("id", "term" or "none") and the colour scaler ("log10" or "linear").
#' @param width the width of the widget
#' @param height the height of the widget
#' @param seriesData a character vector of time points for time-series data
#' @return An object of class 'htmlwidget' (a 'visNetwork' graph).
#' @importFrom visNetwork visNetwork visNodes visEdges visPhysics visOptions
#' @importFrom visNetwork visInteraction visLegend visExport
forceGraph <- function(nodes, links, nMappings, lMappings, options,
                       width = NULL, height = NULL, seriesData = NULL) {

  # nMappings: "id", "size", "color", "scheme", "label", "label_id", "label_term"
  # lMappings: "source", "target", "weight"
  style <- forceGraphStyle(options)

  nodesDF <- nodes[unlist(nMappings)]
  linksDF <- links[unlist(lMappings)]
  names(nodesDF) <- names(nMappings)
  names(linksDF) <- names(lMappings)

  if (is.null(nodesDF$color)) nodesDF$color <- 0
  attr(nodesDF, "colorDomains") <- niceDomain(nodesDF, seriesData)

  visNodes <- prepareGraphNodes(nodesDF, options, style)
  visEdges <- prepareGraphEdges(linksDF, style)
  legend <- graphLegend(nodesDF, options, style)

  graph <- visNetwork::visNetwork(
    visNodes, visEdges,
    main = options$title,
    width = width, height = height
  )
  graph <- visNetwork::visNodes(graph, shape = "dot", shadow = FALSE)
  graph <- visNetwork::visEdges(graph, smooth = list(enabled = TRUE, type = "continuous"))
  graph <- visNetwork::visPhysics(
    graph,
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravitationalConstant = -style$physics$gravity * 5,
      centralGravity = 0.01,
      springLength = 100,
      springConstant = 0.08,
      avoidOverlap = if (isTRUE(style$physics$adjustSizes)) 1 else 0
    ),
    stabilization = list(enabled = TRUE, iterations = 200)
  )
  graph <- visNetwork::visInteraction(
    graph,
    navigationButtons = TRUE,
    tooltipDelay = 120,
    hideEdgesOnDrag = TRUE
  )
  graph <- visNetwork::visOptions(
    graph,
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)
  )
  graph <- visNetwork::visExport(graph, type = "png", name = "network")

  if (!is.null(legend)) {
    graph <- visNetwork::visLegend(
      graph,
      addNodes = legend,
      useGroups = FALSE,
      position = "left",
      main = options$legendTitle,
      ncol = 1
    )
  }

  ## Keep the canonical node frame and mappings on the widget so the Shiny
  ## report can restyle or step through time points in place, without rebuilding
  ## (and therefore without resetting) the layout.
  attr(graph, "htsNodes") <- nodesDF
  attr(graph, "htsOptions") <- options
  attr(graph, "htsSeries") <- seriesData
  graph
}

# Helper function for htmlwidgets/Shiny to create widget output.
#' @importFrom visNetwork visNetworkOutput
forceGraphOutput <- function(outputId, width = "100%", height = "750px") {
  visNetwork::visNetworkOutput(outputId, width = width, height = height)
}

# Helper function for htmlwidgets/Shiny to render the widget.
#' @importFrom visNetwork renderVisNetwork
renderForceGraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  ## Capture the expression here: renderVisNetwork() substitutes its own
  ## argument, so passing it an unevaluated promise would render the wrong
  ## object (and force it outside the reactive context).
  if (!quoted) {
    expr <- substitute(expr)
  }
  visNetwork::renderVisNetwork(expr, env = env, quoted = TRUE)
}

#' Update a rendered network in place
#'
#' Sends new node styling to an already rendered widget through the
#' \pkg{visNetwork} proxy interface. This is used by the Shiny report to move
#' through the time points of a time-series analysis without rebuilding the
#' whole graph.
#'
#' @param outputId the Shiny output id of the rendered widget
#' @param widget the widget returned by \code{\link[HTSanalyzeR2]{forceGraph}}
#' @param options rendering options overriding the ones used for the render
#' @param tick time point to display; when NULL the current columns are kept
#' @return The proxy object returned by \code{\link[visNetwork]{visNetworkProxy}}.
#' @importFrom visNetwork visNetworkProxy visUpdateNodes
updateForceGraph <- function(outputId, widget, options = list(), tick = NULL) {
  nodesDF <- attr(widget, "htsNodes")
  seriesData <- attr(widget, "htsSeries")
  options <- modifyList(attr(widget, "htsOptions") %||% list(), options)
  style <- forceGraphStyle(options)

  if (is.null(nodesDF)) {
    stop("'widget' does not carry rendering data; rebuild it with forceGraph().\n")
  }
  if (!is.null(tick)) {
    ## Time-series renders carry one "color.<tick>"/"scheme.<tick>" pair per
    ## time point; pointing the base columns at the requested tick is enough.
    colorColumn <- paste0("color.", tick)
    schemeColumn <- paste0("scheme.", tick)
    if (!is.null(nodesDF[[colorColumn]])) nodesDF$color <- nodesDF[[colorColumn]]
    if (!is.null(nodesDF[[schemeColumn]])) nodesDF$scheme <- nodesDF[[schemeColumn]]
  }

  if (is.null(nodesDF$color)) nodesDF$color <- 0
  attr(nodesDF, "colorDomains") <- niceDomain(nodesDF, seriesData)

  updated <- prepareGraphNodes(nodesDF, options, style)

  proxy <- visNetwork::visNetworkProxy(outputId)
  proxy <- visNetwork::visUpdateNodes(
    proxy,
    updated[, c("id", "label", "value", "color.background", "color.border")]
  )
  visNetwork::visPhysics(
    proxy,
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravitationalConstant = -style$physics$gravity * 5,
      centralGravity = 0.01,
      springLength = 100,
      springConstant = 0.08,
      avoidOverlap = if (isTRUE(style$physics$adjustSizes)) 1 else 0
    )
  )
}

#' Save an interactive network graph to HTML or PNG
#'
#' Writes a widget produced by \code{\link[HTSanalyzeR2]{forceGraph}} — including
#' the enrichment maps and subnetworks returned by
#' \code{\link[HTSanalyzeR2]{viewEnrichMap}} and
#' \code{\link[HTSanalyzeR2]{viewSubNet}} — to a file. This is the programmatic
#' counterpart of the "Export as png" button in the interactive report, so the
#' figures can be produced from a script without opening a browser by hand.
#'
#' The format is chosen from the file extension. HTML output is self-contained
#' and keeps the graph interactive. PNG output is rendered headlessly through
#' \pkg{webshot2} and therefore also needs that package plus a Chrome-based
#' browser; interactive controls are omitted from the PNG so the result is
#' suitable for a manuscript or slide.
#'
#' @param widget an object returned by \code{\link[HTSanalyzeR2]{forceGraph}},
#' typically the value of \code{viewEnrichMap()} or \code{viewSubNet()}.
#' @param file output file name; must end in \code{.html} or \code{.png}.
#' @param width,height output size in pixels.
#' @param delay seconds to wait before the screenshot, giving the force layout
#' time to settle. Defaults to 2 seconds.
#' @param ... further arguments passed to \code{\link[webshot2]{webshot}}.
#' @return The output path, invisibly.
#' @seealso \code{\link[HTSanalyzeR2]{forceGraph}}
#' @examples
#' \dontrun{
#' data(d7_gsca)
#' map <- viewEnrichMap(d7_gsca, gscs = "GO_MF", gsNameType = "term")
#' saveNetwork(map, "enrichment-map.html")
#' saveNetwork(map, "enrichment-map.png", width = 1200, height = 900)
#' }
#' @importFrom visNetwork visSave
#' @export
saveNetwork <- function(widget, file, width = 1000, height = 750,
                        delay = 2, ...) {
  if (!inherits(widget, "htmlwidget")) {
    stop("'widget' must be an htmlwidget, such as the value returned by ",
         "forceGraph(), viewEnrichMap() or viewSubNet().\n", call. = FALSE)
  }

  if (grepl("\\.html?$", file, ignore.case = TRUE)) {
    visNetwork::visSave(widget, file = file, selfcontained = TRUE)
    return(invisible(file))
  }

  if (!grepl("\\.png$", file, ignore.case = TRUE)) {
    stop("'file' must end in '.html' or '.png'.\n", call. = FALSE)
  }
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop(
      "Saving a PNG needs the optional 'webshot2' package.\n",
      "Please install it with install.packages(\"webshot2\"), or save the ",
      "graph as interactive HTML with saveNetwork(widget, \"graph.html\").\n",
      call. = FALSE
    )
  }

  ## Strip the interactive controls so the bitmap matches a static figure.
  widget$x$options$interaction$navigationButtons <- FALSE
  widget$x$export <- NULL

  html <- tempfile(fileext = ".html")
  on.exit(unlink(html), add = TRUE)
  visNetwork::visSave(widget, file = html, selfcontained = TRUE)
  webshot2::webshot(
    url = html, file = file,
    vwidth = width, vheight = height,
    selector = ".visNetwork", delay = delay, ...
  )
  invisible(file)
}

## Convenience operator used by the widget code.
`%||%` <- function(x, y) if (is.null(x)) y else x
