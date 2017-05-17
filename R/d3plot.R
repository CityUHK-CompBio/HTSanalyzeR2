
#' @export
forceGraph <- function(nodes, links, nMappings, lMappings, options,
                       width = NULL, height = NULL, seriesData = NULL) {

  # nMappings: "id", "size", "color", "label", "desc", "seq"
  # lMappings: "source", "target", "label", "weight"

  node.size <- list(min = 6, max = 20, default = 8)
  link.weight <- list(min = 1, max = 6, default = 2)
  color.default <- 0.5
  color.domain.default <- c(-1, 0, 1)

  nodesDF = nodes[unlist(nMappings)]
  linksDF = links[unlist(lMappings)]
  names(nodesDF) = names(nMappings)
  names(linksDF) = names(lMappings)

  if(is.null(nodesDF$size)){
    nodesDF$size <- node.size$default
  } else {
    nodesDF$size <- norm(nodesDF$size, node.size$min, node.size$max, node.size$default)
  }

  if(is.null(nodesDF$color)) {
    nodesDF$color <- color.default;
  }

  if(is.null(linksDF$weight)){
    linksDF$weight <- link.weight$default
  } else {
    linksDF$weight <- norm(linksDF$weight, link.weight$min, link.weight$max, link.weight$default)
  }

  maxAbs <- max(abs(nodesDF$color))
  if(maxAbs <= 1) {
    colorDomain <- color.domain.default
  } else {
    colorDomain <- c(-maxAbs, 0, maxAbs)
  }

  # create options
  # colorDomain must be three nums
  argOptions <- modifyList(options, list(colorDomain = colorDomain, seriesData = seriesData))

  # create widget
  htmlwidgets::createWidget(
    name = "forceGraph",
    x = list(nodes = nodesDF, links = linksDF, options = argOptions),
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(padding = 10, browser.fill = TRUE),
    package = "HTSanalyzeR2"
  )
}

#' @importFrom htmlwidgets shinyWidgetOutput
#' @export
forceGraphOutput <- function(outputId, width = "100%", height = "750px") {
  shinyWidgetOutput(outputId, "forceGraph", width, height,
                    package = "HTSanalyzeR2")
}

#' @importFrom htmlwidgets shinyRenderWidget
#' @export
renderForceGraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  shinyRenderWidget(expr, forceGraphOutput, env, quoted = TRUE)
}


#' @importFrom htmlwidgets shinyRenderWidget createWidget
#' @export
updateForceGraph <- function(options) {
  x <- list(update = TRUE)
  x <- c(x, options)
  expr <- createWidget(name = "forceGraph", x = x, package = "HTSanalyzeR2")
  renderForceGraph(expr, quoted = FALSE)
}

norm <- function(df, minValue, maxValue, defaultValue) {
  # colnames(df) <- newName
  if(min(df) == max(df)) {
    df <- defaultValue
    return(df)
  }
  tmp <- (df - min(df)) / (max(df) - min(df))
  tmp * (maxValue - minValue) + minValue
}

