
#' @export
forceGraph <- function(nodes, links, nMappings, lMappings,
                       title = NULL, legendTitle = NULL,
                       charge = -200, distance = 200,
                       width = NULL, height = NULL) {

  # nMappings: "id", "size", "color", "label", "desc"
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
    legendDomain <- color.domain.default[-2]
  } else {
    colorDomain <- c(-maxAbs, 0, maxAbs)
    ran <- range(nodesDF$color)
    legendDomain <- if(ran[1] != ran[2]) ran else c(ran[1] - 1, ran[1] + 1)
  }

  # create options
  # colorDomain must be three nums
  options = list(title = title,
                 charge = charge,
                 distance = distance,
                 colorDomain = colorDomain,
                 legendTitle = legendTitle,
                 legendDomain = legendDomain)

  # create widget
  htmlwidgets::createWidget(
    name = "forceGraph",
    x = list(nodes = nodesDF, links = linksDF, options = options),
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(padding = 10, browser.fill = TRUE),
    package = "HTSanalyzeR2"
  )
}

#' @importFrom htmlwidgets shinyWidgetOutput
#' @export
forceGraphOutput <- function(outputId, width = "100%", height = "500px") {
  shinyWidgetOutput(outputId, "forceGraph", width, height,
                    package = "HTSanalyzeR2")
}

#' @importFrom htmlwidgets shinyRenderWidget
#' @export
renderForceGraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  shinyRenderWidget(expr, forceGraphOutput, env, quoted = TRUE)
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

