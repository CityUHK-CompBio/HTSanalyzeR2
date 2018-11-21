#' Create a force-directed graph widget
#'
#' This function creates a force-directed graph using \pkg{htmlwidgets}, which can be rendered
#' in R Markdown documents and shiny applications.
#' The rendering is conducted using a variant of the Sigma which is a JavaScript library dedicated
#' to graph drawing. The force-graph layout algorithm is ForceAtlas2
#' [http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0098679] and implemented by
#' Mathieu Jacomy [https://github.com/jacomyma], Guillaume Plique [https://github.com/Yomguithereal]
#' and Sébastien Heymann. [https://github.com/sheymann].
#'
#' @param nodes the node information of force-directed graph
#' @param links the link information of force-directed graph
#' @param nMappings indicates which attributes are used for node rendering. The available render
#' options are "id", "size", "color", "scheme", "label", "label_id" and "label_term".
#' @param lMappings indicates which attributes are used for link rendering. The available render
#' options are "source", "target" and "weight".
#' @param options A list of internal meta configurations provided for JavaScript library. Users
#' are highly recommended to modify the detailed rendering options in the shiny report by
#' \code{\link[HTSanalyzeR2]{report}}.
#' @param width the width of the widget
#' @param height the height of the widget
#' @param seriesData A list of time points in time series analysis.
#' @importFrom shinydashboard dashboardPage
#' @import colourpicker
#' @import DT
forceGraph <- function(nodes, links, nMappings, lMappings, options,
                       width = NULL, height = NULL, seriesData = NULL) {

  # nMappings: "id", "size", "color", "scheme", "label", "label_id", "label_term"
  # lMappings: "source", "target", "weight"
  node.size <- list(min = 3, max = 20, default = 4)
  link.weight <- list(min = 1, max = 6, default = 2)
  color.default <- 0

  nodesDF = nodes[unlist(nMappings)]
  linksDF = links[unlist(lMappings)]
  names(nodesDF) = names(nMappings)
  names(linksDF) = names(lMappings)

  if(is.null(nodesDF$size)){
    nodesDF$size <- node.size$default
  } else {
    nodesDF$size <- norm(nodesDF$size, node.size$min, node.size$max, node.size$default)
  }

  if(nrow(linksDF) > 0) {
    if(is.null(linksDF$weight)){
      linksDF$weight <- link.weight$default
    }
  }

  if(is.null(nodesDF$color)) {
    nodesDF$color <- color.default
  }
  colorDomain <- niceDomain(nodesDF, seriesData)

  # create options
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

# Helper function for htmlwidgets to create widget.
#' @importFrom htmlwidgets shinyWidgetOutput
forceGraphOutput <- function(outputId, width = "100%", height = "750px") {
  shinyWidgetOutput(outputId, "forceGraph", width, height,
                    package = "HTSanalyzeR2")
}

# Helper function for htmlwidgets to create widget.
#' @importFrom htmlwidgets shinyRenderWidget
renderForceGraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  shinyRenderWidget(expr, forceGraphOutput, env, quoted = TRUE)
}

# Helper function for htmlwidgets to create widget.
#' @importFrom htmlwidgets shinyRenderWidget createWidget
updateForceGraph <- function(options) {
  x <- list(update = TRUE)
  x <- c(x, options)
  expr <- createWidget(name = "forceGraph", x = x, package = "HTSanalyzeR2")
  renderForceGraph(expr, quoted = FALSE)
}

# Helper function. Normalize the values into range [minValue, maxValue].
norm <- function(arr, minValue, maxValue, defaultValue) {
  ran = range(arr, na.rm = TRUE)
  if(ran[1] == ran[2]) {
    arr[!is.na(arr)] <- defaultValue
    return(arr)
  }
  tmp <- (arr - ran[1]) / (ran[2] - ran[1])
  tmp * (maxValue - minValue) + minValue
}


# Helper function. Get the color domain by "color" and "scheme" cols of nodesDF
niceDomain <- function(nodesDF, seriesData) {
  color <- nodesDF$color
  scheme <- nodesDF$scheme
  if(!is.null(seriesData)) {
    for(tick in seriesData) {
      color <- c(color, nodesDF[, paste("color", tick, sep = ".")])
      scheme <- c(scheme, nodesDF[, paste("scheme", tick, sep = ".")])
    }
  }

  domains <- list()
  for(sch in unique(scheme[!is.na(scheme)])) {
    ran <- range(color[scheme == sch], na.rm = TRUE)
    if(ran[1] == ran[2]) {
       ran[2] = ran[2] + 0.05
    }
    domains[[sch]] <- round(ran, 3)
  }

  domains
}
