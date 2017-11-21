# import package in Namesapce as in description for passing biocCheck
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

norm <- function(arr, minValue, maxValue, defaultValue) {
  ran = range(arr, na.rm = TRUE)
  if(ran[1] == ran[2]) {
    arr[!is.na(arr)] <- defaultValue
    return(arr)
  }
  tmp <- (arr - ran[1]) / (ran[2] - ran[1])
  tmp * (maxValue - minValue) + minValue
}


# Get the color domain by "color" and "scheme" cols of nodesDF
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
