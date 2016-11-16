
#' @export
forceGraph <- function(nodes, links, nMappings, lMappings,
                       title = NULL, legendTitle = NULL,
                       charge = -200, distance = 200,
                       colorDomain = c(-1, 0, 1),
                       legendDomain = c(-1, 1),
                       width = NULL, height = NULL) {
  # nMappings = c("term", "size", "value", "term", "term");
  # lMappings = c("from", "to", "label", "value");
  # names(nMappings) = c("id", "size", "color", "label", "desc");
  # names(lMappings) = c("source", "target", "label", "weight");

  nodesDF = nodes[nMappings]
  linksDF = links[lMappings]
  names(nodesDF) = names(nMappings)
  names(linksDF) = names(lMappings)

  nodesDF["size"] <- norm(nodesDF["size"], 6, 20)
  linksDF["weight"] <- norm(linksDF["weight"], 2 , 6)

  # create options
  # colorDomain must be three nums
  options = list(title = "finalllllly",
                 legendTitle = "Adjusted\np-values",
                 charge = charge,
                 distance = distance,
                 colorDomain = colorDomain)

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

norm <- function(df, minValue, maxValue) {
  # colnames(df) <- newName
  if(min(df) == max(df)) {
    df <- ((minValue + maxValue) / 2)
    return(df)
  }
  tmp <- (df - min(df)) / (max(df) - min(df))
  tmp * (maxValue - minValue) + minValue
}

