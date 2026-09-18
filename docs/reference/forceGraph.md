# Create an interactive force-directed network widget

Builds an interactive network graph for enrichment maps and enriched
subnetworks. Rendering is done by visNetwork (vis.js); the widget works
in R Markdown documents, in the Shiny report produced by
[`report`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/report.md)
and as a standalone HTML page.

## Usage

``` r
forceGraph(
  nodes,
  links,
  nMappings,
  lMappings,
  options,
  width = NULL,
  height = NULL,
  seriesData = NULL
)
```

## Arguments

- nodes:

  the node information of the force-directed graph

- links:

  the link information of the force-directed graph

- nMappings:

  indicates which attributes are used for node rendering. The available
  render options are "id", "size", "color", "scheme", "label",
  "label_id" and "label_term".

- lMappings:

  indicates which attributes are used for link rendering. The available
  render options are "source", "target" and "weight".

- options:

  rendering options such as the title, legend title, the label type
  ("id", "term" or "none") and the colour scaler ("log10" or "linear").

- width:

  the width of the widget

- height:

  the height of the widget

- seriesData:

  a character vector of time points for time-series data

## Value

An object of class 'htmlwidget' (a 'visNetwork' graph).
