# Update a rendered network in place

Sends new node styling to an already rendered widget through the
visNetwork proxy interface. This is used by the Shiny report to move
through the time points of a time-series analysis without rebuilding the
whole graph.

## Usage

``` r
updateForceGraph(outputId, widget, options = list(), tick = NULL)
```

## Arguments

- outputId:

  the Shiny output id of the rendered widget

- widget:

  the widget returned by
  [`forceGraph`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/forceGraph.md)

- options:

  rendering options overriding the ones used for the render

- tick:

  time point to display; when NULL the current columns are kept

## Value

The proxy object returned by
[`visNetworkProxy`](https://rdrr.io/pkg/visNetwork/man/visNetwork-shiny.html).
