# Save an interactive network graph to HTML or PNG

Writes a widget produced by
[`forceGraph`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/forceGraph.md)
— including the enrichment maps and subnetworks returned by
[`viewEnrichMap`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/viewEnrichMap-GSCA-method.md)
and
[`viewSubNet`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/viewSubNet-NWA-method.md)
— to a file. This is the programmatic counterpart of the "Export as png"
button in the interactive report, so the figures can be produced from a
script without opening a browser by hand.

## Usage

``` r
saveNetwork(widget, file, width = 1000, height = 750, delay = 2, ...)
```

## Arguments

- widget:

  an object returned by
  [`forceGraph`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/forceGraph.md),
  typically the value of
  [`viewEnrichMap()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/viewEnrichMap-GSCA-method.md)
  or
  [`viewSubNet()`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/viewSubNet-NWA-method.md).

- file:

  output file name; must end in `.html` or `.png`.

- width, height:

  output size in pixels.

- delay:

  seconds to wait before the screenshot, giving the force layout time to
  settle. Defaults to 2 seconds.

- ...:

  further arguments passed to
  [`webshot`](https://rstudio.github.io/webshot2/reference/webshot.html).

## Value

The output path, invisibly.

## Details

The format is chosen from the file extension. HTML output is
self-contained and keeps the graph interactive. PNG output is rendered
headlessly through webshot2 and therefore also needs that package plus a
Chrome-based browser; interactive controls are omitted from the PNG so
the result is suitable for a manuscript or slide.

## See also

[`forceGraph`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/forceGraph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(d7_gsca)
map <- viewEnrichMap(d7_gsca, gscs = "GO_MF", gsNameType = "term")
saveNetwork(map, "enrichment-map.html")
saveNetwork(map, "enrichment-map.png", width = 1200, height = 900)
} # }
```
