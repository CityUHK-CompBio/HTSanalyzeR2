# Plot the identified subnetwork of an NWA object

Plot the identified subnetwork of an NWA object.

## Usage

``` r
# S4 method for class 'NWA'
viewSubNet(object, options = list(), seriesObjs = NULL)
```

## Arguments

- object:

  An NWA object.

- options:

  A list of options to modify the enrichmentmap. Details are not showed
  here due to too many options. Users are highly recommended to modify
  the enrichment map in a shiny report by
  [`report`](https://cityuhk-compbio.github.io/HTSanalyzeR2/reference/report.md).

- seriesObjs:

  A list of NWA object. Internally used in the shiny report for
  visualizing the subnetwork of time series data. No need to explicitly
  set it!

## Value

In the end, this function would plot the identified subnetwork.

## Examples

``` r
if (FALSE) { # \dontrun{
## load a NWA object(see the examples of analyze NWA for details)
data(d7_nwa)

## plot the subnetwork
viewSubNet(d7_nwa)
} # }
```
