# Extract the subnetwork as an igraph object

Extract the subnetwork form an analyzed NWA object as an igraph object
for further external using. Users can also use it to modify the
subnetwork.

## Usage

``` r
# S4 method for class 'NWA'
extractSubNet(object)
```

## Arguments

- object:

  An NWA object.

## Value

This function would return a subnetwork as an 'igraph' object.

## Examples

``` r
## load a NWA object(see the examples of analyze NWA for details)
data(d7_nwa)

## extract the subnetwork as an igraph object
library(igraph)
subnetwork <- extractSubNet(d7_nwa)
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...
```
