# Accessors for the 'para' slot of a 'GSCA' or 'fdr' slot of a 'NWA' object.

This function get all the parameters used in 'GSCA' or 'NWA' analysis.

## Usage

``` r
# S4 method for class 'GSCA'
getPara(object)

# S4 method for class 'NWA'
getPara(object)
```

## Arguments

- object:

  An object of 'GSCA' or 'NWA'.

## Value

This function will return all the parameters.

## Examples

``` r
# ===========================================================
# GSCA class
data(d7_gsca)
para1 <- getPara(d7_gsca)

# ===========================================================
# NWA class
data(d7_nwa)
para1 <- getPara(d7_nwa)
```
