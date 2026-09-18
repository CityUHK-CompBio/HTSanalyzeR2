# Accessors for the 'summary' slot of a 'GSCA' or 'NWA' object.

This 'summary' slot summarized an analyzed 'GSCA' or 'NWA' object.

## Usage

``` r
# S4 method for class 'GSCA'
getSummary(object)

# S4 method for class 'NWA'
getSummary(object)
```

## Arguments

- object:

  An object of 'GSCA' or 'NWA'.

## Value

This function will return the summary information.

## Examples

``` r
# ===========================================================
# GSCA class
data(d7_gsca)
s1 <- getSummary(d7_gsca)
# ===========================================================
# NWA class
data(d7_nwa)
s1 <- getSummary(d7_nwa)
```
