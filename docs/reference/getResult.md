# Accessors for the 'result' slot of a 'GSCA' or 'NWA' object.

This 'result' slot stores all the results of an analyzed 'GSCA' or 'NWA'
object.

## Usage

``` r
# S4 method for class 'GSCA'
getResult(object)

# S4 method for class 'NWA'
getResult(object)
```

## Arguments

- object:

  An object of 'GSCA' or 'NWA'.

## Value

This function will return all the results as a list.

## Examples

``` r
# ===========================================================
# GSCA class
data(d7_gsca)
rslt <- getResult(d7_gsca)

# ===========================================================
# NWA class
data(d7_nwa)
rslt <- getResult(d7_nwa)
```
