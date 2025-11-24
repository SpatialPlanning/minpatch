# Simulated whittling to remove unnecessary planning units

Stage 3 of MinPatch: Remove planning units that are not needed to meet
targets, reduce fragmentation, or meet minimum patch size requirements

## Usage

``` r
simulated_whittling(minpatch_data, verbose = TRUE)
```

## Arguments

- minpatch_data:

  List containing all MinPatch data structures (including prioritizr
  objects)

- verbose:

  Logical, whether to print progress

## Value

Updated minpatch_data with unnecessary units removed
