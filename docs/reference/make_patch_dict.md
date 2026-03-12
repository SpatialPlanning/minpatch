# Create patch dictionary from unit dictionary

Identifies connected components (patches) in the current solution using
igraph and sparse matrix operations. This implementation follows the
wheretowork approach for efficient patch identification using matrix
subsetting.

## Usage

``` r
make_patch_dict(minpatch_data)
```

## Arguments

- minpatch_data:

  List containing all MinPatch data structures

## Value

Named list where each patch contains area, unit count, and unit IDs
