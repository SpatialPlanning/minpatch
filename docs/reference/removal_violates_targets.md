# Check if removing unit would violate conservation targets

Optimized to accept pre-computed feature amounts to avoid redundant
calculations.

## Usage

``` r
removal_violates_targets(unit_id, minpatch_data, feature_amounts = NULL)
```

## Arguments

- unit_id:

  ID of unit to potentially remove

- minpatch_data:

  List containing all MinPatch data structures

- feature_amounts:

  Optional pre-computed feature conservation amounts

## Value

Logical indicating if removal would violate targets
