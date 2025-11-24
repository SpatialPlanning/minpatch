# Check if a planning unit can be removed

Checks multiple criteria to determine if removing a unit is
acceptable: 1. Doesn't violate conservation targets 2. Doesn't make any
patch too small 3. Doesn't increase total cost 4. Doesn't split patches
into non-viable pieces

## Usage

``` r
can_remove_unit(unit_id, minpatch_data)
```

## Arguments

- unit_id:

  ID of unit to potentially remove

- minpatch_data:

  List containing all MinPatch data structures

## Value

Logical indicating if unit can be removed
