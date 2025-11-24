# Create patch radius dictionary

For each planning unit, find all units within the specified patch radius

## Usage

``` r
create_patch_radius_dict(planning_units, patch_radius, verbose = TRUE)
```

## Arguments

- planning_units:

  sf object with planning unit geometries

- patch_radius:

  radius for patch creation

## Value

Named list where each planning unit contains list of units within radius
