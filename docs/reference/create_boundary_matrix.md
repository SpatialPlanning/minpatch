# Create boundary matrix from planning units

Creates a matrix of shared boundary lengths between adjacent planning
units

## Usage

``` r
create_boundary_matrix(planning_units, verbose = TRUE)
```

## Arguments

- planning_units:

  sf object with planning unit geometries

## Value

Named list where each element contains neighbors and shared boundary
lengths
