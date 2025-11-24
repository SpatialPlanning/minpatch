# Create abundance matrix from planning units

Creates a matrix showing the amount of each feature in each planning
unit by extracting feature columns directly from planning_units using
prioritizr problem

## Usage

``` r
create_abundance_matrix(planning_units, prioritizr_problem)
```

## Arguments

- planning_units:

  sf object with planning unit geometries and feature columns

- prioritizr_problem:

  A prioritizr problem object to get feature names

## Value

Named list where each planning unit contains feature abundances
