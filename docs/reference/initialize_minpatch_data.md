# Initialize MinPatch data structures

Creates the internal data structures needed for MinPatch processing

## Usage

``` r
initialize_minpatch_data(
  solution,
  planning_units,
  targets,
  costs,
  min_patch_size,
  patch_radius,
  boundary_penalty,
  prioritizr_problem,
  prioritizr_solution,
  verbose = TRUE
)
```

## Arguments

- solution:

  Binary solution vector

- planning_units:

  sf object with planning units

- targets:

  data.frame with targets

- costs:

  numeric vector of costs

- min_patch_size:

  minimum patch size

- patch_radius:

  patch radius

- boundary_penalty:

  Boundary penalty value

- prioritizr_problem:

  A prioritizr problem object

- prioritizr_solution:

  A solved prioritizr solution object

## Value

List containing all necessary data structures
