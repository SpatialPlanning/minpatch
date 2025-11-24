# Validate MinPatch inputs

Internal function to validate all inputs to the MinPatch algorithm

## Usage

``` r
validate_inputs(
  solution,
  planning_units,
  targets,
  costs,
  min_patch_size,
  patch_radius,
  boundary_penalty
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

  patch radius for adding patches

- boundary_penalty:

  Boundary penalty value

## Value

NULL (throws errors if validation fails)
