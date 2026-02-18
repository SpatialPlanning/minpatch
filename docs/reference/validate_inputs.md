# Validate MinPatch inputs

Internal function to validate all inputs to the MinPatch algorithm,
including locked-in and locked-out constraints

## Usage

``` r
validate_inputs(
  solution,
  planning_units,
  targets,
  costs,
  min_patch_size,
  patch_radius,
  boundary_penalty,
  locked_in_indices = NULL,
  locked_out_indices = NULL,
  area_dict = NULL,
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

  patch radius for adding patches

- boundary_penalty:

  Boundary penalty value

- locked_in_indices:

  Optional indices of locked-in planning units

- locked_out_indices:

  Optional indices of locked-out planning units

- area_dict:

  Optional area dictionary for locked-in patch size validation

- verbose:

  Logical, whether to print warnings

## Value

NULL (throws errors if validation fails)
