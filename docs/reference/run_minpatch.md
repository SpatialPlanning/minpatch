# Run MinPatch algorithm on prioritizr solution

This is the main function that applies the MinPatch algorithm to a
prioritizr solution to ensure all protected areas meet minimum size
thresholds. The function uses prioritizr summary functions where
possible to reduce code duplication and ensure consistency with
prioritizr calculations.

## Usage

``` r
run_minpatch(
  prioritizr_problem,
  prioritizr_solution,
  min_patch_size,
  patch_radius,
  boundary_penalty = 0,
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  solution_column = "solution_1",
  verbose = TRUE,
  debug_boundary = FALSE,
  debug_boundary_every = 50
)
```

## Arguments

- prioritizr_problem:

  A prioritizr problem object

- prioritizr_solution:

  A solved prioritizr solution object

- min_patch_size:

  Minimum patch size threshold

- patch_radius:

  Radius for adding new patches

- boundary_penalty:

  Boundary penalty value (default = 0)

- remove_small_patches:

  Logical, whether to remove small patches (Stage 1, default = TRUE)

- add_patches:

  Logical, whether to add new patches to meet targets (Stage 2, default
  = TRUE)

- whittle_patches:

  Logical, whether to remove unnecessary units (Stage 3, default = TRUE)

- solution_column:

  Name of solution column (default = "solution_1")

- verbose:

  Logical, whether to print progress (default = TRUE)

- debug_boundary:

  Logical, whether to print boundary cost debug info (default = FALSE)

- debug_boundary_every:

  Integer, print debug info every N iterations (default = 50)

## Value

MinPatch result object with enhanced reporting using prioritizr
functions

## Details

The MinPatch algorithm consists of three stages:

1.  Remove small patches: Removes patches smaller than min_patch_size

2.  Add new patches: Adds patches to meet conservation targets

3.  Whittle patches: Removes unnecessary planning units

\*\*Locked Constraints\*\*: MinPatch automatically respects locked-in
and locked-out constraints from prioritizr problems (added via
[`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.html)
and
[`add_locked_out_constraints()`](https://prioritizr.net/reference/add_locked_out_constraints.html)):

- \*\*Locked-in units\*\*: Will never be removed, regardless of patch
  size or whittling. They are treated as "conserved" areas that must be
  retained.

- \*\*Locked-out units\*\*: Will never be selected, even when adding new
  patches to meet conservation targets. They are completely excluded
  from consideration.

If locked-in units form patches smaller than `min_patch_size`, a warning
will be issued, but these units will still be preserved.

\*\*Important\*\*: If you set `remove_small_patches = TRUE` but
`add_patches = FALSE`, the algorithm may remove patches without
compensating, potentially violating conservation targets. In such cases,
a warning will be issued. Consider using `add_patches = TRUE` or a
smaller `min_patch_size` to maintain target achievement.

## Examples

``` r

library(prioritizr)
library(sf)
library(terra)

# Get example data from prioritizr
dat <- c(get_sim_pu_raster(), get_sim_features()) %>%
  as.polygons(dissolve = FALSE, values = TRUE) %>%
  sf::st_as_sf() %>%
  dplyr::rename(cost = layer)

st_crs(dat) <- NA

features = colnames(dat) %>%
  stringr::str_subset("feature_")

# Create prioritizr problem
p <- problem(dat, features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%  # 17% of each feature
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve problem
s <- solve(p)

# Apply MinPatch with all stages
result <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = 0.05,
  patch_radius = 0.3,
)
#> Validating inputs...
#> Initializing data structures...
#> Calculating boundary matrix (optimized version)...
#> Creating patch radius dictionary (optimized)...
#> Calculating initial patch statistics...
#> Stage 1: Removing small patches...
#> Stage 2: Adding new patches...
#>   Initial unmet targets: 5 
#>   Unmet feature IDs: 1, 2, 3, 4, 5 
#>   Iteration 1 - Unmet targets: 5 
#>     Found 90 potential patches with scores
#>     Best score: 0.001699533 for unit 78 
#>     Added patch centered on unit 78 
#>   Iteration 2 - Unmet targets: 5 
#>     Found 76 potential patches with scores
#>     Best score: 0.001651032 for unit 66 
#>     Added patch centered on unit 66 
#>   All conservation targets are now met!
#> Stage 3: Removing unnecessary planning units...
#>     Edge units found: 8 
#>     Keystone units: 0 
#>     New keystone units: 0 
#>     Scoreable units: 8 
#>     Unit 88 cannot be removed - adding to keystone set
#>     Edge units found: 7 
#>     Keystone units: 1 
#>     New keystone units: 0 
#>     Scoreable units: 7 
#>     Removed unit 64 at iteration 2 
#>     Edge units found: 7 
#>     Keystone units: 1 
#>     New keystone units: 0 
#>     Scoreable units: 7 
#>     Removed unit 65 at iteration 3 
#>     Edge units found: 9 
#>     Keystone units: 1 
#>     New keystone units: 0 
#>     Scoreable units: 9 
#>     Removed unit 66 at iteration 4 
#>     Edge units found: 8 
#>     Keystone units: 1 
#>     New keystone units: 0 
#>     Scoreable units: 8 
#>     Removed unit 57 at iteration 5 
#>     Unit 56 cannot be removed - adding to keystone set
#>     Removed unit 47 at iteration 7 
#>     Unit 55 cannot be removed - adding to keystone set
#>     Unit 63 cannot be removed - adding to keystone set
#>     Unit 46 cannot be removed - adding to keystone set
#>   No more edge units to consider - terminating
#> Calculating final statistics...
#> MinPatch processing complete!

# Apply MinPatch with only Stage 1 and 3 (skip adding patches)
result2 <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = 0.05,
  patch_radius = 0.3,
  add_patches = FALSE
)
#> Validating inputs...
#> Initializing data structures...
#> Calculating boundary matrix (optimized version)...
#> Creating patch radius dictionary (optimized)...
#> Calculating initial patch statistics...
#> Stage 1: Removing small patches...
#> Warning: After removing small patches, 5 conservation targets are no longer met. Consider setting add_patches = TRUE to automatically add patches to meet targets, or use a smaller min_patch_size.
#>   Warning: 5 targets are no longer met after removing small patches
#>   Unmet feature IDs: 1, 2, 3, 4, 5 
#> Stage 2: Skipping addition of new patches...
#> Stage 3: Removing unnecessary planning units...
#>     Edge units found: 0 
#>     Keystone units: 0 
#>   No more edge units to consider - terminating
#> Calculating final statistics...
#> MinPatch processing complete!

print_minpatch_summary(result)
#> === MinPatch Processing Summary ===
#> 
#> Patch Statistics:
#>   Initial patches: 9 (valid: 0)
#>   Final patches: 6 (valid: 3)
#>   Area change: 0.04 (25.0%)
#> 
#> Cost Breakdown:
#>   Planning unit cost: 3964.91
#>   Boundary cost: 0.00
#>   Total cost: 3964.91
#>   Selected units: 20
#> 
#> Feature Representation:
#>   Total features: 5
#>   Targets met: 5
#>   Targets unmet: 0
#>   Mean proportion: 0.227
#>   Total shortfall: 0.00
#> 
#> 
#> === End Summary ===
```
