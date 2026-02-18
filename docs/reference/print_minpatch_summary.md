# Print MinPatch results summary

Prints a formatted summary of MinPatch processing results

## Usage

``` r
print_minpatch_summary(minpatch_result)
```

## Arguments

- minpatch_result:

  Result object from run_minpatch function

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

# Run MinPatch
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
