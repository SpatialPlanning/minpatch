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
#> Calculating boundary matrix (this may take a while)...
#> Creating patch radius dictionary...
#> Calculating initial patch statistics...
#> Stage 1: Removing small patches...
#> Stage 2: Adding new patches...
#>   Initial unmet targets: 5 
#>   Unmet feature IDs: 1, 2, 3, 4, 5 
#>   Iteration 1 - Unmet targets: 5 
#>     Found 85 potential patches with scores
#>     Best score: 0.002500108 for unit 90 
#>     Added patch centered on unit 90 
#>   Iteration 2 - Unmet targets: 4 
#>     Found 76 potential patches with scores
#>     Best score: 0.002060928 for unit 78 
#>     Added patch centered on unit 78 
#>   All conservation targets are now met!
#> Stage 3: Removing unnecessary planning units...
#>     Edge units found: 21 
#>     Keystone units: 0 
#>     New keystone units: 0 
#>     Scoreable units: 21 
#>     Unit 90 cannot be removed - adding to keystone set
#>     Edge units found: 20 
#>     Keystone units: 1 
#>     New keystone units: 0 
#>     Scoreable units: 20 
#>     Unit 81 cannot be removed - adding to keystone set
#>     Edge units found: 19 
#>     Keystone units: 2 
#>     New keystone units: 0 
#>     Scoreable units: 19 
#>     Unit 80 cannot be removed - adding to keystone set
#>     Edge units found: 18 
#>     Keystone units: 3 
#>     New keystone units: 0 
#>     Scoreable units: 18 
#>     Unit 89 cannot be removed - adding to keystone set
#>     Edge units found: 17 
#>     Keystone units: 4 
#>     New keystone units: 0 
#>     Scoreable units: 17 
#>     Unit 73 cannot be removed - adding to keystone set
#>     Unit 79 cannot be removed - adding to keystone set
#>     Unit 72 cannot be removed - adding to keystone set
#>     Unit 88 cannot be removed - adding to keystone set
#>     Unit 87 cannot be removed - adding to keystone set
#>     Unit 64 cannot be removed - adding to keystone set
#>   No more edge units to consider - terminating
#> Calculating final statistics...
#> MinPatch processing complete!

print_minpatch_summary(result)
#> === MinPatch Processing Summary ===
#> 
#> Patch Statistics:
#>   Initial patches: 7 (valid: 1)
#>   Final patches: 4 (valid: 2)
#>   Area change: 0.05 (31.2%)
#> 
#> Cost Breakdown:
#>   Planning unit cost: 4137.80
#>   Boundary cost: 0.00
#>   Total cost: 4137.80
#>   Selected units: 21
#> 
#> Feature Representation:
#>   Total features: 5
#>   Targets met: 5
#>   Targets unmet: 0
#>   Mean proportion: 0.236
#>   Total shortfall: 0.00
#> 
#> 
#> === End Summary ===
```
