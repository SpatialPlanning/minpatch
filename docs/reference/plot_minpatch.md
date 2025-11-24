# Visualize MinPatch results

Creates a simple visualization of the MinPatch results showing original
vs. modified solutions

## Usage

``` r
plot_minpatch(minpatch_result, title = "MinPatch Results")
```

## Arguments

- minpatch_result:

  Result from run_minpatch function

- title:

  Plot title (optional)

## Value

ggplot object (if ggplot2 is available)

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
  verbose = FALSE
)

# Visualize results
plot_minpatch(result)
```
