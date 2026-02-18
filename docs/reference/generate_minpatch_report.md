# Generate comprehensive MinPatch report

Creates a detailed report of the MinPatch processing results

## Usage

``` r
generate_minpatch_report(minpatch_result)
```

## Arguments

- minpatch_result:

  Result object from run_minpatch function

## Value

List containing formatted report components

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

generate_minpatch_report(result)
#> $features
#> # A tibble: 5 × 9
#>   feature   met   total_amount absolute_target absolute_held absolute_shortfall
#>   <chr>     <lgl>        <dbl>           <dbl>         <dbl>              <dbl>
#> 1 feature_1 TRUE          74.5           12.7          17.7                   0
#> 2 feature_2 TRUE          28.1            4.77          5.90                  0
#> 3 feature_3 TRUE          64.9           11.0          14.4                   0
#> 4 feature_4 TRUE          38.2            6.49          8.62                  0
#> 5 feature_5 TRUE          50.7            8.61         12.2                   0
#> # ℹ 3 more variables: relative_target <dbl>, relative_held <dbl>,
#> #   relative_shortfall <dbl>
#> 
#> $patch_stats
#>      time all_patch_count all_patch_area median_all_patch valid_patch_count
#> 1 initial               9           0.16            0.010                 0
#> 2   final               6           0.20            0.035                 3
#>   valid_patch_area median_valid_patch
#> 1             0.00               0.00
#> 2             0.15               0.05
#> 
#> $cost
#> # A tibble: 1 × 6
#>   summary  cost     n boundary_length boundary_cost total_cost
#>   <chr>   <dbl> <dbl>           <dbl>         <dbl>      <dbl>
#> 1 overall 3965.    20               0             0      3965.
#> 
```
