# Compare solutions before and after MinPatch

Creates a comprehensive comparison of key metrics between original and
MinPatch solutions, including overall statistics and detailed
feature-level analysis

## Usage

``` r
compare_solutions(minpatch_result)
```

## Arguments

- minpatch_result:

  Result from run_minpatch function

## Value

List containing:

- overall: Data frame with overall solution comparison

- features: Data frame with feature-level area comparisons

- summary: List with summary statistics of feature changes

## Examples

``` r
library(prioritizr)
#> Warning: package 'prioritizr' was built under R version 4.5.2
library(sf)
#> Warning: package 'sf' was built under R version 4.5.2
#> Linking to GEOS 3.13.1, GDAL 3.11.4, PROJ 9.7.0; sf_use_s2() is TRUE
library(terra)
#> Warning: package 'terra' was built under R version 4.5.2
#> terra 1.8.93

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

# Compare solutions
comparison <- compare_solutions(result)

# Print overall comparison
print(comparison$overall)
#>                        Metric Original MinPatch Change Percent_Change
#> 1     Selected Planning Units   16.000   18.000  2.000       12.50000
#> 2                  Total Area    0.160    0.180  0.020       12.50000
#> 3           Number of Patches    7.000    4.000 -3.000      -42.85714
#> 4 Valid Patches (>= min size)    1.000    2.000  1.000      100.00000
#> 5           Median Patch Size    0.010    0.045  0.035      350.00000
#> 6          Planning Unit Cost 3535.082 3535.082  0.000        0.00000
#> 7               Boundary Cost    0.000    0.000  0.000             NA
#> 8                  Total Cost 3535.082 3535.082  0.000        0.00000

# Print feature-level comparison
print(comparison$features)
#>   Feature_ID    Target Original_Area MinPatch_Area Area_Change Percent_Change
#> 1          1 12.670220     14.083429     15.953974  1.87054533       13.28189
#> 2          2  4.774965      5.124808      5.178169  0.05336054        1.04122
#> 3          3 11.029225     11.707674     12.981443  1.27376851       10.87977
#> 4          4  6.489033      6.863962      7.167961  0.30399939        4.42892
#> 5          5  8.613574      9.482534     11.118871  1.63633770       17.25633
#>   Original_Target_Met MinPatch_Target_Met Original_Proportion
#> 1                TRUE                TRUE            1.111538
#> 2                TRUE                TRUE            1.073266
#> 3                TRUE                TRUE            1.061514
#> 4                TRUE                TRUE            1.057779
#> 5                TRUE                TRUE            1.100883
#>   MinPatch_Proportion
#> 1            1.259171
#> 2            1.084441
#> 3            1.177004
#> 4            1.104627
#> 5            1.290855

# Print summary statistics
cat("Features improved:", comparison$summary$features_improved, "\n")
#> Features improved: 5 
cat("Targets gained:", comparison$summary$targets_gained, "\n")
#> Targets gained: 0 
```
