# MinPatch in Tasmania

## Load packages

``` r

library(minpatch)
library(prioritizr)
library(prioritizrdata)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
```

## Load data

``` r

# load data
tas_pu <- get_tas_pu() %>% 
  mutate(cost = cost*10000)

# At present minpatch works with sf objects. Here we convert the data to sf.
tas_features <- get_tas_features() %>% 
  stars::st_as_stars() %>% 
  sf::st_as_sf()

tas <- sf::st_interpolate_aw(tas_features, tas_pu, extensive = FALSE, keep_NA = FALSE, na.rm = FALSE) %>% 
  st_join(tas_pu, join = st_equals)
#> Warning in st_interpolate_aw.sf(tas_features, tas_pu, extensive = FALSE, :
#> st_interpolate_aw assumes attributes are constant or uniform over areas of x


features = tas %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-all_of(c("id", "cost", "locked_in", "locked_out"))) %>% 
  names()

# Convert data to binary again
tas <- tas %>% 
  mutate(across(all_of(features), ~ if_else(.x > 0, 1, 0)))
```

## Run prioritizr analysis

``` r

p <- problem(tas, features = features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%  # 30% of each feature
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

s <- solve(p)
```

### Plot prioritizr solution

``` r

plot_prioritizr(s)
```

![](minpatchTasmania_files/figure-html/unnamed-chunk-4-1.png)

## MinPatch

### Choose a patch size

``` r

# Calculate reasonable parameters based on planning unit characteristics
median_area <- median(st_area(tas))

# Set minimum patch size to 5x median planning unit area
min_patch_size <- median_area * 5

# Set patch radius to encompass approximately 10 planning units
patch_radius <- sqrt(median_area * 10)

cat("MinPatch parameters:\n")
#> MinPatch parameters:
cat("- Minimum patch size:", round(min_patch_size, 3), "square meters\n")
#> - Minimum patch size: 324514429 square meters
cat("- Patch radius:", round(patch_radius,3), "meters\n")
#> - Patch radius: 25476.04 meters
cat("- This means patches must be at least", round(min_patch_size/median_area, 3),
    "times the median planning unit size\n")
#> - This means patches must be at least 5 times the median planning unit size
```

### Run minpatch

``` r

result <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = min_patch_size,
  patch_radius = patch_radius,
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  verbose = TRUE
)
#> Validating inputs...
#> Initializing data structures...
#> Calculating boundary matrix using 14 cores...
#> Processing chunks in parallel...
#> Combining results...
#> Creating patch radius dictionary (optimized)...
#> Processed 100 of 1128 planning units
#> Processed 200 of 1128 planning units
#> Processed 300 of 1128 planning units
#> Processed 400 of 1128 planning units
#> Processed 500 of 1128 planning units
#> Processed 600 of 1128 planning units
#> Processed 700 of 1128 planning units
#> Processed 800 of 1128 planning units
#> Processed 900 of 1128 planning units
#> Processed 1000 of 1128 planning units
#> Processed 1100 of 1128 planning units
#> Calculating initial patch statistics...
#> Stage 1: Removing small patches...
#> Stage 2: Adding new patches...
#>   Initial unmet targets: 30 
#>   Unmet feature IDs: 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 21, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33 
#>   Iteration 1 - Unmet targets: 30 
#>     Found 875 potential patches with scores
#>     Best score: 1.440508e-05 for unit 79 
#>     Added patch centered on unit 79 
#>   Iteration 2 - Unmet targets: 29 
#>     Found 873 potential patches with scores
#>     Best score: 7.880797e-06 for unit 421 
#>     Added patch centered on unit 421 
#>   Iteration 3 - Unmet targets: 27 
#>     Found 864 potential patches with scores
#>     Best score: 5.751114e-06 for unit 387 
#>     Added patch centered on unit 387 
#>   Iteration 4 - Unmet targets: 26 
#>     Found 859 potential patches with scores
#>     Best score: 4.461179e-06 for unit 362 
#>     Added patch centered on unit 362 
#>   Iteration 5 - Unmet targets: 23 
#>     Found 850 potential patches with scores
#>     Best score: 3.090659e-06 for unit 246 
#>     Added patch centered on unit 246 
#>   Iteration 10 - Unmet targets: 17 
#>   All conservation targets are now met!
#> Stage 3: Removing unnecessary planning units...
#>     Edge units found: 154 
#>     Keystone units: 0 
#>     New keystone units: 9 
#>     Scoreable units: 145 
#>     Removed unit 438 at iteration 1 
#>     Edge units found: 145 
#>     Keystone units: 9 
#>     New keystone units: 0 
#>     Scoreable units: 145 
#>     Removed unit 419 at iteration 2 
#>     Edge units found: 146 
#>     Keystone units: 9 
#>     New keystone units: 0 
#>     Scoreable units: 146 
#>     Unit 439 cannot be removed - adding to keystone set
#>     Edge units found: 145 
#>     Keystone units: 10 
#>     New keystone units: 0 
#>     Scoreable units: 145 
#>     Removed unit 111 at iteration 4 
#>     Edge units found: 144 
#>     Keystone units: 10 
#>     New keystone units: 0 
#>     Scoreable units: 144 
#>     Removed unit 401 at iteration 5 
#>     Removed unit 420 at iteration 6 
#>     Removed unit 400 at iteration 7 
#>     Removed unit 213 at iteration 8 
#>     Removed unit 255 at iteration 9 
#>     Removed unit 75 at iteration 10 
#>   Whittling iteration 100 
#>   No units can be removed - all are keystone - terminating
#> Calculating final statistics...
#> MinPatch processing complete!
```

### Visualise the minpatch solution

``` r

plot_minpatch(result, title = "MinPatch Results")
```

![](minpatchTasmania_files/figure-html/unnamed-chunk-7-1.png)

### Analyse the final results

``` r

print_minpatch_summary(result)
#> === MinPatch Processing Summary ===
#> 
#> Patch Statistics:
#>   Initial patches: 58 (valid: 11)
#>   Final patches: 15 (valid: 14)
#>   Area change: -646956349.29 (-3.2%)
#> 
#> Cost Breakdown:
#>   Planning unit cost: 59188469.96
#>   Boundary cost: 0.00
#>   Total cost: 59188469.96
#>   Selected units: 344
#> 
#> Feature Representation:
#>   Total features: 33
#>   Targets met: 33
#>   Targets unmet: 0
#>   Mean proportion: 0.392
#>   Total shortfall: 0.00
#> 
#> 
#> === End Summary ===

# Compare original vs MinPatch solutions
comparison <- compare_solutions(result)

# Print overall comparison
cat("=== Overall Solution Comparison ===\n")
#> === Overall Solution Comparison ===
print(comparison$overall)
#>                        Metric    Original    MinPatch     Change Percent_Change
#> 1     Selected Planning Units         330         344         14       4.242424
#> 2                  Total Area 20052333083 19405376734 -646956349      -3.226340
#> 3           Number of Patches          58          15        -43     -74.137931
#> 4 Valid Patches (>= min size)          11          14          3      27.272727
#> 5           Median Patch Size    64915437   373340097  308424660     475.117591
#> 6          Planning Unit Cost    59188470    59188470          0       0.000000
#> 7               Boundary Cost           0           0          0             NA
#> 8                  Total Cost    59188470    59188470          0       0.000000

# Print feature-level comparison
cat("\n=== Feature-Level Area Comparison ===\n")
#> 
#> === Feature-Level Area Comparison ===
print(comparison$features)
#>    Feature_ID Target Original_Area MinPatch_Area Area_Change Percent_Change
#> 1           1    1.2             2             3           1     50.0000000
#> 2           2   33.0            35            38           3      8.5714286
#> 3           3    2.7             3             5           2     66.6666667
#> 4           4  170.1           177           173          -4     -2.2598870
#> 5           5  213.9           221           214          -7     -3.1674208
#> 6           6  242.1           250           250           0      0.0000000
#> 7           7    9.9            12            11          -1     -8.3333333
#> 8           8  108.3           113           110          -3     -2.6548673
#> 9           9   30.9            41            49           8     19.5121951
#> 10         10  260.4           271           281          10      3.6900369
#> 11         11   62.4            67            66          -1     -1.4925373
#> 12         12  132.0           151           152           1      0.6622517
#> 13         13  133.5           138           134          -4     -2.8985507
#> 14         14  122.4           128           123          -5     -3.9062500
#> 15         15   62.4            66            69           3      4.5454545
#> 16         16  240.0           246           257          11      4.4715447
#> 17         17    0.3             1             1           0      0.0000000
#> 18         18   27.6            29            28          -1     -3.4482759
#> 19         19    6.6             8             8           0      0.0000000
#> 20         20   18.0            19            24           5     26.3157895
#> 21         21   24.3            31            37           6     19.3548387
#> 22         22    7.5            12            11          -1     -8.3333333
#> 23         23   34.5            39            40           1      2.5641026
#> 24         24   11.1            12            12           0      0.0000000
#> 25         25   47.1            51            71          20     39.2156863
#> 26         26    9.3            11            11           0      0.0000000
#> 27         27   86.7            88            87          -1     -1.1363636
#> 28         28    6.6             7            10           3     42.8571429
#> 29         29    1.2             2             2           0      0.0000000
#> 30         30  186.0           192           187          -5     -2.6041667
#> 31         31   66.3            71            78           7      9.8591549
#> 32         32   30.9            36            37           1      2.7777778
#> 33         33   57.9            60            58          -2     -3.3333333
#>    Original_Target_Met MinPatch_Target_Met Original_Proportion
#> 1                 TRUE                TRUE            1.666667
#> 2                 TRUE                TRUE            1.060606
#> 3                 TRUE                TRUE            1.111111
#> 4                 TRUE                TRUE            1.040564
#> 5                 TRUE                TRUE            1.033193
#> 6                 TRUE                TRUE            1.032631
#> 7                 TRUE                TRUE            1.212121
#> 8                 TRUE                TRUE            1.043398
#> 9                 TRUE                TRUE            1.326861
#> 10                TRUE                TRUE            1.040707
#> 11                TRUE                TRUE            1.073718
#> 12                TRUE                TRUE            1.143939
#> 13                TRUE                TRUE            1.033708
#> 14                TRUE                TRUE            1.045752
#> 15                TRUE                TRUE            1.057692
#> 16                TRUE                TRUE            1.025000
#> 17                TRUE                TRUE            3.333333
#> 18                TRUE                TRUE            1.050725
#> 19                TRUE                TRUE            1.212121
#> 20                TRUE                TRUE            1.055556
#> 21                TRUE                TRUE            1.275720
#> 22                TRUE                TRUE            1.600000
#> 23                TRUE                TRUE            1.130435
#> 24                TRUE                TRUE            1.081081
#> 25                TRUE                TRUE            1.082803
#> 26                TRUE                TRUE            1.182796
#> 27                TRUE                TRUE            1.014994
#> 28                TRUE                TRUE            1.060606
#> 29                TRUE                TRUE            1.666667
#> 30                TRUE                TRUE            1.032258
#> 31                TRUE                TRUE            1.070890
#> 32                TRUE                TRUE            1.165049
#> 33                TRUE                TRUE            1.036269
#>    MinPatch_Proportion
#> 1             2.500000
#> 2             1.151515
#> 3             1.851852
#> 4             1.017049
#> 5             1.000468
#> 6             1.032631
#> 7             1.111111
#> 8             1.015697
#> 9             1.585761
#> 10            1.079109
#> 11            1.057692
#> 12            1.151515
#> 13            1.003745
#> 14            1.004902
#> 15            1.105769
#> 16            1.070833
#> 17            3.333333
#> 18            1.014493
#> 19            1.212121
#> 20            1.333333
#> 21            1.522634
#> 22            1.466667
#> 23            1.159420
#> 24            1.081081
#> 25            1.507431
#> 26            1.182796
#> 27            1.003460
#> 28            1.515152
#> 29            1.666667
#> 30            1.005376
#> 31            1.176471
#> 32            1.197411
#> 33            1.001727

# Print summary statistics
cat("\n=== Feature Change Summary ===\n")
#> 
#> === Feature Change Summary ===
print(comparison$summary)
#>   features_improved features_reduced features_unchanged targets_gained
#> 1                15               12                  6              0
#>   targets_lost
#> 1            0
```

## Run different patch sizes

The minimum patch size parameter is the core constraint that drives
MinPatch behaviour - it determines the threshold below which patches are
considered too small and must be either enlarged or removed.

- During Stage 1, MinPatch removes all patches smaller than this
  threshold (except existing protected areas).
- During Stage 2, it adds new patches large enough to meet this minimum
  when targets are unmet.
- During Stage 3 (whittling), it prevents the removal of planning units
  that would make any patch fall below this threshold.

Larger minimum patch sizes result in fewer, bigger patches with
potentially higher total area, as MinPatch must ensure every patch meets
the size requirement. Smaller minimum patch sizes allow more
flexibility, potentially resulting in more patches that are closer to
the original *prioritizr* solution. The choice of minimum patch size
should reflect ecological or management considerations - for example,
larger patches may be needed to support viable populations or reduce
edge effects, while smaller patches may be acceptable in highly
connected landscapes or for features that don’t require large contiguous
areas.

``` r

# Calculate reasonable parameters based on planning unit characteristics
median_area <- median(st_area(tas))
min_patch_size <- median_area * 10
patch_radius <- sqrt(median_area * 10)

result2 <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = min_patch_size,
  patch_radius = patch_radius,
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  verbose = FALSE
)


median_area <- median(st_area(tas))
min_patch_size <- median_area * 20
patch_radius <- sqrt(median_area * 10)

result3 <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = min_patch_size,
  patch_radius = patch_radius,
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  verbose = FALSE
)
```

### Visualise the minpatch solution

``` r

patchwork::wrap_plots(plot_minpatch(result, title = "Patch Size x5"),
                      plot_minpatch(result2, title = "Patch Size x10"),
                      plot_minpatch(result3, title = "Patch Size x20"),
                      guides = "collect",
                      ncol = 3) &
  theme(legend.position = "bottom")
```

![](minpatchTasmania_files/figure-html/unnamed-chunk-10-1.png)

## Run different Boundary Penalties

The boundary penalty controls how much MinPatch prioritizes spatial
compactness during the “simulated whittling” stage. During whittling,
MinPatch considers removing planning units from patch edges, but only if
doing so doesn’t increase the total *priortizr* cost. The boundary
penalty affects this decision by penalizing fragmented solutions -
higher penalties favour more compact patches by making it costly to
create additional “edge” between selected and unselected areas. When
MinPatch evaluates whether to remove a unit, it calculates the change in
boundary length (units with selected neighbours increase boundary when
removed, while units with unselected neighbours decrease boundary) and
multiplies this by the boundary penalty. If the resulting boundary cost
change exceeds the unit’s cost, the unit cannot be removed. In datasets
like Tasmania with long planning unit boundaries relative to unit costs,
even small boundary penalties can be highly influential, potentially
preventing most unit removals and resulting in similar solutions across
different penalty values. Very small penalties (e.g., 1e-10) may be
needed to see meaningful differences in such cases.

``` r

# Calculate reasonable parameters based on planning unit characteristics
median_area <- median(st_area(tas))
min_patch_size <- median_area * 5
patch_radius <- sqrt(median_area * 10)

result4 <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = min_patch_size,
  patch_radius = patch_radius,
  boundary_penalty = 1,
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  verbose = FALSE
)


result5 <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = min_patch_size,
  patch_radius = patch_radius,
  boundary_penalty = 10, 
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  verbose = FALSE
)
```

### Visualise the final solution

``` r


patchwork::wrap_plots(plot_prioritizr(result2$solution, col = "minpatch", title = "Boundary Penalty: 0"),
                      plot_prioritizr(result4$solution, col = "minpatch", title = "Boundary Penalty: 1"),
                      plot_prioritizr(result5$solution, col = "minpatch", title = "Boundary Penalty: 10"),
                      guides = "collect",
                      ncol = 3) &
  theme(legend.position = "bottom")
```

![](minpatchTasmania_files/figure-html/unnamed-chunk-12-1.png)

### Visualise the differences in the minpatch solution

``` r

patchwork::wrap_plots(plot_minpatch(result2, title = "Boundary Penalty: 0"),
                      plot_minpatch(result4, title = "Boundary Penalty: 1"),
                      plot_minpatch(result5, title = "Boundary Penalty: 10"),
                      guides = "collect",
                      ncol = 3) &
  theme(legend.position = "bottom")
```

![](minpatchTasmania_files/figure-html/unnamed-chunk-13-1.png)
