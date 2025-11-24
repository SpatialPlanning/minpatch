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
if (FALSE) { # \dontrun{
# Requires ggplot2
library(ggplot2)

# Create example data
example_data <- create_example_data(n_units = 25, n_features = 3)

# Create prioritizr problem and solve
library(prioritizr)
p <- problem(example_data$planning_units, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_manual_targets(example_data$targets) %>%
  add_binary_decisions()
s <- solve(p)

# Run MinPatch
result <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = 2.0,
  patch_radius = 1.5
)

# Visualize results
plot <- plot_minpatch(result)
print(plot)
} # }
```
