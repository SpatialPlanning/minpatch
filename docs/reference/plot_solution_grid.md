# Plot solution grid across multiple results

Creates a grid of prioritizr-style selection maps with a shared legend.
Useful for comparing solutions across different boundary penalties or
MinPatch multipliers.

## Usage

``` r
plot_solution_grid(
  results,
  get_solution_fun,
  ncol = 3,
  legend_position = "bottom",
  title_size = 13,
  titles = NULL,
  title_fun = NULL,
  subtitle_values = NULL,
  title_prefix = "",
  value_label_fun = function(x) format(x, scientific = TRUE, trim = TRUE),
  include_baseline = FALSE,
  baseline_solution = NULL,
  baseline_title = NULL
)
```

## Arguments

- results:

  List of MinPatch result objects to plot

- get_solution_fun:

  Function to extract solution sf object from each result

- ncol:

  Number of columns in the grid (default = 3)

- legend_position:

  Position of shared legend (default = "bottom")

- title_size:

  Font size for plot titles (default = 13)

- titles:

  Character vector of titles (one per result). Takes priority over other
  title options.

- title_fun:

  Function(i, results) returning title string. Used if titles is NULL.

- subtitle_values:

  Numeric vector of values (e.g., penalties or multipliers) for titles

- title_prefix:

  Prefix string for subtitle_values titles (e.g., "Boundary penalty = ")

- value_label_fun:

  Function to format subtitle_values (default formats scientifically)

- include_baseline:

  Logical, whether to include a baseline plot (default = FALSE)

- baseline_solution:

  sf object for baseline solution (required if include_baseline = TRUE)

- baseline_title:

  Title for baseline plot

## Value

A patchwork object combining all plots
