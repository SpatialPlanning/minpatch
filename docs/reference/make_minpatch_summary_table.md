# Make MinPatch summary table

Builds a single table summarising a baseline solution and multiple
MinPatch solutions. Baseline patch metrics come from
\`compare_solutions(... )\$overall\[,"Original"\]\`, while MinPatch
metrics come from \`compare_solutions(... )\$overall\[,"MinPatch"\]\`.

## Usage

``` r
make_minpatch_summary_table(
  region_sf,
  baseline_solution_sf,
  minpatch_results,
  multipliers,
  compare_solutions_fun = NULL,
  baseline_compare_obj = NULL,
  baseline_elapsed = NA_real_,
  minpatch_elapsed = NULL,
  projected_epsg = 32740,
  baseline_solution_col = NULL,
  minpatch_solution_col = "minpatch",
  cost_col = "cost",
  baseline_boundary_cost = 0,
  make_kable = TRUE
)
```

## Arguments

- region_sf:

  sf of all planning units (used to compute median PU area for min patch
  size).

- baseline_solution_sf:

  sf containing the baseline solution selection column and PU costs.

- minpatch_results:

  List of MinPatch results; each must contain \`\$solution\` (sf).

- multipliers:

  Numeric vector; same length/order as \`minpatch_results\`.

- compare_solutions_fun:

  Function that returns a list with \`\$overall\`. If NULL, the function
  will try to find \`compare_solutions()\` in your session.

- baseline_compare_obj:

  Object to pass into compare_solutions() for baseline "Original"
  metrics. If NULL, defaults to \`minpatch_results\[\[1\]\]\`.

- baseline_elapsed:

  Baseline runtime (seconds). Optional.

- minpatch_elapsed:

  MinPatch runtimes (seconds). Optional; must match \`multipliers\`.

- projected_epsg:

  EPSG for perimeter calcs if inputs are lon/lat (default 32740).

- baseline_solution_col:

  Baseline selection column name (1/0). If NULL, auto-detects.

- minpatch_solution_col:

  MinPatch selection column in \`\$solution\` (default "minpatch").

- cost_col:

  PU cost column in \`baseline_solution_sf\` (default "cost").

- baseline_boundary_cost:

  Baseline boundary cost (default 0).

- make_kable:

  If TRUE, also returns an HTML kable.

## Value

list(data = tibble, kable = html or NULL)
