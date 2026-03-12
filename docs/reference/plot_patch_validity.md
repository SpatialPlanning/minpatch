# Plot rook-adjacency patches for a MinPatch solution and flag valid/invalid patches

A patch is a rook-connected cluster of selected planning units
(edge-sharing). A patch is "valid" if its area \>= (multiplier x median
PU area), with a small numeric tolerance to avoid floating-point edge
cases.

## Usage

``` r
plot_patch_validity(
  multiplier,
  multipliers,
  minpatch_results,
  pu_sf = NULL,
  return = c("plot", "counts", "patches"),
  do_snap = TRUE,
  snap_size = 1,
  eps_rel = 1e-09,
  debug = FALSE
)
```

## Arguments

- multiplier:

  Numeric. The MinPatch multiplier to plot (must exist in
  \`multipliers\`).

- multipliers:

  Numeric vector of multipliers (same order as \`minpatch_results\`).

- minpatch_results:

  List. Each element contains a \`\$solution\` sf object with column
  \`minpatch\`.

- pu_sf:

  sf. Planning-unit sf used to compute median PU area (defaults to
  Seychelles_sf if present).

- return:

  Character. "plot" (ggplot), "counts" (tibble), or "patches" (sf patch
  polygons).

- do_snap:

  Logical. If TRUE, snap geometries before adjacency for robustness.

- snap_size:

  Numeric. Grid size passed to \`lwgeom::st_snap_to_grid()\` (CRS
  units).

- eps_rel:

  Numeric. Relative tolerance applied to the area threshold (default
  1e-9).

- debug:

  Logical. If TRUE, prints patch table (including diff vs threshold).

## Value

ggplot object, or tibble, or sf patch polygons (see \`return\`).
