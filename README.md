
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MinPatch for R

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Windows](https://github.com/SpatialPlanning/minpatch/actions/workflows/Windows.yaml/badge.svg)](https://github.com/SpatialPlanning/minpatch/actions/workflows/Windows.yaml)
[![Ubuntu](https://github.com/SpatialPlanning/minpatch/actions/workflows/Ubuntu.yaml/badge.svg)](https://github.com/SpatialPlanning/minpatch/actions/workflows/Ubuntu.yaml)
[![MacOS](https://github.com/SpatialPlanning/minpatch/actions/workflows/MacOS.yaml/badge.svg)](https://github.com/SpatialPlanning/minpatch/actions/workflows/MacOS.yaml)
[![issues -
zoomss](https://img.shields.io/github/issues/SpatialPlanning/minpatch)](https://github.com/SpatialPlanning/minpatch/issues)
[![Codecov test
coverage](https://codecov.io/gh/SpatialPlanning/minpatch/graph/badge.svg)](https://app.codecov.io/gh/SpatialPlanning/minpatch)
<!-- badges: end -->

**Note: This is still a work in progress and significant bugs may be
present. Use with caution**

More information on the original implemention of MinPatch for Marxan and
QGIS is available here:
<https://cluz-systematic-conservation-planning.github.io>

## Overview

An R implementation of the MinPatch algorithm for post-processing
conservation planning solutions to ensure minimum protected area sizes.

MinPatch is a post-processing tool for conservation planning solutions
that ensures all protected areas meet user-defined minimum size
thresholds. This R package implements the methodology described in Smith
et al. (2010) and is designed to work with solutions from the
`prioritizr` package, though it can work with any binary conservation
solution.

### The Problem

Conservation planning software like Marxan and prioritizr can produce
solutions with many small, fragmented protected areas. While these
solutions may be mathematically optimal, small protected areas are
often:

- Less ecologically viable
- More expensive to manage
- More vulnerable to edge effects
- Less resilient to disturbances

### The Solution

MinPatch addresses this by post-processing conservation solutions
through three stages:

1.  **Remove Small Patches**: Eliminate protected areas smaller than a
    minimum size threshold
2.  **Add New Patches**: Add new areas to meet conservation targets
    using the BestPatch algorithm
3.  **Simulated Whittling**: Remove unnecessary planning units while
    maintaining constraints

## Installation

``` r
# Install from GitHub
pak::pak("SpatialPlanning/minpatch")
```

## Key Features

- **Full MinPatch Algorithm**: Complete implementation of all three
  stages
- **prioritizr Integration**: Seamless workflow with prioritizr
  solutions
- **Flexible Parameters**: Control minimum patch sizes, patch radius,
  and boundary penalties
- **Comprehensive Reporting**: Detailed statistics and comparisons
- **Visualization Support**: Plot results with ggplot2 (optional)
- **Well Documented**: Extensive documentation and examples

## Algorithm Details

### Stage 1: Remove Small Patches

Identifies connected components (patches) in the solution and removes
those smaller than the minimum size threshold. Only removes patches that
weren’t originally designated as conserved areas.

### Stage 2: Add New Patches (BestPatch Algorithm)

Uses the BestPatch scoring system to add new patches:

1.  Calculate current conservation levels for each feature
2.  Identify features with unmet targets
3.  Score potential patches based on their contribution to targets
    relative to cost
4.  Add the highest-scoring patch and repeat

The BestPatch score is calculated as:

    Score = Σ(feature_contribution / target_gap) / patch_cost

### Stage 3: Simulated Whittling

Removes unnecessary planning units through an iterative process:

1.  Identify edge units (on the boundary of selected areas)
2.  Calculate whittling scores based on feature importance
3.  Remove units that don’t violate constraints:
    - Must not cause targets to be unmet
    - Must not make patches too small
    - Must not increase total cost (if boundary penalty \> 0)
    - Must not split patches into non-viable pieces

## Citation

If you use this package, please cite both the original paper and this
implementation:

    Smith, R.J., Di Minin, E., Linke, S., Segan, D.B., Possingham, H.P. (2010). 
    An approach for ensuring minimum protected area size in systematic conservation planning. 
    Biological Conservation, 143(10), 2525-2531.

    Everett J.D., Richardson A.J., Smith R.J. (2025). _minpatch: Post-Processing for Conservation Planning Solutions to Ensure Minimum Patch
      Sizes_. R package version 0.1.0, <https://github.com/SpatialPlanning/minpatch>.

## License

GPL (\>= 3)

## References

Smith, R.J., Di Minin, E., Linke, S., Segan, D.B., Possingham, H.P.
(2010). An approach for ensuring minimum protected area size in
systematic conservation planning. *Biological Conservation*, 143(10),
2525-2531.

## Getting Help

- Check the package vignette: `vignette("minpatch")`
- View function documentation: `?run_minpatch`
- Report bugs: [GitHub
  Issues](https://github.com/SpatialPlanning/minpatch/issues)
