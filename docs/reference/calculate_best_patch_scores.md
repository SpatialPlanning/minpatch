# Calculate BestPatch scores for all available planning units

Implements the BestPatch scoring algorithm from the original paper

## Usage

``` r
calculate_best_patch_scores(minpatch_data, feature_amounts, unmet_targets)
```

## Arguments

- minpatch_data:

  List containing all MinPatch data structures

- feature_amounts:

  Named vector of current conservation amounts

- unmet_targets:

  Character vector of features with unmet targets

## Value

Named vector of BestPatch scores
