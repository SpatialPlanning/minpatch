# Calculate whittling scores for edge units

Calculates the "Low Relevance" score for each edge unit based on feature
importance (Equation A2 from the original paper)

## Usage

``` r
calculate_whittle_scores(edge_units, minpatch_data)
```

## Arguments

- edge_units:

  Character vector of edge unit IDs

- minpatch_data:

  List containing all MinPatch data structures

## Value

Named vector of whittling scores
